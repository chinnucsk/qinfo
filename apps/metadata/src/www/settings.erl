-module(settings).

-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("metadata/include/metadata.hrl").

-export([main/0, header/0, layout/0, event/1, val_is_integer/1, val_is_log_level/1, validate_intervals/1]).

main() ->
   #template{ file="./www/page.html"}.

header() -> "qinfo:settings".

layout() ->
   TopPanel = #panel{
      body =
      [
         #link{ class=a, text = "main", url = "index"},
         #literal{ text = " | "},
         #literal{ text = "settings"},
         #literal{ text = " | " },
         #link{ text = "instruments", url = "instruments"},
         #literal{ text = " | "},
         #link{ class=a, text = "statistics", url = "statistics"}
      ]
   },
   [
      TopPanel,
      #flash{},
      #p{},
      #button{ id = apply_button, text = "Apply", postback = click_apply },
      #literal{ text = " " },
      #button{ id = save_button, text = "Save", postback = click_save },
      #p{} |  build()
   ].

build() ->
   {atomic, Body} = mnesia:transaction(
      fun() ->
         mnesia:foldr(
            fun(#m_service{service = ServerName, description = Descr, settings = Settings, schedule = Schedules}, Acc) ->
               [
                  #p{},
                  #h3{ text = Descr },
                  #table{ rows = build_settings(ServerName, Settings)},
                  #p{},
                  #h3{ text = "Schedule:" },
                  #table{ rows = build_schedule(ServerName, Schedules)} | Acc ]
            end, [], m_service)
      end
   ),
   Body.

build_schedule(_, []) ->
   [];
build_schedule(ServerName, [{DayOfTheWeek, Status, TimeIntervals}|Tail]) ->
   DoW = erlang:atom_to_list(DayOfTheWeek),
   TextId = create_id(ServerName, "_textbox_" ++ DoW),
   CheckId = create_id(ServerName, "_checkbox_" ++ DoW),
   wf:wire(save_button,
      TextId,
      #validate{ validators = [ #custom{ text = "Invalid time intervals",
               tag = CheckId,
               function = fun ?NODULE:validate_intervals/2}]}),
   wf:wire(apply_button,
      TextId,
      #validate{ validators = [ #custom{ text = "Invalid time intervals",
               tag = CheckId,
               function = fun ?MODULE:validate_intervals/2}]}),
   [
      #tablerow{ cells =
      [
         #tablecell{ body = [
            #checkbox{ id = create_id(ServerName, "_checkbox_" ++ DoW),
               text = DoW, checked = if Status == enabled -> true; true -> false end}]},
         #tablecell{ body = [
            #textbox{ id = TextId, text = TimeIntervals, style="width: 200px;"}]}
      ]} | build_schedule(ServerName, Tail)
   ].

build_settings(_ServerName, []) ->
   [];
build_settings(ServerName, [Setting = #setting{name = Name, value = Value} | Rest]) when is_binary(Value) ->
   Id = create_id(ServerName, Name),
   wf:wire(save_button, Id, #validate{ validators = create_validator(Setting)}),
   wf:wire(apply_button, Id, #validate{ validators = create_validator(Setting)}),
   [
      #tablerow{ cells = [
            #tablecell{ text = Name, valign="top" },
            #tablecell{ body = #textarea{ id = Id, text = Value, style="width: 300px; height: 100px;" }}
         ]
      } | build_settings(ServerName, Rest)
   ];
build_settings(ServerName, [Setting = #setting{name = Name, value = Value} | Rest]) ->
   Id = create_id(ServerName, Name),
   wf:wire(save_button, Id, #validate{ validators = create_validator(Setting)}),
   wf:wire(apply_button, Id, #validate{ validators = create_validator(Setting)}),
   [
      #tablerow{ cells = [
            #tablecell{ text = Name},
            #tablecell{ body = #textbox{ id = Id, text = Value, style="width:300px;"} }
         ]
      } | build_settings(ServerName, Rest)
   ].

create_id({_, ServerName}, FieldName) ->
   Id = atom_to_list(ServerName) ++ "_" ++ FieldName,
   Id2 = lists:foldr(
      fun(Ch, Acc) when Ch == $. ->
            [$_| Acc];
         (Ch, Acc) -> [Ch | Acc]
      end, "", Id),
   case catch(list_to_existing_atom(Id2)) of
      {'EXIT', {badarg, _}} ->
         list_to_atom(Id2);
      Res ->
         Res
   end.

create_validator(#setting{validator = undef}) ->
   [];
create_validator(#setting{description = Descr, validator = Val}) ->
   {ok, M, _} = erl_scan:string(Val),
   {ok, F} = erl_parse:parse_exprs(M),
   {value, Fun, _} = erl_eval:exprs(F, []),
   [ #custom{ text = Descr, function = Fun} ].

event(click_apply) ->
   save(mnesia:dirty_first(m_service)),
   notify(mnesia:dirty_first(m_service)),
   scheduler_srv:reload(),
   wf:flash("saved. Services will be reconfigured.");
event(click_save) ->
   save(mnesia:dirty_first(m_service)),
   wf:flash("saved").

save('$end_of_table') ->
   ok;
save(Key) ->
   [Service = #m_service{service = ServerName, settings = Settings, schedule = Schedule}] = mnesia:dirty_read(m_service, Key),
   NewSettings = get_settings(ServerName, Settings),
   NewSchedule = get_schedules(ServerName, Schedule),
   ok = mnesia:dirty_write(Service#m_service{settings = NewSettings, schedule = NewSchedule,
      fmt_schedule = metadata:format_schedule(NewSchedule)}),
   save(mnesia:dirty_next(m_service, Key)).

get_settings(_ServerName, []) ->
   [];
get_settings(ServerName, [S = #setting{name = Name, value = OldVal} | Rest]) when is_binary(OldVal) ->
   Value = wf:q(create_id(ServerName, Name)),
   [ S#setting{value = list_to_binary(Value)} | get_settings(ServerName, Rest)];
get_settings(ServerName, [S = #setting{name = Name} | Rest]) ->
   Value = wf:q(create_id(ServerName, Name)),
   [ S#setting{value = Value} | get_settings(ServerName, Rest)].

get_schedules(_ServerName, []) ->
   [];
get_schedules(ServerName, [{DayOfTheWeek, _, _}|Tail]) ->
   DoW = atom_to_list(DayOfTheWeek),
   CheckId = create_id(ServerName, "_checkbox_" ++ DoW),
   Status = case wf:q(CheckId) of
      undefined ->
         disabled;
      "on" ->
         enabled
   end,
   TextId = create_id(ServerName, "_textbox_" ++ DoW),
   TimeIntervals = wf:q(TextId),
   [{DayOfTheWeek, Status, TimeIntervals} | get_schedules(ServerName, Tail)].

notify('$end_of_table') ->
   ok;
notify(Key) ->
   [#m_service{service = ServerName}] = mnesia:dirty_read(m_service, Key),
   gen_server:cast(ServerName, reconfigure),
   notify(mnesia:dirty_next(m_service, Key)).

% ================== validators =========================================
val_is_integer(Val) when length(Val) == 0 ->
      false;
val_is_integer(Val) ->
   case catch(list_to_integer(Val)) of
      {'EXIT', {badarg, _}} ->
         false;
      _ ->
         true
   end.

val_is_log_level(Val) when Val == "error" orelse Val == "info" orelse Val == "debug" orelse Val == "warning" ->
   true;
val_is_log_level(_) ->
   false.

validate_intervals(CheckId, TimeIntervals) ->
   Status = case wf:q(CheckId) of
      undefined ->
         disabled;
      "on" ->
         enabled
   end,
   case common_utils:validate_time_intervals(TimeIntervals) of
     {error, _} when Status == enabled ->
        false;
     {error, _} ->
        true;
     ok ->
        true
   end.
