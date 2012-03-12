-module(settings).

-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("metadata/include/metadata.hrl").

-export([main/0, layout/0, event/1, val_is_integer/1, val_is_log_level/1, validate_intervals/2, title/0]).

title() ->
   "qinfo:settings".

main() ->
   #template{ file="./www/page.html"}.

layout() ->
   TopPanel = #panel{
      body =
      [
         #link{ class=a, text = "main", url = "index"},
         #literal{ text = " | "},
         #literal{ text = "settings"},
         #literal{ text = " | " },
         #link{ text = "scheduler", url = "scheduler"},
         #literal{ text = " | "},
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
            fun(#service{service = ServerName, description = Descr, settings = Settings}, Acc) ->
               [
                  #p{},
                  #h3{ text = Descr },
                  #table{ rows = build_settings(ServerName, Settings)} | Acc]
            end, [], service)
      end
   ),
   Body.

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
   save(),
   notify(),
   wf:flash("saved. Services will be reconfigured.");
event(click_save) ->
   save(),
   wf:flash("saved").

save() ->
   {atomic, _} = mnesia:transaction(
   fun() ->
      mnesia:foldl(
      fun(Service = #service{service = ServerName, settings = Settings}, _) ->
         NewSettings = get_settings(ServerName, Settings),
         ok = mnesia:dirty_write(Service#service{settings = NewSettings})
      end, ok, service)
   end).

get_settings(_ServerName, []) ->
   [];
get_settings(ServerName, [S = #setting{name = Name, value = OldVal} | Rest]) when is_binary(OldVal) ->
   Value = wf:q(create_id(ServerName, Name)),
   [ S#setting{value = list_to_binary(Value)} | get_settings(ServerName, Rest)];
get_settings(ServerName, [S = #setting{name = Name} | Rest]) ->
   Value = wf:q(create_id(ServerName, Name)),
   [ S#setting{value = Value} | get_settings(ServerName, Rest)].

notify() ->
   {atomic, _} = mnesia:transaction(
   fun() ->
      mnesia:foldl(
      fun(#service{service = ServerName}, _) ->
         gen_server:cast(ServerName, reconfigure)
      end, ok, service)
   end).

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
