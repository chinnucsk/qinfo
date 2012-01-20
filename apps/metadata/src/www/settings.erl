-module(settings).

-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("metadata/include/metadata.hrl").

-export([main/0, header/0, layout/0, event/1, val_is_integer/1, val_is_log_level/1]).

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
   Body = build([], mnesia:dirty_first(m_service)),
   [
      TopPanel,
      #p{},
      #button{ id = apply_button, text = "Apply", postback = click_apply },
      #literal{ text = " " },
      #button{ id = save_button, text = "Save", postback = click_save },
      #p{}|
      Body
   ].

build(Body, '$end_of_table') ->
   Body;
build(Body, Key) ->
   [#m_service{service = ServerName, description = Descr, enabled = Enabled, settings = Settings}] = mnesia:dirty_read(m_service, Key),
   Id = create_id(ServerName, "checkbox_enabled"),
   build(
      [
         #p{},
         #h3{ text = Descr },
         #table{ rows = [
            #tablerow{ cells = [
               #tablecell{ text = "Enabled"},
               #tablecell{ body = #checkbox{ id = Id, checked = Enabled}}]},
            build_settings(ServerName, Settings) ]}|
         Body
      ], mnesia:dirty_next(m_service, Key)
   ).

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
   wf:flash("saved. Services will be reconfigured.");
event(click_save) ->
   save(mnesia:dirty_first(m_service)),
   wf:flash("saved").

save('$end_of_table') ->
   ok;
save(Key) ->
   [Service = #m_service{service = ServerName, settings = Settings}] = mnesia:dirty_read(m_service, Key),
   NewSettings = get_settings(ServerName, Settings),
   Enabled = wf:q(create_id(ServerName, "checkbox_enabled")),
   ok = mnesia:dirty_write(Service#m_service{enabled = if Enabled == "on" -> true; true -> false end, settings = NewSettings}),
   save(mnesia:dirty_next(m_service, Key)).

get_settings(_ServerName, []) ->
   [];
get_settings(ServerName, [S = #setting{name = Name, value = OldVal} | Rest]) when is_binary(OldVal) ->
   Value = wf:q(create_id(ServerName, Name)),
   [ S#setting{value = list_to_binary(Value)} | get_settings(ServerName, Rest)];
get_settings(ServerName, [S = #setting{name = Name} | Rest]) ->
   Value = wf:q(create_id(ServerName, Name)),
   [ S#setting{value = Value} | get_settings(ServerName, Rest)].

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
