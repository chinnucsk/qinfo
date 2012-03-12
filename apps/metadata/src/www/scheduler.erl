-module(scheduler).

-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("metadata/include/metadata.hrl").

-export([main/0, layout/0, event/1, title/0]).

title() ->
   "qinfo:scheduler".

main() ->
   #template{ file="./www/page.html"}.

layout() ->
   TopPanel = #panel{
      body =
      [
         #link{ class=a, text = "main", url = "index"},
         #literal{ text = " | "},
         #link{ class=a, text = "settings", url = "settings"},
         #literal{ text = " | " },
         #literal{ class=a, text = "scheduler"},
         #literal{ text = " | " },
         #link{ class=a, text = "instruments", url = "instruments"},
         #literal{ text = " | "},
         #link{ class=a, text = "statistics", url = "statistics"}
      ]
   },
   [
      TopPanel,
      #flash{},
      #p{},
      #button{ id = reschedule_button, text = "Reschedule", postback = click_reschedule },
      #p{} |
      build()
   ].

build() ->
   {atomic, Body} = mnesia:transaction(
      fun() ->
         mnesia:foldr(
            fun(#service{service = ServerName, description = Descr, schedule = Schedules}, Acc) ->
               [
                  #h3{ text = Descr },
                  #table{ rows = build_schedule(ServerName, Schedules)} | Acc ]
            end, [], service)
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
               function = fun ?MODULE:validate_intervals/2}]}),
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

event(click_reschedule) ->
   save(),
   scheduler_srv:reload(),
   wf:flash("Saved. Services will be rescheduled.").

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

save() ->
   {atomic, _} = mnesia:transaction(
   fun() ->
      mnesia:foldl(
      fun(Service = #service{service = ServerName, schedule = Schedule}, _) ->
         NewSchedule = get_schedules(ServerName, Schedule),
         ok = mnesia:dirty_write(Service#service{schedule = NewSchedule})
      end, ok, service)
   end).
