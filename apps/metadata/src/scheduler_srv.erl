-module(scheduler_srv).

-behaviour(gen_server).

-export([start/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, reload/0,
         schedule/1]).

-include_lib("common/include/names.hrl").

-record(service, {name, status, schedule_list}).

-record(state, {services = [], timer_ref = undef}).

%=======================================================================================================================
% public
%=======================================================================================================================
reload() ->
   gen_server:cast(?qinfo_scheduler, reload).

schedule(ServiceName) ->
   gen_server:cast(?qinfo_scheduler, {schedule, ServiceName}).

start() ->
   gen_server:start_link({local, ?qinfo_scheduler}, ?MODULE, [], []).

init(_Args) ->
   TRef = erlang:start_timer(1000, self(), check_schedule),
   {ok, #state{timer_ref = TRef}}.

%=======================================================================================================================
handle_call(_Msg, _From, State) ->
   {reply, ok, State}.

%=======================================================================================================================
handle_cast({schedule, ServiceName}, State = #state{services = Services}) ->
   {ServiceName, SchedList} = metadata:get_schedules(ServiceName),
   Now = calendar:local_time(),
   SchedListFmt = format_schedule(SchedList),
   Status = get_status(Now, SchedListFmt),
   gen_server:cast(ServiceName, Status),
   NewServices = case lists:keyfind(ServiceName, 2, Services) of
      false ->
         [#service{name = ServiceName, status = Status, schedule_list = SchedListFmt} | Services];
      Service ->
         lists:keyreplace(ServiceName, 2, Services, Service#service{status = Status, schedule_list = SchedListFmt})
   end,
   {noreply, State#state{services = NewServices}};

handle_cast(reload, State) ->
   Schedules = metadata:get_schedules(),
   Now = calendar:local_time(),
   NewServices = lists:foldl(
      fun({ServiceName, SchedList}, Acc) ->
            SchedListFmt = format_schedule(SchedList),
            Status = get_status(Now, SchedListFmt),
            gen_server:cast(ServiceName, Status),
            [#service{name = ServiceName, status = Status, schedule_list = SchedListFmt} | Acc]
      end, [], Schedules),
   {noreply, State#state{services = NewServices}};

handle_cast(Msg, State) ->
   error_logger:warning_msg("Unexpected message: ~p.~n", [Msg]),
   {noreply, State}.

%=======================================================================================================================
handle_info({timeout, _, check_schedule}, State = #state{services = Services}) ->
   NewServices = check(Services),
   {noreply, State#state{services = NewServices}};

handle_info(Msg, State) ->
   error_logger:error_msg("Unexpected message: ~p.~n", [Msg]),
   {noreply, State}.

%=======================================================================================================================
terminate(Reason, #state{timer_ref = TRef}) ->
   erlang:cancel_timer(TRef),
   error_logger:info_msg("Terminate. Reason = ~p.~n", [Reason]),
   ok.

%=======================================================================================================================
code_change(_OldVsn, State, _Extra) ->
   {ok, State}.

%=======================================================================================================================
% private
%=======================================================================================================================

check(Services) ->
   Now = calendar:local_time(),
   lists:foldl(
      fun(S = #service{name = ServiceName, status = OldStatus, schedule_list = SchedList}, Acc) ->
         NewStatus = get_status(Now, SchedList),
         if OldStatus =/= NewStatus ->
               gen_server:cast(ServiceName, NewStatus),
               [ S#service{status = NewStatus} | Acc];
            true ->
               [S | Acc]
         end
      end, [], Services).

get_status({Date, {H, M, _}}, SchedList) ->
   DayOfTheWeek = calendar:day_of_the_week(Date),
   Now = {H, M},
   InService = lists:foldl(
      fun({DoW, enabled, TimeIntervals}, Res) when DayOfTheWeek == DoW ->
         Res or lists:foldl(
            fun({I1, I2}, Res1) ->
               Res1 or (I1 >= Now andalso Now =< I2)
            end, false, TimeIntervals);
         (_, Res) ->
            Res
      end, false, SchedList),
   if InService == true -> online; true -> offline end.

format_schedule([]) ->
   [];
format_schedule([{DayOfWeek, Status, TimeIntervals}|Tail]) ->
   [{day_to_num(DayOfWeek), Status, common_utils:format_time_intervals(TimeIntervals)} | format_schedule(Tail)].

day_to_num(mon) -> 1;
day_to_num(tue) -> 2;
day_to_num(wed) -> 3;
day_to_num(thu) -> 4;
day_to_num(fri) -> 5;
day_to_num(sat) -> 6;
day_to_num(sun) -> 7.

%=======================================================================================================================
%  unit testing
%=======================================================================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-endif.
