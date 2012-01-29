-module(scheduler_srv).

-behaviour(gen_server).

-export([start/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, reload/0]).

-include_lib("common/include/names.hrl").

-record(service, {name, status, schedule_list}).

-record(state, {services, timer_ref = undef}).

%=======================================================================================================================
% public
%=======================================================================================================================
reload() ->
   gen_server:call({local, ?qinfo_scheduler}, reload).

start() ->
   gen_server:start_link({local, ?qinfo_scheduler}, ?MODULE, [], []).

init(_Args) ->
   Schedules = metadata:get_schedules(),
   Services = lists:foldl(fun({ServiceName, SchedList}, Acc) ->
         [#service{name = ServiceName, status = offline, schedule_list = SchedList} | Acc]
      end, [], Schedules),
   TRef = erlang:start_timer(1000, self(), check_schedule),
   {ok, #state{services = Services, timer_ref = TRef}}.

%=======================================================================================================================
handle_call(_Msg, _From, State) ->
   {reply, ok, State}.

%=======================================================================================================================
handle_cast(reload, State = #state{services = OldServices}) ->
   Schedules = metadata:get_schedules(),
   NewServices = lists:foldl(
      fun({ServiceName, SchedList}, Acc) ->
         #service{status = Status} = lists:keyfind(ServiceName, 2, OldServices),
         [#service{name = ServiceName, status = Status, schedule_list = SchedList} | Acc]
      end, [], Schedules),
   NewServices2 = check(NewServices),
   {noreply, State#state{services = NewServices2}};

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
      fun(S = #service{name = ServiceName, status = Status, schedule_list = SchedList}, Acc) ->
         InService = is_in_service(Now, SchedList),
         if Status == offline andalso InService == true ->
               ServiceName ! online,
               [ S#service{status = online} | Acc];
            Status == online andalso InService == false ->
               ServiceName ! offline,
               [ S#service{status = offline} | Acc];
            true ->
               [S | Acc]
         end
      end, [], Services).

is_in_service({Date, {H, M, _}}, SchedList) ->
   DayOfTheWeek = calendar:day_of_the_week(Date),
   Now = {H, M},
   lists:foldl(
      fun({DoW, enabled, TimeIntervals}, Res) when DayOfTheWeek == DoW ->
         Res bor lists:foldl(
            fun({{I1}, {I2}}, Res1) ->
               Res1 bor (I1 >= Now andalso Now =< I2)
            end, false, TimeIntervals)
      end, false, SchedList).

%=======================================================================================================================
%  unit testing
%=======================================================================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-endif.
