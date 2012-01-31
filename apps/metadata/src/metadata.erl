-module(metadata).

-include_lib("common/include/names.hrl").

-export([register_service/4, get_settings/1, get_instruments/2, get_schedules/0, get_schedules/1, format_schedule/1]).

%=======================================================================================================================
%  public
%=======================================================================================================================

register_service(ServiceName, Description, Settings, Schedule) ->
   gen_server:call({global, ?qinfo_metadata}, {register, ServiceName, Description, Settings, Schedule}).

get_settings(ServiceName) ->
   gen_server:call({global, ?qinfo_metadata}, {get_settings, ServiceName}).

get_instruments(Exchange, OnlyEnabled) ->
   gen_server:call({global, ?qinfo_metadata}, {get_instruments, Exchange, OnlyEnabled}).

get_schedules() ->
   gen_server:call({global, ?qinfo_metadata}, get_schedules).

get_schedules(ServiceName) ->
   gen_server:call({global, ?qinfo_metadata}, {get_schedules, ServiceName}).

format_schedule([]) ->
   [];
format_schedule([{DayOfWeek, Status, TimeIntervals}|Tail]) ->
   [{day_to_num(DayOfWeek), Status, common_utils:format_time_intervals(TimeIntervals)} | format_schedule(Tail)].

%=======================================================================================================================
%  private
%=======================================================================================================================

day_to_num(mon) -> 1;
day_to_num(tue) -> 2;
day_to_num(wed) -> 3;
day_to_num(thu) -> 4;
day_to_num(fri) -> 5;
day_to_num(sat) -> 6;
day_to_num(sun) -> 7.
