-module(metadata).

-include_lib("common/include/names.hrl").

-export([register_service/4, get_settings/1, get_instruments/2, get_schedules/0]).

register_service(ServiceName, Description, Settings, Schedule) ->
   gen_server:call({global, ?qinfo_metadata}, {register, ServiceName, Description, Settings, Schedule}).

get_settings(ServiceName) ->
   gen_server:call({global, ?qinfo_metadata}, {get_settings, ServiceName}).

get_instruments(Exchange, OnlyEnabled) ->
   gen_server:call({global, ?qinfo_metadata}, {get_instruments, Exchange, OnlyEnabled}).

get_schedules() ->
   gen_server:call({global, ?qinfo_metadata}, get_schedules).
