-include("names.hrl")

register_service(ServiceName, Settings, Schedule) ->
   gen_server:call({global, ?qinfo_metadata}, {register, ServiceName, Settings, Schedule}).

get_settings(ServiceName) ->
   gen_server:call({global, ?qinfo_metadata}, {get_settings, ServiceName}).

get_instruments(Exchange, OnlyEnabled) ->
   gen_server:call({global, ?qinfo_metadata}, {get_instruments, Exchange, OnlyEnabled}).
