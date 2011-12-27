-define(group_rts_instruments,   'RTS.instruments').
-define(group_rts_bba,           'RTS.bba').
-define(group_rts_trades,        'RTS.trades').

-define(qinfo_metadata,     'qinfo.metadata').
-define(qinfo_plaza2,       'qinfo.plaza2').
-define(qinfo_micex_mtesrl, 'qinfo.micex_mtesrl').

register_service(ServiceName, Settings) ->
   gen_server:call({global, ?qinfo_metadata}, {register, ServiceName, Settings}).

get_settings(ServiceName) ->
   gen_server:call({global, ?qinfo_metadata}, {get_settings, ServiceName}).
