-module(metadata_srv_SUITE).

-include_lib("metadata/include/metadata.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("qinfo_common/include/names.hrl").

-compile([export_all]).

-define(new_instrument(Exch, ClassCode, Name, FullName, Commodity, Type),
        #new_instrument{
            exchange = Exch,
            class_code = ClassCode,
            name = Name,
            full_name = FullName,
            expiration = {{2012,06,10},{0,0,0}},
            commodity = Commodity,
            type = Type,
            limit_up = 10000,
            limit_down = 20000,
            lot_size = 100,
            ref = 1000000001}).

all() ->
   [insert_test].

init_per_suite(Config) ->
   {ok, _Pid} = metadata_srv:test_start(),
   Config.

end_per_unit(Config) ->
   Config.

init_per_testcase(_TestCase, Config) ->
   Config.

end_per_testcase(_TestCase, Config) ->
   Config.

insert_test(_Config) ->
   pg:send( ?group_rts_instruments, ?new_instrument("RTS", 'RTS_STD', "LKOH", "Lukoil", "LKOH", spot)),
   pg:send( ?group_rts_instruments, ?new_instrument("RTS", 'RTS_STD', "SBER", "Sberbank", "SBER", spot)),
   timer:sleep(100),
   io:format("~p~n", [metadata:get_instruments("RTS", false)]),
   ok.
