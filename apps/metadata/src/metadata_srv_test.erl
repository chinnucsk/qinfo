-module(metadata_srv_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("metadata/include/metadata.hrl").
-include_lib("qinfo_common/include/names.hrl").

-compile([export_all]).

setup_test() ->
   metadata_srv:start(),
   timer:sleep(100),
   ok.

insert_test() ->
   Instr = #new_instrument{
      exchange = "RTS",
      class_code = 'RTS_STD',
      name = "LKOH",
      full_name = "Lukoil",
      expiration = {{2012, 06, 10}, {0,0,0}},
      commodity = "LKOH",
      type = spot,
      limit_up = 10000,
      limit_down = 20000,
      lot_size = 100,
      ref = 1000000001},
   pg:send(?group_rts_instruments, Instr),
   ?debugFmt("~p~n", metadata:get_instruments("RTS", false)).

tear_down_test() ->
   ok.
