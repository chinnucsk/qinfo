-module(metadata_srv_test).

-include_lib("eunit/include/eunit.hrl").

-compile([export_all]).

setup_test() ->
   metadata_srv:start(),
   timer:sleep(1000),
   ok.

tear_down_test() ->
   ok.
