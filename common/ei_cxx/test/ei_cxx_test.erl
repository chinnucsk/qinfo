-module(ei_cxx_test).

-define(shared_lib, "libei_cxx_test").

-include_lib("eunit/include/eunit.hrl").

-compile([export_all]).

-define(test_data(Num, Term), <<Num:8/integer, (term_to_binary(Term))/binary>>).

test_eq(Pattern) ->
   receive
      {_, {data, Msg}} ->
         ?assertEqual(Pattern, binary_to_term(Msg))
   end.

init_test() ->
   case erl_ddll:load_driver(".", ?shared_lib) of
      ok ->
         Port = open_port({spawn, ?shared_lib}, [binary]),
         register(ei_cxx_test, Port);
      {error, already_loaded} ->
         ok;
      Err ->
         exit(Err)
   end.

test1_test() ->
   erlang:port_command(ei_cxx_test, ?test_data(1, 100)),
   test_eq(200).

test2_test() ->
   erlang:port_command(ei_cxx_test, ?test_data(2, "Test string")),
   test_eq("Test string_REPLY").

test3_test() ->
   erlang:port_command(ei_cxx_test, ?test_data(3, test_atom)),
   test_eq(test_atom_REPLY).

test4_test() ->
   erlang:port_command(ei_cxx_test, ?test_data(4, 123.345)),
   test_eq(323.345).

test5_test() ->
   erlang:port_command(ei_cxx_test, ?test_data(5, [1,2,3,4])),
   test_eq([1,2,3,4,100,200,300]).

test6_test() ->
   erlang:port_command(ei_cxx_test, ?test_data(6, -10000)),
   test_eq(-20000).

test7_test() ->
   erlang:port_command(ei_cxx_test, ?test_data(7, [10000, 20000, 30000, 40000])),
   test_eq([10000, 20000, 30000, 40000, 100, 200, 300]).

test8_test() ->
   erlang:port_command(ei_cxx_test, ?test_data(8, ["abc", "123", "ABC", "TEST"])),
   test_eq(["TEST", "ABC", "123", "abc"]).

test9_test() ->
   erlang:port_command(ei_cxx_test, ?test_data(9, [100, "abc", 123.45, just_an_atom])),
   test_eq([just_an_atom, 123.45, "abc", 100]).

test10_test() ->
   erlang:port_command(ei_cxx_test, ?test_data(10, [{1, 2}, {"abc", "123"}, {atom1, atom2}])),
   test_eq({1, 2, "abc", "123", atom1, atom2}).

test11_test() ->
   erlang:port_command(ei_cxx_test, ?test_data(11, <<"test_binary">>)),
   test_eq(<<"test_binary">>).

test12_test() ->
   erlang:port_command(ei_cxx_test, ?test_data(12, lists:seq(1, 2000))),
   test_eq(lists:seq(2000, 1, -1)).

test13_test() ->
   erlang:port_command(ei_cxx_test, ?test_data(13, [<<0:2048>>, <<1:1024>>])),
   test_eq([<<1:1024>>, <<0:2048>>]).

test14_test() ->
   erlang:port_command(ei_cxx_test, ?test_data(14, <<1, 2, 3, 4:3>>)),
   test_eq(<<1, 2, 3, 4>>).
