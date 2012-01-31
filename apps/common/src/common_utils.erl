-module(common_utils).

-export([list_to_atom/1, cp1251_to_unicode/1, validate_time_intervals/1, format_time_intervals/1]).

-compile({no_auto_import, [list_to_atom/1]}).

-on_load(load_dll/0).

load_dll() ->
   erlang:load_nif(code:priv_dir(common) ++ "/common_utils", 0).

%=======================================================================================================================
%  public
%=======================================================================================================================

cp1251_to_unicode(_) ->
   exit(nif_library_not_loaded).

list_to_atom(Lst) ->
   case catch(list_to_existing_atom(Lst)) of
      {'EXIT', {badarg, _}} ->
         erlang:list_to_atom(Lst);
      Res ->
         Res
   end.

validate_time_intervals(TimeIntervals) ->
   {ok, Re} = re:compile("^(\s*[0-9]{1,2}\s*:\s*[0-9]{1,2}\s*-\s*[0-9]{1,2}\s*:\s*[0-9]{1,2}\s*,?)+$"),
   case re:run(TimeIntervals, Re) of
      nomatch ->
         {error, invalid_format};
      {match, _} ->
         Intervals = format_time_intervals(TimeIntervals),
         try lists:map(fun(Interval) -> validate_interval(Interval) end, Intervals) of
            _ -> ok
         catch
            throw:Err -> {error, Err}
         end
   end.

format_time_intervals([]) ->
   [];
format_time_intervals(TimeIntervals) ->
   {ok, Re} = re:compile("\s*([0-9]{1,2})\s*:\s*([0-9]{1,2})\s*-\s*([0-9]{1,2})\s*:\s*([0-9]{1,2})\s*"),
   Res = re:split(TimeIntervals, Re, [{return, list}, group, trim]),
   lists:foldr(
     fun([_, H1, M1, H2, M2], Acc) ->
        [{{list_to_integer(H1), list_to_integer(M1)}, {list_to_integer(H2), list_to_integer(M2)}} | Acc]
     end, [], Res).

%=======================================================================================================================
%  private
%=======================================================================================================================

validate_interval({I1 = {HH1, MM1}, I2 = {HH2, MM2}})
   when (HH1 >= 0 andalso HH1 =< 23) andalso (MM1 >= 0 andalso MM1 =< 59) andalso
        (HH2 >= 0 andalso HH2 =< 23) andalso (MM2 >= 0 andalso MM2 =< 59) andalso
         I1 < I2 ->
   ok;
validate_interval(_I) ->
   throw(invalid_interval).

%=======================================================================================================================
%  unit testing
%=======================================================================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

format_time_intervals_test() ->
   Res = format_time_intervals("09 : 30 - 10:45, 10:50-13:45 "),
   ?assertEqual(Res, [
         {{9,30},{10,45}},
         {{10,50},{13,45}}
      ]).

validate_interval_test() ->
   ?assertEqual(validate_interval({{9, 30}, {11, 55}}), ok),
   ?assertThrow(invalid_interval, validate_interval({{11, 55}, {9, 30}})),
   ?assertThrow(invalid_interval, validate_interval({{9, 30}, {29, 30}})),
   ?assertThrow(invalid_interval, validate_interval({{9, 60}, {19, 30}})).

validate_time_intervals_test() ->
   ?assertEqual({error, invalid_format}, validate_time_intervals("09;30-11:30, 11:55-14:20")),
   ?assertEqual({error, invalid_interval}, validate_time_intervals("09:30-11:30, 14:55-14:20")),
   ?assertEqual(ok, validate_time_intervals("09:30-11:30, 14:55-17:20")).

-endif.
