-module(common_utils).

-export([list_to_atom/1, cp1251_to_unicode/1]).

-compile({no_auto_import, [list_to_atom/1]}).

-on_load(load_dll/0).

load_dll() ->
   erlang:load_nif(code:priv_dir(common) ++ "/common_utils", 0).

cp1251_to_unicode(_) ->
   exit(n)if_library_not_loaded).

list_to_atom(Lst) ->
   case catch(list_to_existing_atom(Lst)) of
      {'EXIT', {badarg, _}} ->
         erlang:list_to_atom(Lst);
      Res ->
         Res
   end.
