-module(micex_mtesrl_sup).

-behaviour(supervisor).

-export([start_link/1, init/1]).

start_link(Args) ->
   supervisor:start_link({local, micex_mtesrl_sup}, ?MODULE, Args).

init(_Args) ->
   {
      ok,
      {
         {one_for_one, 10, 1},
         [
            {1,{micex_mtesrl_fond, start, []}, permanent, 1000, worker, [micex_mtesrl_fond]}
         ]
      }
   }.
