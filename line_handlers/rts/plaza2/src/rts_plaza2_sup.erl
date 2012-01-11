-module(rts_plaza2_sup).

-behaviour(supervisor).

-export([start_link/1, init/1]).

start_link(Args) ->
   supervisor:start_link({local, rts_plaza2_sup}, ?MODULE, Args).

init(_Args) ->
   {
      ok,
      {
         {one_for_one, 10, 1},
         [
            {1,{rts_plaza2_srv, start, []}, permanent, 1000, worker, [rts_plaza2_srv]}
         ]
      }
   }.
