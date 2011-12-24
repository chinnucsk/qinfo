-module(plaza2_sup).

-behaviour(supervisor).

-export([start_link/1, init/1]).

start_link(Args) ->
   supervisor:start_link({local, plaza2_sup}, ?MODULE, Args).

init(_Args) ->
   {
      ok,
      {
         {one_for_one, 10, 1},
         [
            {1,{plaza2_srv, start, []}, permanent, 1000, worker, [plaza2_srv]}
         ]
      }
   }.
