-module(metadata_sup).

-behaviour(supervisor).

-export([start_link/1, init/1]).

start_link(Args) ->
   supervisor:start_link({local, metadata_sup}, ?MODULE, Args).

init(_Args) ->
   {
      ok,
      {
         {one_for_one, 10, 1},
         [
            {1, {metadata_srv,  start, []}, permanent, 1000, worker, [metadata_srv]},
            {2, {scheduler_srv, start, []}, permanent, 1000, worker, [scheduler_srv]}
         ]
      }
   }.
