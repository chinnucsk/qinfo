-module(plaza2).

-behaviour(application).

-export([start/2, stop/1]).

start(_Type, Args) ->
   plaza2_sup:start_link(Args).

stop(_) ->
   ok.
