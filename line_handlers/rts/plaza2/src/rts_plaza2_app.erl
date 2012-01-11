-module(rts_plaza2_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_Type, Args) ->
   rts_plaza2_sup:start_link(Args).

stop(_) ->
   ok.
