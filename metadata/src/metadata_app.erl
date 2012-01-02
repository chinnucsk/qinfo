-module(metadata_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_Type, Args) ->
   metadata_sup:start_link(Args).

stop(_) ->
   ok.
