-module(micex_mtesrl_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_Type, Args) ->
   micex_mtesrl_sup:start_link(Args).

stop(_) ->
   ok.
