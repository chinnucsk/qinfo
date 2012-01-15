-module(metadata_app).
-behaviour(application).
-export([start/2, stop/1, do/1]).
-include_lib("nitrogen_core/include/wf.hrl").

start(_Type, Args) ->
   inets:start(),
   {ok, Pid} = inets:start(httpd, [
      {port, get_port()},
      {server_name, "qinfo"},
      {server_root, "."},
      {document_root, "."},
      {modules, [?MODULE]},
      {mime_types, [{"css", "text/css"}, {"js", "text/javascript"}, {"html", "text/html"}]}
   ]),
   link(Pid),
   metadata_sup:start_link(Args).

stop(_) ->
   httpd:stop_service({any, get_port()}),
   ok.

do(Info) ->
    RequestBridge = simple_bridge:make_request(inets_request_bridge, Info),
    ResponseBridge = simple_bridge:make_response(inets_response_bridge, Info),
    nitrogen:init_request(RequestBridge, ResponseBridge),
    nitrogen:run().

get_port() ->
   case application:get_env(metadata, inets_port) of
      undefined ->
         8000;
      {ok, Val} ->
         Val
   end.
