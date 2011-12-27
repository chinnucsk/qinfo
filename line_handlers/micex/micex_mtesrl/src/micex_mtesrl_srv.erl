-module(micex_mtesrl_srv).

-behaviour(gen_server).

-export([start/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("protocol.hrl").
-include("public.hrl").

-record(settings, {host, port, log_level}).
-record(state, {settings}).

%% ========= public ============

start() ->
   gen_server:start_link({global, ?qinfo_micex_mtesrl}, ?MODULE, [], []).

init(_Args) ->
   ok = register_service(?qinfo_micex_mtesrl, [{host, "localhost"}, {port, 4001}, {log_level, debug}]),
   {ok, #state{}}.

handle_call(Msg, _From, State) ->
   error_logger:warning_msg("Unexpected message: ~p", [Msg]),
   {reply, undef, State}.

handle_cast(S = #service{settings = SLst}, State) ->
   Settings = extract_settings(SLst),
   error_logger:debug_msg("Settings ~p received.", [S]),
   {noreply, State#state{settings = Settings}};

handle_cast(Msg, State) ->
   error_logger:warning_msg("Unexpected message: ~p", [Msg]),
   {noreply, State}.

handle_info(_Info, State) ->
   {noreply, State}.

terminate(Reason, _State) ->
   error_logger:info_msg("Terminate. Reason = ~p", [Reason]),
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.

extract_settings(SLst) ->
   #settings{
      %ini_file = lists:keyfind(ini_file, 1, SLst),
      host     = lists:keyfind(host, 1, SLst),
      port     = lists:keyfind(port, 1, SLst),
      %app_name = lists:keyfind(app_name, 1, SLst),
      %passwd   = lists:keyfind(passwd, 1, SLst),
      log_level = lists:keyfind(log_level, 1, SLst)
   }.
