-module(plaza2_srv).

-behaviour(gen_server).

-export([start/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(plaza2_port_dll, "plaza2_port").

-include("protocol.hrl").
-include("public.hrl").

-record(settings, {ini_file, host, port, app_name, passwd, log_level}).
-record(state, {drv_port, settings}).
-record('FORTS_FUTINFO_REPL.fut_sess_contents', {event_name, isin_id, short_isin, isin, name, instr_term, code_vbc}).

%% ========= public ============

start() ->
   gen_server:start_link({global, ?qinfo_plaza2}, ?MODULE, [], []).

init(_Args) ->
   {ok, #service{settings = SList}} = register_service(
      ?qinfo_plaza2, [{ini_file, "P2ClientGate.ini"}, {host, "192.168.1.99"}, {port, 4001}, {app_name, "qinfo"}, {passwd, "123"}, {log_level, debug}]),
   Settings = extract_settings(SList),
   DrvPort = open(Settings#settings.ini_file, Settings#settings.host, Settings#settings.port, Settings#settings.app_name,
      Settings#settings.passwd, Settings#settings.log_level,
      [
         {"FORTS_FUTINFO_REPL", "fut_info.ini", 'RT_COMBINED_DYNAMIC'}
      ]
   ),
   {ok, #state{drv_port = DrvPort, settings = Settings}}.

handle_call(Request, _From, State) ->
   io:format("~p~n", [Request]),
   {reply, undef, State}.

handle_cast(Msg, State) ->
   error_logger:warning_msg("Unexpected message: ~p", [Msg]),
   {noreply, State}.

handle_info(Info, State) ->
   processInfo(Info),
   {noreply, State}.

terminate(Reason, _State) ->
   error_logger:info_msg("Terminate. Reason = ~p", [Reason]),
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.

%% ========= private ===========

open(IniFile, RouterHost, RouterPort, AppName, Password, LogLevel, Streams) ->
   case erl_ddll:load_driver("../priv", ?plaza2_port_dll)  of
      ok ->
         Port = open_port({spawn, ?plaza2_port_dll}, [binary]),
         connect(Port, IniFile, RouterHost, RouterPort, AppName, Password, LogLevel, Streams),
         {ok, Port};
      Res ->
         Res
   end.

close(DrvPort) ->
   disconnect(DrvPort),
   port_close(DrvPort),
   erl_ddll:unload(?plaza2_port_dll).

connect(DrvPort, IniFile, Host, Port, AppName, Password, LogLevel, Streams) ->
   erlang:port_command(DrvPort, term_to_binary({connect, IniFile, Host, Port, AppName, Password, LogLevel, Streams})).

disconnect(DrvPort) ->
   erlang:port_command(DrvPort, term_to_binary({disconnect})).

processInfo(Msg) ->
   io:format("~p~n", [Msg]);

%processInfo(#'FORTS_FUTINFO_REPL.fut_sess_contents'{name = Name, isin = Isin, instr_term}, #state{instr_proc = InstrProc}) ->
%   InstrProc ! #instrument{exch = 'RTS', class_code = 'SPBFUT', name = Name, isin = isin, issuer = ""};

processInfo(_Msg) ->
   ok.

extract_settings(SLst) ->
   #settings{
      ini_file = lists:keyfind(ini_file, 1, SLst),
      host     = lists:keyfind(host, 1, SLst),
      port     = lists:keyfind(port, 1, SLst),
      app_name = lists:keyfind(app_name, 1, SLst),
      passwd   = lists:keyfind(passwd, 1, SLst),
      log_level = lists:keyfind(log_level, 1, SLst)
   }.
