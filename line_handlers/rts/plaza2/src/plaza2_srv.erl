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
   error_logger:info_msg("~p settings: ~p.~n", [?qinfo_plaza2, Settings]),
   ok = erl_ddll:load_driver(".", ?plaza2_port_dll),
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
   error_logger:warning_msg("Unexpected message: ~p~n", [Msg]),
   {noreply, State}.

handle_info(Info, State) ->
   processInfo(Info),
   {noreply, State}.

terminate(Reason, #state{drv_port = DrvPort}) ->
   error_logger:info_msg("Terminate. Reason = ~p.~n", [Reason]),
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.

%% ========= private ===========

open(IniFile, Host, Port, AppName, Password, LogLevel, Streams) ->
   DrvPort = erlang:open_port({spawn, ?plaza2_port_dll}, [binary]),
   erlang:port_command(DrvPort, term_to_binary({connect, IniFile, Host, Port, AppName, Password, LogLevel, Streams})),
   DrvPort.

close(DrvPort) ->
   erlang:port_command(DrvPort, term_to_binary({disconnect})),
   port_close(DrvPort).

processInfo({_Port, {data, Msg}}) ->
   error_logger:info_msg("~p: Msg = ~p.~n", [?qinfo_plaza2, binary_to_term(Msg)]);

%processInfo(#'FORTS_FUTINFO_REPL.fut_sess_contents'{name = Name, isin = Isin, instr_term}, #state{instr_proc = InstrProc}) ->
%   InstrProc ! #instrument{exch = 'RTS', class_code = 'SPBFUT', name = Name, isin = isin, issuer = ""};

processInfo(Msg) ->
   error_logger:error_msg("Unexpected msg: ~p.~n", [Msg]).

extract_settings(SLst) ->
   #settings{
      ini_file = element(2, lists:keyfind(ini_file, 1, SLst)),
      host     = element(2, lists:keyfind(host, 1, SLst)),
      port     = element(2, lists:keyfind(port, 1, SLst)),
      app_name = element(2, lists:keyfind(app_name, 1, SLst)),
      passwd   = element(2, lists:keyfind(passwd, 1, SLst)),
      log_level = element(2, lists:keyfind(log_level, 1, SLst))
   }.
