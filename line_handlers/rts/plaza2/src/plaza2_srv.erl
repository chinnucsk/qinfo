-module(plaza2_srv).

-behaviour(gen_server).

-export([start/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(plaza2_port_dll, "plaza2_port").

-record(state, {drv_port}).
-record('FORTS_FUTINFO_REPL.fut_sess_contents', {event_name, isin_id, short_isin, isin, name, instr_term, code_vbc}).

%% ========= public ============

start() ->
   gen_server:start_link({global, plaza2_srv}, ?MODULE, [], []).

init(_Args) ->
   DrvPort = open("P2ClientGate.ini", "osmds.ru", 4001, "Application1", "123", log_debug,
      [
         {"FORTS_FUTINFO_REPL", "fut_info.ini", 'RT_COMBINED_DYNAMIC'}
      ],
   {ok, #state{drv_port = DrvPort}}.

handle_call(Request, _From, State) ->
   io:format("~p~n", [Request]),
   {reply, undef, State}.

handle_cast(Request, State) ->
   io:format("~p~n", [Request]),
   {noreply, State}.

handle_info(Info, State) ->
   processInfo(Info),
   {noreply, State}.

terminate(Reason, _State) ->
   io:format("~p~n", [Reason]),
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
   end,

close(DrvPort) ->
   disconnect(DrvPort),
   port_close(DrvPort),
   erl_ddll:unload(?plaza2_port_dll).

connect(DrvPort, IniFile, Host, Port, AppName, Password, LogLevel, Streams) ->
   erlang:port_command(DrvPort, term_to_binary({connect, IniFile, Host, Port, AppName, Password, LogLevel, Streams})).

disconnect(DrvPort) ->
   erlang:port_command(DrvPort, term_to_binary({disconnect})).

processInfo(#'FORTS_FUTINFO_REPL.fut_sess_contents'{isin = Isin}) ->
   io:format("~ts~n", [Isin]),
   ok;
processInfo(_Msg) ->
   ok.
