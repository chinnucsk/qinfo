-module(plaza2_srv).

-behaviour(gen_server).

-export([start/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(plaza2_port_dll, "plaza2_port").

-include("protocol.hrl").
-include("public.hrl").

-define(def_settings, [{ini_file, "P2ClientGate.ini"}, {host, "192.168.1.99"}, {port, 4001}, {app_name, "qinfo"},
      {passwd, "123"}, {log_level, info}]).
-define(def_schedule,
   [
      {mon, {10, 0, 0}, {23, 00, 00}},
      {tue, {10, 0, 0}, {23, 00, 00}},
      {wed, all_day},
      {thu, {10, 0, 0}, {23, 00, 00}},
      {fri, all_day},
      {sat, not_working},
      {sun, not_working}
   ]).

-record(settings, {ini_file, host, port, app_name, passwd, log_level}).
-record(state, {drv_port, settings}).
-record('FORTS_FUTINFO_REPL.fut_sess_contents', {event_name, replID, replRev, replAct, sess_id, isin_id, short_isin,
      isin, name, commodity, limit_up, limit_down, lot_size, expiration, signs}).

%% ========= public ============

start() ->
   gen_server:start_link({global, ?qinfo_plaza2}, ?MODULE, [], []).

init(_Args) ->
   {ok, #service{settings = SList}} = register_service(
      ?qinfo_plaza2, ?def_settings, ?def_schedule),
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

handle_info({_Port, {data, Msg}}, State) ->
   processInfo(binary_to_term(Msg)),
   {noreply, State};

handle_info(Msg, State) ->
   error_logger:error_msg("Unexpected msg: ~p.~n", [Msg]).

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

processInfo(
   #'FORTS_FUTINFO_REPL.fut_sess_contents'{
      event_name = 'StreamDataInserted',
      short_isin = ShortIsin,
      isin = Isin,
      name = Name,
      expiration = Expiration,
      commodity = Commodity,
      signs = Signs,
      limit_up = LUp,
      limit_down = LDown,
      lot_size = LSize}) when (Signs band 16#4 == 0) and (Signs band 16#8 == 0) and (Signs band 16#100 == 0) and (Signs
   band 16#800 == 0) and (Signs band 16#1000 == 0) ->
   Instr = #new_instrument{
      exch = 'RTS',
      class_code = 'SPBFUT',
      short_isin = ShortIsin,
      isin = Isin,
      name = Name,
      expiration = format_datetime(Expiration),
      commodity = Commodity,
      type = future,
      limit_up = LUp,
      limit_down = LDown,
      lot_size = LSize},
   pg:send(?group_rts_instruments, Instr);

processInfo(Msg) ->
   ok.

format_datetime(DateTime) ->
   Year = DateTime div 10000000000000,
   Month = (DateTime - Year * 10000000000000) div 100000000000,
   Day = (DateTime - DateTime div 100000000000 * 100000000000) div 1000000000,
   Hour = (DateTime - DateTime div 1000000000 * 1000000000) div 10000000,
   Min = (DateTime - DateTime div 10000000 * 10000000) div 100000,
   Sec = (DateTime - DateTime div 100000 * 100000) div 1000,
   MSec = DateTime rem 1000,
   {{Year, Month, Day},{Hour, Min, Sec, MSec}}.

extract_settings(SLst) ->
   #settings{
      ini_file = element(2, lists:keyfind(ini_file, 1, SLst)),
      host     = element(2, lists:keyfind(host, 1, SLst)),
      port     = element(2, lists:keyfind(port, 1, SLst)),
      app_name = element(2, lists:keyfind(app_name, 1, SLst)),
      passwd   = element(2, lists:keyfind(passwd, 1, SLst)),
      log_level = element(2, lists:keyfind(log_level, 1, SLst))
   }.
