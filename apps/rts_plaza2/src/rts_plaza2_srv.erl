-module(rts_plaza2_srv).

-behaviour(gen_server).

-export([start/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(plaza2_driver_dll, "plaza2_driver").
-define(serverName, {global, ?qinfo_rts_plaza2}).

-include_lib("common/include/names.hrl").
-include_lib("metadata/include/metadata.hrl").

-define(def_settings, [
      {"LogLevel", "info", "possible values are: info, debug, warning, error"}
      {"Host", "192.168.1.99", "Plaza2 router host"},
      {"Port", "4001", "Plaza2 router port"},
      {"Application", "qinfo", "name of this application"},
      {"Password", "123", "should be set if router is on another box"}
   ]).

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

-record(settings, {host, port, app_name, passwd, log_level}).
-record(state, {drv_port, settings}).
-record('FORTS_FUTINFO_REPL.fut_sess_contents', {event_name, replID, replRev, replAct, sess_id, isin_id, short_isin,
      isin, name, commodity, limit_up, limit_down, lot_size, expiration, signs}).

%% ========= public ============

start() ->
   gen_server:start_link(?server_name, ?MODULE, [], []).

init(_Args) ->
   {ok, #service{settings = SList}} = metadata:register_service(
      ?serverName, "RTS Plaza2 market data", ?def_settings, ?def_schedule),
   Settings = extract_settings(SList),
   error_logger:info_msg("~p settings: ~p.~n", [?qinfo_rts_plaza2, Settings]),
   IniDir = code:lib_dir(rts_plaza2) ++ "/ini/",
   load_dll(),
   DrvPort = open(IniDir ++ "P2ClientGate.ini", Settings#settings.host, Settings#settings.port, Settings#settings.app_name,
      Settings#settings.passwd, Settings#settings.log_level,
      [
         {"FORTS_FUTINFO_REPL", IniDir ++ "fut_info.ini", 'RT_COMBINED_DYNAMIC'}
      ]
   ),
   {ok, #state{drv_port = DrvPort, settings = Settings}}.

handle_call(Msg, _From, State) ->
   error_logger:error_msg("Unexpected message: ~p~n", [Msg]),
   {reply, undef, State}.

handle_cast(Msg, State) ->
   error_logger:error_msg("Unexpected message: ~p~n", [Msg]),
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

load_dll() ->
   case erl_ddll:load_driver(code:priv_dir(rts_plaza2), ?plaza2_driver_dll) of
     {error, already_loaded} ->
        ok;
     Res ->
        Res
   end.

open(IniFile, Host, Port, AppName, Password, LogLevel, Streams) ->
   DrvPort = erlang:open_port({spawn, ?plaza2_driver_dll}, [binary]),
   true = erlang:port_command(DrvPort, term_to_binary({connect, IniFile, Host, Port, AppName, Password, LogLevel, Streams})),
   DrvPort.

close(DrvPort) ->
   true = erlang:port_command(DrvPort, term_to_binary({disconnect})),
   port_close(DrvPort).

processInfo(
   #'FORTS_FUTINFO_REPL.fut_sess_contents'{
      event_name = 'StreamDataInserted',
      isin = Isin,
      name = FullName,
      expiration = Expiration,
      commodity = Commodity,
      signs = Signs,
      limit_up = LUp,
      limit_down = LDown,
      sess_id = SessId,
      isin_id = IsinId,
      lot_size = LSize}) when (Signs band 16#100 == 0) and (Signs band 16#800 == 0) and (Signs band 16#1000 == 0)
                               and ((Signs band 16#8 =/= 0) or (Signs band 16#4 == 0))->
   Instr = #new_instrument{
      exch = 'RTS',
      class_code = if (Signs band 16#8 =/= 0) -> 'RTS_STD'; true -> 'RTS_FUT' end,
      name = if (Signs band 16#8 =/= 0) -> Commodity; true -> Isin end,
      full_name = FullName,
      expiration = format_datetime(Expiration),
      commodity = Commodity,
      type = if (Signs band 16#8 =/= 0) -> standard; true -> future end,
      limit_up = LUp,
      limit_down = LDown,
      lot_size = LSize,
      ref = SessId * 1000000000 + IsinId},
   pg:send(?group_rts_instruments, Instr);

processInfo(#'FORTS_FUTINFO_REPL.fut_sess_contents'{}) ->
   ok;
processInfo({log, info, Str}) ->
   error_logger:info_msg(Str);
processInfo({log, debug, Str}) ->
   error_logger:info_msg(Str);
processInfo({log, error, Str}) ->
   error_logger:error_msg(Str);
processInfo({log, warn, Str}) ->
   error_logger:warning_msg(Str);
processInfo(Msg) ->
   error_logger:error_msg("Unexpected message: ~p", [Msg]).

format_datetime(DateTime) ->
   Year = DateTime div 10000000000000,
   Month = (DateTime - Year * 10000000000000) div 100000000000,
   Day = (DateTime - DateTime div 100000000000 * 100000000000) div 1000000000,
   Hour = (DateTime - DateTime div 1000000000 * 1000000000) div 10000000,
   Min = (DateTime - DateTime div 10000000 * 10000000) div 100000,
   Sec = (DateTime - DateTime div 100000 * 100000) div 1000,
   MSec = DateTime rem 1000,
   {{Year, Month, Day},{Hour, Min, Sec, MSec}}.

extract_settings([{'SERVICE', SList}, {'PLAZA2', PList}]) ->
   #settings{
      host     = element(2, lists:keyfind("Host", 1, PList)),
      port     = list_to_integer(element(2, lists:keyfind("Port", 1, PList))),
      app_name = element(2, lists:keyfind("Application", 1, PList)),
      passwd   = element(2, lists:keyfind("Password", 1, PList)),
      log_level = list_to_atom(element(2, lists:keyfind("LogLevel", 1, SList)))
   }.
