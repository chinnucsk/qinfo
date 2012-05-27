-module(rts_plaza2_srv).

-behaviour(gen_server).

-export([start/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(plaza2_driver_dll, "plaza2_driver").
-define(server_name, {global, ?qinfo_rts_plaza2}).

-include_lib("qinfo_common/include/names.hrl").
-include_lib("metadata/include/metadata.hrl").

-define(def_settings, [
      #setting{name = "LogLevel", value = "info", description = "possible values are: info, debug, warning, error",
         validator = "fun(_,Val) -> settings:val_is_log_level(Val) end."},
      #setting{name = "Host", value = "192.168.1.99", description = "Plaza2 router host",
         validator = "fun(_,Val) -> length(Val) > 0 end."},
      #setting{name = "Port", value = "4001", description = "Plaza2 router port",
         validator = "fun(_,Val) -> settings:val_is_integer(Val) end."},
      #setting{name = "Application", value = "qinfo", description = "name of this application",
         validator = "fun(_,Val) -> length(Val) > 0 end."},
      #setting{name = "Password", value = "123", description = "should be set if router is on another box"}
   ]).

-define(def_schedule,
   [
      {mon, disabled, "09:55-18:45, 18:59-23:50"},
      {tue, disabled, "09:55-18:45, 18:59-23:50"},
      {wed, disabled, "09:55-18:45, 18:59-23:50"},
      {thu, disabled, "09:55-18:45, 18:59-23:50"},
      {fri, disabled, "09:55-18:45, 18:59-23:50"},
      {sat, disabled, ""},
      {sun, disabled, ""}
   ]).

-record(settings, {ini_file, host, port, app_name, passwd, log_level, streams}).
-record(state, {status = offline, drv_port = undef, settings}).
-record('fut_sess_contents@FORTS_FUTINFO_REPL', {event_name, replID, replRev, replAct, sess_id, isin_id, short_isin,
      isin, name, commodity, limit_up, limit_down, lot_size, expiration, signs}).

%% ========= public ============

start() ->
   gen_server:start_link(?server_name, ?MODULE, [], []).

init(_Args) ->
   {ok, Service} = metadata:register_service(
      ?server_name, "RTS Plaza2 market data", ?def_settings, ?def_schedule),
   Settings = extract_settings(Service),
   error_logger:info_msg("~p settings: ~p.~n", [?qinfo_rts_plaza2, Settings]),
   load_dll(),
   {ok, #state{settings = Settings}}.

handle_call(Msg, _From, State) ->
   error_logger:error_msg("Unexpected message: ~p~n", [Msg]),
   {reply, undef, State}.

handle_cast(reconfigure, State = #state{status = Status, drv_port = DrvPort, settings = OldSettings}) ->
   {ok, Service} = metadata:get_settings(?server_name),
   NewSettings = extract_settings(Service),
   error_logger:info_msg("~p settings: ~p.~n", [?qinfo_rts_plaza2, NewSettings]),
   case NewSettings == OldSettings of
      true ->
         error_logger:info_msg("New settings are the same as old one. Nothing to do."),
         {noreply, State};
      false when Status == online ->
         close(DrvPort),
         NewDrvPort = open(NewSettings),
         {noreply, State#state{drv_port = NewDrvPort, settings = NewSettings}};
      false ->
         error_logger:info_msg("Settings are applied."),
         {noreply, State#state{settings = NewSettings}}
   end;
handle_cast(online, State = #state{status = online}) ->
   error_logger:info_msg("Service ~p is already online.", [?qinfo_rts_plaza2]),
   {noreply, State};
handle_cast(offline, State = #state{status = offline}) ->
   error_logger:info_msg("Service ~p is already offline.", [?qinfo_rts_plaza2]),
   {noreply, State};
handle_cast(online, State = #state{settings = Settings}) ->
   NewDrvPort = open(Settings),
   error_logger:info_msg("Service ~p is online.", [?qinfo_rts_plaza2]),
   {noreply, State#state{status = online, drv_port = NewDrvPort}};
handle_cast(offline, State = #state{drv_port = DrvPort}) ->
   close(DrvPort),
   error_logger:info_msg("Service ~p is offline.", [?qinfo_rts_plaza2]),
   {noreply, State#state{status = offline, drv_port = undef}};
handle_cast(Msg, State) ->
   error_logger:error_msg("Unexpected message: ~p~n", [Msg]),
   {noreply, State}.

handle_info({_Port, {data, Msg}}, State) ->
   processInfo(binary_to_term(Msg)),
   {noreply, State};

handle_info(Msg, State) ->
   error_logger:error_msg("Unexpected msg: ~p.~n", [Msg]),
   {noreply, State}.

terminate(Reason, #state{drv_port = DrvPort}) ->
   error_logger:info_msg("Terminate. Reason = ~p.~n", [Reason]),
   close(DrvPort),
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

open(Settings) ->
   DrvPort = erlang:open_port({spawn, ?plaza2_driver_dll}, [binary]),
   true = erlang:port_command(DrvPort, term_to_binary({init, Settings#settings.ini_file})),
   true = erlang:port_command(DrvPort, term_to_binary({connect, Settings#settings.host, Settings#settings.port,
            Settings#settings.app_name, Settings#settings.passwd, Settings#settings.log_level, Settings#settings.streams})),
   DrvPort.

close(undef) ->
   ok;
close(DrvPort) ->
   true = erlang:port_command(DrvPort, term_to_binary({disconnect})),
   port_close(DrvPort).

processInfo(
   #'fut_sess_contents@FORTS_FUTINFO_REPL'{
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
      exchange = "RTS",
      class_code = if (Signs band 16#8 =/= 0) -> 'RTS_STD'; true -> 'RTS_FUT' end,
      name = if (Signs band 16#8 =/= 0) -> Commodity; true -> Isin end,
      full_name = FullName,
      expiration = format_datetime(Expiration),
      commodity = Commodity,
      type = if (Signs band 16#8 =/= 0) -> spot; true -> future end,
      limit_up = LUp,
      limit_down = LDown,
      lot_size = LSize,
      ref = SessId * 1000000000 + IsinId},
   pg:send(?group_rts_instruments, Instr);

processInfo(#'fut_sess_contents@FORTS_FUTINFO_REPL'{}) ->
   ok;
processInfo({log, info, Str}) ->
   error_logger:info_msg(Str);
processInfo({log, debug, Str}) ->
   error_logger:info_msg(Str);
processInfo({log, error, Str}) ->
   error_logger:error_msg(Str);
processInfo({log, warn, Str}) ->
   error_logger:warning_msg(Str);
processInfo({'StreamDataBegin', "FORTS_FUTINFO_REPL"}) ->
   pg:send(?group_rts_instruments, {begin_load, "RTS"});
processInfo({'StreamDataEnd', "FORTS_FUTINFO_REPL"}) ->
   pg:send(?group_rts_instruments, {end_load, "RTS"});
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

extract_settings(#service{settings = Settings}) ->
   IniDir = code:lib_dir(rts_plaza2) ++ "/ini/",
   #settings{
      ini_file = IniDir ++ "P2ClientGate.ini",
      host     = (lists:keyfind("Host", 2, Settings))#setting.value,
      port     = list_to_integer((lists:keyfind("Port", 2, Settings))#setting.value),
      app_name = (lists:keyfind("Application", 2, Settings))#setting.value,
      passwd   = (lists:keyfind("Password", 2, Settings))#setting.value,
      log_level = list_to_atom((lists:keyfind("LogLevel", 2, Settings))#setting.value),
      streams  =  [{"FORTS_FUTINFO_REPL", IniDir ++ "fut_info.ini", 'RT_COMBINED_DYNAMIC'}]
   }.
