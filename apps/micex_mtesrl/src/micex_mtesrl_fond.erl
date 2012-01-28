-module(micex_mtesrl_fond).

-behaviour(gen_server).

-export([start/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(micex_driver_dll, "micex_driver").
-define(server_name, {global, ?qinfo_micex_mtesrl_fond}).

-include_lib("common/include/names.hrl").
-include_lib("metadata/include/metadata.hrl").

-define(def_settings, [
      #setting{name = "LibFullPath", value = "mtesrl.dll", description = "full path to mtesrl.dll library",
         validator = "fun(_, Val) -> length(Val) > 0 end."},
      #setting{name = "LogLevel", value = "info", description = "possible values are info, debug, warning, error",
         validator = "fun(_, Val) -> settings:val_is_log_level(Val) end."},
      #setting{name = "ConnParams",
           value = <<"HOST=<server_host:port>\r\nSERVER=<server_name>\r\nUSERID=<user_id>\r\nPASSWORD=<you_password>\r\nINTERFACE=<exchange_interface>">>,
           description = "connection parameters as decribed in mtesrl documentation",
           validator = "fun(_, Val) -> length(Val) > 0 end."}
   ]
).

-define(def_schedule,
   [
      {mon, disabled, "9:59-19:00"},
      {tue, disabled, "9:59-19:00"},
      {wed, disabled, "9:59-19:00"},
      {thu, disabled, "9:59-19:00"},
      {fri, disabled, "9:59-19:00"},
      {sat, disabled, ""},
      {sun, disabled, ""}
   ]).

-record(settings, {lib_full_path, log_level, conn_params, tables}).
-record(state, {status = offline, drv_port, settings}).
-record(data_row, {table, sec_board, sec_code, sec_name, lot_size, decimals, sec_type}).

%% ========= public ============

start() ->
   gen_server:start_link(?server_name, ?MODULE, [], []).

init(_Args) ->
   {ok, Service} = metadata:register_service(?server_name, "MICEX (FOND) MTESrl market data", ?def_settings, ?def_schedule),
   Settings = extract_settings(Service),
   error_logger:info_msg("~p settings: ~p~n", [?qinfo_micex_mtesrl_fond, Settings]),
   ok = load_dll(),
   DrvPort = open(Settings),
   {ok, #state{drv_port = DrvPort, settings = Settings}}.

handle_call(Msg, _From, State) ->
   error_logger:warning_msg("Unexpected message: ~p", [Msg]),
   {reply, undef, State}.

handle_cast(reconfigure, State = #state{status = Status, drv_port = DrvPort, settings = OldSettings}) ->
   {ok, Service} = metadata:get_settings(?server_name),
   NewSettings = extract_settings(Service),
   error_logger:info_msg("~p settings: ~p.~n", [?qinfo_micex_mtesrl_fond, NewSettings]),
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
handle_cast(online, #state{status = online}) ->
   error_logger:info_msg("Service ~p is already online.", [?qinfo_micex_mtesrl_fond]);
handle_cast(offline, #state{status = offline}) ->
   error_logger:info_msg("Service ~p is already offline.", [?qinfo_micex_mtesrl_fond]);
handle_cast(online, State = #state{settings = Settings}) ->
   NewDrvPort = open(Settings),
   error_logger:info_msg("Service ~p is online.", [?qinfo_micex_mtesrl_fond]),
   {noreply, State#state{status = online, drv_port = NewDrvPort}};
handle_cast(offline, State = #state{drv_port = DrvPort}) ->
   close(DrvPort),
   error_logger:info_msg("Service ~p is offline.", [?qinfo_micex_mtesrl_fond]),
   {noreply, State#state{status = offline, drv_port = undef}};
handle_cast(Msg, State) ->
   error_logger:warning_msg("Unexpected message: ~p", [Msg]),
   {noreply, State}.

handle_info({_Port, {data, Msg}}, State) ->
   processInfo(binary_to_term(Msg)),
   {noreply, State}.

terminate(Reason, _State) ->
   error_logger:info_msg("Terminate. Reason = ~p", [Reason]),
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.

%% ==================== private =========================

load_dll() ->
   case erl_ddll:load_driver(code:priv_dir(micex_mtesrl), ?micex_driver_dll) of
      {error, already_loaded} ->
         ok;
      Msg ->
         Msg
   end.

open(#settings{lib_full_path = LibFullPath, log_level = LogLevel, conn_params = ConnParams, tables = Tables}) ->
   DrvPort = erlang:open_port({spawn, ?micex_driver_dll}, [binary]),
   erlang:port_command(DrvPort, term_to_binary({connect, LibFullPath, LogLevel, ConnParams, Tables})),
   DrvPort.

close(undef) ->
   ok;
close(DrvPort) ->
   erlang:port_command(DrvPort, term_to_binary({disconnect})),
   port_close(DrvPort).

processInfo(#data_row{table = 'SECURITIES', sec_board = SecBoard, sec_code = SecCode, sec_name = SecName, lot_size =
      LotSize, sec_type = SecType}) ->
   Instr = #new_instrument{
      exchange = "MICEX",
      class_code = SecBoard,
      name = SecCode,
      full_name = SecName,
      expiration = undef,
      commodity = SecCode,
      type = format_sec_type(SecType),
      limit_up = undef,
      limit_down = undef,
      lot_size = LotSize,
      ref = undef
   },
   pg:send(?group_micex_instruments, Instr);

processInfo({log, info, Str}) ->
   error_logger:info_msg(Str);
processInfo({log, debug, Str}) ->
   error_logger:info_msg(Str);
processInfo({log, error, Str}) ->
   error_logger:error_msg(Str);
processInfo({log, warning, Str}) ->
   error_logger:warning_msg(Str);
processInfo(_Msg) ->
   ok.

format_sec_type([SecType]) when SecType == $1 orelse SecType == $2 ->
   equity;
format_sec_type([SecType]) when (SecType >= $3 andalso SecType =< $8) orelse (SecType == $C) ->
   bond;
format_sec_type([SecType]) when SecType == $9 orelse SecType == $A orelse SecType == $B ->
   itf;
format_sec_type([_SecType]) ->
   unknown.

extract_settings(#service{settings = Settings}) ->
   #settings{
      lib_full_path = (lists:keyfind("LibFullPath", 2, Settings))#setting.value,
      log_level     = list_to_atom((lists:keyfind("LogLevel", 2, Settings))#setting.value),
      conn_params   = (lists:keyfind("ConnParams", 2, Settings))#setting.value,
      tables =
      [
         {
            "SECURITIES", true, true,
            [{"MARKETID", "FOND"}],
            ["SECBOARD", "SECCODE", "SECNAME", "LOTSIZE", "DECIMALS", "SECTYPE"]
         }
      ]
   }.
