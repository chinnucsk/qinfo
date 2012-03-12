-module(metadata_srv).

-behaviour(gen_server).

-export([start/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-compile([export_all]).

-include_lib("metadata/include/metadata.hrl").
-include_lib("common/include/names.hrl").
-include_lib("stdlib/include/qlc.hrl").

%=======================================================================================================================
%  public
%=======================================================================================================================
start() ->
   gen_server:start_link({global, ?qinfo_metadata}, ?MODULE, [], []).

init(_Args) ->
   create_db(),
   pg:create(?group_micex_instruments),
   pg:create(?group_rts_instruments),
   pg:join(?group_micex_instruments, self()),
   pg:join(?group_rts_instruments, self()),
   {ok, undef}.

%======================================================================================================================
handle_call({register, ServiceName, Description, Settings, Schedule}, _From, State) ->
   case mnesia:dirty_read(service, ServiceName) of
      [] ->
         mnesia:dirty_write(#service{service = ServiceName,
               description = Description, settings = Settings, schedule = Schedule}),
         error_logger:info_msg("Service ~p has been registered.~n", [ServiceName]),
         Msg = #service{service = ServiceName, settings = Settings},
         scheduler_srv:schedule(ServiceName),
         {reply, {ok, Msg}, State};
      [#service{service = Service, settings = OldSettings}] ->
         Msg = #service{service = Service, settings = OldSettings},
         scheduler_srv:schedule(ServiceName),
         {reply, {ok, Msg}, State}
   end;

handle_call({get_settings, ServiceName}, _From, State) ->
   case mnesia:dirty_read(service, ServiceName) of
      [] ->
         {reply, {error, no_such_service}, State};
      [#service{service = Service, settings = Settings}] ->
         Msg = #service{service = Service, settings = Settings},
         {reply, {ok, Msg}, State}
   end;

handle_call({get_instruments, Exchange, OnlyEnabled}, _From, State) ->
   Q = qlc:q([ I ||
         I <- mnesia:table(instrument),
         C <- mnesia:table(commodity),
         I#instrument.commodity == C#commodity.key andalso
         C#commodity.enabled == OnlyEnabled andalso
         C#instrument.key#instrument_key.exchange == Exchange]),
   {atomic, Res} = mnesia:transaction(fun() -> qlc:e(Q) end),
   {reply, Res, State};

handle_call(get_schedules, _From, State) ->
   {atomic, Services} = mnesia:transaction(
   fun() ->
      mnesia:foldl(
         fun(#service{service = ServiceName, schedule = SchedList}, Acc) ->
            [{ServiceName, SchedList} | Acc]
         end, [], service)
   end),
   {reply, Services, State};

handle_call({get_schedules, ServiceName}, _From, State) ->
   Res = case mnesia:dirty_read(service, ServiceName) of
      [] ->
         no_such_service;
      [#service{service = ServiceName, schedule = SchedList}] ->
         {ServiceName, SchedList}
   end,
   {reply, Res, State};

handle_call(Msg, From, State) ->
   error_logger:warning_msg("Unexpected message ~p from ~p.~n", [Msg, From]),
   {reply, unexpected, State}.

%======================================================================================================================
handle_cast(Msg, State) ->
   error_logger:warning_msg("Unexpected message: ~p.~n", [Msg]),
   {noreply, State}.

%======================================================================================================================
handle_info({pg_message, _, _, I = #new_instrument{}}, State) ->
   {atomic, ok} = mnesia:transaction(
      fun() ->
         insert_commodity(I),
         insert_instrument(I),
         insert_exchange(I),
         ok
      end),
   {noreply, State};

handle_info(Msg, State) ->
   error_logger:error_msg("Unexpected message: ~p.~n", [Msg]),
   {noreply, State}.

%======================================================================================================================
terminate(Reason, _State) ->
   error_logger:info_msg("Terminate. Reason = ~p.~n", [Reason]),
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.

%======================================================================================================================
%  private
%======================================================================================================================
insert_commodity(#new_instrument{
         exchange = Exch,
         class_code = ClassCode,
         commodity = Commodity,
         type = Type}) ->
   Key = #commodity_key{commodity = Commodity, type = Type, exchange = Exch},
   case mnesia:read(commodity, Key) of
      [] ->
         NewCommodity = #commodity{key = Key, class_code = ClassCode},
         ok = mnesia:write(NewCommodity),
         NewCommodity;
      [Comm] ->
         Comm
   end.

insert_instrument(#new_instrument{
         name = Name,
         exchange = Exch,
         full_name = FullName,
         expiration = Expiration,
         commodity = Commodity,
         limit_up = LUp,
         limit_down = LDown,
         lot_size = LSize,
         type = Type,
         ref = Ref}) ->
   Key = #instrument_key{exch_symbol = Name, type = Type, exchange = Exch},
   case mnesia:read(instrument, Key) of
      [] ->
         NewInstr = #instrument{
                  key = Key,
                  commodity = #commodity_key{commodity = Commodity, type = Type, exchange = Exch},
                  full_name = common_utils:cp1251_to_unicode(FullName),
                  expiration = Expiration,
                  limit_up = LUp,
                  limit_down = LDown,
                  lot_size = LSize,
                  updated = calendar:local_time(),
                  ref = Ref},
         ok = mnesia:write(NewInstr),
         NewInstr;
      [Instr] ->
         UpdateInstr = Instr#instrument{
            full_name = common_utils:cp1251_to_unicode(FullName),
            expiration = Expiration,
            limit_up = LUp,
            limit_down = LDown,
            lot_size = LSize,
            updated = calendar:local_time(),
            ref = Ref},
         ok = mnesia:write(UpdateInstr),
         UpdateInstr
   end.

insert_exchange(#new_instrument{exchange = Exch}) ->
   case mnesia:read(exchange, exch) of
      [] ->
         NewExchange = #exchange{name = Exch},
         ok = mnesia:write(NewExchange),
         NewExchange;
      [Exch] ->
         Exch
   end.

-define(create_table(Table, Type),
   case (catch mnesia:table_info(Table, version)) of
      {'EXIT', {aborted, {no_exists, Table, _}}} ->
         {atomic, ok} = mnesia:create_table(
            Table, [{disc_copies, [node()]}, {type, Type}, {attributes, record_info(fields, Table)}]);
      _ ->
         ok
   end).

create_db() ->
   case mnesia:create_schema([]) of
      ok ->
         ok = mnesia:start(),
         ?create_table(instrument, ordered_set),
         ?create_table(commodity, ordered_set),
         ?create_table(service, set),
         ?create_table(exchange, set),
         {atomic, ok} = mnesia:add_table_index(instrument, #instrument.commodity),
         {atomic, ok} = mnesia:add_table_index(commodity, #commodity.alias);
      {error, {_, {already_exists, _}}} ->
         ok = mnesia:start(),
         ok
   end.
