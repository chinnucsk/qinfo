-module(metadata_srv).

-behaviour(gen_server).

-export([start/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-compile([export_all]).

-include_lib("metadata/include/metadata.hrl").
-include_lib("common/include/names.hrl").

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
   case mnesia:dirty_read(m_service, ServiceName) of
      [] ->
         mnesia:dirty_write(#m_service{service = ServiceName,
               description = Description, settings = Settings, schedule = Schedule}),
         error_logger:info_msg("Service ~p has been registered.~n", [ServiceName]),
         Msg = #service{service = ServiceName, settings = Settings},
         scheduler_srv:schedule(ServiceName),
         {reply, {ok, Msg}, State};
      [#m_service{service = Service, settings = OldSettings}] ->
         Msg = #service{service = Service, settings = OldSettings},
         scheduler_srv:schedule(ServiceName),
         {reply, {ok, Msg}, State}
   end;

handle_call({get_settings, ServiceName}, _From, State) ->
   case mnesia:dirty_read(m_service, ServiceName) of
      [] ->
         {reply, {error, no_such_service}, State};
      [#m_service{service = Service, settings = Settings}] ->
         Msg = #service{service = Service, settings = Settings},
         {reply, {ok, Msg}, State}
   end;

handle_call({get_instruments, _Exchange, OnlyEnabled}, _From, State) ->
   {atomic, Res} = mnesia:transaction
   (
      fun() ->
         mnesia:select(m_instrument, [{#m_instrument{key = {'_', '_', '$1'}, _='_'}, [{'==', '$1', 'RTS'}], ['$_']}])
      end
   ),
   {reply, Res, State};

handle_call(get_schedules, _From, State) ->
   {atomic, Services} = mnesia:transaction(
   fun() ->
      mnesia:foldl(
         fun(#m_service{service = ServiceName, schedule = SchedList}, Acc) ->
            [{ServiceName, SchedList} | Acc]
         end, [], m_service)
   end),
   {reply, Services, State};

handle_call({get_schedules, ServiceName}, _From, State) ->
   Res = case mnesia:dirty_read(m_service, ServiceName) of
      [] ->
         no_such_service;
      [#m_service{service = ServiceName, schedule = SchedList}] ->
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
   Key = {Commodity, Type, Exch},
   case mnesia:read(m_commodity, Key) of
      [] ->
         NewCommodity = #m_commodity{ key = Key, class_code = ClassCode},
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
   Key = {Name, Type, Exch},
   case mnesia:read(m_instrument, Key) of
      [] ->
         NewInstr = #m_instrument{
                  key = Key,
                  commodity = {Commodity, Type, Exch},
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
         UpdateInstr = Instr#m_instrument{
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
   case mnesia:read(m_exchange, exch) of
      [] ->
         NewExchange = #m_exchange{name = Exch},
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
         ?create_table(m_instrument, ordered_set),
         ?create_table(m_commodity, ordered_set),
         ?create_table(m_service, set),
         ?create_table(m_exchange, set),
         {atomic, ok} = mnesia:add_table_index(m_instrument, #m_instrument.commodity);
      {error, {_, {already_exists, _}}} ->
         ok = mnesia:start(),
         ok
   end.

month_to_symbol(1) -> $F;
month_to_symbol(2) -> $G;
month_to_symbol(3) -> $H;
month_to_symbol(4) -> $J;
month_to_symbol(5) -> $K;
month_to_symbol(6) -> $M;
month_to_symbol(7) -> $N;
month_to_symbol(8) -> $Q;
month_to_symbol(9) -> $U;
month_to_symbol(10) -> $V;
month_to_symbol(11) -> $X;
month_to_symbol(12) -> $Z.

get_expiration({{Year, Month, _Day}, _}) ->
   Y = Year - (Year div 10 * 10),
   [month_to_symbol(Month)] ++ integer_to_list(Y).

%create_internal_symbol(#m_instrument{key = {_, future, Exchange}, expiration = Expiration},
%   #m_commodity{alias = Alias}) ->
%   lists:flatten(io_lib:format("~s.~c.~s.~s", [Exchange, type_to_symbol(future), Alias, get_expiration(Expiration)]));

%create_internal_symbol(#m_instrument{key = {_, Type, Exchange}},
%   #m_commodity{alias = Alias}) ->
%   lists:flatten(io_lib:format("~s.~c.~s", [Exchange, type_to_symbol(Type), Alias])).


%======================================================================================================================
%  unit testing
%======================================================================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%get_expiration_test() ->
%   ?assertEqual("F2", get_expiration({{2012, 1, 0}, {10, 10, 0}})),
%   ?assertEqual("M5", get_expiration({{2015, 6, 0}, {10, 10, 0}})).

%create_internal_symbol_test() ->
%   ?assertEqual("RTS.S.SBER",
%      create_internal_symbol(
%         #m_instrument{exchange = 'RTS', type = standard, expiration = {{2012, 1, 1}, {10, 0, 0}}},
%         #m_commodity{alias = "SBER"})),
%   ?assertEqual("RTS.F.LKOH.F2",
%      create_internal_symbol(
%         #m_instrument{exchange = 'RTS', type = future, expiration = {{2012, 1, 1}, {10, 0, 0}}},
%         #m_commodity{alias = "LKOH"})).

-endif.
