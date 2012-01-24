-module(metadata_srv).

-behaviour(gen_server).

-export([start/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("metadata/include/metadata.hrl").
-include_lib("common/include/names.hrl").

%% ========= public ============
start() ->
   gen_server:start_link({global, ?qinfo_metadata}, ?MODULE, [], []).

init(_Args) ->
   create_db(),
   pg:create(?group_micex_instruments),
   pg:create(?group_rts_instruments),
   pg:join(?group_micex_instruments, self()),
   pg:join(?group_rts_instruments, self()),
   {ok, undef}.

handle_call({register, ServiceName, Description, Enabled, Settings, Schedule}, _From, State) ->
   case mnesia:dirty_read(m_service, ServiceName) of
      [] ->
         mnesia:dirty_write(#m_service{service = ServiceName, description = Description, enabled = Enabled, settings = Settings, schedule = Schedule}),
         error_logger:info_msg("Service ~p has been registered.~n", [ServiceName]),
         Msg = #service{service = ServiceName, enabled = Enabled, settings = Settings},
         {reply, {ok, Msg}, State};
      [#m_service{service = Service, enabled = OldEnabled, settings = OldSettings}] ->
         Msg = #service{service = Service, enabled = OldEnabled, settings = OldSettings},
         {reply, {ok, Msg}, State}
   end;

handle_call({get_settings, ServiceName}, _From, State) ->
   case mnesia:dirty_read(m_service, ServiceName) of
      [] ->
         {reply, {error, no_such_service}, State};
      [#m_service{service = Service, enabled = Enabled, settings = Settings}] ->
         Msg = #service{service = Service, enabled = Enabled, settings = Settings},
         {reply, {ok, Msg}, State}
   end;

handle_call({get_instruments, Exchange, OnlyEnabled}, _From, State) ->
   {atomic, Res} = mnesia:transaction
   (
      fun() ->
         mnesia:select(m_instrument, [{#m_instrument{exchange = '$1', _='_'}, [{'==', '$1', 'RTS'}], ['$_']}])
      end
   ),
   {reply, Res, State};

handle_call(Msg, From, State) ->
   error_logger:warning_msg("Unexpected message ~p from ~p.~n", [Msg, From]),
   {reply, unexpected, State}.

handle_cast(Msg, State) ->
   error_logger:warning_msg("Unexpected message: ~p.~n", [Msg]),
   {noreply, State}.

handle_info({pg_message, _, _, #new_instrument{
         name = Name,
         exchange = Exch,
         full_name = FullName,
         class_code = ClassCode,
         expiration = Expiration,
         commodity = Commodity,
         limit_up = LUp,
         limit_down = LDown,
         lot_size = LSize,
         type = Type,
         ref = Ref}}, State) ->
   {atomic, ok} = mnesia:transaction(fun() ->
            Key = {Commodity, Type, Exch},
            case mnesia:read(m_commodity, Key) of
               [] ->
                  mnesia:write(#m_commodity{ key = Key, class_code = ClassCode}),
                  false;
               Res ->
                  ok
            end,
            mnesia:write(
               #m_instrument{
                  name = {Name, Type, Exch},
                  full_name = common_utils:cp1251_to_unicode(FullName),
                  exchange = Exch,
                  expiration = Expiration,
                  commodity = Commodity,
                  limit_up = LUp,
                  limit_down = LDown,
                  lot_size = LSize,
                  type = Type,
                  ref = Ref}),
            mnesia:write(#m_exchange{name = Exch})
      end),
   {noreply, State};

handle_info(Msg, State) ->
   error_logger:error_msg("Unexpected message: ~p.~n", [Msg]),
   {noreply, State}.

terminate(Reason, _State) ->
   error_logger:info_msg("Terminate. Reason = ~p.~n", [Reason]),
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.

%% ========= private ============}

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
         mnesia:start(),
         ?create_table(m_instrument, ordered_set),
         ?create_table(m_commodity, set),
         ?create_table(m_service, set),
         ?create_table(m_exchange, set);
      {error, {_, {already_exists, _}}} ->
         mnesia:start(),
         ok
   end.

type_to_symbol(future)    -> $F;
type_to_symbol(standard)  -> $S;
type_to_symbol(equity)    -> $E;
type_to_symbol(bond)      -> $B;
type_to_symbol(itf)       -> $I.

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

create_internal_symbol(#m_instrument{exchange = Exchange, type = Type = future, expiration = Expiration},
   #m_commodity{alias = Alias}) ->
   lists:flatten(io_lib:format("~s.~c.~s.~s", [Exchange, type_to_symbol(Type), Alias, get_expiration(Expiration)]));

create_internal_symbol(#m_instrument{exchange = Exchange, type = Type, expiration = Expiration},
   #m_commodity{alias = Alias}) ->
   lists:flatten(io_lib:format("~s.~c.~s", [Exchange, type_to_symbol(Type), Alias])).


%=======================================================================================================================
%  unit testing
%=======================================================================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

get_expiration_test() ->
   ?assertEqual("F2", get_expiration({{2012, 1, 0}, {10, 10, 0}})),
   ?assertEqual("M5", get_expiration({{2015, 6, 0}, {10, 10, 0}})).

create_internal_symbol_test() ->
   ?assertEqual("RTS.S.SBER",
      create_internal_symbol(
         #m_instrument{exchange = 'RTS', type = standard, expiration = {{2012, 1, 1}, {10, 0, 0}}},
         #m_commodity{alias = "SBER"})),
   ?assertEqual("RTS.F.LKOH.F2",
      create_internal_symbol(
         #m_instrument{exchange = 'RTS', type = future, expiration = {{2012, 1, 1}, {10, 0, 0}}},
         #m_commodity{alias = "LKOH"})).

-endif.
