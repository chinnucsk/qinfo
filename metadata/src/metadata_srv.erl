-module(metadata_srv).

-behaviour(gen_server).

-export([start/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("protocol.hrl").
-include("public.hrl").

-record(m_instrument, {isin, short_isin, full_name, exch, class_code, expiration = undef, commodity, limit_up, limit_down,
      lot_size, type, enabled}).
-record(m_commodity, {key, enabled = false, alias = undef}).
-record(m_service, {service, settings = [], schedule = []}).

%% ========= public ============

start() ->
   gen_server:start_link({global, ?qinfo_metadata}, ?MODULE, [], []).

init(_Args) ->
   create_db(),
   pg:create(?group_rts_instruments),
   pg:join(?group_rts_instruments, self()),
   {ok, undef}.

handle_call({register, ServiceName, Settings, Schedule}, _From, State) ->
   case mnesia:dirty_read(m_service, ServiceName) of
      [] ->
         mnesia:dirty_write(#m_service{service = ServiceName, settings = Settings, schedule = Schedule}),
         error_logger:info_msg("Service ~p has been registered.~n", [ServiceName]),
         Msg = #service{service = ServiceName, settings = Settings},
         {reply, {ok, Msg}, State};
      [#m_service{service = Service, settings = Settings}] ->
         Msg = #service{service = Service, settings = Settings},
         {reply, {ok, Msg}, State}
   end;

handle_call({get_settings, ServiceName}, _From, State) ->
   case mnesia:dirty_read(m_service, ServiceName) of
      [] ->
         {reply, {error, no_such_service}, State};
      [#m_service{service = Service, settings = Settings}] ->
         Msg = #service{service = Service, settings = Settings},
         {reply, Msg, State}
   end;

handle_call({get_instruments, Exchange, OnlyEnabled}, _From, State) ->
   {reply, ok, State};

handle_call(Msg, From, State) ->
   error_logger:warning_msg("Unexpected message ~p from ~p.~n", [Msg, From]),
   {reply, unexpected, State}.

handle_cast(Msg, State) ->
   error_logger:warning_msg("Unexpected message: ~p.~n", [Msg]),
   {noreply, State}.

handle_info({pg_message, _, _, #new_instrument{
         name = Name,
         exch = Exch,
         class_code = ClassCode,
         short_isin = ShortIsin,
         isin = Isin,
         expiration = Expiration,
         commodity = Commodity,
         limit_up = LUp,
         limit_down = LDown,
         lot_size = LSize,
         type = Type}}, State) ->
   {atomic, ok} = mnesia:transaction(fun() ->
            Res = mnesia:read(m_commodity, {Exch, ClassCode, Commodity}),
            Enabled = case Res of
                        [] ->
                           mnesia:write(#m_commodity{ key = {Exch, ClassCode, Commodity}, alias = Commodity}),
                           false;
                        [#m_commodity{key = {Exch, ClassCode, Commodity}, enabled = E}] -> % already exists, nothing to do
                           E
            end,
            mnesia:write(
               #m_instrument{
                  isin = Isin,
                  short_isin = ShortIsin,
                  full_name = Name,
                  exch = Exch,
                  class_code = ClassCode,
                  expiration = Expiration,
                  commodity = Commodity,
                  limit_up = LUp,
                  limit_down = LDown,
                  lot_size = LSize,
                  type = Type,
                  enabled = Enabled})
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

%% ========= private ============

-define(create_table(Table, Type),
   case (catch mnesia:table_info(Table, version)) of
      {'EXIT', {aborted, {no_exists, Table, _}}} ->
         mnesia:create_table(
            Table, [{disc_copies, [node()]}, {type, Type}, {attributes, record_info(fields, Table)}]);
      _ ->
         ok
   end).

create_db() ->
   case mnesia:create_schema([]) of
      ok ->
         mnesia:start(),
         ?create_table(m_instrument, set),
         ?create_table(m_commodity, set),
         ?create_table(m_service, set);
      {error, {_, {already_exists, _}}} ->
         mnesia:start(),
         ok
   end.
