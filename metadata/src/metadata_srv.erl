-module(metadata_srv).

-behaviour(gen_server).

-export([start/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("protocol.hrl").
-include("public.hrl").

-record(m_instrument, {exch, class_code, name, full_name, isin, issuer, expired = undef}).
-record(m_service, {service, settings = []}).

%% ========= public ============

start() ->
   gen_server:start_link({global, ?qinfo_metadata}, ?MODULE, [], []).

init(_Args) ->
   create_db(),
   pg:create(?group_rts_instruments),
   pg:join(?group_rts_instruments, self()),
   {ok, undef}.

handle_call({register, ServiceName, Settings}, _From, State) ->
   case mnesia:dirty_read(m_service, ServiceName) of
      [] ->
         mnesia:dirty_write(#m_service{service = ServiceName, settings = Settings}),
         error_logger:info_msg("Service ~p has been registered.", [ServiceName]),
         Msg = #service{service = ServiceName, settings = Settings},
         {reply, {ok, Msg}, State};
      [#m_service{service = Service, settings = Settings}] ->
         Msg = #service{service = Service, settings = Settings},
         error_logger:debug_msg("Settings ~p has been sent to ~p.", [Msg]),
         {reply, {ok, Msg}, State}
   end;

handle_call({get_settings, ServiceName}, _From, State) ->
   case mnesia:dirty_read(m_service, ServiceName) of
      [] ->
         {reply, {error, no_such_service}, State};
      [#m_service{service = Service, settings = Settings}] ->
         Msg = #service{service = Service, settings = Settings},
         {reply, Msg, State}
   end.

handle_cast(Msg, State) ->
   error_logger:warning_msg("Unexpected message: ~p", [Msg]),
   {noreply, State}.

handle_info(Msg, State) ->
   error_logger:warning_msg("Unexpected message: ~p", [Msg]),
   {noreply, State}.

terminate(Reason, _State) ->
   error_logger:info_msg("Terminate. Reason = ~p", [Reason]),
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
         ?create_table(m_service, set);
      {error, {_, {already_exists, _}}} ->
         ok
   end.
