-module(metadata_srv).

-behaviour(gen_server).

-export([start/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("protocol.hrl").

-record(m_instrument, {exchange, class_code, name, full_name, isin, issuer, expired = undef}).

%% ========= public ============

start() ->
   gen_server:start_link({global, metadata_srv}, ?MODULE, [], []).

init(_Args) ->
   create_db(),
   {ok, undef}.

handle_call(Request, _From, State) ->
   io:format("~p~n", [Request]),
   {reply, undef, State}.

handle_cast(Request, State) ->
   io:format("~p~n", [Request]),
   {noreply, State}.

handle_info(_Info, State) ->
   {noreply, State}.

terminate(Reason, _State) ->
   io:format("~p~n", [Reason]),
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
         ?create_table(m_instrument, bag);
      {error, {_, {already_exists, _}}} ->
         ok
   end.
