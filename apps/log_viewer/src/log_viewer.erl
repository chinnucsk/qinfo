%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2011. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
-module(log_viewer).

%% External exports
-export([rescan/0, rescan/1, list/0, list/1, show/1, grep/1]).

rescan() -> rescan([]).
rescan(Options) ->
    call({rescan, Options}).

list() -> list([]).
list(Type) when is_list(Type) =/= true ->
   {error, wrong_type_list};
list(Type) -> call({list, Type}).

show(Number) when is_integer(Number) ->
    call({show_number, Number}).

grep(RegExp) -> call({grep, RegExp}).

%filter(Filters) when is_list(Filters) ->
%    call({filter, Filters}).

%filter(Filters, FDates) when is_list(Filters) andalso is_tuple(FDates) ->
%    call({filter, {Filters, FDates}}).

%%-----------------------------------------------------------------
%% Internal functions.
%%-----------------------------------------------------------------
call(Req) ->
    gen_server:call(log_viewer_srv, Req, infinity).