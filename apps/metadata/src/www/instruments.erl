-module(instruments).

-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("metadata/include/metadata.hrl").

-export([main/0, layout/0, event/1]).

-define(white, "#FFFFFF;").
-define(gray,  "#EEEEEE;").
-define(type_list, [equity, spot, bond, itf, future]).

main() ->
   #template{ file="./www/page.html"}.

layout() ->
   TopPanel = #panel{
      body = [
         #link{ class=a, text = "main", url = "index"},
         #literal{ text = " | "},
         #link{ text = "settings", url = "settings"},
         #literal{ text = " | " },
         #link{ text = "scheduler", url = "scheduler"},
         #literal{ text = " | " },
         #literal{ class=a, text = "instruments"},
         #literal{ text = " | "},
         #link{ class=a, text = "statistics", url = "statistics"}
      ]
   },
   {Filter, Exchanges} = build_filter(),
   wf:state(alpha, $0),
   {AlphaFilter, Pages, Instruments} = build_instr(Exchanges, ?type_list, false, $0, 1),
   [
      TopPanel,
      #flash{},
      #p{},
      Filter,
      AlphaFilter,
      Pages,
      Instruments
   ].

select_commodities(Exchanges, Types, OnlyEnabled, Alpha) ->
   {atomic, {Commodities, AlphaList}} = mnesia:transaction(
      fun() ->
         mnesia:foldr(
            fun(C = #m_commodity{ key = {[Alpha1|_], Type, Exchange}, enabled = Enabled}, {CommAcc, AlphaAcc}) ->
               InFilter = lists:member(Type, Types) andalso lists:member(Exchange, Exchanges) andalso
               (OnlyEnabled == false orelse Enabled == true),
               case InFilter of
                  true when Alpha1 == Alpha ->
                     {[C | CommAcc], [Alpha1 | AlphaAcc]};
                  true ->
                     {CommAcc, [Alpha1 | AlphaAcc]};
                  false ->
                     {CommAcc, AlphaAcc}
               end
            end
            , {[], []}, m_commodity)
      end
   ),
   {Commodities, lists:usort(AlphaList)}.

build_filter() ->
   {Checkboxes, Exchanges} = build_exchanges(mnesia:dirty_first(m_exchange), []),
   {
      [#panel{ body = Checkboxes },
       #panel{ body = lists:foldr(fun(Type, Acc) ->
            [#checkbox{ id = checkbox_type, text = atom_to_list(Type), value = Type, checked = true, postback =
                  filter_changed} , #literal{ text = " "} | Acc]
         end, [], ?type_list)},
       #panel{ body = [ #checkbox{ id = checkbox_enabled, text = "Only enabled instruments", postback = filter_changed}]}
      ],
      Exchanges
   }.

build_exchanges('$end_of_table', Exchanges) ->
   {[], Exchanges};
build_exchanges(Key, Exchanges) ->
   [#m_exchange{ name = ExchName }] = mnesia:dirty_read(m_exchange, Key),
   {Checkboxes, NewExchanges} = build_exchanges(mnesia:dirty_next(m_exchange, Key), [ExchName | Exchanges]),
   {[#checkbox{
         id        = checkbox_exchange,
         text      = ExchName,
         checked   = true,
         value     = ExchName,
         postback  = filter_changed}
      | Checkboxes],
   NewExchanges}.

build_instr_header() ->
   #tablerow{ cells = [
      #tableheader{ text = "Name",        style = "width: 110px;"},
      #tableheader{ text = "Full name",   style = "width: 300px;"},
      #tableheader{ text = "QinfoName",   style = "width: 100px;"},
      #tableheader{ text = "ClassCode",   style = "width: 100px;"},
      #tableheader{ text = "ExchName",    style = "width: 100px;"},
      #tableheader{ text = "Type",        style = "width: 80px;"},
      #tableheader{ text = "Lot size",    style = "width: 80px;"},
      #tableheader{ text = "Expiration",  style = "width: 100px;"},
      #tableheader{ text = "LastUpdated", style = "width: 150px;"},
      #tableheader{ text = "Enabled",     style = "width: 80px;"},
      #tableheader{ text = "Alias",       style = "width: 160px;"}
   ], style = "background-color: #999797;"}.

build_alpha_filter(AlphaList) ->
   AllAlphas = lists:flatten(lists:seq($0, $9), lists:seq($A, $Z)),
   InFilterAlphas = lists:foldr(
      fun(A, Acc) ->
         case lists:member(A, AlphaList) of
            true ->
               [#link{text = [A], postback = {alpha, A}}, #literal{text = " "} | Acc];
            false ->
               [#literal{text = [A]}, #literal{text=" "} | Acc]
         end
      end, [], AllAlphas),
   InFilterAlphas.


build_pages(Commodities, SelectedPage) ->
   case build_pages_impl(Commodities, SelectedPage, 1) of
      [] ->
         #p{id = pages, text="Pages: 1"};
      Res ->
         #p{id = pages, text="Pages: ", body = Res}
   end.

build_pages_impl([], _, _) ->
   [];
build_pages_impl(Commodities, SelectedPage, PageNum) when length(Commodities) =< 5 ->
   [
      #literal{ text = " "},
      if (SelectedPage == PageNum) ->
         #literal{ text = integer_to_list(SelectedPage)};
      true ->
         #link{ text = integer_to_list(PageNum), postback = {page, PageNum}}
      end
   ];
build_pages_impl(Commodities, SelectedPage, PageNum) ->
   [#literal{ text = " "},
      if (SelectedPage == PageNum) ->
         #literal{ text = integer_to_list(SelectedPage)};
      true ->
         #link{ text = integer_to_list(PageNum), postback = {page, PageNum}}
      end |
    build_pages_impl(lists:nthtail(if length(Commodities) =< 5 -> length(Commodities); true -> 5 end, Commodities),
       SelectedPage, PageNum + 1)].

build_instr(Exchanges, Types, OnlyEnabled, Alpha, Page) ->
   {Commodities, AlphaList} = select_commodities(Exchanges, Types, OnlyEnabled, Alpha),
   {
      #p{id = alpha_filter, body = build_alpha_filter(AlphaList)},
      build_pages(Commodities, Page),
      #table{id = instruments, rows = build_instr_header()}
   }.

event(filter_changed) ->
   {AlphaFilter, Pages, Instruments} = build_instr(
      wf:qs(checkbox_exchange), get_type_list(), is_checked(checkbox_enabled), wf:state(alpha), 1),
   wf:replace(alpha_filter, AlphaFilter),
   wf:replace(pages, Pages),
   wf:replace(instruments, Instruments);

event({alpha, A}) ->
   wf:state(alpha, A),
   {AlphaFilter, Pages, Instruments} = build_instr(
      wf:qs(checkbox_exchange), get_type_list(), is_checked(checkbox_enabled), A, 1),
   wf:replace(alpha_filter, AlphaFilter),
   wf:replace(pages, Pages),
   wf:replace(instruments, Instruments);

event({page, PageNum}) ->
   {AlphaFilter, Pages, Instruments} = build_instr(
      wf:qs(checkbox_exchange), get_type_list(), is_checked(checkbox_enabled), wf:state(alpha), PageNum),
   wf:replace(alpha_filter, AlphaFilter),
   wf:replace(pages, Pages),
   wf:replace(instruments, Instruments);

event(Event) ->
   io:format("~p~n", [Event]),
   ok.

alias_to_list(undef) ->
   "-";
alias_to_list(Alias) ->
   Alias.

expiration_to_list(undef) ->
   "-";
expiration_to_list({{Year, Month, Day},_}) ->
   lists:flatten(io_lib:format("~2.10.0B.~2.10.0B.~.10B", [Day, Month, Year])).

date_to_list({{Y,M,D},{H,MM,S}}) ->
   lists:flatten(io_lib:format("~2.10.0B.~2.10.0B.~4.10B ~2.10.0B.~2.10.0B.~2.10B", [D, M, Y, H, MM, S])).

get_type_list() ->
   TypeList = wf:qs(checkbox_type),
   lists:foldr(fun(Type, Acc) -> [common_utils:list_to_atom(Type) | Acc] end, [], TypeList).

is_checked(Checkbox) ->
   case wf:q(Checkbox) of
      "on" ->
         true;
      _ ->
         false
   end.

invert_color(?white) ->
   ?gray;
invert_color(?gray) ->
   ?white.

get_expiration({{Year, Month, _Day}, _}) ->
   Y = Year - (Year div 10 * 10),
   [common_utils:month_to_symbol(Month)] ++ integer_to_list(Y).

create_internal_symbol(Exchange, Commodity, undef, future, Expiration) ->
   lists:flatten(io_lib:format("~s.~c.~s.~s", [Exchange, common_utils:type_to_symbol(future), Commodity, get_expiration(Expiration)]));

create_internal_symbol(_Exchange, _Commodity, Alias, future, Expiration) ->
   lists:flatten(io_lib:format("~s.~s", [Alias, get_expiration(Expiration)]));

create_internal_symbol(Exchange, Commodity, undef, Type, _Expiration) ->
   lists:flatten(io_lib:format("~s.~c.~s", [Exchange, common_utils:type_to_symbol(Type), Commodity]));

create_internal_symbol(_Exchange, _Commodity, Alias, _Type, _Expiration) ->
   lists:flatten(io_lib:format("~s", [Alias])).

%======================================================================================================================
%  unit testing
%======================================================================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-endif.
