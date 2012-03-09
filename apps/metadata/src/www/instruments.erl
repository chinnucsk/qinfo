-module(instruments).

-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("metadata/include/metadata.hrl").

-export([main/0, layout/0, event/1, inplace_textbox_event/2, valid_alias/1, uniq_alias/2, title/0]).

-define(page_size, 15).
-define(white, "#FFFFFF;").
-define(gray,  "#EEEEEE;").
-define(type_list, [equity, spot, bond, itf, future]).

title() ->
   "qinfo:instruments".

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
      #button{ id = apply_button, text = "Apply", postback = click_apply },
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
      #tableheader{ text = "Full name",   style = "width: 340px;"},
      #tableheader{ text = "QinfoName",   style = "width: 150px;"},
      #tableheader{ text = "ClassCode",   style = "width:  70px;"},
      #tableheader{ text = "ExchName",    style = "width:  70px;"},
      #tableheader{ text = "Type",        style = "width:  60px;"},
      #tableheader{ text = "Lot size",    style = "width:  60px;"},
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
   case build_pages_impl(Commodities, SelectedPage, 1, 0, []) of
      {Res, PageSize, PageCommodities} when length(Res) =< 2 ->
         {#p{id = pages, text="Pages:", style="color: #FFFFFF;"}, PageSize, PageCommodities};
      {Res, PageSize, PageCommodities} ->
         {#p{id = pages, text="Pages: ", body = Res}, PageSize, PageCommodities}
   end.

build_pages_impl([], _, _, PageSize, PageCommodities) ->
   {[], PageSize, PageCommodities};
build_pages_impl(Commodities, SelectedPage, SelectedPage, _, _)
when length(Commodities) =< ?page_size ->
   {
      [#literal{ text = " "}, #literal{ text = integer_to_list(SelectedPage)}],
      length(Commodities),
      Commodities
   };
build_pages_impl(Commodities, _SelectedPage, PageNum, 0, [])
when length(Commodities) =< ?page_size ->
   {
      [#literal{ text = " "}, #link{ text = integer_to_list(PageNum), postback = {page, PageNum}}],
      length(Commodities),
      Commodities
   };
build_pages_impl(Commodities, _SelectedPage, PageNum, PageSize, PageCommodities)
when length(Commodities) =< ?page_size ->
   {
      [#literal{ text = " "}, #link{ text = integer_to_list(PageNum), postback = {page, PageNum}}],
      PageSize,
      PageCommodities
   };
build_pages_impl(Commodities, SelectedPage, SelectedPage, PageSize, PageCommodities) ->
   {Res, _, _} =
      build_pages_impl(lists:nthtail(?page_size, Commodities), SelectedPage, SelectedPage + 1, PageSize, PageCommodities),
   {
      [#literal{ text = " "}, #literal{ text = integer_to_list(SelectedPage)} | Res], ?page_size, Commodities
   };
build_pages_impl(Commodities, SelectedPage, PageNum, PageSize, PageCommodities) ->
   {Res, NewPageSize, NewPageCommodities} =
      build_pages_impl(lists:nthtail(?page_size, Commodities), SelectedPage, PageNum + 1, PageSize, PageCommodities),
   {
      [#literal{ text = " "}, #link{ text = integer_to_list(PageNum), postback = {page, PageNum}} | Res],
       NewPageSize,
       NewPageCommodities
   }.

build_instr_table_impl([], _, _) ->
   [];
build_instr_table_impl(_, 0, _) ->
   [];
build_instr_table_impl([Commodity|Rest], PageSize, BackColor) ->
   [
      build_by_commodity(Commodity, BackColor) |
      build_instr_table_impl(Rest, PageSize - 1, invert_color(BackColor))
   ].

build_by_commodity(#m_commodity{key = Key = {Commodity, Type, Exch}, alias = Alias, class_code = ClassCode, enabled = Enabled}, BackColor) ->
   Instruments = mnesia:dirty_index_read(m_instrument, Key, #m_instrument.commodity),
   SortedInstruments = lists:sort(
      fun(#m_instrument{expiration = Exp1}, #m_instrument{expiration = Exp2}) ->
         Exp1 < Exp2;
      (_,_) -> false end, Instruments),
   Size = length(SortedInstruments),
   lists:foldl(
      fun(#m_instrument{key = {ExchName, _, _}, expiration = Expiration, updated = Updated, full_name = FullName, lot_size = LSize}, []) ->
            CheckId = wf:temp_id(),
            [#tablerow{ cells = [
               #tablecell{ text = ExchName },
               #tablecell{ text = unicode:characters_to_binary(FullName) },
               #tablecell{ text = create_internal_symbol(Exch, Commodity, Alias, Type, Expiration) },
               #tablecell{ text = ClassCode },
               #tablecell{ text = Exch },
               #tablecell{ text = Type },
               #tablecell{ text = LSize },
               #tablecell{ text = expiration_to_list(Expiration)},
               #tablecell{ text = date_to_list(Updated)},
               #tablecell{ body = [ #checkbox{
                        id = CheckId, checked = Enabled,
                        postback = {checkbox_enabled, CheckId, Key}}], rowspan = if (Size == 1) -> 0; true -> Size end},
               #tablecell{ body = [
                     #inplace_textbox2{ tag = Key, text = alias_to_list(Alias), style = "width: 160px;"}
                  ], rowspan = if (Size == 1) -> 0; true -> Size end}
            ], style = "background-color: " ++ BackColor}];
        (#m_instrument{key = {ExchName, _, _}, expiration = Expiration, updated = Updated, full_name = FullName, lot_size = LSize}, Acc) ->
             Acc ++ [#tablerow{ cells = [
               #tablecell{ text = ExchName },
               #tablecell{ text = unicode:characters_to_binary(FullName) },
               #tablecell{ text = create_internal_symbol(Exch, Commodity, Alias, Type, Expiration) },
               #tablecell{ text = ClassCode },
               #tablecell{ text = Exch },
               #tablecell{ text = Type },
               #tablecell{ text = LSize },
               #tablecell{ text = expiration_to_list(Expiration)},
               #tablecell{ text = date_to_list(Updated)}
            ], style = "background-color: " ++ BackColor}]
      end, [], SortedInstruments).

build_instr_table(Commodities, PageSize) ->
   [
      build_instr_header(),
      build_instr_table_impl(Commodities, PageSize, ?white)
   ].

build_instr(Exchanges, Types, OnlyEnabled, Alpha, Page) ->
   {Commodities, AlphaList} = select_commodities(Exchanges, Types, OnlyEnabled, Alpha),
   {Pages, PageSize, PageCommodities} = build_pages(Commodities, Page),
   {
      #p{id = alpha_filter, body = build_alpha_filter(AlphaList)},
      Pages,
      #table{id = instruments, rows = build_instr_table(PageCommodities, PageSize)}
   }.

inplace_textbox_event(Key, []) ->
   [Commodity] = mnesia:dirty_read(m_commodity, Key),
   ok = mnesia:dirty_write(Commodity#m_commodity{alias = undef}),
   {true, "-"};
inplace_textbox_event(Key, "-") ->
   [Commodity] = mnesia:dirty_read(m_commodity, Key),
   ok = mnesia:dirty_write(Commodity#m_commodity{alias = undef}),
   {true, "-"};
inplace_textbox_event(Key, Value) ->
   try valid_alias(Value), uniq_alias(Key, Value) of
      true ->
         [Commodity] = mnesia:dirty_read(m_commodity, Key),
         ok = mnesia:dirty_write(Commodity#m_commodity{alias = Value}),
         event(refresh),
         {true, Value}
   catch
      throw:Err ->
         wf:flash(Err),
         false
   end.

event(refresh) ->
   {AlphaFilter, Pages, Instruments} = build_instr(
      wf:qs(checkbox_exchange), get_type_list(), is_checked(checkbox_enabled), wf:state(alpha), wf:state(page)),
   wf:replace(alpha_filter, AlphaFilter),
   wf:replace(pages, Pages),
   wf:replace(instruments, Instruments);

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
   wf:state(page, PageNum),
   {AlphaFilter, Pages, Instruments} = build_instr(
      wf:qs(checkbox_exchange), get_type_list(), is_checked(checkbox_enabled), wf:state(alpha), PageNum),
   wf:replace(alpha_filter, AlphaFilter),
   wf:replace(pages, Pages),
   wf:replace(instruments, Instruments);

event({checkbox_enabled, CheckId, CommodityKey}) ->
   [Commodity] = mnesia:dirty_read(m_commodity, CommodityKey),
   ok = mnesia:dirty_write(Commodity#m_commodity{ enabled = is_checked(CheckId) }),
   case is_checked(checkbox_enabled) of
      true ->
         event(filter_changed);
      false ->
         ok
   end;

event(alias_changed) ->
   event(refresh);

event(Event) ->
   error_logger:error_msg("Unknown event: ~p", [Event]),
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

valid_alias(Value) ->
   {ok, Re} = re:compile("^[A-Z0-9]+$"),
   case re:run(Value, Re) of
      nomatch ->
         throw("ERROR: Only A..Z, 0..9 symbols are allowed.");
      {match, _} ->
         true
   end.

uniq_alias(Key, Value) ->
   case mnesia:dirty_index_read(m_commodity, Value, #m_commodity.alias) of
     [] ->
        true;
     [#m_commodity{key = Key}|_] ->
        true;
     [#m_commodity{key = Key1}|_] when Key1 =/= Key ->
        throw("ERROR: alias already exists.")
   end.

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
