-module(instruments).

-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("metadata/include/metadata.hrl").

-export([main/0, layout/0, event/1, inplace_textbox_event/2, valid_alias/1, uniq_alias/2, select_commodities/4]).

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
   Alpha = $0,
   wf:state(alpha, Alpha),
   %Pages = build_pages(Alpha),
   {AlphaFilter, Instruments} = build_instr(Alpha, Exchanges, ?type_list),
   [
      TopPanel,
      #flash{},
      #p{},
      Filter,
      AlphaFilter,
      #p{},
      Instruments
   ].

%build_pages(Alpha) ->
%   {atomic, {_, _, Pages}} = mnesia:transaction(
%      fun() ->
%         mnesia:foldr(
%            fun(#m_commodity{ key = {[Alpha1|_], _, _}}, {0, PNum, Acc}) when Alpha == Alpha1 ->
%                  {5, PNum + 1, [ #link{text = integer_to_list(PNum)}, #literal{ text = " " } | Acc ]};
%               (#m_commodity{ key = {[Alpha1|_], _, _}}, {PSize, PNum, Acc}) when Alpha == Alpha1 ->
%                  {PSize - 1, PNum, Acc};
%               (_, Acc) ->
%                  Acc
%            end, {5, 1, []}, m_commodity)
%      end),
%   [#literal{ text = "Pages: "}, Pages].

select_commodities(Alpha, Exchanges, Types, OnlyEnabled) ->
   {atomic, Commodities} = mnesia:transaction(
      fun() ->
         mnesia:foldr(
            fun(C = #m_commodity{ key = {[Alpha1|_], Type, Exchange}, enabled = Enabled}, Acc) ->
               Res = Alpha == Alpha1 andalso lists:member(Type, Types) andalso
               lists:member(Exchange, Exchanges) andalso (OnlyEnabled == false orelse Enabled == true),
               if Res == true ->
                  [C | Acc];
               true ->
                  Acc
               end
            end
            , [], m_commodity)
      end
   ),
   Commodities.

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

build_instr(Alpha, Exchanges, TypeList) ->
   AlphaList = lists:flatten(
      [ [ [#literal{ text = [X]}, #literal{ text = " "}] || X <- lists:seq($0, $9)],
        [ [#literal{ text = [X]}, #literal{ text = " "}] || X <- lists:seq($A, $Z)]]),
   EnabledOnly = is_checked(checkbox_enabled),
   {NewAlphaList, Instrs} = build_instr_impl(Alpha, AlphaList, TypeList, EnabledOnly, Exchanges, mnesia:dirty_first(m_commodity),
   ?white),
   InstrHeader = build_instr_header(),
   {#panel{ id = alpha_filter, body = NewAlphaList}, #table{ id = instruments, rows = [InstrHeader, Instrs]}}.

build_instr_impl(_,  AlphaList, _, _, _, '$end_of_table', _) ->
   {AlphaList, []};
build_instr_impl(Alpha, AlphaList, TypeList, EnabledOnly, SelectedExchs, Key, BackColor) ->
   [Commodity = #m_commodity{key = {[FL|_], _, _}}] = mnesia:dirty_read(m_commodity, Key),
   case is_in_filter(TypeList, SelectedExchs, EnabledOnly, Commodity) of
      true when Alpha == FL ->
         NewAlphaList = replace_to_link(FL, AlphaList),
         CommodityInstrs = build_by_commodity(Commodity, BackColor),
         {NewAlphaList2, Instrs} = build_instr_impl(Alpha, NewAlphaList, TypeList, EnabledOnly, SelectedExchs,
            mnesia:dirty_next(m_commodity, Key), invert_color(BackColor)),
         {
            NewAlphaList2,
            [CommodityInstrs, Instrs]
         };
      true ->
         NewAlphaList = replace_to_link(FL, AlphaList),
         build_instr_impl(Alpha, NewAlphaList, TypeList, EnabledOnly, SelectedExchs, mnesia:dirty_next(m_commodity, Key),
            BackColor);
      false ->
         build_instr_impl(Alpha, AlphaList, TypeList, EnabledOnly, SelectedExchs, mnesia:dirty_next(m_commodity, Key), BackColor)
   end.

build_by_commodity(#m_commodity{key = Key = {Commodity, Type, Exch}, class_code = ClassCode, alias = Alias, enabled = Enabled}, BackColor) ->
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
         event(filter_changed),
         {true, Value}
   catch
      throw:Err ->
         wf:flash(Err),
         false
   end.

event({alpha, A}) ->
   wf:state(alpha, A),
   {AlphaFilter, Instruments} = build_instr(A, wf:qs(checkbox_exchange), get_type_list()),
   wf:replace(alpha_filter, AlphaFilter),
   wf:replace(instruments, Instruments);

event(filter_changed) ->
   {AlphaFilter, Instruments} = build_instr(wf:state(alpha), wf:qs(checkbox_exchange), get_type_list()),
   wf:replace(alpha_filter, AlphaFilter),
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
   event(filter_changed).

is_checked(Checkbox) ->
   case wf:q(Checkbox) of
      "on" ->
         true;
      _ ->
         false
   end.

replace_to_link(Alpha, AlphaList) ->
   lists:foldr(fun(#literal{text = T}, Acc) when T == [Alpha] ->
                     [ #link{ text = [Alpha], postback = {alpha, Alpha}} | Acc ];
                  (X, Acc) -> [X|Acc]
               end, [], AlphaList).

is_in_filter(TypeList, SelectedExchs, EnabledOnly, #m_commodity{key = {_, Type, Exch}, enabled = Enabled}) ->
   (EnabledOnly == false orelse (EnabledOnly == true andalso Enabled == true)) andalso
   lists:member(Exch, SelectedExchs) andalso
   lists:member(Type, TypeList).

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
