-module(instruments).

-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("metadata/include/metadata.hrl").

-export([main/0, layout/0, event/1]).

main() ->
   #template{ file="./www/page.html"}.

layout() ->
   TopPanel = #panel{
      body = [
         #link{ class=a, text = "main", url = "index"},
         #literal{ text = " | "},
         #link{ text = "settings", url = "settings"},
         #literal{ text = " | " },
         #literal{ class=a, text = "instruments"},
         #literal{ text = " | "},
         #link{ class=a, text = "statistics", url = "statistics"}
      ]
   },
   {Filter, Exchanges} = build_filter(),
   {AlphaFilter, Instruments} = build_instr($0, Exchanges),
   [
      TopPanel,
      #p{},
      Filter,
      AlphaFilter,
      #p{},
      Instruments
   ].

build_filter() ->
   {Checkboxes, Exchanges} = build_exchanges(mnesia:dirty_first(m_exchange), []),
   {[
      #panel{ body = Checkboxes },
      #panel{ body = [ #checkbox{ id = checkbox_enabled, text = "Only enabled instruments", postback = filter_changed}]}
   ],
   Exchanges}.

build_exchanges('$end_of_table', Exchanges) ->
   {[], Exchanges};
build_exchanges(Key, Exchanges) ->
   [#m_exchange{ name = ExchName }] = mnesia:dirty_read(m_exchange, Key),
   {Checkboxes, NewExchanges} = build_exchanges(mnesia:dirty_next(m_exchange, Key), [ExchName | Exchanges]),
   {[
      #checkbox{
         id= checkbox_exchange,
         text = ExchName,
         checked = true,
         postback = filter_changed}
      | Checkboxes],
   NewExchanges}.

build_instr_header() ->
   #tablerow{ cells = [
      #tableheader{ text = "Name", style = "width: 100px;"},
      #tableheader{ text = "Full name", style = "width: 300px;"},
      #tableheader{ text = "ExchName", style = "width: 100px;"},
      #tableheader{ text = "Lot size", align = center, style = "width: 80px;"},
      #tableheader{ text = "Enabled", align = center}
   ], style = "background-color: #999797;"}.

build_instr(Alpha, Exchanges) ->
   AlphaList = lists:flatten(
      [ [ [#literal{ text = [X]}, #literal{ text = " "}] || X <- lists:seq($0, $9)],
        [ [#literal{ text = [X]}, #literal{ text = " "}] || X <- lists:seq($A, $Z)]]),
   EnabledOnly = get_enabled_only(),
   {NewAlphaList, Instrs} = build_instr_impl(Alpha, AlphaList, EnabledOnly, Exchanges, mnesia:dirty_first(m_instrument)),
   InstrHeader = build_instr_header(),
   {#panel{ id = alpha_filter, body = NewAlphaList}, #table{ id = instruments, rows = [InstrHeader, Instrs]}}.

build_instr_impl(_,  AlphaList, _, _, '$end_of_table') ->
   {AlphaList, []};
build_instr_impl(Alpha, AlphaList, EnabledOnly, SelectedExchs, Key) ->
   [#m_instrument{
         commodity = [FL|_] = Commodity,
         exch_name = ExchName,
         exchange = Exch,
         full_name = FullName,
         lot_size = LS}] = mnesia:dirty_read(m_instrument, Key),
   case is_in_filter(SelectedExchs, EnabledOnly, Exch, false) of
      true when Alpha == FL ->
         {NewAlphaList, Instrs} = build_instr_impl(Alpha, AlphaList, EnabledOnly, SelectedExchs, mnesia:dirty_next(m_instrument, Key)),
         NewAlphaList2 = replace_to_link(Alpha, NewAlphaList),
         {NewAlphaList2, [
            #tablerow{ cells = [
               #tablecell{ text = ExchName },
               #tablecell{ text = unicode:characters_to_binary(FullName) },
               #tablecell{ text = Exch },
               #tablecell{ text = LS, align = center },
               #tablecell{ body = [ #checkbox{ checked = false}], align = center }
            ]} | Instrs]};
      true ->
         NewAlphaList = replace_to_link(Commodity, AlphaList),
         build_instr_impl(Alpha, NewAlphaList, EnabledOnly, SelectedExchs, mnesia:dirty_next(m_instrument, Key));
      false ->
         build_instr_impl(Alpha, AlphaList, EnabledOnly, SelectedExchs, mnesia:dirty_next(m_instrument, Key))
   end.

event({alpha, A}) ->
   {AlphaFilter, Instruments} = build_instr(A, wf:qs(checkbox_exchange)),
   wf:replace(alpha_filter, AlphaFilter),
   wf:replace(instruments, Instruments);
event(filter_changed) ->
   {AlphaFilter, Instruments} = build_instr($0, wf:qs(checkbox_exchange)),
   wf:replace(alpha_filter, AlphaFilter),
   wf:replace(instruments, Instruments).

get_enabled_only() ->
   case wf:q(checkbox_enabled) of
      "on" ->
         true;
      _ ->
         false
   end.

replace_to_link([FL|_], AlphaList) ->
   replace_to_link(FL, AlphaList);
replace_to_link(Alpha, AlphaList) ->
   lists:foldr(fun(#literal{text = T}, Acc) when T == [Alpha] ->
                     [ #link{ text = [Alpha], postback = {alpha, Alpha}} | Acc ];
                  (#literal{text = "0-9"}, Acc) when Alpha > $0 andalso Alpha =< $9 ->
                     [ #link{ text = "0-9", postback = {alpha, Alpha}} | Acc ];
                  (X, Acc) -> [X|Acc]
               end, [], AlphaList).

is_in_filter(SelectedExchs, EnabledOnly, Exch, Enabled) ->
   (EnabledOnly == false orelse (EnabledOnly == true andalso Enabled == true)) andalso
   lists:member(Exch, SelectedExchs);

is_in_filter(_, _, _, _) ->
   false.
