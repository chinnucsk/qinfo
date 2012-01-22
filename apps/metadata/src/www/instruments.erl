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
   Filter = build_filter(),
   {AlphaFilter, Instruments} = build_instr(digits),
   [TopPanel, Filter, AlphaFilter, Instruments].

build_filter() ->
   [
      #panel{ body = build_exchanges(mnesia:dirty_first(m_exchange))},
      #panel{ body = [ #checkbox{ id = checkbox_enabled, text = "Only enabled instruments", postback = filter_changed}]}
   ].

build_exchanges('$end_of_table') ->
   [];
build_exchanges(Key) ->
   [#m_exchange{ name = ExchName }] = mnesia:dirty_read(m_exchange, Key),
   [
      #checkbox{
         id= common_utils:list_to_atom("checkbox_" ++ ExchName),
         text = ExchName,
         checked = true,
         postback = filter_changed}
      | build_exchanges(mnesia:dirty_next(m_exchange, Key))
   ].

build_instr_header() ->
   #tablerow{ cells = [
      #tableheader{ text = "Name", style="width: 100px;"},
      #tableheader{ text = "Full name"},
      #tableheader{ text = "Lot size"}]}.

build_instr(Alpha) ->
   AlphaList = lists:flatten(
      [ #literal{ text="0-9"} , #literal{ text = " "}, [ [#literal{ text = [X]}, #literal{ text = " "}] || X <- lists:seq($A, $Z)]]),
   SelectedExchs = get_selected_exchanges(mnesia:dirty_first(m_exchange)),
   EnabledOnly = get_enabled_only(),
   {NewAlphaList, Instrs} = build_instr_impl(Alpha, AlphaList, EnabledOnly, SelectedExchs, mnesia:dirty_first(m_instrument)),
   InstrHeader = build_instr_header(),
   {#panel{ id = alpha_filter, body = NewAlphaList}, #table{ id = instruments, rows = [InstrHeader, Instrs]}}.

build_instr_impl(_,  AlphaList, _, _, '$end_of_table') ->
   {AlphaList, []};
build_instr_impl(Alpha, AlphaList, EnabledOnly, SelectedExchs, Key) ->
   [I = #m_instrument{ commodity = Commodity, full_name = FullName, lot_size = LS}] = mnesia:dirty_read(m_instrument, Key),
   case is_in_filter(SelectedExchs, EnabledOnly, Alpha, I) of
      true ->
         {NewAlphaList, Instrs} = build_instr_impl(Alpha, AlphaList, EnabledOnly, SelectedExchs, mnesia:dirty_next(m_instrument, Key)),
         NewAlphaList2 = replace_to_link(Alpha, NewAlphaList),
         {NewAlphaList2, [
            #tablerow{ cells = [
               #tablecell{ text = Commodity },
               #tablecell{ text = unicode:characters_to_binary(FullName) },
               #tablecell{ text = LS }
            ]} | Instrs]};
      false ->
         NewAlphaList = replace_to_link(Commodity, AlphaList),
         build_instr_impl(Alpha, NewAlphaList, EnabledOnly, SelectedExchs, mnesia:dirty_next(m_instrument, Key))
   end.

event({alpha, A}) ->
   {AlphaFilter, Instruments} = build_instr(A),
   wf:replace(alpha_filter, AlphaFilter),
   wf:replace(instruments, Instruments);
event(filter_changed) ->
   {AlphaFilter, Instruments} = build_instr(digits),
   wf:replace(alpha_filter, AlphaFilter),
   wf:replace(instruments, Instruments).

get_selected_exchanges('$end_of_table') ->
   [];
get_selected_exchanges(Key) ->
   [#m_exchange{ name = Name}] = mnesia:dirty_read(m_exchange, Key),
   Id = common_utils:list_to_atom("checkbox_" ++ Name),
   case wf:q(Id) of
      "on" ->
         [Name | get_selected_exchanges(mnesia:dirty_next(m_exchange, Key))];
      _ ->
         get_selected_exchanges(mnesia:dirty_next(m_exchange, Key))
   end.

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
                  (#literal{text = "0-9"}, Acc) when Alpha == digits ->
                     [ #link{ text = "0-9", postback = {alpha, digits}} | Acc ];
                  (X, Acc) -> [X|Acc]
               end, [], AlphaList).

is_in_filter(SelectedExchs, EnabledOnly, Alpha, #m_instrument{ commodity = [A | _], exchange = Exch, enabled = Enabled})
   when Alpha >= $A andalso Alpha =< $Z ->
   Alpha == A andalso
   (EnabledOnly == false orelse (EnabledOnly == true andalso Enabled == true)) andalso
   lists:member(Exch, SelectedExchs);

is_in_filter(SelectedExchs, EnabledOnly, digits, #m_instrument{ commodity = [A | _], exchange = Exch, enabled = Enabled})
   when A >= $0 andalso A =< $9 ->
   (EnabledOnly == false orelse (EnabledOnly == true andalso Enabled == true)) andalso
   lists:member(Exch, SelectedExchs);

is_in_filter(_, _, _, _) ->
   false.
