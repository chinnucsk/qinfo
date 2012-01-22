-module(instruments).

-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("metadata/include/metadata.hrl").

-export([main/0, layout/0]).

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
   [TopPanel, Filter].

build_filter() ->
   AlphaBody = [ [#link{ class = list_to_atom([X]), text = [X] }, #literal{ text = " "}] || X <- lists:seq($A, $Z) ],
   [
      #panel{ body = build_exchanges(mnesia:dirty_first(m_exchange))},
      #panel{ body = [ #checkbox{ id = only_enabled, text = "Only enabled instruments"}]},
      #panel{
      body = [
         #link{ class = digits, text = "0-9" },
         #literal{ text = " " }
         | AlphaBody
      ]}
   ].

build_exchanges('$end_of_table') ->
   [];
build_exchanges(Key) ->
   [#m_exchange{ name = ExchName }] = mnesia:dirty_read(m_exchange, Key),
   [
      #checkbox{ text = ExchName, checked = true}
      | build_exchanges(mnesia:dirty_next(m_exchange, Key))
   ].
