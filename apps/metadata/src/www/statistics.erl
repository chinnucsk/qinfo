-module(statistics).

-include_lib("nitrogen_core/include/wf.hrl").

-export([main/0, layout/0]).

main() ->
   #template{ file="./www/page.html"}.

layout() ->
   TopPanel = #panel{
      body =
      [
         #link{ class=a, text = "main", url = "index"},
         #literal{ text = " | "},
         #link{ class=a, text = "settings", url = "settings"},
         #literal{ text = " | " },
         #link{ class=a, text = "instruments", url = "instruments"},
         #literal{ text = " | "},
         #literal{ text = "statistics"}
      ]
   },
   [TopPanel, #panel{ text = <<208,159,209,128,208,184,208,178,208,181,209,130>>}].
