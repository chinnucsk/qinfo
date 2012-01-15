-module(instruments).

-include_lib("nitrogen_core/include/wf.hrl").

-export([main/0, layout/0]).

main() ->
   #template{ file="./www/page.html"}.

layout() ->
   #panel{
      body =
      [
         #link{ class=a, text = "main", url = "index"},
         #literal{ text = " | "},
         #link{ text = "settings", url = "settings"},
         #literal{ text = " | " },
         #literal{ class=a, text = "instruments"},
         #literal{ text = " | "},
         #link{ class=a, text = "statistics", url = "statistics"}
      ]
   }.
