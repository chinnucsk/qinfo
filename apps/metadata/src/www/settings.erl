-module(settings).

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
         #literal{ text = "settings"},
         #literal{ text = " | " },
         #link{ text = "instruments", url = "instruments"},
         #literal{ text = " | "},
         #link{ class=a, text = "statistics", url = "statistics"}
      ]
   }.
