-module(index).

-include_lib("nitrogen_core/include/wf.hrl").

-export([main/0, layout/0]).

main() ->
   #template{ file="./www/page.html"}.

layout() ->
   #panel{
      body =
      [
         #literal{ text = "main | "},
         #link{ class=a, text = "settings", url = "settings"},
         #literal{ text = " | " },
         #link{ class=a, text = "scheduler", url = "scheduler"},
         #literal{ text = " | " },
         #link{ class=a, text = "instruments", url = "instruments"},
         #literal{ text = " | "},
         #link{ class=a, text = "statistics", url = "statistics"}
      ]
   }.
