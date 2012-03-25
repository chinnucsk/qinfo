-module(statistics).

-include_lib("nitrogen_core/include/wf.hrl").

-export([main/0, layout/0, title/0]).

title() ->
   "qinfo:statistics".

main() ->
   #template{file="./www/page.html"}.

layout() ->
   TopPanel = #panel{
      body =
      [
         #link{ class=a, text = "main", url = "index"},
         #literal{ text = " | "},
         #link{ class=a, text = "settings", url = "settings"},
         #literal{ text = " | " },
         #link{ class=a, text = "scheduler", url = "scheduler"},
         #literal{ text = " | " },
         #link{ class=a, text = "instruments", url = "instruments"},
         #literal{ text = " | "},
         #literal{ text = "statistics"},
         #literal{ text = " | "},
         #link{ class=a, text = "logs", url = "logs"}
      ]
   },
   TopPanel.
