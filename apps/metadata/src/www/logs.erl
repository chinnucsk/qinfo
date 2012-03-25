-module(logs).

-include_lib("nitrogen_core/include/wf.hrl").

-export([main/0, layout/0, title/0]).

title() ->
   "qinfo:statistics".

main() ->
   #template{file="./www/logs.html"}.

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
         %#literal{ text = " | "},
         %#link{ class=a, text = "statistics", url = "statistics"},
         #literal{ text = " | "},
         #literal{ text = "logs"}
      ]
   },
   TopPanel.
