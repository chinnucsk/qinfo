-module(statistics).

-include_lib("nitrogen_core/include/wf.hrl").

-export([main/0, layout/0, event/1]).

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
   [TopPanel, #table{ rows = [
         #tablerow{ cells = [
               #tablecell{ text = "sdfsdf"},
               #tablecell{ body = [ #textbox{ text = "123", postback = a} ]}
            ]}
      ]}].

event(E) ->
   io:format("~p~n", [E]).
