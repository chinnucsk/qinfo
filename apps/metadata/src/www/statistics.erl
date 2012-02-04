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
         #link{ class=a, text = "scheduler", url = "scheduler"},
         #literal{ text = " | " },
         #link{ class=a, text = "instruments", url = "instruments"},
         #literal{ text = " | "},
         #literal{ text = "statistics"}
      ]
   },
   [
      TopPanel,
      #table{ style="border-size: 2px; border-spacing: 10px;", rows=[
            #tablerow{ cells=[
                  #tablecell{ body = #button{ text="sdfsdfsdf", postback=show } },
                  #tablecell{ body = #button{ text="sdfsdfsd", postback=hide } }
            ]},
            #tablerow{ cells=[
               #tablecell{ text="sdfsdf"},
               #tablecell{ text="sdfsdfsd"}
            ]}
      ]},
   #lightbox { id=lightbox1, body=[
  #panel { class=myPanel, body=[
    #h1 { text="Title" },
    "Some body text.",
    #button { text="Close" }
  ]}
]}
   ].

event(show) ->
   wf:wire(div1, #appear{speed = 500});
event(hide) ->
   wf:wire(div1, #hide{});
event(button_ok) ->
   wf:wire(div1, #hide{});
event(button_cancel) ->
   wf:wire(div1, #hide{}).
