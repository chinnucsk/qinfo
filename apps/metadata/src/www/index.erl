-module(index).

-include_lib("nitrogen_core/include/wf.hrl").

-export([main/0, layout/0, title/0]).

title() ->
   "qinfo:main".

main() ->
   #template{ file="./www/page.html"}.

layout() ->
   TopPanel = #panel{
      body =
      [
         #literal{ text = "main | "},
         #link{ class=a, text = "settings", url = "settings"},
         #literal{ text = " | " },
         #link{ class=a, text = "scheduler", url = "scheduler"},
         #literal{ text = " | " },
         #link{ class=a, text = "instruments", url = "instruments"},
         #literal{ text = " | "},
         %#link{ class=a, text = "statistics", url = "statistics"},
         %#literal{ text = " | "},
         #link{ class=a, text = "logs", url = "logs"}
      ]
   },
   [
      TopPanel,
      #p{},
      build_apps()
   ].

build_apps() ->
   Apps = application:loaded_applications(),
   Rows = lists:foldr(
      fun({Name, _, Ver}, Acc) ->
         [
            #tablerow{cells =
               [
                  #tablecell{text = atom_to_list(Name)},
                  #tablecell{text = Ver}
               ]
            } | Acc
         ]
      end, [], Apps),
   #table{ rows =
      [
         #tablerow{ cells =
            [
               #tableheader{ text = "Loaded apps", style = "width: 100px;"},
               #tableheader{ text = "Version", style = "width: 100px;"}
            ]} | Rows
      ]}.
