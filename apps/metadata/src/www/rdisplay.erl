-module(rdisplay).

-include_lib("nitrogen_core/include/wf.hrl").

-export([main/0, layout/0, title/0]).

title() ->
   "qinfo:logs".

main() ->
   #template{file="./www/rdisplay.html"}.

layout() ->
   "".
