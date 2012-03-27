-module(rdisplay).

-include_lib("nitrogen_core/include/wf.hrl").

-export([main/0, layout/0, title/0]).

title() -> "".

main() ->
   #template{file="./www/rdisplay.html"}.

layout() ->
   {ok, Pid} = wf:comet(fun() -> process_msg() end, rdetails),
   wf:session(rdetails, Pid),
   #panel{id = field}.


process_msg() ->
   receive
      'INIT' ->
         wf:insert_bottom(field, #literal{text = "First one!"}),
         wf:flush(),
         process_msg();
      {details, No} ->
         log_viewer:show(No),
         wf:flush(),
         process_msg()
   end.