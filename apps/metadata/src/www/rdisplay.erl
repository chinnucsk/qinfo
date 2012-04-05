-module(rdisplay).

-include_lib("nitrogen_core/include/wf.hrl").

-export([main/0, layout/0, title/0]).

-define(line, 79).

title() -> "".

main() ->
   #template{file="./www/rdisplay.html"}.

layout() ->
   {ok, Pid} = wf:comet(fun() -> process_msg() end, rdetails),
   wf:session(rdetails, Pid),
   #panel{id = rec_details, body = []}.


process_msg() ->
   receive
      'INIT' ->
         process_msg();
      {details, No} ->
         Report = format_report:format(log_viewer:show(No)),
         Res = #panel{id = rec_details, body = Report},
         wf:replace(rec_details, Res),
         wf:flush(),
         process_msg()
   end.