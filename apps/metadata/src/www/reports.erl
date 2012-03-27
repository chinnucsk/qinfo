-module(reports).

-include_lib("nitrogen_core/include/wf.hrl").

-export([main/0, layout/0, title/0, event/1]).

title() ->
   "qinfo:logs".

main() ->
   #template{file="./www/reports.html"}.

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
         %#link{ class=a, text = "statistics", url = "statistics"}
         #literal{ text = " | "},
         #literal{ text = "logs"}
      ]
   },
   Records = build_records(),
   [
      TopPanel,
      Records
   ].

build_records() ->
   Records = log_viewer:list(),
   Header = #tablerow{ cells = [
      #tableheader{text = "Date",         style = "width: 150px;"},
      #tableheader{text = "NN",           style = "width: 70px;" },
      #tableheader{text = "Type",         style = "width: 70px;" },
      #tableheader{text = "Process",      style = "width: 150px;"}
   ], style = "background-color: #999797;"},
   #table{rows = [ Header | build_records(Records)]}.
build_records([]) ->
   [];
build_records([{No, Type,  ShortDescr, Date}|T]) ->
   [
      #tablerow{ cells = [
         #tablecell{text = Date},
         #tablecell{body = [#link{text = No, postback = {details, No}}]},
         #tablecell{text = Type},
         #tablecell{text = ShortDescr}]} | build_records(T)
   ].

event(M = {details, _No}) ->
   Pid = wf:session(rdetails),
   Pid ! M.