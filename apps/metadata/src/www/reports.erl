-module(reports).

-include_lib("nitrogen_core/include/wf.hrl").

-export([main/0, layout/0, title/0, event/1]).

-define(delimiter, #literal{text=" "}).
-define(def_rec_on_page, 30).

title() ->
   "qinfo:logs".

main() ->
   #template{file="./www/reports.html"}.

layout() ->
   wf:session(rec_on_page, ?def_rec_on_page),
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
   RescanFilter = #p{body = [#literal{text = "Max records"}, #dropdown{id = max_reports, value = "100", options=
                                                                       [
                  #option{text = "100",  value = "100"},
                  #option{text = "500",  value = "500"},
                  #option{text = "1000", value = "1000"},
                  #option{text = "all",  value = "all"}
                  ]}, #button{id = rescan_button, text = "Rescan", postback = click_rescan}]},
   RecordFilter = #p{body = [#literal{text = "Grep"}, #textbox{id = reg_exp},
                             ?delimiter,
                             #literal{text = "Records on page"},
                             #dropdown{id = rec_on_page, value = erlang:integer_to_list(?def_rec_on_page), options=
                                       [
                  #option{text = "30",  value = "30"},
                  #option{text = "50",  value = "50"},
                  #option{text = "100", value = "100"}
                  ]}, #button{id = apply_button, text = "Apply", postback = click_apply}]},
   Types = build_types(),
   {Records, Pages} = build_records([], 1, ?def_rec_on_page),
   [
      TopPanel,
      #p{},
      RescanFilter,
      RecordFilter,
      Types,
      Pages,
      Records
      ].

build_records(Filters, Page, RecOnPage) ->
   Records = log_viewer:list(Filters),
   Pages = #p{id = pages, body = build_pages(length(Records), Page, RecOnPage)},
   Header = #tablerow{ cells = [
            #tableheader{text = "Date",         style = "width: 150px;"},
            #tableheader{text = "NN",           style = "width: 70px;" },
            #tableheader{text = "Type",         style = "width: 70px;" },
            #tableheader{text = "Process",      style = "width: 150px;"}
            ], style = "background-color: #999797;"},
   OffSet = (Page - 1) * RecOnPage,
   {#table{id = records, rows = [Header|build_records(lists:nthtail(OffSet, Records), RecOnPage)]}, Pages}.
build_records([], _) ->
   [];
build_records(_, 0) ->
   [];
build_records([{No, Type,  ShortDescr, Date}|T], RecNum) ->
   [
      #tablerow{actions = #event{type = click, postback={details, No}},
            cells = [
            #tablecell{text = common_utils:date_to_str(Date, true)},
            #tablecell{text = No},
            #tablecell{text = Type},
            #tablecell{text = ShortDescr}], class=get_class(Type)} | build_records(T, RecNum - 1)
      ].

build_pages(Records, _Page, RecOnPage) when Records =< RecOnPage ->
   [];
build_pages(Records, Page, RecOnPage) ->
   build_pages(Records, Page, RecOnPage, 1).
build_pages(Records, Page, RecOnPage, CurrPage) when Records =< RecOnPage ->
   if
      Page == CurrPage ->
         [#literal{text = integer_to_list(Page)}];
      true ->
         [#link{text = integer_to_list(CurrPage), postback = {click_page, CurrPage}}]
   end;
build_pages(Records, Page, RecOnPage, Page) ->
   [#literal{text = integer_to_list(Page)}, ?delimiter | build_pages(Records - RecOnPage, Page, RecOnPage, Page + 1)];
build_pages(Records, Page, RecOnPage, CurrPage) ->
   [#link{text = integer_to_list(CurrPage), postback = {click_page, CurrPage}}, ?delimiter | build_pages(Records - RecOnPage, Page, RecOnPage, CurrPage + 1)].

build_types() ->
   Types = log_viewer:get_types(),
   #panel{id = types, body = [#literal{text = "Types"}, build_types(Types)]}.
build_types([]) ->
   [];
build_types([Type|Tail]) ->
   [#checkbox{id = checkbox_type, text = Type, value = Type, checked = true} | build_types(Tail)].

get_filter() ->
   Types = lists:foldr(fun(Type, Acc) -> [common_utils:list_to_atom(Type)|Acc] end, [], wf:qs(checkbox_type)),
   RegExp = wf:q(reg_exp),
   [{types, Types}, {reg_exp, RegExp}].

event({click_page, N}) ->
   {Records, Pages} = build_records(get_filter(), N, wf:session(rec_on_page)),
   wf:replace(records, Records),
   wf:replace(pages, Pages);
event(click_apply) ->
   RecOnPage = list_to_integer(wf:q(rec_on_page)),
   wf:session(rec_on_page, RecOnPage),
   {Records, Pages} = build_records(get_filters(), 1, RecOnPage),
   wf:replace(records, Records),
   wf:replace(pages, Pages);
event(M = {details, _No}) ->
   Pid = wf:session(rdetails),
   Pid ! M;
event(click_rescan) ->
   MaxReportsStr = wf:q(max_reports),
   MaxReports =
   case MaxReportsStr of
      "all" ->
         common_utils:list_to_atom(MaxReportsStr);
      N ->
         list_to_integer(N)
   end,
   log_viewer:rescan(MaxReports),
   Types = build_types(),
   wf:replace(types, Types),
   event(click_apply).

get_class(Type) when Type == error orelse Type == error_report orelse Type == crash_report ->
   "error";
get_class(Type) when Type == warning ->
   "warning";
get_class(_) ->
   "".