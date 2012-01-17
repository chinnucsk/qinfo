-module(settings).

-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("metadata/include/metadata.hrl").

-export([main/0, layout/0]).

main() ->
   #template{ file="./www/page.html"}.

layout() ->
   TopPanel = #panel{
      body =
      [
         #link{ class=a, text = "main", url = "index"},
         #literal{ text = " | "},
         #literal{ text = "settings"},
         #literal{ text = " | " },
         #link{ text = "instruments", url = "instruments"},
         #literal{ text = " | "},
         #link{ class=a, text = "statistics", url = "statistics"}
      ]
   },
   Body = build([], mnesia:dirty_first(m_service)),
   [
      TopPanel,
      #p{},
      #button{ text = "Apply" },
      #literal{ text = " " },
      #button{ text = "Save" },
      #p{}|
      Body
   ].

build(Body, '$end_of_table') ->
   Body;
build(Body, Key) ->
   [#m_service{description = Descr, settings = Settings}] = mnesia:dirty_read(m_service, Key),
   build(
      [
         #p{},
         #literal{ text = Descr },
         #table{ rows = [ build_settings(Settings) ]}|
         Body
      ], mnesia:dirty_next(m_service, Key)
   ).

build_settings([]) ->
   [];
build_settings([{Name, Value, _}|Rest]) when is_binary(Value) ->
   [#tablerow{ cells = [ #tablecell{ text = Name }, #tablecell{ body = #textarea{ text = Value} } ] } | build_settings(Rest)];
build_settings([{Name, Value, _}|Rest]) ->
   [#tablerow{ cells = [ #tablecell{ text = Name }, #tablecell{ body = #textbox{ text = Value} } ] } | build_settings(Rest)].
