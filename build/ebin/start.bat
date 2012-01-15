set ERL_LIBS=%CD%

werl -boot start_sasl -config start.config -eval "application:start(metadata)"