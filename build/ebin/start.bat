set ERL_LIBS=%CD%

werl -boot start_sasl -metadata inets_port 8000 -config start.config -eval "application:start(metadata)"