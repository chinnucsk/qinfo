#!/bin/bash

export ERL_LIBS=.

erl -boot start_sasl -metadata inets_port 8000 -config start.config \
   -eval "application:start(nprocreg)" \
   -eval "application:start(metadata)"
