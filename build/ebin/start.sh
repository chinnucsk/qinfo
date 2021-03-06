#!/bin/bash

export ERL_LIBS=.

mkdir -p log

erl -boot start_sasl \
    -name 'metadata@192.168.1.171' \
    -metadata inets_port 8000 \
    -config start.config \
    -setcookie qinfo \
    -eval "application:start(nprocreg)" \
    -eval "application:start(metadata)" \
    -eval "application:start(log_viewer)"
