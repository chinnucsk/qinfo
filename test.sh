#!/bin/bash

ERL_LIBS=build/ebin ./rebar eunit
ERL_LIBS=build/ebin ./rebar ct
