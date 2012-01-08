#!/bin/bash

mkdir -p rel/ebin/metadata
mkdir -p rel/ebin/nitrogen_core
mkdir -p rel/ebin/nprocreg
mkdir -p rel/ebin/simple_bridge
mkdir -p rel/ebin/site

cd deps/nitrogen_core
erl -pa ebin -make
cp ebin/*.beam ../../rel/ebin/nitrogen_core

cd ../nprocreg
erl -pa ebin -make
cp ebin/*.beam ../../rel/ebin/nprocreg
cp src/nprocreg.app ../../rel/ebin

cd ../simple_bridge
erl -pa ebin -make
cp ebin/*.beam ../../rel/ebin/simple_bridge

cd ../../metadata

erl -make
cp ebin/*.beam ../rel/ebin/metadata
cp src/metadata.app ../rel/ebin
cp -r src/web_ui/templates ../rel/ebin/site
cp -r src/web_ui/static ../rel/ebin/site

cd ../..
