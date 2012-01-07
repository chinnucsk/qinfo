#!/bin/bash

mkdir -p rel/ebin

cd metadata
erl -make
cp ebin/* ../rel/ebin
cp src/metadata.app ../rel/ebin

cd ..

# build nitrogen framwwork

mkdir -p rel/ebin/nitrogen_core
mkdir -p rel/ebin/nprocreg
mkdir -p rel/ebin/simple_bridge

cd web_ui/nitrogen_core
erl -pa ebin -make
cp ebin/* ../../rel/ebin/nitrogen_core

cd ../nprocreg
erl -pa ebin -make
cp ebin/* ../../rel/ebin/nprocreg
cp src/nprocreg.app ../../rel/ebin

cd ../simple_bridge
erl -pa ebin -make
cp ebin/* ../../rel/ebin/simple_bridge

cd ../../
