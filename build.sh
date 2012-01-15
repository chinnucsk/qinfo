#!/bin/bash

./rebar compile

mkdir -p build/ebin/nitrogen_core/ebin
mkdir -p build/ebin/nprocreg/ebin
mkdir -p build/ebin/simple_bridge/ebin
mkdir -p build/ebin/metadata/ebin
mkdir -p build/ebin/www

cp -r apps/common/ebin/*.app build/ebin
cp -pr apps/nitrogen_core/ebin/*.beam build/ebin/nitrogen_core/ebin
cp -r apps/nprocreg/ebin/*.beam build/ebin/nprocreg/ebin
cp -r apps/nprocreg/ebin/*.app build/ebin/
cp -r apps/simple_bridge/ebin/*.beam build/ebin/simple_bridge/ebin
cp -r apps/simple_bridge/ebin/*.app build/ebin/
cp -r apps/metadata/ebin/*.beam build/ebin/metadata/ebin
cp -r apps/metadata/ebin/*.app build/ebin/
cp -r apps/metadata/src/www/static/* build/ebin/www
