#!/bin/bash

mkdir -p build/cpp
cd build/cpp
cmake ../../cpp
make
cd ../../

./rebar compile

mkdir -p build/ebin/common/ebin
mkdir -p build/ebin/common/priv
mkdir -p build/ebin/micex_mtesrl/ebin
mkdir -p build/ebin/rts_plaza2/ebin
mkdir -p build/ebin/rts_plaza2/ini
mkdir -p build/ebin/nitrogen_core/ebin
mkdir -p build/ebin/nprocreg/ebin
mkdir -p build/ebin/simple_bridge/ebin
mkdir -p build/ebin/metadata/ebin
mkdir -p build/ebin/www

cp -r apps/common/ebin/*.beam build/ebin/common/ebin/
cp -r apps/common/ebin/*.app build/ebin/
cp -r apps/micex_mtesrl/ebin/*.beam build/ebin/micex_mtesrl/ebin
cp -r apps/micex_mtesrl/ebin/*.app build/ebin
cp -r apps/rts_plaza2/ebin/*.beam build/ebin/rts_plaza2/ebin
cp -r apps/rts_plaza2/ebin/*.app build/ebin
cp -r apps/rts_plaza2/ini/* build/ebin/rts_plaza2/ini
cp -r apps/nitrogen_core/ebin/*.beam build/ebin/nitrogen_core/ebin
cp -r apps/nprocreg/ebin/*.beam build/ebin/nprocreg/ebin
cp -r apps/nprocreg/ebin/*.app build/ebin
cp -r apps/simple_bridge/ebin/*.beam build/ebin/simple_bridge/ebin
cp -r apps/simple_bridge/ebin/*.app build/ebin
cp -r apps/metadata/ebin/*.beam build/ebin/metadata/ebin
cp -r apps/metadata/ebin/*.app build/ebin
cp -r apps/metadata/src/www/static/* build/ebin/www
cp    build/cpp/common_utils.so build/ebin/common/priv
