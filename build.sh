#!/bin/bash

BUILD_TYPE=Debug

mkdir -p build/cpp
cd build/cpp
cmake -DCMAKE_BUILD_TYPE=$BUILD_TYPE ../../cpp
make
cd ../../

./rebar get-deps
./rebar compile

mkdir -p build/ebin/qinfo_common/ebin
mkdir -p build/ebin/qinfo_common/priv
mkdir -p build/ebin/qinfo_common/include
mkdir -p build/ebin/micex_mtesrl/ebin
mkdir -p build/ebin/rts_plaza2/ebin
mkdir -p build/ebin/rts_plaza2/ini
mkdir -p build/ebin/nitrogen_core/ebin
mkdir -p build/ebin/nitrogen_core/include
mkdir -p build/ebin/nprocreg/ebin
mkdir -p build/ebin/simple_bridge/ebin
mkdir -p build/ebin/metadata/ebin
mkdir -p build/ebin/metadata/include
mkdir -p build/ebin/www
mkdir -p build/ebin/log_viewer/ebin

cp -r apps/qinfo_common/ebin/*.beam build/ebin/qinfo_common/ebin
cp -r apps/qinfo_common/ebin/*.app build/ebin
cp -r apps/qinfo_common/include/*.hrl build/ebin/qinfo_common/include
cp -r apps/micex_mtesrl/ebin/*.beam build/ebin/micex_mtesrl/ebin
cp -r apps/micex_mtesrl/ebin/*.app build/ebin
cp -r apps/rts_plaza2/ebin/*.beam build/ebin/rts_plaza2/ebin
cp -r apps/rts_plaza2/ebin/*.app build/ebin
cp -r apps/rts_plaza2/ini/* build/ebin/rts_plaza2/ini
cp -r apps/nitrogen_core/ebin/*.beam build/ebin/nitrogen_core/ebin
cp -r apps/nitrogen_core/include/*.hrl build/ebin/nitrogen_core/include
cp -r apps/nprocreg/ebin/*.beam build/ebin/nprocreg/ebin
cp -r apps/nprocreg/ebin/*.app build/ebin
cp -r apps/simple_bridge/ebin/*.beam build/ebin/simple_bridge/ebin
cp -r apps/simple_bridge/ebin/*.app build/ebin
cp -r apps/metadata/ebin/*.beam build/ebin/metadata/ebin
cp -r apps/metadata/ebin/*.app build/ebin
cp -r apps/metadata/include/*.hrl build/ebin/metadata/include
cp -r deps/log_viewer/ebin/*.beam build/ebin/log_viewer/ebin
cp -r deps/log_viewer/ebin/*.app build/ebin
cp -r apps/metadata/src/www/static/* build/ebin/www
cp    build/cpp/qinfo_common_utils.so build/ebin/qinfo_common/priv/qinfo_common_utils.so
