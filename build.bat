call "%VS100COMNTOOLS%\vsvars32.bat"

set ROOT=%CD%
set LogFile=%ROOT%\build.log
set BuildType=Debug

mkdir build\cpp
pushd build\cpp
cmake -G "Visual Studio 10" ..\..\cpp\
devenv.exe Project.sln /build %BuildType% /Out %LogFile%
popd

escript.exe rebar compile

xcopy apps\common\ebin\*.beam build\ebin\common\ebin\  /Y
xcopy apps\micex_mtesrl\ebin\*.beam build\ebin\micex_mtesrl\ebin\  /Y
xcopy apps\micex_mtesrl\ebin\*.app build\ebin\ /Y
xcopy apps\rts_plaza2\ebin\*.beam build\ebin\rts_plaza2\ebin\  /Y
xcopy apps\rts_plaza2\ebin\*.app build\ebin\  /Y
xcopy apps\rts_plaza2\ini\* build\ebin\rts_plaza2\ini\ /Y
xcopy apps\metadata\ebin\*.beam build\ebin\metadata\ebin\ /Y
xcopy apps\metadata\ebin\*.app build\ebin\ /Y
xcopy apps\metadata\src\www\static\* build\ebin\www\ /Y /S
xcopy apps\nitrogen_core\ebin\*.beam build\ebin\nitrogen_core\ebin\ /Y
xcopy apps\nprocreg\ebin\*.beam build\ebin\nprocreg\ebin\ /Y
xcopy apps\nprocreg\ebin\*.app build\ebin\ /Y
xcopy apps\simple_bridge\ebin\*.beam build\ebin\simple_bridge\ebin\ /Y
xcopy build\cpp\Debug\micex_driver.dll build\ebin\micex_mtesrl\priv\ /Y
xcopy build\cpp\Debug\plaza2_driver.dll build\ebin\rts_plaza2\priv\ /Y
xcopy build\cpp\Debug\common_utils.dll build\ebin\common\priv\ /Y
