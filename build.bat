call "%VS100COMNTOOLS%\vsvars32.bat"

set ROOT=%CD%
set LogFile=%ROOT%\build.log
set BuildType=Release

mkdir build\cpp
pushd build\cpp
cmake -G "Visual Studio 10" ..\..\cpp\
devenv.exe Project.sln /build %BuildType% /Out %LogFile%
popd

escript.exe rebar get-deps
escript.exe rebar compile

xcopy deps\log_viewer\ebin\*.beam build\ebin\log_viewer\ebin\ /Y
xcopy deps\log_viewer\ebin\*.app build\ebin\ /Y
xcopy apps\qinfo_common\ebin\*.beam build\ebin\qinfo_common\ebin\  /Y
xcopy apps\qinfo_common\include\*.hrl build\ebin\qinfo_common\include\ /Y
xcopy apps\micex_mtesrl\ebin\*.beam build\ebin\micex_mtesrl\ebin\  /Y
xcopy apps\micex_mtesrl\ebin\*.app build\ebin\ /Y
xcopy apps\rts_plaza2\ebin\*.beam build\ebin\rts_plaza2\ebin\  /Y
xcopy apps\rts_plaza2\ebin\*.app build\ebin\  /Y
xcopy apps\rts_plaza2\ini\* build\ebin\rts_plaza2\ini\ /Y
xcopy apps\metadata\ebin\*.beam build\ebin\metadata\ebin\ /Y
xcopy apps\metadata\ebin\*.app build\ebin\ /Y
xcopy apps\metadata\include\*.hrl build\ebin\metadata\include\ /Y
xcopy apps\metadata\src\www\static\* build\ebin\www\ /Y /S
xcopy apps\nitrogen_core\ebin\*.beam build\ebin\nitrogen_core\ebin\ /Y
xcopy apps\nitrogen_core\include\*.hrl build\ebin\nitrogen_core\include\ /Y
xcopy apps\nprocreg\ebin\*.beam build\ebin\nprocreg\ebin\ /Y
xcopy apps\nprocreg\ebin\*.app build\ebin\ /Y
xcopy apps\simple_bridge\ebin\*.beam build\ebin\simple_bridge\ebin\ /Y
xcopy build\cpp\%BuildType%\micex_driver.dll build\ebin\micex_mtesrl\priv\ /Y
xcopy build\cpp\%BuildType%\plaza2_driver.dll build\ebin\rts_plaza2\priv\ /Y
xcopy build\cpp\%BuildType%\qinfo_common_utils.dll build\ebin\qinfo_common\priv\ /Y
