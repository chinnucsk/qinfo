call "%VS100COMNTOOLS%\vsvars32.bat"

set ROOT=%CD%
set LogFile=%ROOT%\build.log
set BuildType=Debug

mkdir build
pushd build
cmake -G "Visual Studio 10" ..\cpp\
devenv.exe Project.sln /build %BuildType% /Out %LogFile%
popd

mkdir rel\ebin

copy build\%BuildType%\plaza2_port.dll %ROOT%\rel\ebin

pushd line_handlers\rts\plaza2
mkdir ebin
erl -make
copy src\plaza2.app %ROOT%\rel\ebin
xcopy ebin\*.beam %ROOT%\rel\ebin\ /Y
xcopy ini\*.ini %ROOT%\rel\ebin\ /Y
popd

pushd metadata
mkdir ebin
erl -make
copy src\metadata.app %ROOT%\rel\ebin
xcopy ebin\*.beam %ROOT%\rel\ebin\ /Y
popd

mkdir rel\ebin\nitrogen_core

pushd web\nitrogen_core
mkdir ebin
erl -pa ebin -make
xcopy ebin\*.beam %ROOT\rel\ebin\nitrogen_core /Y
popd

mkdir rel\ebin\nprocreg

pushd web\nprocreg
mkdir ebin
erl -pa ebin -make
xcopy ebin\*.beam %ROOT\rel\ebin\nprocreg /Y
copy ebin\nprocreg.app %ROOT%\rel\ebin /Y
popd

pushd web\simple_bridge
mkdir ebin
erl -pa ebin -make
xcopy ebin\*.beam %ROOT\rel\ebin\simple_bridge /Y
popd
