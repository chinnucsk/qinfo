call "%VS100COMNTOOLS%\vsvars32.bat"

set ROOT=%CD%
set LogFile=%ROOT%\build.log

mkdir build
pushd build
cmake -G "Visual Studio 10" ..\
devenv.exe Project.sln /build Release /Out %LogFile%
popd

mkdir rel\ebin rel\priv
copy build\Release\plaza2_port.dll rel\priv
xcopy line_handlers\rts\plaza2\priv\*.dll rel\priv

pushd line_handlers\rts\plaza2\src
erl -make
copy plaza2.app %ROOT%\rel\ebin
xcopy ..\ebin\*.beam %ROOT%\rel\ebin\ /Y
popd
