call "%VS100COMNTOOLS%\vsvars32.bat"

set ROOT=%CD%
set LogFile=%ROOT%\build.log
set BuildType=Debug

mkdir build
pushd build
cmake -G "Visual Studio 10" ..\cpp\
devenv.exe Project.sln /build %BuildType% /Out %LogFile%
popd

escript.exe rebar compile