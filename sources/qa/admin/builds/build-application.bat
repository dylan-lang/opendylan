@echo off

echo Building %1%.exe
pushd %WEBSTER_PERSONAL_BUILD%\%1%
build install-app
popd
