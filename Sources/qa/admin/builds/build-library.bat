@echo off

echo Building %1%.dll
pushd %WEBSTER_PERSONAL_BUILD%\%1%
build dll
popd
