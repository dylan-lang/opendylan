@echo off

set DIRECTORY=%1%

pushd %DIRECTORY%
attrib /s -R *.hdp >nul
popd
