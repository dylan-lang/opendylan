@echo off

set INDENT=  %INDENT%

if "%CONVERTONE%" == "yes" goto CONVERT

for %%L in (%ALLLIBS%) do call %WEBSTER_SYSTEM_BIN%\build\invoke-build %%L

:CONVERT
echo %INDENT%  Converting '%DYLANAPP%\build.bat' to '%DYLANAPP%\dylanmakefile'
call %WEBSTER_SYSTEM_BIN%\build\buildfile-conversion %DYLANAPP% > dylanmakefile
