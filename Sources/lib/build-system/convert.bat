@echo off

setlocal
if "%1" == "" goto END
if "%2" == "only" set CONVERTONE=yes
set DYLANENVSET=yes
set WEBSTER_SYSTEM_BIN=U:\dylan\lib\build-system
call %WEBSTER_SYSTEM_BIN%\build\invoke-build %1

:END
endlocal