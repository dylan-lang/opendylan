@echo off

setlocal

set SCRIPT=%1
set SCRIPTS_DIRECTORY=%OPEN_DYLAN_USER_SOURCES%\qa\scripts
set SCRIPT_FILE=%SCRIPTS_DIRECTORY%\%SCRIPT%
set LOG_NAME=test-%SCRIPT%.log
set LOG=%OPEN_DYLAN_BUILD_LOGS%\%LOG_NAME%

set CONSOLE_DYLAN=%OPEN_DYLAN_USER_ROOT%\bin\basic-console-environment.exe
if exist "%CONSOLE_DYLAN%" goto console_dylan_found
set CONSOLE_DYLAN=%OPEN_DYLAN_USER_ROOT%\bin\console-environment.exe
if exist "%CONSOLE_DYLAN%" goto console_dylan_found
set CONSOLE_DYLAN=%OPEN_DYLAN_USER_ROOT%\bin\console-dylan.exe
if exist "%CONSOLE_DYLAN%" goto console_dylan_found
set CONSOLE_DYLAN=%DYLAN_RELEASE_ROOT%\bin\basic-console-environment.exe
if exist "%CONSOLE_DYLAN%" goto console_dylan_found
set CONSOLE_DYLAN=%DYLAN_RELEASE_ROOT%\bin\console-environment.exe
if exist "%CONSOLE_DYLAN%" goto console_dylan_found
set CONSOLE_DYLAN=%DYLAN_RELEASE_ROOT%\bin\console-dylan.exe
if exist "%CONSOLE_DYLAN%" goto console_dylan_found
set CONSOLE_DYLAN=console-dylan

:CONSOLE_DYLAN_FOUND
call ensure-sources qa-scripts %SCRIPTS_DIRECTORY%
if not exist "%SCRIPT_FILE%" goto missing_script

:INVOKE_SCRIPT
%CONSOLE_DYLAN% /echo-input /profile-commands < %SCRIPT_FILE% > %LOG%
if %ERRORLEVEL% NEQ 0 goto console_dylan_error
type %LOG%
goto end

:MISSING_SCRIPT
echo No such script file %SCRIPT_FILE%
goto generate_error

:CONSOLE_DYLAN_ERROR
echo Testing of %SCRIPT% failed (status code %ERRORLEVEL%)
echo [see log %LOG%]
goto generate_error

:GENERATE_ERROR
endlocal
bogus-command-to-cause-an-error-exit 2>nul

:END
