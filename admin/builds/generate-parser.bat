@echo off

setlocal

set GRAMMAR_FILE=
set DYLAN_FILE=
set DEBUGGING=no
set QUIET=yes
set DEBUG_FAILURE=no
set SHOW_FAILURE_LOG=no

REM //
REM // Loop through the command line arguments //
REM //
:PARAM_LOOP
If "%1%"==""                  GOTO PARAM_DONE
If "%1%"=="/debugger"         GOTO SET_DEBUGGING
If "%1%"=="/nodebugger"       GOTO SET_NODEBUGGING
If "%1%"=="/quiet"            GOTO SET_QUIET
If "%1%"=="/verbose"          GOTO SET_VERBOSE
If "%1%"=="/debug-failure"    GOTO SET_DEBUG_FAILURE
If "%1%"=="/show-failure-log" GOTO SET_SHOW_FAILURE_LOG

set GRAMMAR_FILE=%1%
set DYLAN_DIRECTORY=%2%
set DYLAN_FILE=%3%
if "%DYLAN_FILE%"=="" goto generate_error
if "%DEBUGGING%"=="yes" set DEBUG_FAILURE=no

GOTO PARAM_DONE

REM //
REM // Set all the variables depending upon command line args //
REM //
:SET_DEBUGGING
set DEBUGGING=yes
shift
goto PARAM_LOOP

:SET_NODEBUGGING
set DEBUGGING=no
shift
goto PARAM_LOOP

:SET_QUIET
set QUIET=yes
shift
goto PARAM_LOOP

:SET_VERBOSE
set QUIET=no
shift
goto PARAM_LOOP

:SET_DEBUG_FAILURE
set DEBUG_FAILURE=yes
shift
goto PARAM_LOOP

:SET_SHOW_FAILURE_LOG
set SHOW_FAILURE_LOG=yes
shift
goto PARAM_LOOP

:PARAM_DONE
set COMPILER=%DYLAN_RELEASE_COMPILER%\..\parser-compiler.exe
if exist "%COMPILER%" goto compiler_found
set COMPILER=%DYLAN_RELEASE_ROOT%\Bin\parser-compiler.exe
if exist "%COMPILER%" goto compiler_found
set COMPILER=%OLD_RELEASE_ROOT%\Bin\parser-compiler.exe
if exist "%COMPILER%" goto compiler_found
set COMPILER=C:\Program Files\Functional Objects\Functional Developer\Bin\parser-compiler.exe

:COMPILER_FOUND

set LOG=%FUNCTIONAL_DEVELOPER_BUILD_LOGS%\generate-%DYLAN_FILE%.log
set OPERATION=Generating parser for %GRAMMAR_FILE%
set COMPILER_OPTIONS=%GRAMMAR_FILE% %DYLAN_DIRECTORY%\%DYLAN_FILE%.dylan

:FIND_DEBUGGER
set DEBUGGER=%DYLAN_RELEASE_ROOT%\bin\batch-debug.exe
if exist "%DEBUGGER%" goto setup_debugging
set DEBUGGER=C:\Program Files\Functional Objects\Functional Developer\bin\batch-debug.exe
if exist "%DEBUGGER%" goto setup_debugging
set DEBUGGER=batch-debug

:SETUP_DEBUGGING
call :fixup_PATHS "%COMPILER%" "%DEBUGGER%"
set MAYBE_DEBUGGER=
set DEBUGGER_OPTIONS=
set DEBUG_OPERATION=
if "%DEBUGGING%"=="no" goto start_compile
set MAYBE_DEBUGGER=%DEBUGGER%
set DEBUGGER_OPTIONS=/debugger
set DEBUGGER_OPERATION=under debugger

:START_COMPILE
echo %OPERATION% %DEBUGGER_OPERATION%
if "%QUIET%"=="yes" goto start_quiet_compile
echo   [compiler: %COMPILER%]
echo   [options:  %COMPILER_OPTIONS% %DEBUGGER_OPTIONS%]
if not "%MAYBE_DEBUGGER%"=="" echo   [debugger: %MAYBE_DEBUGGER%]
echo   [log:      %LOG%]

:START_QUIET_COMPILE
call %MAYBE_DEBUGGER% %COMPILER% %COMPILER_OPTIONS% %DEBUGGER_OPTIONS% > %LOG%
if %ERRORLEVEL% NEQ 0 goto build_error
if "%QUIET%"=="no" echo %OPERATION% completed successfully

:SUCCESS
endlocal
goto end

:fixup_PATHS
if not "%~$PATH:1"=="" set COMPILER=%~sf$PATH:1
if not "%~$PATH:2"=="" set DEBUGGER=%~sf$PATH:2
goto :EOF

:BUILD_ERROR
echo Compilation failed (status code %ERRORLEVEL%)
if "%DEBUG_FAILURE%"=="yes" goto debug_failure
if "%SHOW_FAILURE_LOG%"=="yes" goto verbose_build_error
echo [see log %LOG%]
goto generate_error

:DEBUG_FAILURE
set LOG=%FUNCTIONAL_DEVELOPER_BUILD_LOGS%\debug-generate-%IDL%.log
echo %OPERATION% again under debugger
call %DEBUGGER% %COMPILER% %COMPILER_OPTIONS% /debugger > %LOG%
if "%SHOW_FAILURE_LOG%"=="yes" goto verbose_build_error
echo [see log %LOG%]
goto generate_error

:VERBOSE_BUILD_ERROR
echo Contents of log %LOG%:
echo --------------------------------------------------
type %LOG%
echo --------------------------------------------------
echo %OPERATION% %DEBUGGER_OPERATION% failed
goto generate_error

:GENERATE_ERROR
endlocal
bogus-command-to-cause-an-error-exit 2>nul

:END
