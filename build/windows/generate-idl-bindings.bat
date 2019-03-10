@echo off

setlocal

set COMPILER=
set DIRECTORY=
set IDL=
set LIBRARIES=
set MODULES=
set SCEPTER_INCLUDE=
set PROTOCOL=no
set STUBS=no
set SKELETONS=no
set PREFIX=
set MAIN=no
set DEBUGGING=no
set QUIET=yes
set DEBUG_FAILURE=no
set SHOW_FAILURE_LOG=no

REM //
REM // Loop through the command line arguments //
REM //
:PARAM_LOOP
If "%1%"==""                  GOTO PARAM_DONE
If "%1%"=="/libmod"           GOTO SET_LIBRARY_AND_MODULE
If "%1%"=="/library"          GOTO SET_LIBRARY
If "%1%"=="/module"           GOTO SET_MODULE
If "%1%"=="/include"          GOTO SET_INCLUDE
If "%1%"=="/protocol"         GOTO SET_PROTOCOL
If "%1%"=="/stubs"            GOTO SET_STUBS
If "%1%"=="/skeletons"        GOTO SET_SKELETONS
If "%1%"=="/prefix"           GOTO SET_PREFIX
If "%1%"=="/main"             GOTO SET_MAIN
If "%1%"=="/debugger"         GOTO SET_DEBUGGING
If "%1%"=="/nodebugger"       GOTO SET_NODEBUGGING
If "%1%"=="/quiet"            GOTO SET_QUIET
If "%1%"=="/verbose"          GOTO SET_VERBOSE
If "%1%"=="/debug-failure"    GOTO SET_DEBUG_FAILURE
If "%1%"=="/show-failure-log" GOTO SET_SHOW_FAILURE_LOG

set DIRECTORY=%1%
set IDL=%2%
if "%IDL%"=="" goto generate_error
if "%DEBUGGING%"=="yes" set DEBUG_FAILURE=no

GOTO PARAM_DONE

REM //
REM // Set all the variables depending upon command line args //
REM //
:SET_LIBRARY_AND_MODULE
if "%2%"=="" goto generate_error
if not "%LIBRARIES%"=="" set LIBRARIES=%LIBRARIES%,
set LIBRARIES=%LIBRARIES%%2%
if not "%MODULES%"=="" set MODULES=%MODULES%,
set MODULES=%MODULES%%2%
shift
shift
goto PARAM_LOOP

:SET_LIBRARY
if "%2%"=="" goto generate_error
if not "%LIBRARIES%"=="" set LIBRARIES=%LIBRARIES%,
set LIBRARIES=%LIBRARIES%%2%
shift
shift
goto PARAM_LOOP

:SET_MODULE
if "%2%"=="" goto generate_error
if not "%MODULES%"=="" set MODULES=%MODULES%,
set MODULES=%MODULES%%2%
shift
shift
goto PARAM_LOOP

:SET_INCLUDE
if "%2%"=="" goto generate_error
set SCEPTER_INCLUDE=%2%
shift
shift
goto PARAM_LOOP

:SET_PROTOCOL
set PROTOCOL=yes
shift
goto PARAM_LOOP

:SET_STUBS
set STUBS=yes
shift
goto PARAM_LOOP

:SET_SKELETONS
set SKELETONS=yes
shift
goto PARAM_LOOP

:SET_PREFIX
if "%2%"=="" goto generate_error
set PREFIX=%2%
shift
shift
goto PARAM_LOOP

:SET_MAIN
set MAIN=yes
shift
goto PARAM_LOOP

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
set COMPILER=%DYLAN_RELEASE_COMPILER%\..\console-scepter.exe
if exist "%COMPILER%" goto compiler_found
set COMPILER=%DYLAN_RELEASE_ROOT%\bin\console-scepter.exe
if exist "%COMPILER%" goto compiler_found
set COMPILER=%OLD_RELEASE_ROOT%\bin\console-scepter.exe
if exist "%COMPILER%" goto compiler_found
set COMPILER=%OPEN_DYLAN_DEFAULT_ROOT%\Bin\console-scepter.exe

:COMPILER_FOUND

set LOG=%OPEN_DYLAN_BUILD_LOGS%\generate-%IDL%.log
set OPERATION=Generating IDL bindings for %IDL%.idl
set COMPILER_OPTIONS=/language:dylan /clean
if "%PROTOCOL%"=="yes" set COMPILER_OPTIONS=%COMPILER_OPTIONS% /protocol
if "%STUBS%"=="yes" set COMPILER_OPTIONS=%COMPILER_OPTIONS% /stubs
if "%SKELETONS%"=="yes" set COMPILER_OPTIONS=%COMPILER_OPTIONS% /skeletons
if not "%PREFIX%"=="" set COMPILER_OPTIONS=%COMPILER_OPTIONS% /prefix:%PREFIX%
if "%MAIN%"=="yes" set COMPILER_OPTIONS=%COMPILER_OPTIONS% /main
if not "%LIBRARIES%"=="" set COMPILER_OPTIONS=%COMPILER_OPTIONS% /libraries:%LIBRARIES%
if not "%MODULES%"=="" set COMPILER_OPTIONS=%COMPILER_OPTIONS% /modules:%MODULES%
set COMPILER_OPTIONS=%COMPILER_OPTIONS% /directory:%DIRECTORY%
if not "%SCEPTER_INCLUDE%"=="" set COMPILER_OPTIONS=%COMPILER_OPTIONS% /include:%SCEPTER_INCLUDE%
if "%QUIET%"=="no" set COMPILER_OPTIONS=%COMPILER_OPTIONS% /trace
set COMPILER_OPTIONS=%COMPILER_OPTIONS% %DIRECTORY%\%IDL%.idl

:FIND_DEBUGGER
set DEBUGGER=%DYLAN_RELEASE_ROOT%\bin\batch-debug.exe
if exist "%DEBUGGER%" goto setup_debugging
set DEBUGGER=C:\Program Files\Open Dylan\bin\batch-debug.exe
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
set LOG=%OPEN_DYLAN_BUILD_LOGS%\debug-generate-%IDL%.log
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
