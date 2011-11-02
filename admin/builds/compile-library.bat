@echo off

rem echo "in compile-library: %1, %2, %3, %4, %5 "

setlocal

set DEBUG=full
set DEBUGGING=no
set QUIET=yes
set SAVE=yes
set CLEAN=no
set MAYBE=no
set DLL=no
set EXE=no
set GNU=no
set PROJECT=
set EXT=
set EXPORTS=no
set WARNINGS=yes
set WARNINGS_OPTIONS=
set DEBUG_FAILURE=no
set SHOW_FAILURE_LOG=no
set BUILD_COUNTS=check

REM //
REM // Loop through the command line arguments //
REM //
:PARAM_LOOP
If "%1%"==""                  GOTO PARAM_DONE
If "%1%"=="/project"          GOTO SET_PROJECT
If "%1%"=="/debug"            GOTO SET_DEBUG_FULL
If "%1%"=="/debug-full"       GOTO SET_DEBUG_FULL
If "%1%"=="/debug-min"        GOTO SET_DEBUG_MIN
If "%1%"=="/debug-none"       GOTO SET_NO_DEBUG
If "%1%"=="/build-counts"     goto SET_BUILD_COUNTS
If "%1%"=="/debugger"         GOTO SET_DEBUGGING
If "%1%"=="/nodebugger"       GOTO SET_NODEBUGGING
If "%1%"=="/quiet"            GOTO SET_QUIET
If "%1%"=="/verbose"          GOTO SET_VERBOSE
If "%1%"=="/save"             GOTO SET_SAVE
If "%1%"=="/nosave"           GOTO SET_NOSAVE
If "%1%"=="/clean"            GOTO SET_CLEAN
If "%1%"=="/maybe"            GOTO SET_MAYBE
If "%1%"=="/dll"              GOTO SET_DLL
If "%1%"=="/exe"              GOTO SET_EXE
If "%1%"=="/gnu"              GOTO SET_GNU
If "%1%"=="/microsoft"        GOTO SET_MICROSOFT
If "%1%"=="/exports"          GOTO SET_EXPORTS
If "%1%"=="/noexports"        GOTO SET_NOEXPORTS
If "%1%"=="/warnings"         GOTO SET_WARNINGS
If "%1%"=="/nowarnings"       GOTO SET_NOWARNINGS
If "%1%"=="/serious-warnings" GOTO SET_SERIOUS_WARNINGS
If "%1%"=="/debug-failure"    GOTO SET_DEBUG_FAILURE
If "%1%"=="/show-failure-log" GOTO SET_SHOW_FAILURE_LOG

set LIBRARY=%1%
set LIBRARY_TARGET=%2%
if "%LIBRARY_TARGET%"=="" set LIBRARY_TARGET=%LIBRARY%
if "%PROJECT%"=="" set PROJECT=%LIBRARY%
if "%DEBUGGING%"=="yes" set DEBUG_FAILURE=no

GOTO PARAM_DONE

REM //
REM // Set all the variables depending upon command line args //
REM //
:SET_PROJECT
if "%2%"=="" goto generate_error
set PROJECT=%2%
shift
shift
goto PARAM_LOOP

:SET_DEBUG_FULL
set DEBUG=full
shift
goto PARAM_LOOP

:SET_DEBUG_MIN
set DEBUG=min
shift
goto PARAM_LOOP

:SET_BUILD_COUNTS
if "%2%"=="" goto generate_error
set BUILD_COUNTS=%2%
shift
shift
goto PARAM_LOOP

:SET_NO_DEBUG
set DEBUG=none
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

:SET_SAVE
set SAVE=yes
shift
goto PARAM_LOOP

:SET_NOSAVE
set SAVE=no
shift
goto PARAM_LOOP

:SET_CLEAN
set CLEAN=yes
shift
goto PARAM_LOOP

:SET_MAYBE
set MAYBE=yes
shift
goto PARAM_LOOP

:SET_DLL
set DLL=yes
set EXT=dll
shift
goto PARAM_LOOP

REM Need this line to make the shell recognize the SET_EXE label
:SET_EXE
set EXE=yes
set EXT=exe
shift
goto PARAM_LOOP

:SET_GNU
set GNU=yes
shift
goto PARAM_LOOP

:SET_MICROSOFT
set GNU=no
shift
goto PARAM_LOOP

:SET_EXPORTS
set EXPORTS=yes
shift
goto PARAM_LOOP

:SET_NOEXPORTS
set EXPORTS=no
shift
goto PARAM_LOOP

:SET_WARNINGS
set WARNINGS=yes
set WARNINGS_OPTIONS=
shift
goto PARAM_LOOP

:SET_NOWARNINGS
set WARNINGS=no
shift
goto PARAM_LOOP

:SET_SERIOUS_WARNINGS
set WARNINGS=yes
set WARNINGS_OPTIONS=/nowarnings
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
if "%GNU%"=="yes" goto ensure_no_exports
if "%EXT%"=="exe" goto ensure_no_exports
goto maybe_clean

:ENSURE_NO_EXPORTS
set EXPORTS=no

:MAYBE_CLEAN

if "%CLEAN%"=="no" goto maybe_compile

:REMOVE_LIBRARY
if "%EXT%"=="exe" goto remove_application
call remove-library %LIBRARY% %LIBRARY_TARGET%
goto setup_compile

:REMOVE_APPLICATION
call remove-application %LIBRARY%
goto setup_compile

:MAYBE_COMPILE

if "%MAYBE%"=="no" goto setup_compile

if exist "%OPEN_DYLAN_USER_INSTALL%\bin\%LIBRARY_TARGET%.%EXT%" goto success 

:SETUP_COMPILE

if exist "%DYLAN_RELEASE_COMPILER%" goto compiler_found
call find-compiler

:COMPILER_FOUND

set LOG=%OPEN_DYLAN_BUILD_LOGS%\compile-%LIBRARY%.log
set BUILD=%DYLAN_RELEASE_ROOT%\bin\build
set OPERATION=Building
set COMPILER_OPTIONS=/messages:internal
if "%SAVE%"=="yes" set COMPILER_OPTIONS=/save %COMPILER_OPTIONS%
if "%SAVE%"=="no" set OPERATION=Building (but not saving)
if "%DLL%"=="yes" set COMPILER_OPTIONS=/link-dll %COMPILER_OPTIONS%
if "%DLL%"=="yes" set OPERATION=%OPERATION% library
if "%EXE%"=="yes" set COMPILER_OPTIONS=/link-exe %COMPILER_OPTIONS%
if "%EXE%"=="yes" set OPERATION=%OPERATION% application
if "%GNU%"=="yes" set COMPILER_OPTIONS=/gnu %COMPILER_OPTIONS%
if "%GNU%"=="no" set COMPILER_OPTIONS=/microsoft %COMPILER_OPTIONS%
if "%EXPORTS%"=="yes" set COMPILER_OPTIONS=/gnu-exports %COMPILER_OPTIONS%
if "%DEBUG%"=="min" set COMPILER_OPTIONS=/debug:min %COMPILER_OPTIONS%
if "%DEBUG%"=="no" set COMPILER_OPTIONS=/debug:none %COMPILER_OPTIONS%
if "%BUILD_COUNTS%"=="ignore" set OPEN_DYLAN_MAJOR_MINOR_CHECKS_ONLY=yes

:FIND_DEBUGGER
set DEBUGGER=%DYLAN_RELEASE_ROOT%\bin\batch-debug.exe
if exist "%DEBUGGER%" goto setup_debugging
set DEBUGGER=C:\Program Files\Functional Objects\Functional Developer\bin\batch-debug.exe
if exist "%DEBUGGER%" goto setup_debugging
set DEBUGGER=batch-debug

:SETUP_DEBUGGING
call :fixup_PATHS "%DEBUGGER%"
set MAYBE_DEBUGGER=
set DEBUGGER_OPTIONS=
set DEBUG_OPERATION=
if "%DEBUGGING%"=="no" goto ensure_project_writable
set MAYBE_DEBUGGER=%DEBUGGER%
set DEBUGGER_OPTIONS=/debugger
set DEBUGGER_OPERATION=under debugger

:ENSURE_PROJECT_WRITABLE
if not exist "%PROJECT%" goto start_compile
attrib -r "%PROJECT%"

:START_COMPILE
echo %OPERATION% %LIBRARY% %DEBUGGER_OPERATION%
if "%QUIET%"=="yes" goto start_quiet_compile
echo   [compiler: %DYLAN_RELEASE_COMPILER%]
if not "%PROJECT%" == "%LIBRARY%" \
echo   [project:  %PROJECT%]
echo   [options:  %COMPILER_OPTIONS% %DEBUGGER_OPTIONS%]
if not "%MAYBE_DEBUGGER%" == "" \
echo   [debugger: %MAYBE_DEBUGGER%]
echo   [log:      %LOG%]

:START_QUIET_COMPILE
call %MAYBE_DEBUGGER% %DYLAN_RELEASE_COMPILER% %COMPILER_OPTIONS% %DEBUGGER_OPTIONS% %PROJECT% > %LOG%
if %ERRORLEVEL% NEQ 0 goto build_error
if "%QUIET%"=="no" echo %OPERATION% completed successfully
if not "%WARNINGS%"=="no" call show-build-warnings %WARNINGS_OPTIONS% /log %LOG%

:SUCCESS
endlocal
goto end

:fixup_PATHS
if not "%~$PATH:1"=="" set DEBUGGER=%~sf$PATH:1
goto :EOF

:BUILD_ERROR
echo Compilation failed (status code %ERRORLEVEL%)
if "%DEBUG_FAILURE%"=="yes" goto debug_failure
if "%SHOW_FAILURE_LOG%"=="yes" goto verbose_build_error
echo [see log %LOG%]
goto generate_error

:DEBUG_FAILURE
set LOG=%OPEN_DYLAN_BUILD_LOGS%\debug-compile-%LIBRARY%.log
echo %OPERATION% %LIBRARY% again under debugger
call %DEBUGGER% %DYLAN_RELEASE_COMPILER% %COMPILER_OPTIONS% /debugger %PROJECT% > %LOG%
if "%SHOW_FAILURE_LOG%"=="yes" goto verbose_build_error
echo [see log %LOG%]
goto generate_error

:VERBOSE_BUILD_ERROR
echo Contents of log %LOG%:
echo --------------------------------------------------
type %LOG%
echo --------------------------------------------------
echo %OPERATION% %LIBRARY% %DEBUGGER_OPERATION% failed
goto generate_error

:GENERATE_ERROR
endlocal
bogus-command-to-cause-an-error-exit 2>nul

:END
