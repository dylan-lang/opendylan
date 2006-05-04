@echo off

rem echo "in import-project: %1, %2, %3, %4, %5 "

setlocal

set DEBUGGING=no
set DLL=no
set EXE=no
set EXT=
set MODE=tight
set QUIET=yes
if "%OPEN_DYLAN_DEFAULT_ROOT%"=="" set OPEN_DYLAN_DEFAULT_ROOT=C:\Program Files\Functional Objects\Functional Developer

REM //
REM // Loop through the command line arguments //
REM //
:PARAM_LOOP
If "%1%"==""                  GOTO PARAM_DONE
If "%1%"=="/debugger"         GOTO SET_DEBUGGING
If "%1%"=="/nodebugger"       GOTO SET_NODEBUGGING
If "%1%"=="/dll"              GOTO SET_DLL
If "%1%"=="/exe"              GOTO SET_EXE
If "%1%"=="/loose"            GOTO SET_LOOSE
If "%1%"=="/tight"            GOTO SET_TIGHT
If "%1%"=="/quiet"            GOTO SET_QUIET
If "%1%"=="/verbose"          GOTO SET_VERBOSE
If "%1%"=="/exports"          GOTO IGNORE_ARG
If "%1%"=="/noexports"        GOTO IGNORE_ARG
If "%1%"=="/debug-failure"    GOTO IGNORE_ARG
If "%1%"=="/show-failure-log" GOTO IGNORE_ARG
set PROJECT=%1%
set DIRECTORY=%2%
set LID=%3%
if "%LID%"=="" set LID=%PROJECT%

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

:SET_DLL
set DLL=yes
set EXT=dll
shift
goto PARAM_LOOP

:SET_EXE
set EXE=yes
set EXT=exe
shift
goto PARAM_LOOP

:SET_TIGHT
set MODE=tight
shift
goto PARAM_LOOP

:SET_LOOSE
set MODE=loose
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

:IGNORE_ARG
shift
goto PARAM_LOOP

:PARAM_DONE

REM // if exist "%DYLAN_RELEASE_COMPILER%" goto compiler_found
REM // call find-compiler
set DYLAN_RELEASE_COMPILER=%DYLAN_RELEASE_ROOT%\bin\pentium-dw.exe
if exist "%DYLAN_RELEASE_COMPILER%" goto compiler_found
set DYLAN_RELEASE_COMPILER=%DYLAN_RELEASE_ROOT%\bin\basic-pentium-dw.exe
if exist "%DYLAN_RELEASE_COMPILER%" goto compiler_found
set DYLAN_RELEASE_COMPILER=%DYLAN_RELEASE_ROOT%\bin\personal-pentium-dw.exe
if exist "%DYLAN_RELEASE_COMPILER%" goto compiler_found
set DYLAN_RELEASE_COMPILER=%DYLAN_RELEASE_ROOT%\bin\fdbc.exe
if exist "%DYLAN_RELEASE_COMPILER%" goto compiler_found
set DYLAN_RELEASE_COMPILER=%DYLAN_RELEASE_ROOT%\bin\dylan-compile.exe
if exist "%DYLAN_RELEASE_COMPILER%" goto compiler_found
REM // Try what we just built
REM //
set DYLAN_RELEASE_COMPILER=%OPEN_DYLAN_RELEASE_ROOT%\bin\pentium-dw.exe
if exist "%DYLAN_RELEASE_COMPILER%" goto compiler_found
set DYLAN_RELEASE_COMPILER=%OPEN_DYLAN_RELEASE_ROOT%\bin\basic-pentium-dw.exe
if exist "%DYLAN_RELEASE_COMPILER%" goto compiler_found
set DYLAN_RELEASE_COMPILER=%OPEN_DYLAN_RELEASE_ROOT%\bin\personal-pentium-dw.exe
if exist "%DYLAN_RELEASE_COMPILER%" goto compiler_found
set DYLAN_RELEASE_COMPILER=%OPEN_DYLAN_RELEASE_ROOT%\bin\fdbc.exe
if exist "%DYLAN_RELEASE_COMPILER%" goto compiler_found
set DYLAN_RELEASE_COMPILER=%OPEN_DYLAN_RELEASE_ROOT%\bin\dylan-compile.exe
if exist "%DYLAN_RELEASE_COMPILER%" goto compiler_found
REM //... finally hope that we can find it on the path.
REM //
set DYLAN_RELEASE_COMPILER=%OPEN_DYLAN_DEFAULT_ROOT%\bin\fdbc.exe
if exist "%DYLAN_RELEASE_COMPILER%" goto compiler_found
set DYLAN_RELEASE_COMPILER=fdbc.exe

:COMPILER_FOUND

set DEBUGGER=
set DEBUGGER_OPTIONS=
set DEBUG_OPERATION=
if "%DEBUGGING%"=="no" goto start_import

set DEBUGGER_OPTIONS=-debugger
set DEBUGGER_OPERATION=under debugger
set DEBUGGER=%DYLAN_RELEASE_ROOT%\bin\batch-debug.exe
if exist "%DEBUGGER%" goto start_import
set DEBUGGER=%OPEN_DYLAN_DEFAULT_ROOT%\bin\batch-debug.exe
if exist "%DEBUGGER%" goto start_import
set DEBUGGER=batch-debug

:START_IMPORT
call :fixup_PATHS "%DEBUGGER%"
set LOG=%OPEN_DYLAN_BUILD_LOGS%\import-%PROJECT%.log
set COMPILER_OPTIONS=/%MODE%
set LID_PATHNAME=%DIRECTORY%\%LID%.lid
set INCORRECT_HDP_PATHNAME=%DIRECTORY%\%LID%.hdp
set HDP_PATHNAME=%PROJECT%.hdp

if "%DLL%"=="yes" set COMPILER_OPTIONS=/dll %COMPILER_OPTIONS% 
if "%DLL%"=="no" set COMPILER_OPTIONS=/exe %COMPILER_OPTIONS%

REM // Locally unbind the OPEN_DYLAN_USER settings so that these
REM // projects are built as if by a user.
set OPEN_DYLAN_USER_PROJECTS=
set OPEN_DYLAN_USER_REGISTRIES=
set OPEN_DYLAN_USER_BUILD=
set OPEN_DYLAN_USER_INSTALL=
set OPEN_DYLAN_USER_ROOT=

echo Importing %PROJECT% %DEBUGGER_OPERATION%
if "%QUIET%"=="yes" goto start_quiet_import
echo   [compiler: %DYLAN_RELEASE_COMPILER%]
if not "%PROJECT%" == "%LIBRARY%" \
echo   [project:  %PROJECT%]
echo   [options:  %COMPILER_OPTIONS% /nocompile /nolink %DEBUGGER_OPTIONS%]
if not "DEBUGGER%" == "" \
echo   [debugger: %DEBUGGER%]
echo   [log:      %LOG%]

:START_QUIET_IMPORT
REM echo call %DEBUGGER% "%DYLAN_RELEASE_COMPILER%" %COMPILER_OPTIONS% /nocompile /nolink %DEBUGGER_OPTIONS% %LID_PATHNAME%
call %DEBUGGER% "%DYLAN_RELEASE_COMPILER%" %COMPILER_OPTIONS% /nocompile /nolink %DEBUGGER_OPTIONS% %LID_PATHNAME% > %LOG%
if %ERRORLEVEL% NEQ 0 goto import_error
if "%LID%"=="%PROJECT%" goto success

:RENAME
REM echo rename %INCORRECT_HDP_PATHNAME% %HDP_PATHNAME%
rename %INCORRECT_HDP_PATHNAME% %HDP_PATHNAME%
if %ERRORLEVEL% NEQ 0 goto rename_error
REM echo Import completed successfully

:SUCCESS
call remove-directory %DIRECTORY%\build
if %ERRORLEVEL% NEQ 0 goto remove_directory_error
endlocal
goto end

:fixup_PATHS
if not "%~$PATH:1"=="" set DEBUGGER=%~sf$PATH:1
goto :EOF

:IMPORT_ERROR
echo Import of project %PROJECT% failed (status code %ERRORLEVEL%)
echo [see log %LOG%]
goto generate_error

:RENAME_ERROR
echo Renaming of %INCORRECT_HDP_PATHNAME% to %HDP_PATHNAME% failed
goto generate_error

:REMOVE_DIRECTORY_ERROR
echo Removing of directory %DIRECTORY%\build failed
goto generate_error

:GENERATE_ERROR
endlocal
bogus-command-to-cause-an-error-exit 2>nul

:END

