@echo off
goto START
REM //    Script: test-release.bat
REM //    Author: Andy Armstrong (andrewa)
REM //

:PRINT_USAGE
echo ------------------------------------------------------------------------
echo - SYNOPSIS: Tests a Dylan release
echo -
echo - USAGE:    test-release [Release Directory] {options}*
echo -
echo -
echo -   Options:
echo -     /target
echo -       Specifies the target release name. [default: test-basic-release]
echo -     /exports
echo -       Switches on generation of GNU exports [default: no exports]
echo -     /dylan
echo -       Specifies the Dylan directory.
echo -        [default: C:\Program Files\Open Dylan]
echo -     /sources
echo -       Specifies the location of the sources [default: under release]
echo -     /warnings
echo -       Shows a warning summary after each generation [default: none]
echo -     /serious-warnings
echo -       Shows only a serious warning summary [default: none]
echo -     /debugger
echo -       Runs the builds under batch-debug [default: no debugging]
echo -
echo - EXAMPLES:
echo -
echo -   test-release c:\dylan
echo -
echo -     Build a new release in c:\dylan
echo -
echo -   test-release c:\dylan /target pentium-dw
echo -
echo -     Build just the compiler
echo -
echo -   test-release c:\dylan /sources u:\andrewa\dylan
echo -
echo -     Build the release from the sources in u:\andrewa\dylan.
echo -     Note that if any pieces are missing, it will fail.
echo ------------------------------------------------------------------------
goto END

:START
If "%1%"=="help" GOTO PRINT_USAGE
If "%1%"=="/help" GOTO PRINT_USAGE
If "%1%"=="-help" GOTO PRINT_USAGE
If "%1%"=="?" GOTO PRINT_USAGE
If "%1%"=="/?" GOTO PRINT_USAGE
set ERROR_MESSAGE=
set OLD_RELEASE_ROOT=C:\Program Files\Open Dylan
set OPEN_DYLAN_USER_SOURCES=
set TEST_TARGET=test-basic-release
set CLEANUP=
set EXPORTS=no
set DEBUGGER=no
set WARNINGS=no
set WARNINGS_OPTIONS=
set TEST=no

REM //
REM // The first argument is the release directory
REM //
if "%1%"=="" GOTO NO_RELEASE_ERROR
set RELEASE_ROOT=%1%
shift

REM //
REM // Loop through the command line arguments //
REM //
:PARAM_LOOP
if "%1%"==""                  GOTO PARAM_DONE
If "%1%"=="-dylan"            GOTO SET_OLD_RELEASE_ROOT
If "%1%"=="/dylan"            GOTO SET_OLD_RELEASE_ROOT
if "%1%"=="-target"           GOTO SET_TEST_TARGET
if "%1%"=="/target"           GOTO SET_TEST_TARGET
if "%1%"=="-sources"          GOTO SET_SOURCES
if "%1%"=="/sources"          GOTO SET_SOURCES
if "%1%"=="-rm-early-builds"  GOTO SET_CLEANUP
if "%1%"=="/rm-early-builds"  GOTO SET_CLEANUP
if "%1%"=="-exports"          GOTO SET_EXPORTS
if "%1%"=="/exports"          GOTO SET_EXPORTS
if "%1%"=="-debugger"         GOTO SET_DEBUGGER
if "%1%"=="/debugger"         GOTO SET_DEBUGGER
if "%1%"=="-warnings"         GOTO SET_WARNINGS
if "%1%"=="/warnings"         GOTO SET_WARNINGS
if "%1%"=="-serious-warnings" GOTO SET_SERIOUS_WARNINGS
if "%1%"=="/serious-warnings" GOTO SET_SERIOUS_WARNINGS
if "%1%"=="-test"             GOTO SET_TEST
if "%1%"=="/test"             GOTO SET_TEST
set ERROR_MESSAGE=Invalid command line argument %1%
GOTO PRINT_ERROR

REM //
REM // Set all the variables depending upon command line args //
REM //
:SET_OLD_RELEASE_ROOT
If "%2%"=="" GOTO NO_ARG
set OLD_RELEASE_ROOT=%2%
shift
shift
goto PARAM_LOOP

REM Need this line to make Windows recognize the SET_TEST_TARGET label. 
:SET_TEST_TARGET
If "%2%"=="" GOTO NO_ARG
set TEST_TARGET=%2%
shift
shift
goto PARAM_LOOP

:SET_SOURCES
If "%2%"=="" GOTO NO_ARG
set OPEN_DYLAN_USER_SOURCES=%2%
shift
shift
goto PARAM_LOOP

:SET_CLEANUP
set CLEANUP="yes"
shift
goto PARAM_LOOP

:SET_EXPORTS
set EXPORTS="yes"
shift
goto PARAM_LOOP

:SET_DEBUGGER
set DEBUGGER="yes"
shift
goto PARAM_LOOP

:SET_WARNINGS
set WARNINGS="yes"
shift
goto PARAM_LOOP

:SET_SERIOUS_WARNINGS
set WARNINGS="yes"
set WARNINGS_OPTIONS=/nowarnings
shift
goto PARAM_LOOP

:SET_TEST
set TEST="yes"
shift
goto PARAM_LOOP

REM //
REM // Default user variables //
REM //

:PARAM_DONE
call :fixup_PATHS "%OLD_RELEASE_ROOT%"

REM // Option defaulting
if "%OPEN_DYLAN_USER_SOURCES%"=="" set OPEN_DYLAN_USER_SOURCES=%RELEASE_ROOT%\Sources
set QA_RELEASE_ROOT=%RELEASE_ROOT%\QA

REM // Build options
set OPTIONS=
set QUOTED_OPTIONS=
if "%DEBUGGER%"=="no" goto skip_debugger_option
set OPTIONS=/debugger
set QUOTED_OPTIONS=OPTIONS=%OPTIONS%

:SKIP_DEBUGGER_OPTION
set BUILD_OPTIONS=-p %QA_RELEASE_ROOT% -s %RELEASE_ROOT% %COMMON_BUILD_OPTIONS% -sources %OPEN_DYLAN_USER_SOURCES% -nopath -environment

:TEST_RELEASE
echo --------------------------------------------------
echo Testing release
echo set-build-variables %BUILD_OPTIONS%
call set-build-variables %BUILD_OPTIONS%
echo Testing starting at:
call time /t
call ensure-release-area
nmake /s /nologo %QUOTED_OPTIONS% %TEST_TARGET%
if %ERRORLEVEL% NEQ 0 goto build_error
if not "%WARNINGS%"=="no" call show-build-warnings %WARNINGS_OPTIONS%
goto end

REM //
REM // Logical end of script //
REM //

REM //
REM // Error handling //
REM //
:NO_RELEASE_ERROR
set ERROR_MESSAGE=No arguments supplied -- must supply a release directory
goto print_error

:NO_ARG
set ERROR_MESSAGE=Keyword options must be followed by arguments
goto print_error

:PRINT_ERROR
echo Error: %ERROR_MESSAGE%
goto generate_error

:BUILD_ERROR
echo Build of %TEST_TARGET% failed at:
call time /t
goto generate_error

:GENERATE_ERROR
bogus-command-to-cause-an-error-exit 2>nul
goto END

:fixup_PATHS
set OLD_RELEASE_ROOT=%~sf1
goto :EOF

:END
