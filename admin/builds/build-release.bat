@echo off
goto START
REM //    Script: build-release.bat
REM //    Author: Andy Armstrong (andrewa)
REM //

:PRINT_USAGE
echo ------------------------------------------------------------------------
echo - SYNOPSIS: Builds a Dylan release
echo -
echo - USAGE:    build-release [Release Directory] {options}*
echo -
echo -
echo -   Options:
echo -     /target
echo -       Specifies the target release name. [default: minimal-internal-release]
echo -     /generations
echo -       Specifies the number of generations. [default: 3]
echo -     /exports
echo -       Switches on generation of GNU exports [default: no exports]
echo -     /strip-runtime
echo -       Strips out debug information from the runtime DLLs [default: no]
echo -     /dylan
echo -       Specifies the directory to build from (previous release)
echo -       [default: C:\Program Files\Open Dylan]
echo -     /sources
echo -       Specifies the location of the sources [default: under release]
echo -     /branch
echo -       Specifies the CVS branch. [default: trunk]
echo -     /rm-early-builds
echo -       Cleans up build products of non-last generations.
echo -     /warnings
echo -       Shows a warning summary after each generation [default: none]
echo -     /serious-warnings
echo -       Shows only a serious warning summary [default: none]
echo -     /timings
echo -       Generates timing data for each generation [default: no timings]
echo -     /compare-timings
echo -       Generates a comparison of the timing data for the last generation
echo -       against the comparison compiler (specifed by /timings-root)
echo -       [default: no comparison]
echo -     /timings-root
echo -       Specifies the directory against whose logs/ directory the timing
echo -       comparisons are made [default: first generation directory for
echo -       multiple generations;  specified by /dylan for one generation]
echo -     /statistics
echo -       Turns on a reasonable default set of statistics [default: none]
echo -     /debugger
echo -       Runs the builds under batch-debug [default: no debugging]
echo -     /fullcrt
echo -       will build the pentium run time using full version of the C runtime
echo -     /runtime-expiration YYYYMMDD
echo -       sets the expiration date for the Dylan runtime to YYYYMMDD [default: none]
echo -     /verbose
echo -       Show more information during building
echo - 
echo -
echo - PREREQUISITES:
echo -   - You must have the Hacker Edition installed.
echo -   - You should remove <hacker-edition>\bin\projects.txt
echo -   - If you didn't install the Hacker Edition in the standard location
echo -     you should use the /dylan argument to specify its location.
echo -   - It's a good idea to also set OPEN_DYLAN_DEFAULT_ROOT to
echo -     the same value passed as the /dylan argument.
echo -   - You must set the CVSROOT environment variable appropriately.
echo -   - Some options cause perl scripts to be invoked.  Have perl on your PATH.
echo -   - VC++ 6 and the MSSDK should be installed.  Make sure that the VC++ directories
echo -     appear before the MSSDK directories on your PATH, and after them on your
echo -     LIB and INCLUDE variables.
echo -
echo - EXAMPLES:
echo - 
echo -   build-release c:\dylan
echo -
echo -     Build a new internal release in c:\dylan
echo -
echo -   build-release c:\dylan /target pentium-dw
echo -
echo -     Build just the compiler
echo -
echo -   build-release c:\dylan /generations 1
echo -
echo -     Build the release without bootstrapping, presumably because
echo -     it is known that the compiler hasn't changed enough to need
echo -     to bootstrap.
echo -
echo -   build-release c:\dylan /sources u:\andrewa\dylan
echo -
echo -     Build the release from the sources in u:\andrewa\dylan
echo -     rather than checking them out from CVS. Note that if
echo -     any pieces are missing, that they will get checked out
echo -     as part of the build process [this may not be what you
echo -     want, we should work on a solution if you need it].
echo -     The build products will be stored in c:\dylan.
echo ------------------------------------------------------------------------
goto END

:START
if "%1"=="help" GOTO PRINT_USAGE
if "%1"=="/help" GOTO PRINT_USAGE
if "%1"=="-help" GOTO PRINT_USAGE
if "%1"=="?" GOTO PRINT_USAGE
if "%1"=="/?" GOTO PRINT_USAGE

set ERROR_MESSAGE=
if "%OPEN_DYLAN_DEFAULT_ROOT%"=="" set OPEN_DYLAN_DEFAULT_ROOT=C:\Program Files\Open Dylan
set OLD_RELEASE_ROOT=%OPEN_DYLAN_DEFAULT_ROOT%
set OPEN_DYLAN_USER_SOURCES=
set SAVED_OPEN_DYLAN_LIBRARY_PACKS=%OPEN_DYLAN_LIBRARY_PACKS%
set GENERATIONS=3
set CVS_BRANCH=trunk
set BOOTSTRAP_TARGET=
set COMPILER_TARGET=
set FINAL_COMPILER_TARGET=
set COMPILER_FILENAME=
set FINAL_COMPILER_FILENAME=
set RELEASE_TARGET=
set CLEANUP=no
set EXPORTS=no
set DEBUGGING=no
set WARNINGS=no
set WARNINGS_OPTIONS=/nonits
set DEBUG_FAILURE=no
set SHOW_FAILURE_LOG=no
set TEST=no
set TIMINGS=no
set COMPARE_TIMINGS=no
set TIMINGS_ROOT=
set STRIP_RUNTIME=no
set OLD_RUNTIME_PREFIX=D3
set RUNTIME_PREFIX=Dx
set SANITIZE=no
set USE_ENVIRONMENT=yes
set USE_FULL_C_RUNTIME=no
set RUNTIME_EXPIRATION=none
set VERBOSE=no

set OPEN_DYLAN_USER_SOURCES=

REM //
REM // The first argument is the release directory
REM //
if "%1"=="" GOTO NO_RELEASE_ERROR
set NEW_RELEASE_ROOT=%1
shift

REM //
REM // Loop through the command line arguments //
REM //
:PARAM_LOOP
if "%1"==""                    GOTO PARAM_DONE
if "%1"=="-environment"        GOTO SET_USE_ENVIRONMENT
if "%1"=="/environment"        GOTO SET_USE_ENVIRONMENT
if "%1"=="-pentium-dw"         GOTO SET_USE_PENTIUM_DW
if "%1"=="/pentium-dw"         GOTO SET_USE_PENTIUM_DW
if "%1"=="-dylan"              GOTO SET_OLD_RELEASE_ROOT
if "%1"=="/dylan"              GOTO SET_OLD_RELEASE_ROOT
if "%1"=="-branch"             GOTO SET_CVS_BRANCH
if "%1"=="/branch"             GOTO SET_CVS_BRANCH
if "%1"=="-generations"        GOTO SET_GENERATIONS
if "%1"=="/generations"        GOTO SET_GENERATIONS
if "%1"=="-target"             GOTO SET_RELEASE_TARGET
if "%1"=="/target"             GOTO SET_RELEASE_TARGET
if "%1"=="-bootstrap-target"   GOTO SET_BOOTSTRAP_TARGET
if "%1"=="/bootstrap-target"   GOTO SET_BOOTSTRAP_TARGET
if "%1"=="-sources"            GOTO SET_SOURCES
if "%1"=="/sources"            GOTO SET_SOURCES
if "%1"=="-rm-early-builds"    GOTO SET_CLEANUP
if "%1"=="/rm-early-builds"    GOTO SET_CLEANUP
if "%1"=="-exports"            GOTO SET_EXPORTS
if "%1"=="/exports"            GOTO SET_EXPORTS
if "%1"=="/exports:yes"        GOTO SET_EXPORTS
if "%1"=="/exports:no"         GOTO RESET_EXPORTS
if "%1"=="-debugger"           GOTO SET_DEBUGGING
if "%1"=="/debugger"           GOTO SET_DEBUGGING
if "%1"=="-debug-failure"      GOTO SET_DEBUG_FAILURE
if "%1"=="/debug-failure"      GOTO SET_DEBUG_FAILURE
if "%1"=="-show-failure-log"   GOTO SET_SHOW_FAILURE_LOG
if "%1"=="/show-failure-log"   GOTO SET_SHOW_FAILURE_LOG
if "%1"=="-warnings"           GOTO SET_WARNINGS
if "%1"=="/warnings"           GOTO SET_WARNINGS
if "%1"=="-serious-warnings"   GOTO SET_SERIOUS_WARNINGS
if "%1"=="/serious-warnings"   GOTO SET_SERIOUS_WARNINGS
if "%1"=="-timings"            GOTO SET_TIMINGS
if "%1"=="/timings"            GOTO SET_TIMINGS
if "%1"=="-compare-timings"    GOTO SET_COMPARE_TIMINGS
if "%1"=="/compare-timings"    GOTO SET_COMPARE_TIMINGS
if "%1"=="-timings-root"       GOTO SET_TIMINGS_ROOT
if "%1"=="/timings-root"       GOTO SET_TIMINGS_ROOT
if "%1"=="-statistics"         GOTO SET_STATISTICS
if "%1"=="/statistics"         GOTO SET_STATISTICS
if "%1"=="-test"               GOTO SET_TEST
if "%1"=="/test"               GOTO SET_TEST
if "%1"=="-strip-runtime"      GOTO SET_STRIP_RUNTIME
if "%1"=="/strip-runtime"      GOTO SET_STRIP_RUNTIME
if "%1"=="-sanitize"           GOTO SET_SANITIZE
if "%1"=="/sanitize"           GOTO SET_SANITIZE
if "%1"=="-internal"           GOTO SET_INTERNAL
if "%1"=="/internal"           GOTO SET_INTERNAL
if "%1"=="-external"           GOTO SET_EXTERNAL
if "%1"=="/external"           GOTO SET_EXTERNAL
if "%1"=="-fullcrt"            GOTO SET_FULLCRT
if "%1"=="/fullcrt"            GOTO SET_FULLCRT
if "%1"=="/runtime-expiration" GOTO SET_RUNTIME_EXPIRATION
if "%1"=="-runtime-expiration" GOTO SET_RUNTIME_EXPIRATION
if "%1%"=="/verbose"           GOTO SET_VERBOSE
if "%1%"=="-verbose"           GOTO SET_VERBOSE
set ERROR_MESSAGE=Invalid command line argument %1
GOTO PRINT_ERROR

REM //
REM // Set all the variables depending upon command line args //
REM //
:SET_USE_ENVIRONMENT
set USE_ENVIRONMENT=yes
shift
goto PARAM_LOOP

:SET_USE_PENTIUM_DW
set USE_ENVIRONMENT=no
shift
goto PARAM_LOOP

:SET_OLD_RELEASE_ROOT
if "%2"=="" GOTO NO_ARG
set OLD_RELEASE_ROOT=%2
shift
shift
goto PARAM_LOOP

:SET_CVS_BRANCH
if "%2"=="" GOTO NO_ARG
set CVS_BRANCH=%2
shift
shift
goto PARAM_LOOP

:SET_GENERATIONS
if "%2"=="" GOTO NO_ARG
set GENERATIONS=%2
shift
shift
goto PARAM_LOOP

:SET_RELEASE_TARGET
if "%2"=="" GOTO NO_ARG
set RELEASE_TARGET=%2
shift
shift
goto PARAM_LOOP

:SET_BOOTSTRAP_TARGET
if "%2"=="" GOTO NO_ARG
set BOOTSTRAP_TARGET=%2
shift
shift
goto PARAM_LOOP

:SET_SOURCES
if "%2"=="" GOTO NO_ARG
set OPEN_DYLAN_USER_SOURCES=%2
shift
shift
goto PARAM_LOOP

:SET_CLEANUP
set CLEANUP=yes
shift
goto PARAM_LOOP

:SET_EXPORTS
set EXPORTS=yes
shift
goto PARAM_LOOP

:RESET_EXPORTS
set EXPORTS=no
shift
goto PARAM_LOOP

:SET_DEBUGGING
set DEBUGGING=yes
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

:SET_WARNINGS
set WARNINGS=yes
shift
goto PARAM_LOOP

:SET_SERIOUS_WARNINGS
set WARNINGS=yes
set WARNINGS_OPTIONS=%WARNINGS_OPTIONS% /nowarnings
shift
goto PARAM_LOOP

:SET_TIMINGS
set TIMINGS=yes
shift
goto PARAM_LOOP

:SET_COMPARE_TIMINGS
set TIMINGS=yes
set COMPARE_TIMINGS=yes
shift
goto PARAM_LOOP

:SET_TIMINGS_ROOT
if "%2"=="" GOTO NO_ARG
set TIMINGS_ROOT=%2
shift
shift
goto PARAM_LOOP

:SET_STATISTICS
set WARNINGS=yes
set TIMINGS=yes
set COMPARE_TIMINGS=yes
shift
goto PARAM_LOOP

:SET_TEST
set TEST=yes
shift
goto PARAM_LOOP

:SET_STRIP_RUNTIME
set STRIP_RUNTIME=yes
shift
goto PARAM_LOOP

:SET_SANITIZE
set SANITIZE=yes
shift
goto PARAM_LOOP

REM //
REM // Setup the internal release options //
REM //
:SET_INTERNAL
set GENERATIONS=4
set EXPORTS=yes
set STRIP_RUNTIME=no
set WARNINGS=yes
set TIMINGS=yes
set COMPARE_TIMINGS=yes
set TIMINGS_ROOT=/ORR
shift
goto PARAM_LOOP

REM //
REM // Setup the external release options //
REM //
:SET_EXTERNAL
set GENERATIONS=4
set EXPORTS=yes
set STRIP_RUNTIME=yes
set WARNINGS=yes
set TIMINGS=yes
set COMPARE_TIMINGS=yes
set TIMINGS_ROOT=/ORR
shift
goto PARAM_LOOP

:SET_FULLCRT
set USE_FULL_C_RUNTIME=yes
shift
goto PARAM_LOOP

:SET_RUNTIME_EXPIRATION
if "%2%"=="" GOTO NO_ARG
set RUNTIME_EXPIRATION=%2%
shift
shift
goto PARAM_LOOP

:SET_VERBOSE
set VERBOSE=yes
shift
goto PARAM_LOOP

REM //
REM // Default user variables //
REM //

:PARAM_DONE
call :fixup_PATHS "%OLD_RELEASE_ROOT%"
if "%OPEN_DYLAN_USER_SOURCES%"=="" set OPEN_DYLAN_USER_SOURCES=%NEW_RELEASE_ROOT%\Sources
set OPEN_DYLAN_USER_REGISTRIES=%OPEN_DYLAN_USER_SOURCES%\registry
if "%USE_ENVIRONMENT%"=="no" goto setup_dw_default_build_options
goto setup_environment_default_build_options

REM //
REM // Setup the default build options for the environment/console compiler build //
REM //
:SETUP_ENVIRONMENT_DEFAULT_BUILD_OPTIONS
if "%COMPILER_TARGET%"=="" set COMPILER_TARGET=minimal-console-compiler
if "%FINAL_COMPILER_TARGET%"=="" set FINAL_COMPILER_TARGET=console-compiler console-scepter
if "%COMPILER_FILENAME%"=="" set COMPILER_FILENAME=minimal-console-compiler.exe
if "%FINAL_COMPILER_FILENAME%"=="" set FINAL_COMPILER_FILENAME=console-compiler.exe
if "%RELEASE_TARGET%"=="" set RELEASE_TARGET=minimal-internal-release
goto setup_default_release_options

REM //
REM // Setup the default build options for the pentium-dw build //
REM //
:SETUP_DW_DEFAULT_BUILD_OPTIONS
if "%COMPILER_TARGET%"=="" set COMPILER_TARGET=minimal-compiler
if "%FINAL_COMPILER_TARGET%"=="" set FINAL_COMPILER_TARGET=compiler console-scepter
if "%COMPILER_FILENAME%"=="" set COMPILER_FILENAME=minimal-pentium-dw.exe
if "%FINAL_COMPILER_FILENAME%"=="" set FINAL_COMPILER_FILENAME=pentium-dw.exe
if "%RELEASE_TARGET%"=="" set RELEASE_TARGET=minimal-internal-release
goto setup_default_release_options

REM //
REM // Default release variables //
REM //
:SETUP_DEFAULT_RELEASE_OPTIONS
if "%VERBOSE%"=="yes" set MAKE=nmake /d /nologo
if "%VERBOSE%"=="no"  set MAKE=nmake /s /nologo
set OPEN_DYLAN_RELEASE_SOURCES=%OLD_RELEASE_ROOT%\Sources
set OPEN_DYLAN_RELEASE_REGISTRIES=%OPEN_DYLAN_RELEASE_SOURCES%\registry
set OPEN_DYLAN_PLATFORM_NAME=x86-win32

REM // Pentium runtime build options
set PENTIUM_RUNTIME_OPTIONS=
set QUOTED_PENTIUM_RUNTIME_OPTIONS=
if "%RUNTIME_EXPIRATION%"=="none" goto skip_expiration_option
set PENTIUM_RUNTIME_OPTIONS=EXPIRATION=%RUNTIME_EXPIRATION%
set QUOTED_PENTIUM_RUNTIME_OPTIONS=PENTIUM_RUNTIME_OPTIONS="%PENTIUM_RUNTIME_OPTIONS%"

:SKIP_EXPIRATION_OPTION
if "%USE_FULL_C_RUNTIME%"=="no" goto setup_build_options
set PENTIUM_RUNTIME_OPTIONS=%PENTIUM_RUNTIME_OPTIONS% fullcrt=yes
set QUOTED_PENTIUM_RUNTIME_OPTIONS=PENTIUM_RUNTIME_OPTIONS="%PENTIUM_RUNTIME_OPTIONS%"

REM // Build options
:SETUP_BUILD_OPTIONS
set OPTIONS=
set QUOTED_OPTIONS=
if "%VERBOSE%"=="no" goto skip_verbose_option
set OPTIONS=%OPTIONS% /verbose
set QUOTED_OPTIONS=OPTIONS="%OPTIONS%"

:SKIP_VERBOSE_OPTION
if "%DEBUGGING%"=="no" goto skip_debugging_option
set OPTIONS=%OPTIONS% /debugger
set QUOTED_OPTIONS=OPTIONS="%OPTIONS%"

:SKIP_DEBUGGING_OPTION
if "%DEBUG_FAILURE%"=="no" goto skip_debug_failure_option
set OPTIONS=%OPTIONS% /debug-failure
set QUOTED_OPTIONS=OPTIONS="%OPTIONS%"

:SKIP_DEBUG_FAILURE_OPTION
if "%SHOW_FAILURE_LOG%"=="no" goto skip_show_failure_log_option
set OPTIONS=%OPTIONS% /show-failure-log
set QUOTED_OPTIONS=OPTIONS="%OPTIONS%"

:SKIP_SHOW_FAILURE_LOG_OPTION
set COMMON_BUILD_OPTIONS= -branch %CVS_BRANCH% -nopath
set BUILD_OPTIONS=-p %NEW_RELEASE_ROOT% -s %OLD_RELEASE_ROOT% %COMMON_BUILD_OPTIONS%
if not "%OPEN_DYLAN_USER_SOURCES%"=="" set BUILD_OPTIONS=%BUILD_OPTIONS% -sources %OPEN_DYLAN_USER_SOURCES%
if "%USE_ENVIRONMENT%"=="yes" set BUILD_OPTIONS=%BUILD_OPTIONS% -environment

REM // Source sanitizing options
set CHECKOUT_OPTIONS=
if "%SANITIZE%"=="no" goto setup_bootstrap_targets
set CHECKOUT_OPTIONS=/sanitize
set QUOTED_CHECKOUT_OPTIONS=CHECKOUT_OPTIONS=%CHECKOUT_OPTIONS%

:SETUP_BOOTSTRAP_TARGETS
if "%RELEASE_TARGET%"=="minimal-console-compiler" goto setup_minimal_builds
if "%RELEASE_TARGET%"=="minimal-console-environment" goto setup_minimal_builds
if "%RELEASE_TARGET%"=="minimal-win32-environment" goto setup_minimal_builds
if "%RELEASE_TARGET%"=="minimal-internal-release" goto setup_minimal_builds
goto build

:SETUP_MINIMAL_BUILDS
set FINAL_COMPILER_TARGET=%COMPILER_TARGET%
set FINAL_COMPILER_FILENAME=%COMPILER_FILENAME%

REM //
REM // Start the build for real
REM //

:BUILD
if "%GENERATIONS%"=="1" goto build_release

:MAYBE_CREATE_BOOTSTRAP_REGISTRY
set USER_REGISTRY=%OPEN_DYLAN_USER_SOURCES%\registry
set OPEN_DYLAN_USER_REGISTRIES=%OPEN_DYLAN_USER_SOURCES%\bootstrap1-registry
if exist "%OPEN_DYLAN_USER_REGISTRIES%" goto build_bootstrap
if not exist "%USER_REGISTRY%" goto build_bootstrap

:CREATE_BOOTSTRAP_REGISTRY
echo Creating bootstrap registry
call ensure-directory %OPEN_DYLAN_USER_REGISTRIES%
xcopy /E /I %USER_REGISTRY%\generic %OPEN_DYLAN_USER_REGISTRIES%\generic >nul
xcopy /E /I %USER_REGISTRY%\x86-win32 %OPEN_DYLAN_USER_REGISTRIES%\x86-win32 >nul

echo Removing runtime registry entries
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\dylan" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\dylan" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\functional-extensions" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\functional-extensions" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\machine-word" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\machine-word" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\byte-vector" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\byte-vector" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\threads" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\threads" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\transcendentals" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\transcendentals" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\functional-dylan" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\functional-dylan" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\common-extensions" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\common-extensions" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\common-dylan" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\common-dylan" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\c-ffi" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\c-ffi" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\bit-vector" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\bit-vector" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\bit-set" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\bit-set" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\collectors" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\collectors" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\plists" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\plists" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\set" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\set" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\table-extensions" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\table-extensions" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\collections" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\collections" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\streams" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\streams" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\standard-io" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\standard-io" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\print" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\print" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\format" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\format" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\format-out" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\format-out" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\io" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\io" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\date" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\date" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\file-system" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\file-system" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\operating-system" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\operating-system" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\locators" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\locators" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\settings" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\settings" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\system" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\system" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\generic-arithmetic" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\generic-arithmetic" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\big-integers" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\big-integers" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\duim-utilities" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\duim-utilities" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\duim-geometry" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\duim-geometry" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\duim-dcs" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\duim-dcs" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\duim-sheets" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\duim-sheets" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\duim-graphics" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\duim-graphics" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\duim-layouts" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\duim-layouts" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\duim-gadgets" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\duim-gadgets" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\duim-frames" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\duim-frames" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\duim-core" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\duim-core" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\duim-extended-geometry" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\duim-extended-geometry" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\duim-gadget-panes" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\duim-gadget-panes" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\win32-duim" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\win32-duim" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\duim" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\duim" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\channels" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\channels" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\commands" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\commands" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\winsock2" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\winsock2" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\sockets" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\sockets" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\network" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\network" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\win32-common" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\win32-common" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\win32-controls" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\win32-controls" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\win32-dde" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\win32-dde" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\win32-dialog" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\win32-dialog" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\win32-gdi" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\win32-gdi" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\win32-html-help" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\win32-html-help" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\win32-kernel" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\win32-kernel" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\win32-registry" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\win32-registry" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\win32-resources" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\win32-resources" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\win32-rich-edit" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\win32-rich-edit" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\win32-shell" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\win32-shell" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\win32-user" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\win32-user" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\win32-version" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\win32-version" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\win32-gl" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\win32-gl" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\win32-glu" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\win32-glu" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\win32-multimedia" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\win32-multimedia" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\midi" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\midi" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\deuce" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\deuce" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\duim-deuce" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\duim-deuce" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\com" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\com" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\ole" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\ole" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\ole-server" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\ole-server" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\win32-automation" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\win32-automation" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\ole-automation" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\ole-automation" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\ole-container" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\ole-container" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\ole-controls" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\ole-controls" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\ole-control-framework" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\ole-control-framework" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\ole-dialogs" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\ole-dialogs" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\duim-ole-container" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\duim-ole-container" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\duim-ole-control" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\duim-ole-control" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\duim-ole-server" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\duim-ole-server" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\sql" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\sql" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\odbc-ffi" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\odbc-ffi" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\sql-odbc" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\sql-odbc" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\corba-dylan" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\corba-dylan" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\corba-protocol" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\corba-protocol" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\dylan-orb" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\dylan-orb" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\iop-protocol" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\iop-protocol" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\ir-protocol" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\ir-protocol" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\ir-stubs" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\ir-stubs" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\orb-connections" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\orb-connections" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\orb-core" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\orb-core" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\orb-iiop" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\orb-iiop" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\orb-ir" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\orb-ir" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\orb-poa" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\orb-poa" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\orb-streams" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\orb-streams" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\orb-utilities" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\orb-utilities" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\portableserver-protocol" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\portableserver-protocol" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\naming-protocol" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\naming-protocol" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\naming-stubs" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\naming-stubs" 2>nul
del "%OPEN_DYLAN_USER_REGISTRIES%\generic\naming-client" "%OPEN_DYLAN_USER_REGISTRIES%\x86-win32\naming-client" 2>nul

:BUILD_BOOTSTRAP
echo --------------------------------------------------
call set-build-variables %BUILD_OPTIONS% -generation 1
set OPEN_DYLAN_RELEASE_BUILD=ignore
set OPEN_DYLAN_LIBRARY_PACKS=0xFFFF
set BOOTSTRAP_COMPILER_FILENAME=%COMPILER_FILENAME%
set BOOTSTRAP_COMPILER_TARGET=%COMPILER_TARGET%
if "%TIMINGS_ROOT%"=="" set TIMINGS_ROOT=%BOOTSTRAP_ROOT%
if "%GENERATIONS%"=="2" goto prepare_bootstrap
if "%GENERATIONS%"=="3" goto prepare_bootstrap
goto ensure_bootstrap_compiler

:PREPARE_BOOTSTRAP
set BOOTSTRAP_COMPILER_FILENAME=%FINAL_COMPILER_FILENAME%
set BOOTSTRAP_COMPILER_TARGET=%FINAL_COMPILER_TARGET%

:ENSURE_BOOTSTRAP_COMPILER
if exist "%OPEN_DYLAN_USER_INSTALL%\bin\%BOOTSTRAP_COMPILER_FILENAME%" goto cont1
echo Generation 1 build starting at:
call date /t
call time /t
call ensure-release-area

if "%BOOTSTRAP_TARGET%"=="" goto bootstrap_compiler
%MAKE% %QUOTED_OPTIONS% %QUOTED_PENTIUM_RUNTIME_OPTIONS% %BOOTSTRAP_TARGET%
if %ERRORLEVEL% NEQ 0 goto build_error

:BOOTSTRAP_COMPILER
%MAKE% %QUOTED_OPTIONS% %QUOTED_PENTIUM_RUNTIME_OPTIONS% %BOOTSTRAP_COMPILER_TARGET%
if %ERRORLEVEL% NEQ 0 goto build_error
if "%WARNINGS%"=="yes" call show-build-warnings %WARNINGS_OPTIONS%
if "%TIMINGS%"=="yes" call generate-compiler-timings %OPEN_DYLAN_BUILD_LOGS%
if "%CLEANUP%"=="no" goto CONT1
call remove-directory %OPEN_DYLAN_USER_BUILD%
call remove-directory %OPEN_DYLAN_USER_INSTALL%\lib
call remove-directory %OPEN_DYLAN_USER_INSTALL%\databases
call remove-directory %OPEN_DYLAN_USER_REGISTRIES%
:CONT1
set OPEN_DYLAN_RELEASE_BUILD=
echo Generation 1 build completed at:
call date /t
call time /t

:MAYBE_COPY_OLD_MANGLER
REM This enables compiler-bootstrapping with new mangler scheme
if exist "%DYLAN_RELEASE_ROOT%\bin\old-dfmc-mangling.dll" copy %DYLAN_RELEASE_ROOT%\bin\old-dfmc-mangling.dll %OPEN_DYLAN_USER_INSTALL%\bin >nul
if exist "%DYLAN_RELEASE_ROOT%\bin\old-dfmc-mangling.dll" copy %DYLAN_RELEASE_ROOT%\bin\variable-search.dll %OPEN_DYLAN_USER_INSTALL%\bin >nul

:MAYBE_COPY_OLD_VARIABLE_SEARCH
REM This enables compiler-bootstrapping with new variable-searching
if exist "%DYLAN_RELEASE_ROOT%\bin\old-variable-search.dll" copy %DYLAN_RELEASE_ROOT%\bin\old-variable-search.dll %OPEN_DYLAN_USER_INSTALL%\bin\variable-search.dll >nul

if exist "%OPEN_DYLAN_USER_INSTALL%\bin\%OLD_RUNTIME_PREFIX%fundyl.dll" goto maybe_copy_bootstrap_registry
if exist "%OPEN_DYLAN_USER_INSTALL%\bin\%RUNTIME_PREFIX%fundyl.dll" goto maybe_copy_bootstrap_registry
echo Installing DLLs from old release %DYLAN_RELEASE_ROOT%
copy %DYLAN_RELEASE_ROOT%\bin\equal-table.dll %OPEN_DYLAN_USER_INSTALL%\bin 1>nul 2>nul

REM // Copy redistributable libraries (including those from a previous version)
REM // Be careful not to wipe out redistributable libraries
set OLD_LIBRARIES=bin
if exist "%DYLAN_RELEASE_ROOT%\Redistributable" set OLD_LIBRARIES=Redistributable
call ensure-directory %OPEN_DYLAN_USER_INSTALL%\Bin\temp
move %OPEN_DYLAN_USER_INSTALL%\Bin\%RUNTIME_PREFIX%*.dll %OPEN_DYLAN_USER_INSTALL%\Bin\temp >nul
copy %DYLAN_RELEASE_ROOT%\%OLD_LIBRARIES%\%OLD_RUNTIME_PREFIX%*.dll %OPEN_DYLAN_USER_INSTALL%\Bin 1>nul 2>nul
copy %DYLAN_RELEASE_ROOT%\%OLD_LIBRARIES%\%RUNTIME_PREFIX%*.dll %OPEN_DYLAN_USER_INSTALL%\Bin 1>nul 2>nul
copy %OPEN_DYLAN_USER_INSTALL%\Bin\temp\*.dll %OPEN_DYLAN_USER_INSTALL%\Bin >nul
REM call remove-directory %OPEN_DYLAN_USER_INSTALL%\Bin\temp

REM // These DLLs might exist if DLL merging didn't take place
copy %DYLAN_RELEASE_ROOT%\%OLD_LIBRARIES%\dylan.dll %OPEN_DYLAN_USER_INSTALL%\bin >nul
copy %DYLAN_RELEASE_ROOT%\%OLD_LIBRARIES%\functional-extensions.dll %OPEN_DYLAN_USER_INSTALL%\bin >nul
copy %DYLAN_RELEASE_ROOT%\%OLD_LIBRARIES%\machine-word.dll %OPEN_DYLAN_USER_INSTALL%\bin >nul
copy %DYLAN_RELEASE_ROOT%\%OLD_LIBRARIES%\byte-vector.dll %OPEN_DYLAN_USER_INSTALL%\bin >nul
copy %DYLAN_RELEASE_ROOT%\%OLD_LIBRARIES%\threads.dll %OPEN_DYLAN_USER_INSTALL%\bin >nul
copy %DYLAN_RELEASE_ROOT%\%OLD_LIBRARIES%\transcendentals.dll %OPEN_DYLAN_USER_INSTALL%\bin >nul

copy %DYLAN_RELEASE_ROOT%\%OLD_LIBRARIES%\bit-vector.dll %OPEN_DYLAN_USER_INSTALL%\bin >nul
copy %DYLAN_RELEASE_ROOT%\%OLD_LIBRARIES%\bit-set.dll %OPEN_DYLAN_USER_INSTALL%\bin >nul
copy %DYLAN_RELEASE_ROOT%\%OLD_LIBRARIES%\collectors.dll %OPEN_DYLAN_USER_INSTALL%\bin >nul
copy %DYLAN_RELEASE_ROOT%\%OLD_LIBRARIES%\plists.dll %OPEN_DYLAN_USER_INSTALL%\bin >nul
copy %DYLAN_RELEASE_ROOT%\%OLD_LIBRARIES%\set.dll %OPEN_DYLAN_USER_INSTALL%\bin >nul
copy %DYLAN_RELEASE_ROOT%\%OLD_LIBRARIES%\table-extensions.dll %OPEN_DYLAN_USER_INSTALL%\bin >nul

copy %DYLAN_RELEASE_ROOT%\%OLD_LIBRARIES%\streams.dll %OPEN_DYLAN_USER_INSTALL%\bin >nul
copy %DYLAN_RELEASE_ROOT%\%OLD_LIBRARIES%\print.dll %OPEN_DYLAN_USER_INSTALL%\bin >nul
copy %DYLAN_RELEASE_ROOT%\%OLD_LIBRARIES%\standard-io.dll %OPEN_DYLAN_USER_INSTALL%\bin >nul
copy %DYLAN_RELEASE_ROOT%\%OLD_LIBRARIES%\format.dll %OPEN_DYLAN_USER_INSTALL%\bin >nul
copy %DYLAN_RELEASE_ROOT%\%OLD_LIBRARIES%\format-out.dll %OPEN_DYLAN_USER_INSTALL%\bin >nul

copy %DYLAN_RELEASE_ROOT%\%OLD_LIBRARIES%\date.dll %OPEN_DYLAN_USER_INSTALL%\bin >nul
copy %DYLAN_RELEASE_ROOT%\%OLD_LIBRARIES%\file-system.dll %OPEN_DYLAN_USER_INSTALL%\bin >nul
copy %DYLAN_RELEASE_ROOT%\%OLD_LIBRARIES%\locators.dll %OPEN_DYLAN_USER_INSTALL%\bin >nul
copy %DYLAN_RELEASE_ROOT%\%OLD_LIBRARIES%\operating-system.dll %OPEN_DYLAN_USER_INSTALL%\bin >nul
copy %DYLAN_RELEASE_ROOT%\%OLD_LIBRARIES%\settings.dll %OPEN_DYLAN_USER_INSTALL%\bin >nul

:MAYBE_COPY_BOOTSTRAP_REGISTRY
if exist "%USER_REGISTRY%" goto finish_bootstrap
if not exist "%OPEN_DYLAN_USER_REGISTRIES%" goto finish_bootstrap
echo Copying bootstrap registry entries to main registry
call ensure-directory %USER_REGISTRY%
call ensure-directory %USER_REGISTRY%\generic
call ensure-directory %USER_REGISTRY%\x86-win32
copy %OPEN_DYLAN_USER_REGISTRIES%\generic\*.* %USER_REGISTRY%\generic >nul
xcopy /E /I %OPEN_DYLAN_USER_REGISTRIES%\generic\CVS %USER_REGISTRY%\generic\CVS >nul
copy %OPEN_DYLAN_USER_REGISTRIES%\x86-win32\*.* %USER_REGISTRY%\x86-win32 >nul
xcopy /E /I %OPEN_DYLAN_USER_REGISTRIES%\x86-win32\CVS %USER_REGISTRY%\x86-win32\CVS >nul

:FINISH_BOOTSTRAP
set OPEN_DYLAN_USER_REGISTRIES=%USER_REGISTRY%
set /a GENERATION=2

:BUILD_NEXT_GENERATION
if "%GENERATION%"=="%GENERATIONS%" goto build_release
echo --------------------------------------------------
call set-build-variables %BUILD_OPTIONS% -generation %GENERATION%
set OPEN_DYLAN_LIBRARY_PACKS=0xFFFF
set /a NEXT_GENERATION=%GENERATION%
set /a NEXT_GENERATION+=1
set /a NEXT_BUT_ONE_GENERATION=%NEXT_GENERATION%
set /a NEXT_BUT_ONE_GENERATION+=1
set BOOTSTRAP_COMPILER_FILENAME=%COMPILER_FILENAME%
set BOOTSTRAP_COMPILER_TARGET=%COMPILER_TARGET%
if "%NEXT_GENERATION%"=="%GENERATIONS%" goto prepare_next_bootstrap_generation
if "%NEXT_BUT_ONE_GENERATION%"=="%GENERATIONS%" goto prepare_next_bootstrap_generation
goto ensure_next_generation

:PREPARE_NEXT_BOOTSTRAP_GENERATION
set BOOTSTRAP_COMPILER_FILENAME=%FINAL_COMPILER_FILENAME%
set BOOTSTRAP_COMPILER_TARGET=%FINAL_COMPILER_TARGET%

:ENSURE_NEXT_GENERATION
if exist "%OPEN_DYLAN_USER_INSTALL%\bin\%BOOTSTRAP_COMPILER_FILENAME%" goto cont_next
echo Generation %GENERATION% build starting at:
call date /t
call time /t
call ensure-release-area
%MAKE% %QUOTED_OPTIONS% %QUOTED_PENTIUM_RUNTIME_OPTIONS% %BOOTSTRAP_COMPILER_TARGET%
if %ERRORLEVEL% NEQ 0 goto build_error
if "%WARNINGS%"=="yes" call show-build-warnings %WARNINGS_OPTIONS%
if "%TIMINGS%"=="yes" call generate-compiler-timings %OPEN_DYLAN_BUILD_LOGS%
if "%CLEANUP%"=="no" goto CONT_NEXT
call remove-directory %OPEN_DYLAN_USER_BUILD%
call remove-directory %OPEN_DYLAN_USER_INSTALL%\lib
call remove-directory %OPEN_DYLAN_USER_INSTALL%\databases

:CONT_NEXT
echo Generation %GENERATION% build completed at:
call date /t
call time /t
set /a GENERATION+=1
goto build_next_generation

:BUILD_RELEASE
REM // Switch on GNU exports if requested
if "%EXPORTS%"=="no" goto handle_runtime_options
if "%OPTIONS%"=="" goto set_exports_only
set OPTIONS=%OPTIONS% /exports
set QUOTED_OPTIONS=OPTIONS="%OPTIONS%"
goto handle_runtime_options

:SET_EXPORTS_ONLY
set OPTIONS=/exports
set QUOTED_OPTIONS=OPTIONS=%OPTIONS%

:HANDLE_RUNTIME_OPTIONS
if "%STRIP_RUNTIME%"=="no" goto no_runtime_stripping
set QUOTED_RUNTIME_OPTIONS=RUNTIME_OPTIONS="/debug-min /build-counts ignore"
goto start_build

:NO_RUNTIME_STRIPPING
set QUOTED_RUNTIME_OPTIONS=RUNTIME_OPTIONS="/build-counts ignore"

:START_BUILD
echo --------------------------------------------------
call set-build-variables %BUILD_OPTIONS% -final-generation %GENERATIONS%
set OPEN_DYLAN_LIBRARY_PACKS=0xFFFF
echo Final generation build starting at:
call date /t
call time /t
call ensure-release-area
%MAKE% %QUOTED_OPTIONS% %QUOTED_PENTIUM_RUNTIME_OPTIONS% %QUOTED_RUNTIME_OPTIONS% %QUOTED_CHECKOUT_OPTIONS% %RELEASE_TARGET%
if %ERRORLEVEL% NEQ 0 goto build_error
if "%WARNINGS%"=="yes" call show-build-warnings %WARNINGS_OPTIONS%
if "%TIMINGS%"=="yes" call generate-compiler-timings %OPEN_DYLAN_BUILD_LOGS%
echo Build of %RELEASE_TARGET% completed at:
call date /t
call time /t
if "%TIMINGS_ROOT%"=="/ORR" set TIMINGS_ROOT=%OLD_RELEASE_ROOT%
if "%COMPARE_TIMINGS%"=="yes" call compare-timings %TIMINGS_ROOT%\logs %OPEN_DYLAN_BUILD_LOGS%
set PATH=%OPEN_DYLAN_USER_INSTALL%\bin;%PATH%
set OPEN_DYLAN_LIBRARY_PACKS=%SAVED_OPEN_DYLAN_LIBRARY_PACKS%

:TEST_BUILD
if "%TEST%"=="no" goto end
call test-release %NEW_RELEASE_ROOT% -dylan %OLD_RELEASE_ROOT% -target test-%RELEASE_TARGET% -sources %OPEN_DYLAN_USER_SOURCES%
goto end

:fixup_PATHS
set OLD_RELEASE_ROOT=%~sf1
goto :EOF

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
echo Build of %RELEASE_TARGET% failed at:
call date /t
call time /t
goto generate_error

:GENERATE_ERROR
set OPEN_DYLAN_LIBRARY_PACKS=%SAVED_OPEN_DYLAN_LIBRARY_PACKS%
bogus-command-to-cause-an-error-exit 2>nul

:END
