@echo off
goto START
REM //    Script: set-build-variables.bat
REM //    Author: Shri Amit(amit), Andy Armstrong(andrewa)
REM //

:PRINT_USAGE
echo ---------------------------------------------------------------------------------
echo - SYNOPSIS: Sets up all the Dylan environment variables for x86-win32.
echo -
echo - USAGE:    set-build-variables -sr [System Root] -pr [Personal Root] -sd [System Drive] -dr [Dylan Root] -l [Target Platform]
echo -
echo -           All arguments except [Personal Root] are optional
echo -           Must supply optional arguments if Defaults are not satisfactory
echo -
echo -           Args:     -sr  [System Root],
echo -                     -s   [System Root],
echo -                     -r   [System Root]       Specifies the Dylan System root
echo -                                              Supplied pathname is interpreted relative to [System Drive]
echo -                                              Default: [System Drive]\dylan\releases\pentium-kan
echo -                                                    or [System Drive]\dylan\releases\kan (if "-l c" is specified)
echo -                     -pr  [Personal Root],    
echo -                     -p   [Personal Root]     Specifies the users personal root
echo -                                              Must supply an absolute pathname
echo -                                              Default: none
echo -  
echo -                     -sp  [Root],
echo -                     -ps  [Root]              Sets both [System Root] and [Personal Root] to [Root]  
echo -                                              For convenience only 
echo -                                              One would typically specify a [Dylan Root] when this is used
echo -  
echo -                     -dr  [Dylan Root],
echo -                     -d   [Dylan Root]        Specifies the Dylan Release root
echo -                                              Supplied pathname is interpreted relative to [System Drive]
echo -                                              Only supply this argument if a different System Root is desired
echo -                                              (This Root does not get added to PATH)
echo -                                              Default: [System Root]
echo -
echo -                     -sd  [System Drive]      Specifies the drive on which the [System Root] and [Dylan Root] live
echo -                                              Only supply this argument if [System Root] or [Dylan Root] are relative
echo -                                              Default: current drive
echo -
echo -                     -l  [Target Platform]    Specifies the pc back end to be used
echo -                                              Default: pentium(sets root to \dylan\releases\pentium-kan)
echo -                                              The other acceptable value for this option is 'c' which
echo -                                              sets the root to \dylan\releases\kan
echo -
echo -                     -branch  [CVS Branch]    Specifies the CVS branch to be used for checkouts.
echo -                                              Default: trunk
echo -
echo -                     -nopath                  Does not modify the PATH variable.
echo -
echo -                     -help,
echo -                     -?                        Display help info
echo -
echo -
echo - EXAMPLES:
echo - 
echo - SET UP ENVIRONMENT USING SYSTEM(Dylan Users)
echo - 
echo -   If network drive "U" is mapped as superdirectory of dylan
echo -   (these examples are identical)
echo -   C: set-build-variables -p C:\users\user\dylan -sd U:
echo -   C: set-build-variables -p C:\users\user\dylan -s U:\dylan\releases\pentium-kan
echo -   C: set-build-variables -p C:\users\user\dylan -s \dylan\releases\pentium-kan -sd U:
echo -   C: set-build-variables -p C:\users\user\dylan -s \dylan\releases\pentium-kan -d \dylan\releases\pentium-kan -sd U:
echo - 
echo -   (these examples are identical)
echo -   U: set-build-variables -p \user\dylan
echo -   U: set-build-variables -p \user\dylan -s \dylan\releases\pentium-kan
echo -   U: set-build-variables -p \user\dylan -s \dylan\releases\pentium-kan -d \dylan\releases\pentium-kan
echo - 
echo -   If network drive "D" is mapped as \dylan
echo -   C: set-build-variables -p C:\users\user\dylan -s D:\releases\pentium-kan
echo - 
echo -   To point to a PC System Root
echo -   C: set-build-variables -p C:\users\user\dylan -s C:\Progra~1\Functi~1\Dylan\system
echo - 
echo - SET UP ENVIRONMENT TO DO PERSONAL DEVEL WORK INDEPENDENTLY OF SYSTEM(Dylan Developers)
echo - (you must supply additional Dylan Root)
echo - 
echo -   If network drive "U" is mapped as superdirectory of dylan
echo -   C: set-build-variables -ps C:\users\user\dylan -d U:\dylan\releases\pentium-kan
echo -   C: set-build-variables -ps C:\users\user\dylan -d U:\dylan\releases\kan-970524
echo -   C: set-build-variables -ps C:\users\user\dylan -d C:\Progra~1\Functi~1\Dylan\system
echo -   U: set-build-variables -ps \user\dylan -d \dylan\releases\pentium-kan
echo - 
echo -   If network drive "D" is mapped as \dylan
echo -   C: set-build-variables -ps C:\users\user\dylan -d D:\releases\pentium-kan
echo -  
echo - SET UP ENVIRONMENT TO BUILD A NEW DYLAN RELEASE(Dylan Administrators)
echo - (you must supply additional Dylan Root)
echo - 
echo -   If network drive "U" is mapped as superdirectory of dylan
echo -   U: set-build-variables -ps \dylan\releases\pentium-kan-next -d \dylan\releases\pentium-kan
echo -   U: set-build-variables -ps \dylan\releases\pentium-kan-next -d \dylan\releases\kan-970524
echo -   U: set-build-variables -ps \dylan\releases\pentium-kan-next -d C:\Progra~1\Functi~1\Dylan\system
echo -   U: set-build-variables -ps \dylan\releases\pentium-kan-next -d C:\Progra~1\Functi~1\Dylan\system -branch D-kan
echo - 
echo -   If network drive "D" is mapped as \dylan
echo -   D: set-build-variables -ps \releases\pentium-kan-next -d \releases\pentium-kan
echo - 
echo ---------------------------------------------------------------------------------
goto END

:START
If "%1%"=="help" GOTO PRINT_USAGE
If "%1%"=="-help" GOTO PRINT_USAGE
If "%1%"=="?" GOTO PRINT_USAGE
set OPEN_DYLAN_RELEASE_ROOT=
set OPEN_DYLAN_RELEASE_SOURCES=
set OPEN_DYLAN_RELEASE_INSTALL=
set OPEN_DYLAN_USER_ROOT=
set OPEN_DYLAN_USER_SOURCES=
set OPEN_DYLAN_USER_BUILD=
set OPEN_DYLAN_USER_INSTALL=
set DYLAN_SYSTEM_DRIVE=
set ERROR_MESSAGE=
set BACK_END=pentium
set GENERATION=
set FINAL_GENERATION=
if "%OPEN_DYLAN_DEFAULT_ROOT%"=="" set OPEN_DYLAN_DEFAULT_ROOT=C:\Program Files\Open Dylan
set DYLAN_CVS_BRANCH=
set DYLAN_RELEASE_ROOT=
set NOPATH=
set QUIET=no
set USE_ENVIRONMENT=no

REM //
REM // Loop through the command line arguments //
REM //
:PARAM_LOOP
If "%1%"==""    GOTO PARAM_DONE
If "%1%"=="-r"  GOTO SET_ROOT
If "%1%"=="-s"  GOTO SET_ROOT
If "%1%"=="-sr"  GOTO SET_ROOT
If "%1%"=="-d"  GOTO SET_KAN_ROOT
If "%1%"=="-dr"  GOTO SET_KAN_ROOT
If "%1%"=="-l"  GOTO SET_TARGET_PLATFORM
If "%1%"=="-branch"  GOTO SET_CVS_BRANCH
If "%1%"=="-pr" GOTO SET_USER_ROOT
If "%1%"=="-p"  GOTO SET_USER_ROOT
If "%1%"=="-ps" GOTO SET_BOTH_ROOTS
If "%1%"=="-sp" GOTO SET_BOTH_ROOTS
If "%1%"=="-sd" GOTO SET_SYSTEM_DRIVE
if "%1%"=="-sources" GOTO SET_USER_SOURCES
if "%1%"=="-build" GOTO SET_USER_BUILD
if "%1%"=="-install" GOTO SET_USER_INSTALL
If "%1%"=="-release"  GOTO SET_ROOT
if "%1%"=="-release-sources" GOTO SET_RELEASE_SOURCES
if "%1%"=="-release-install" GOTO SET_RELEASE_INSTALL
If "%1%"=="-nopath"  GOTO SET_NOPATH
If "%1%"=="-generation"  GOTO SET_GENERATION
If "%1%"=="-final-generation"  GOTO SET_FINAL_GENERATION
If "%1%"=="-quiet"  GOTO SET_QUIET
If "%1%"=="-verbose"  GOTO SET_VERBOSE
If "%1%"=="-environment"  GOTO SET_USE_ENVIRONMENT
If "%1%"=="-environment"  GOTO SET_USE_ENVIRONMENT
set ERROR_MESSAGE="Invalid command line argument %1%"
GOTO PRINT_ERROR

REM //
REM // Set all the variables depending upon command line args //
REM //
:SET_TARGET_PLATFORM
If "%2%"=="" GOTO NO_ARG
set BACK_END=%2%
shift
shift
goto PARAM_LOOP

:SET_SYSTEM_DRIVE
If "%2%"=="" GOTO NO_ARG
set DYLAN_SYSTEM_DRIVE=%2%
shift
shift
goto PARAM_LOOP

:SET_CVS_BRANCH
If "%2%"=="" GOTO NO_ARG
set DYLAN_CVS_BRANCH=%2%
shift
shift
goto PARAM_LOOP

:SET_ROOT
If "%2%"=="" GOTO NO_ARG
set OPEN_DYLAN_RELEASE_ROOT=%2%
shift
shift
goto PARAM_LOOP

:SET_USER_ROOT
If "%2%"=="" GOTO NO_ARG
set OPEN_DYLAN_USER_ROOT=%2%
shift
shift
goto PARAM_LOOP

:SET_USER_SOURCES
If "%2%"=="" GOTO NO_ARG
set OPEN_DYLAN_USER_SOURCES=%2%
shift
shift
goto PARAM_LOOP

:SET_USER_INSTALL
If "%2%"=="" GOTO NO_ARG
set OPEN_DYLAN_USER_INSTALL=%2%
shift
shift
goto PARAM_LOOP

:SET_USER_BUILD
If "%2%"=="" GOTO NO_ARG
set OPEN_DYLAN_USER_BUILD=%2%
shift
shift
goto PARAM_LOOP

:SET_RELEASE_SOURCES
If "%2%"=="" GOTO NO_ARG
set OPEN_DYLAN_RELEASE_SOURCES=%2%
shift
shift
goto PARAM_LOOP

:SET_RELEASE_INSTALL
If "%2%"=="" GOTO NO_ARG
set OPEN_DYLAN_RELEASE_INSTALL=%2%
shift
shift
goto PARAM_LOOP

:SET_BOTH_ROOTS
If "%2%"=="" GOTO NO_ARG
set OPEN_DYLAN_USER_ROOT=%2%
set OPEN_DYLAN_RELEASE_ROOT=%2%
shift
shift
goto PARAM_LOOP

:SET_KAN_ROOT
If "%2%"=="" GOTO NO_ARG
set DYLAN_RELEASE_ROOT=%2%
shift
shift
goto PARAM_LOOP

:SET_NOPATH
set NOPATH="T"
shift
goto PARAM_LOOP

:SET_GENERATION
If "%2%"=="" GOTO NO_ARG
set GENERATION=%2%
shift
shift
goto PARAM_LOOP

:SET_FINAL_GENERATION
If "%2%"=="" GOTO NO_ARG
set FINAL_GENERATION=%2%
shift
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

:SET_USE_ENVIRONMENT
set USE_ENVIRONMENT=yes
shift
goto PARAM_LOOP

REM //
REM // Set the defaults and check for unset variables //
REM //
:PARAM_DONE

If "%OPEN_DYLAN_RELEASE_ROOT%"=="" set OPEN_DYLAN_RELEASE_ROOT=%OPEN_DYLAN_DEFAULT_ROOT%
If "%DYLAN_RELEASE_ROOT%"=="" set DYLAN_RELEASE_ROOT=%OPEN_DYLAN_RELEASE_ROOT%
call :fixup_PATHS "%OPEN_DYLAN_RELEASE_ROOT%" "%DYLAN_RELEASE_ROOT%"
If "%DYLAN_CVS_BRANCH%"==""   set DYLAN_CVS_BRANCH=trunk
If "%OPEN_DYLAN_USER_ROOT%"=="" goto :NO_PR

REM //
REM // Default user variables //
REM //

if "%OPEN_DYLAN_USER_SOURCES%"=="" set OPEN_DYLAN_CVS_LOGS=%OPEN_DYLAN_USER_ROOT%\logs
if "%OPEN_DYLAN_USER_SOURCES%"=="" set OPEN_DYLAN_USER_SOURCES=%OPEN_DYLAN_USER_ROOT%\Sources
if "%OPEN_DYLAN_USER_INSTALL%"=="" set OPEN_DYLAN_USER_INSTALL=%OPEN_DYLAN_USER_ROOT%
if "%OPEN_DYLAN_USER_BUILD%"=="" set OPEN_DYLAN_BUILD_LOGS=%OPEN_DYLAN_USER_ROOT%\logs
if "%OPEN_DYLAN_USER_BUILD%"=="" set OPEN_DYLAN_USER_BUILD=%OPEN_DYLAN_USER_ROOT%\Build
set OPEN_DYLAN_USER_REGISTRIES=%OPEN_DYLAN_USER_SOURCES%\registry
if "%OPEN_DYLAN_CVS_LOGS%"=="" set OPEN_DYLAN_CVS_LOGS=%OPEN_DYLAN_USER_SOURCES%\..\logs
if "%OPEN_DYLAN_BUILD_LOGS%"=="" set OPEN_DYLAN_BUILD_LOGS=%OPEN_DYLAN_USER_BUILD%\..\logs

REM //
REM // Default release variables //
REM //

set DYLAN_RELEASE_ROOT=%DYLAN_SYSTEM_DRIVE%%DYLAN_RELEASE_ROOT%
set OPEN_DYLAN_RELEASE_ROOT=%DYLAN_SYSTEM_DRIVE%%OPEN_DYLAN_RELEASE_ROOT%
if "%OPEN_DYLAN_RELEASE_SOURCES%"=="" set OPEN_DYLAN_RELEASE_SOURCES=%OPEN_DYLAN_RELEASE_ROOT%\Sources
if "%OPEN_DYLAN_RELEASE_INSTALL%"=="" set OPEN_DYLAN_RELEASE_INSTALL=%OPEN_DYLAN_RELEASE_ROOT%
set OPEN_DYLAN_PLATFORM_NAME=x86-win32

REM //
REM // Set the path
REM // (is there anyway to stop it growing everytime we get invoked?)
REM //

If "%BACK_END%"=="pentium" IF "%NOPATH%"=="" set PATH=%OPEN_DYLAN_USER_INSTALL%\bin;%OPEN_DYLAN_RELEASE_INSTALL%\bin;%PATH%

REM //
REM // Patchup to handle different generations
REM //
if "%GENERATION%"=="1" goto setup_generation1
if not "%FINAL_GENERATION%"=="" goto setup_final_generation
if "%GENERATION%"=="" goto finish_setup
goto setup_middle_generation

:SETUP_GENERATION1
set BOOTSTRAP_ROOT=%OPEN_DYLAN_USER_ROOT%\Bootstrap.1
set OPEN_DYLAN_USER_BUILD=%BOOTSTRAP_ROOT%\Build
set OPEN_DYLAN_BUILD_LOGS=%BOOTSTRAP_ROOT%\logs
set OPEN_DYLAN_USER_INSTALL=%BOOTSTRAP_ROOT%
set OPEN_DYLAN_USER_REGISTRIES=%OPEN_DYLAN_USER_SOURCES%\bootstrap1-registry
goto finish_setup

:SETUP_MIDDLE_GENERATION
set BOOTSTRAP_GENERATION=%GENERATION%
set /a BOOTSTRAP_GENERATION-=1
set DYLAN_RELEASE_ROOT=%OPEN_DYLAN_USER_ROOT%\Bootstrap.%BOOTSTRAP_GENERATION%
set BOOTSTRAP_ROOT=%OPEN_DYLAN_USER_ROOT%\Bootstrap.%GENERATION%
set OPEN_DYLAN_USER_BUILD=%BOOTSTRAP_ROOT%\Build
set OPEN_DYLAN_BUILD_LOGS=%BOOTSTRAP_ROOT%\logs
set OPEN_DYLAN_USER_INSTALL=%BOOTSTRAP_ROOT%
set OPEN_DYLAN_RELEASE_ROOT=%OPEN_DYLAN_USER_ROOT%
set OPEN_DYLAN_RELEASE_SOURCES=%OPEN_DYLAN_USER_SOURCES%
set OPEN_DYLAN_RELEASE_INSTALL=%OPEN_DYLAN_USER_INSTALL%
goto finish_setup

:SETUP_FINAL_GENERATION
set BOOTSTRAP_GENERATION=%FINAL_GENERATION%
set /a BOOTSTRAP_GENERATION-=1
if not "%BOOTSTRAP_GENERATION%"=="0" set DYLAN_RELEASE_ROOT=%OPEN_DYLAN_USER_ROOT%\Bootstrap.%BOOTSTRAP_GENERATION%
set OPEN_DYLAN_RELEASE_ROOT=%OPEN_DYLAN_USER_ROOT%
set OPEN_DYLAN_RELEASE_SOURCES=%OPEN_DYLAN_USER_SOURCES%
set OPEN_DYLAN_RELEASE_INSTALL=%OPEN_DYLAN_USER_INSTALL%
set OPEN_DYLAN_BUILD_LOGS=%OPEN_DYLAN_USER_ROOT%\logs
goto finish_setup

:FINISH_SETUP
set OPEN_DYLAN_RELEASE_REGISTRIES=%OPEN_DYLAN_RELEASE_SOURCES%\registry
if "%USE_ENVIRONMENT%"=="yes" goto find_console_compiler
call find-compiler
goto describe_build

:FIND_CONSOLE_COMPILER
call find-compiler -environment

REM //
REM // Describe the build that will happen
REM //

:DESCRIBE_BUILD
if "%QUIET%"=="yes" goto end
if not "%FINAL_GENERATION%"=="" goto describe_final_generation
if not "%GENERATION%"=="" goto describe_build_generation
echo Build in %OPEN_DYLAN_USER_ROOT%
goto describe_settings

:DESCRIBE_BUILD_GENERATION
echo Generation %GENERATION% build in %OPEN_DYLAN_USER_ROOT%
goto describe_settings

:DESCRIBE_FINAL_GENERATION
echo Final build in %OPEN_DYLAN_USER_ROOT%
goto describe_settings

REM //
REM // Inform the user of the settings
REM //

:DESCRIBE_SETTINGS
echo   [Compiler:           %DYLAN_RELEASE_COMPILER%]

if "%OPEN_DYLAN_RELEASE_ROOT%"=="%OPEN_DYLAN_USER_ROOT%" goto skip_release_settings
echo   [Bootstrap root:     %OPEN_DYLAN_RELEASE_ROOT%]
if "%OPEN_DYLAN_RELEASE_SOURCES%"=="%OPEN_DYLAN_RELEASE_ROOT%\Sources" goto skip_release_sources
echo   [Bootstrap sources:  %OPEN_DYLAN_RELEASE_SOURCES%]

:SKIP_RELEASE_SOURCES
if "%OPEN_DYLAN_RELEASE_REGISTRIES%"=="%OPEN_DYLAN_RELEASE_SOURCES%\registry" goto skip_release_registries
echo   [Bootstrap registry: %OPEN_DYLAN_RELEASE_REGISTRIES%]

:SKIP_RELEASE_REGISTRIES
if "%OPEN_DYLAN_RELEASE_INSTALL%"=="%OPEN_DYLAN_RELEASE_ROOT%" goto skip_release_settings
if "%OPEN_DYLAN_RELEASE_INSTALL%"=="%OPEN_DYLAN_USER_INSTALL%" goto skip_release_settings
echo   [Bootstrap install:  %OPEN_DYLAN_RELEASE_INSTALL%]

:SKIP_RELEASE_SETTINGS
if "%DYLAN_CVS_BRANCH%"=="trunk" goto skip_cvs_branch
echo   [CVS branch:         %DYLAN_CVS_BRANCH%]

:SKIP_CVS_BRANCH
echo   [CVS logs:           %OPEN_DYLAN_CVS_LOGS%]

if "%OPEN_DYLAN_USER_SOURCES%"=="%OPEN_DYLAN_USER_ROOT%\Sources" goto skip_sources
echo   [Sources:            %OPEN_DYLAN_USER_SOURCES%]

:SKIP_SOURCES
if "%OPEN_DYLAN_USER_REGISTRIES%"=="%OPEN_DYLAN_USER_SOURCES%\registry" goto skip_registries
echo   [Registry:           %OPEN_DYLAN_USER_REGISTRIES%]

:SKIP_REGISTRIES
if "%OPEN_DYLAN_USER_INSTALL%"=="%OPEN_DYLAN_USER_ROOT%" goto skip_install
echo   [Install:            %OPEN_DYLAN_USER_INSTALL%]

:SKIP_INSTALL
if "%OPEN_DYLAN_USER_BUILD%"=="%OPEN_DYLAN_USER_ROOT%\Build" goto skip_build
echo   [Build:              %OPEN_DYLAN_USER_BUILD%]

:SKIP_BUILD
echo   [Build logs:         %OPEN_DYLAN_BUILD_LOGS%]

goto end

REM //
REM // Logical end of script //
REM //

REM //
REM // Error handling //
REM //
:NO_PR
set ERROR_MESSAGE=Personal root must be specified
GOTO :PRINT_ERROR

:NO_ARG
set ERROR_MESSAGE=Keyword options must be followed by arguments

:PRINT_ERROR
echo "Error: %ERROR_MESSAGE%"
GOTO END

:fixup_PATHS
set OPEN_DYLAN_RELEASE_ROOT=%~sf1
set DYLAN_RELEASE_ROOT=%~sf2
goto :EOF

:END
