@echo off

setlocal

set LOG=
set SUMMARY=no
set WARNINGS=yes
set MESSAGES=no
set NITS=yes

REM //
REM // Loop through the command line arguments //
REM //
:PARAM_LOOP
If "%1%"==""            GOTO PARAM_DONE
If "%1%"=="/log"        GOTO SET_LOG
If "%1%"=="/summary"    GOTO SET_SUMMARY
If "%1%"=="/warnings"   GOTO SET_WARNINGS
If "%1%"=="/nowarnings" GOTO SET_NOWARNINGS
If "%1%"=="/nonits"     GOTO SET_NONITS
If "%1%"=="/messages"   GOTO SET_MESSAGES
If "%1%"=="/nomessages" GOTO SET_NOMESSAGES
goto no_such_argument

REM //
REM // Set all the variables depending upon command line args //
REM //
:SET_LOG
set log=%2%
shift
shift
goto PARAM_LOOP

:SET_SUMMARY
set summary=yes
shift
goto PARAM_LOOP

:SET_WARNINGS
set warnings=yes
shift
goto PARAM_LOOP

:SET_NOWARNINGS
set warnings=no
shift
goto PARAM_LOOP

:SET_NONITS
set nits=no
shift
goto PARAM_LOOP

:SET_MESSAGES
set messages=yes
shift
goto PARAM_LOOP

:SET_NOMESSAGES
set messages=no
shift
goto PARAM_LOOP

:PARAM_DONE

set OPTIONS=-g
if "%WARNINGS%"=="yes" set OPTIONS=%OPTIONS%b
if "%SUMMARY%"=="yes"  set OPTIONS=%OPTIONS%s
if "%MESSAGES%"=="yes" set OPTIONS=%OPTIONS%o
if "%NITS%"=="no"      set OPTIONS=%OPTIONS%t
if "%LOG%"=="" goto show_all_warnings

:SHOW_LOG_WARNINGS
perl find-warnings.pl %OPTIONS% %LOG%
if "%ERRORLEVEL%"=="9009" echo PERL.EXE not found
goto end

:SHOW_ALL_WARNINGS
perl find-warnings.pl %OPTIONS% %FUNCTIONAL_DEVELOPER_BUILD_LOGS%\compile-*.log
if "%ERRORLEVEL%"=="9009" echo PERL.EXE not found
goto end

:NO_SUCH_ARGUMENT
echo No such argument %1%
goto end

:END
endlocal
