@echo off
setlocal

set MYDIR=%~dp0.
set NAME=%1
set REPOSITORY=%2
set DIRECTORY=%3
set PARENT=%~dp3.
set DIRNAME=%~nx3
set UNIT=%4

call %MYDIR%\ensure-directory %PARENT%

if "%UNIT%"=="" goto set_BRANCH

if not exist "%DIRECTORY%" goto set_BRANCH
if exist "%DIRECTORY%\CVS" goto set_BRANCH

call %MYDIR%\ensure-directory %DIRECTORY%\CVS
echo D> %DIRECTORY%\CVS\Entries
echo %CVSROOT%> %DIRECTORY%\CVS\Root
echo %REPOSITORY%> %DIRECTORY%\CVS\Repository

:set_BRANCH
set CVS_BRANCH_ARG=
if "%DYLAN_CVS_BRANCH%"=="" goto checkout
if "%DYLAN_CVS_BRANCH%"=="trunk" goto checkout
set CVS_BRANCH_ARG=-r %DYLAN_CVS_BRANCH%

:CHECKOUT
set LOG=%FUNCTIONAL_DEVELOPER_CVS_LOGS%\checkout-%NAME%.log

if "%UNIT%"=="" goto checkout_TREE
pushd %PARENT%
cvs -q -r checkout -d %DIRNAME% %CVS_BRANCH_ARG% %REPOSITORY%/%UNIT% 1>%LOG%
set saved_ERRORLEVEL=%ERRORLEVEL%
popd
goto check_RESULT

:checkout_TREE
pushd %PARENT%
cvs -q -r checkout -d %DIRNAME% %CVS_BRANCH_ARG% %REPOSITORY% 1>%LOG%
set saved_ERRORLEVEL=%ERRORLEVEL%
popd
goto check_RESULT

:check_RESULT
if %saved_ERRORLEVEL% NEQ 0 goto checkout_ERROR
endlocal
goto END

:checkout_ERROR
echo Checkout of %NAME% failed (error level %saved_ERRORLEVEL%)
echo [See log %LOG%]
goto generate_ERROR

:generate_ERROR
endlocal
bogus-command-to-cause-an-error-exit 2>nul

:END
