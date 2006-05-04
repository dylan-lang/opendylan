@echo off
setlocal

set LIBRARY=%1%
set REGISTRY=%2%
set LOG=%OPEN_DYLAN_CVS_LOGS%\checkout-%LIBRARY%-registry-entry.log

if exist "%OPEN_DYLAN_USER_REGISTRIES%" goto set_BRANCH
mkdir %OPEN_DYLAN_USER_REGISTRIES%
if %ERRORLEVEL% NEQ 0 goto generate_error

:set_BRANCH
set CVS_BRANCH_ARG=
if "%DYLAN_CVS_BRANCH%"=="" goto checkout
if "%DYLAN_CVS_BRANCH%"=="trunk" goto checkout
set CVS_BRANCH_ARG=-r %DYLAN_CVS_BRANCH%

:CHECKOUT
if exist "%OPEN_DYLAN_USER_REGISTRIES%\%REGISTRY%\%LIBRARY%" goto DONE
echo Checking out registry entry for %LIBRARY%
pushd %OPEN_DYLAN_USER_REGISTRIES%
cvs -q -r checkout -d %REGISTRY% %CVS_BRANCH_ARG% fundev/Sources/registry/%REGISTRY%/%LIBRARY% > %LOG%
set saved_ERRORLEVEL=%ERRORLEVEL%
popd
if %saved_ERRORLEVEL% NEQ 0 goto checkout_error

:DONE
endlocal
goto end

:CHECKOUT_ERROR
echo Checkout of %LIBRARY% failed (error level %saved_ERRORLEVEL%)
echo [See log %LOG%]
goto generate_error

:GENERATE_ERROR
endlocal
bogus-command-to-cause-an-error-exit 2>nul

:END
