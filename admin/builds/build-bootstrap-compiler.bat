@echo off

set OLD_DYLAN_HOPE_BRANCH=%DYLAN_HOPE_BRANCH%
set BOOTSTRAP_HOPE_BRANCH=%1%
If "%BOOTSTRAP_HOPE_BRANCH%"=="" set BOOTSTRAP_HOPE_BRANCH=D-kan

echo Bootstrapping %OLD_DYLAN_HOPE_BRANCH% compiler using %BOOTSTRAP_HOPE_BRANCH% branch
echo Phase one: creating basic hierarchy using %DYLAN_HOPE_BRANCH% branch.
call ensure-release-area
if %ERRORLEVEL% NEQ 0 goto error
echo Phase one complete.

set DYLAN_HOPE_BRANCH=%BOOTSTRAP_HOPE_BRANCH%
echo Phase two: building core libraries from %DYLAN_HOPE_BRANCH% branch.
call nmake /nologo user-libraries
if %ERRORLEVEL% NEQ 0 goto error
set DYLAN_HOPE_BRANCH=%OLD_DYLAN_HOPE_BRANCH%
echo Phase two complete.

echo Phase three: building compiler from %DYLAN_HOPE_BRANCH% branch.
call nmake /nologo internal-compiler build
if %ERRORLEVEL% NEQ 0 goto error
echo Phase three complete.

echo Compiler bootstrapped -- type pentium-dw to run it.
goto end

:error
echo Bootstrap failed with error code %ERRORLEVEL%
set DYLAN_HOPE_BRANCH=%OLD_DYLAN_HOPE_BRANCH%
bogus-command-to-cause-an-error-exit 2>nul

:end
