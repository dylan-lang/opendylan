@echo off

if "%*"=="" goto :NO_ARGUMENTS
call set-build-variables -quiet %*
goto start_compiler

:NO_ARGUMENTS
if "%DYLAN_RELEASE_COMPILER%"=="" goto unknown_compiler_error

:START_COMPILER
echo Starting %DYLAN_RELEASE_COMPILER%
%DYLAN_RELEASE_COMPILER%
goto end

:UNKNOWN_COMPILER_ERROR
echo RELEASE-COMPILER started with no arguments and variables not set
goto generate_error

:GENERATE_ERROR
bogus-command-to-cause-an-error-exit 2>nul

:END
