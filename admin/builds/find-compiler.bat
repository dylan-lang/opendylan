@echo off

If "%1%"=="-environment"        GOTO FIND_ENVIRONMENT
If "%1%"=="/environment"        GOTO FIND_ENVIRONMENT

REM // First try to find pentium-dw
REM //
set DYLAN_RELEASE_COMPILER=%DYLAN_RELEASE_ROOT%\bin\pentium-dw.exe
if exist "%DYLAN_RELEASE_COMPILER%" goto compiler_found
set DYLAN_RELEASE_COMPILER=%DYLAN_RELEASE_ROOT%\bin\minimal-pentium-dw.exe
if exist "%DYLAN_RELEASE_COMPILER%" goto compiler_found
set DYLAN_RELEASE_COMPILER=%DYLAN_RELEASE_ROOT%\bin\basic-pentium-dw.exe
if exist "%DYLAN_RELEASE_COMPILER%" goto compiler_found
set DYLAN_RELEASE_COMPILER=%DYLAN_RELEASE_ROOT%\bin\personal-pentium-dw.exe
if exist "%DYLAN_RELEASE_COMPILER%" goto compiler_found
set DYLAN_RELEASE_COMPILER=%DYLAN_RELEASE_ROOT%\bin\fdbc.exe
if exist "%DYLAN_RELEASE_COMPILER%" goto compiler_found
set DYLAN_RELEASE_COMPILER=%DYLAN_RELEASE_ROOT%\bin\dylan-compile.exe
if exist "%DYLAN_RELEASE_COMPILER%" goto compiler_found

REM //... then try to find the console-compiler/environment
REM //
:FIND_ENVIRONMENT
set DYLAN_RELEASE_COMPILER=%DYLAN_RELEASE_ROOT%\bin\console-compiler.exe
if exist "%DYLAN_RELEASE_COMPILER%" goto compiler_found
set DYLAN_RELEASE_COMPILER=%DYLAN_RELEASE_ROOT%\bin\minimal-console-compiler.exe
if exist "%DYLAN_RELEASE_COMPILER%" goto compiler_found
set DYLAN_RELEASE_COMPILER=%DYLAN_RELEASE_ROOT%\bin\basic-console-compiler.exe
if exist "%DYLAN_RELEASE_COMPILER%" goto compiler_found
set DYLAN_RELEASE_COMPILER=%DYLAN_RELEASE_ROOT%\bin\fdce.exe
if exist "%DYLAN_RELEASE_COMPILER%" goto compiler_found
set DYLAN_RELEASE_COMPILER=%DYLAN_RELEASE_ROOT%\bin\console-environment.exe
if exist "%DYLAN_RELEASE_COMPILER%" goto compiler_found
set DYLAN_RELEASE_COMPILER=%DYLAN_RELEASE_ROOT%\bin\minimal-console-environment.exe
if exist "%DYLAN_RELEASE_COMPILER%" goto compiler_found
set DYLAN_RELEASE_COMPILER=%DYLAN_RELEASE_ROOT%\bin\basic-console-environment.exe
if exist "%DYLAN_RELEASE_COMPILER%" goto compiler_found

REM //... finally hope that we can find it on the path.
REM //
set DYLAN_RELEASE_COMPILER=fdbc.exe

:COMPILER_FOUND
