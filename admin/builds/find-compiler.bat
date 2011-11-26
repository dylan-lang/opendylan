@echo off

REM //... then try to find the console-compiler/environment
REM //
set DYLAN_RELEASE_COMPILER=%DYLAN_RELEASE_ROOT%\bin\dylan-environment-with-tools.exe
if exist "%DYLAN_RELEASE_COMPILER%" goto compiler_found
set DYLAN_RELEASE_COMPILER=%DYLAN_RELEASE_ROOT%\bin\dylan-environment.exe
if exist "%DYLAN_RELEASE_COMPILER%" goto compiler_found
set DYLAN_RELEASE_COMPILER=%DYLAN_RELEASE_ROOT%\bin\console-environment.exe
if exist "%DYLAN_RELEASE_COMPILER%" goto compiler_found
set DYLAN_RELEASE_COMPILER=%DYLAN_RELEASE_ROOT%\bin\minimal-console-environment.exe
if exist "%DYLAN_RELEASE_COMPILER%" goto compiler_found
set DYLAN_RELEASE_COMPILER=%DYLAN_RELEASE_ROOT%\bin\dylan-compiler-with-tools.exe
if exist "%DYLAN_RELEASE_COMPILER%" goto compiler_found
set DYLAN_RELEASE_COMPILER=%DYLAN_RELEASE_ROOT%\bin\dylan-compiler.exe
if exist "%DYLAN_RELEASE_COMPILER%" goto compiler_found
set DYLAN_RELEASE_COMPILER=%DYLAN_RELEASE_ROOT%\bin\console-compiler.exe
if exist "%DYLAN_RELEASE_COMPILER%" goto compiler_found
set DYLAN_RELEASE_COMPILER=%DYLAN_RELEASE_ROOT%\bin\minimal-console-compiler.exe
if exist "%DYLAN_RELEASE_COMPILER%" goto compiler_found

REM //... finally hope that we can find it on the path.
REM //
set DYLAN_RELEASE_COMPILER=dylan-compiler.exe

:COMPILER_FOUND
