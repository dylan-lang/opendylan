@echo off

if exist %DYLAN_RELEASE_ROOT%\install\x86-win32\bin\build.exe goto BUILD

echo %DYLAN_RELEASE_ROOT%\install\x86-win32\bin\build.exe not found  -- aborting
goto END

:BUILD

REM How do you convince DOS to pass on more than 9 arguments?

%DYLAN_RELEASE_ROOT%\install\x86-win32\bin\build %1 %2 %3 %4 %5 %6 %7 %8 %9

:END
