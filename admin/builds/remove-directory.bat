@echo off

set DIRECTORY=%1%

if not exist "%DIRECTORY%" goto end
rmdir /S /Q %DIRECTORY%

:END
