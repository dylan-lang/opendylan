@echo off

set FILE=%1%

if not exist "%FILE%" goto end
del /F /Q %FILE%

:END


