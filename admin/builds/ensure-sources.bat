@echo off

set NAME=%1%
set SOURCE_DIRECTORY=%2%

if exist "%SOURCE_DIRECTORY%" goto END
checkout %NAME%

:END
