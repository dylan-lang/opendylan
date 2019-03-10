@echo off

set SOURCE_DIRECTORY=%1%

if exist "%SOURCE_DIRECTORY%" goto END
REM echo Making directory %SOURCE_DIRECTORY%
mkdir %SOURCE_DIRECTORY%

:END

