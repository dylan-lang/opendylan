@echo off

set NAME=%1%
set SOURCE_DIRECTORY=%2%

if exist "%SOURCE_DIRECTORY%" goto END
echo Directory %SOURCE_DIRECTORY% does not exist.  Aborting.
exit 1

:END
