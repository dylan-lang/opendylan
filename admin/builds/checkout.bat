@echo off

set NAME=%1%

echo Checking out %NAME%
nmake /s /nologo checkout-%NAME%

