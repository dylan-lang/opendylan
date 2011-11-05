@echo off

if exist "opendylan.chm" goto sources
echo opendylan.chm not found
goto end

:SOURCES
if exist "%OPEN_DYLAN_RELEASE_SOURCES%" goto license
echo "cannot find release sources"
goto end

:LICENSE
if exist "%OPEN_DYLAN_RELEASE_SOURCES%\..\License.txt" goto root
echo "cannot find License.txt"
goto end

:ROOT
if exist "%OPEN_DYLAN_RELEASE_ROOT%" goto bin
echo "cannot find release root"
goto end

:BIN
if exist "%OPEN_DYLAN_RELEASE_ROOT%\bin" goto lib
echo "cannot find release root"
goto end

:LIB
if exist "%OPEN_DYLAN_RELEASE_ROOT%\lib" goto databases
echo "cannot find release root"
goto end

:DATABASES
if exist "%OPEN_DYLAN_RELEASE_ROOT%\databases" goto templates
echo "cannot find release root"
goto end

:TEMPLATES
if exist "%OPEN_DYLAN_RELEASE_ROOT%\Templates" goto examples
echo "cannot find release root"
goto end

:EXAMPLES
if exist "%OPEN_DYLAN_RELEASE_ROOT%\examples" goto redistributable
echo "cannot find release root"
goto end

:REDISTRIBUTABLE
if exist "%OPEN_DYLAN_RELEASE_ROOT%\redistributable" goto version
echo "cannot find release root"
goto end

:VERSION
if exist "%OPEN_DYLAN_RELEASE_ROOT%\bin\console-compiler" goto set_version
echo "cannot find console-compiler"
goto end

:SET_VERSION
for /f %%X in ('%OPEN_DYLAN_RELEASE_ROOT%\bin\console-compiler /shortversion') DO SET OPEN_DYLAN_VERSION=%%X
if "%OPEN_DYLAN_VERSION%"=="" goto end

:MAKENSIS
makensis opendylan.nsi

:END
