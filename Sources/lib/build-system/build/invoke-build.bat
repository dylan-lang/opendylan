if not exist ..\%1 goto END
if not exist ..\%1\build.bat goto END
if exist ..\%1\dylanmakefile goto END

echo %INDENT%Library %1
pushd ..\%1
call build

echo %INDENT%    Deleting %1\build.bat
del /f /q build.bat
echo %INDENT%    Deleting %1\%1.build
del /f /q %1.build

popd

:END