@echo off

set LIBRARY=%1%
set LIBRARY_TARGET=%2%
if "%LIBRARY_TARGET%"=="" goto no_dll_name
if "%LIBRARY_TARGET%"=="%LIBRARY%" goto no_dll_name
echo Removing %LIBRARY% (DLL %LIBRARY_TARGET%.dll)
goto remove_library

:NO_DLL_NAME
set LIBRARY_TARGET=%LIBRARY%
echo Removing %LIBRARY%
goto remove_library

:REMOVE_LIBRARY
call remove-directory %OPEN_DYLAN_USER_BUILD%\%LIBRARY%
call remove-file %OPEN_DYLAN_USER_INSTALL%\databases\%LIBRARY%.ddb
call remove-file %OPEN_DYLAN_USER_INSTALL%\lib\%LIBRARY%.mkf
call remove-file %OPEN_DYLAN_USER_INSTALL%\lib\%LIBRARY_TARGET%.lib
call remove-file %OPEN_DYLAN_USER_INSTALL%\lib\%LIBRARY_TARGET%.defs
call remove-file %OPEN_DYLAN_USER_INSTALL%\bin\%LIBRARY_TARGET%.dll
