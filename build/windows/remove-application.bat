@echo off

set LIBRARY=%1%

echo Removing %LIBRARY%
call remove-directory %OPEN_DYLAN_USER_BUILD%\%LIBRARY%
call remove-file %OPEN_DYLAN_USER_INSTALL%\databases\%LIBRARY%.ddb
call remove-file %OPEN_DYLAN_USER_INSTALL%\bin\%LIBRARY%.exe
call remove-file %OPEN_DYLAN_USER_INSTALL%\lib\%LIBRARY%.lib
