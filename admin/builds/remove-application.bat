@echo off

set LIBRARY=%1%

echo Removing %LIBRARY%
call remove-directory %FUNCTIONAL_DEVELOPER_USER_BUILD%\%LIBRARY%
call remove-file %FUNCTIONAL_DEVELOPER_USER_INSTALL%\databases\%LIBRARY%.ddb
call remove-file %FUNCTIONAL_DEVELOPER_USER_INSTALL%\bin\%LIBRARY%.exe
call remove-file %FUNCTIONAL_DEVELOPER_USER_INSTALL%\lib\%LIBRARY%.lib
