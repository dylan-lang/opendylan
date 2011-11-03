@echo off

:ENSURE_BUILD_LOGS
if exist "%OPEN_DYLAN_BUILD_LOGS%" goto ensure_build
mkdir %OPEN_DYLAN_BUILD_LOGS%

:ENSURE_BUILD
if exist "%OPEN_DYLAN_USER_BUILD%" goto ensure_bin
mkdir %OPEN_DYLAN_USER_BUILD%

:ENSURE_BIN
if exist "%OPEN_DYLAN_USER_INSTALL%"\bin goto ensure_lib
mkdir %OPEN_DYLAN_USER_INSTALL%\bin

:ENSURE_LIB
if exist "%OPEN_DYLAN_USER_INSTALL%"\lib goto end
mkdir %OPEN_DYLAN_USER_INSTALL%\lib
goto end

:END
