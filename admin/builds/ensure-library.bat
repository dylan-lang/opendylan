@echo off

set OPTIONS=
set PENTIUM_RUNTIME_OPTIONS=
set RUNTIME_OPTIONS=
set LIBRARY_TARGET=
set OLD_LIBRARY_TARGET=
set OLD_RUNTIME_PREFIX=D3
set RUNTIME_PREFIX=Dx
set QUIET=yes

rem echo Options: %*%

:PROCESS_OPTIONS
if "%1%" == "/pentium-runtime" goto start_processing_pentium_runtime_options
if "%1%" == "/runtime" goto start_processing_runtime_options
if "%2%" == "" goto finish_options
set OPTIONS=%OPTIONS% %1%
if "%1%" == "/verbose" set QUIET=no
if "%1%" == "-verbose" set QUIET=no
shift
goto process_options

:START_PROCESSING_PENTIUM_RUNTIME_OPTIONS
shift

:PROCESS_PENTIUM_RUNTIME_OPTIONS
if !%1%! == !/runtime! goto start_processing_runtime_options
if "%2%" == "" goto finish_options
set PENTIUM_RUNTIME_OPTIONS=%~nx1%
shift
goto process_pentium_runtime_options

:START_PROCESSING_RUNTIME_OPTIONS
shift

:PROCESS_RUNTIME_OPTIONS
if "%1%" == "/dll" goto process_dll_name
if "%2%" == "" goto finish_options
set RUNTIME_OPTIONS=%RUNTIME_OPTIONS% %1%
shift
goto process_runtime_options

:PROCESS_DLL_NAME
shift
set LIBRARY_TARGET=%1%
shift
goto process_runtime_options

:FINISH_OPTIONS
set LIBRARY=%1%
if "%LIBRARY_TARGET%"=="%RUNTIME_PREFIX%bigint" set OLD_LIBRARY_TARGET="%OLD_RUNTIME_PREFIX%bigint"
if "%LIBRARY_TARGET%"=="%RUNTIME_PREFIX%cffi" set OLD_LIBRARY_TARGET="%OLD_RUNTIME_PREFIX%cffi"
if "%LIBRARY_TARGET%"=="%RUNTIME_PREFIX%chnnls" set OLD_LIBRARY_TARGET="%OLD_RUNTIME_PREFIX%chnnls"
if "%LIBRARY_TARGET%"=="%RUNTIME_PREFIX%collns" set OLD_LIBRARY_TARGET="%OLD_RUNTIME_PREFIX%collns"
if "%LIBRARY_TARGET%"=="%RUNTIME_PREFIX%commnd" set OLD_LIBRARY_TARGET="%OLD_RUNTIME_PREFIX%commnd"
if "%LIBRARY_TARGET%"=="%RUNTIME_PREFIX%dylan" set OLD_LIBRARY_TARGET="%OLD_RUNTIME_PREFIX%dylan"
if "%LIBRARY_TARGET%"=="%RUNTIME_PREFIX%fundyl" set OLD_LIBRARY_TARGET="%OLD_RUNTIME_PREFIX%fundyl"
if "%LIBRARY_TARGET%"=="%RUNTIME_PREFIX%io" set OLD_LIBRARY_TARGET="%OLD_RUNTIME_PREFIX%io"
if "%LIBRARY_TARGET%"=="%RUNTIME_PREFIX%oleaut" set OLD_LIBRARY_TARGET="%OLD_RUNTIME_PREFIX%oleaut"
if "%LIBRARY_TARGET%"=="%RUNTIME_PREFIX%orb" set OLD_LIBRARY_TARGET="%OLD_RUNTIME_PREFIX%orb"
if "%LIBRARY_TARGET%"=="%RUNTIME_PREFIX%system" set OLD_LIBRARY_TARGET="%OLD_RUNTIME_PREFIX%system"
if "%LIBRARY_TARGET%"=="%RUNTIME_PREFIX%w32reg" set OLD_LIBRARY_TARGET="%OLD_RUNTIME_PREFIX%w32reg"
if "%LIBRARY_TARGET%"=="" set LIBRARY_TARGET=%LIBRARY%
if "%OLD_LIBRARY_TARGET%"=="" set OLD_LIBRARY_TARGET=%LIBRARY_TARGET%

:CONT
if "%QUIET%"=="yes" goto cont2
echo Ensuring library %LIBRARY%
if not "%LIBRARY%"=="%LIBRARY_TARGET%" echo  [dll %LIBRARY_TARGET%.dll]
if not "%LIBRARY_TARGET%"=="%OLD_LIBRARY_TARGET%" echo  [old dll %OLD_LIBRARY_TARGET%.dll]

:CONT2
if "%OPEN_DYLAN_RELEASE_INSTALL%"=="%OPEN_DYLAN_USER_INSTALL%" goto ensure_library
if exist "%WINDIR%\%LIBRARY_TARGET%" goto end

:ENSURE_LIBRARY

if exist "%OPEN_DYLAN_RELEASE_INSTALL%\bin\%LIBRARY_TARGET%.dll" goto end
if exist "%OPEN_DYLAN_RELEASE_INSTALL%\bin\%OLD_LIBRARY_TARGET%.dll" goto found_old_target
if exist "%OPEN_DYLAN_USER_INSTALL%\bin\%LIBRARY_TARGET%.dll" goto end
if exist "%OPEN_DYLAN_USER_BUILD%\%LIBRARY%\dylanmakefile.mkf" goto link
if exist "%OPEN_DYLAN_RELEASE_ROOT%\Build\%LIBRARY%\dylanmakefile.mkf" goto system_link

if "%QUIET%" == "yes" set NMAKE_OPTIONS=/s /nologo
if "%QUIET%" == "no" set NMAKE_OPTIONS=/d /nologo
rem echo nmake %NMAKE_OPTIONS% OPTIONS="%OPTIONS%" PENTIUM_RUNTIME_OPTIONS="%PENTIUM_RUNTIME_OPTIONS%" RUNTIME_OPTIONS="%RUNTIME_OPTIONS%" %LIBRARY% 
nmake %NMAKE_OPTIONS% OPTIONS="%OPTIONS%" PENTIUM_RUNTIME_OPTIONS="%PENTIUM_RUNTIME_OPTIONS%" RUNTIME_OPTIONS="%RUNTIME_OPTIONS%" %LIBRARY% 
if %ERRORLEVEL% NEQ 0 goto generate_error
goto end

:LINK
echo [%LIBRARY% found unlinked]
goto end

:SYSTEM_LINK
echo [%LIBRARY% found unlinked in system area]
goto end

:FOUND_OLD_TARGET
echo [Previous version DLL %OLD_LIBRARY_TARGET% found]
goto end

:GENERATE_ERROR
bogus-command-to-cause-an-error-exit 2>nul

:END
