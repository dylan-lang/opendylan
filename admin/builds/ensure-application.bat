@echo off

set OPTIONS=
set NMAKE_OPTIONS=/s /nologo

:PROCESS_OPTIONS
if "%2%" == "" goto finish_options
set OPTIONS=%OPTIONS% %1%
if "%1%" == "/verbose" set NMAKE_OPTIONS=/d /nologo
if "%1%" == "-verbose" set NMAKE_OPTIONS=/d /nologo
shift
goto process_options

:FINISH_OPTIONS
set LIBRARY=%1%

:ENSURE_APPLICATION
if exist "%FUNCTIONAL_DEVELOPER_RELEASE_INSTALL%\bin\%LIBRARY%.exe" goto end
if exist "%FUNCTIONAL_DEVELOPER_USER_INSTALL%\bin\%LIBRARY%.exe" goto end
if exist "%FUNCTIONAL_DEVELOPER_USER_BUILD%\%LIBRARY%\dylanmakefile.mkf" goto link
nmake %NMAKE_OPTIONS% OPTIONS="%OPTIONS%" %LIBRARY%
if %ERRORLEVEL% NEQ 0 goto generateerror
goto end

:LINK
build-library %OPTIONS% %LIBRARY%
if %ERRORLEVEL% NEQ 0 goto generateerror
goto end

:GENERATEERROR
bogus-command-to-cause-an-error-exit 2>nul

:END
