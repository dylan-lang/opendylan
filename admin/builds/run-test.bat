@echo off

setlocal

set QUIET=yes
set DEBUG=no
set DEBUGGER_OPERATION=

if "%1%"=="/debugger" goto enable_debugger
if "%1%"=="-debugger" goto enable_debugger
goto setup_required_arguments

:ENABLE_DEBUGGER
if "%QUIET%"=="no" echo Enabling debugger!
set DEBUG=yes
set DEBUGGER_OPERATION=under debugger
shift

:SETUP_REQUIRED_ARGUMENTS
set TEST_APP=%1%
set TEST_FULL_NAME=%OPEN_DYLAN_USER_INSTALL%\bin\%TEST_APP%.exe
set TEST_NAME=%2%
set FORMATTED=%3%
shift
shift
shift

set COMMAND_ARGS=

:COLLECT_ARGS
if "%1%"=="" goto got_args
set COMMAND_ARGS=%COMMAND_ARGS% %1%
shift
goto collect_args

:GOT_ARGS
if "%FORMATTED%"=="" set FORMATTED="yes"

if "%TEST_NAME%"=="" goto no_test_name
goto find_test_report

:NO_TEST_NAME
set TEST_NAME=%TEST_APP%
goto find_test_report

:FIND_TESTWORKS_REPORT
call ensure-application testworks-report
set TESTWORKS_REPORT=%OPEN_DYLAN_USER_INSTALL%\bin\testworks-report.exe
if exist "%TESTWORKS_REPORT%" goto setup_variables
set TESTWORKS_REPORT=%DYLAN_RELEASE_ROOT%\bin\testworks-report.exe
if not exist "%TESTWORKS_REPORT%" goto no_test_report

:SETUP_VARIABLES
set LOG_NAME=test-%TEST_NAME%.log
set LOG=%OPEN_DYLAN_BUILD_LOGS%\%LOG_NAME%
set OLD_LOG_DIRECTORY=%OLD_RELEASE_ROOT%\qa\logs
set OLD_LOG=%OLD_LOG_DIRECTORY%\%LOG_NAME%
set REPORT_LOG=%OPEN_DYLAN_BUILD_LOGS%\diff-%TEST_NAME%.log

:FIND_DEBUGGER
set DEBUGGER=
if "%DEBUG%"=="no" goto run_test
set DEBUGGER=%OPEN_DYLAN_RELEASE_ROOT%\bin\batch-debug.exe
if exist "%DEBUGGER%" goto run_test
set DEBUGGER=C:\Program Files\Functional Objects\Functional Developer\bin\batch-debug.exe
if exist "%DEBUGGER%" goto run_test
set DEBUGGER=batch-debug

:RUN_TEST
call :fixup_PATHS "%DEBUGGER%"
if not exist "%TEST_FULL_NAME%" goto no_app

echo Testing %TEST_NAME% %DEBUGGER_OPERATION%
set PATH=%OPEN_DYLAN_RELEASE_ROOT%\Redistributable;%OPEN_DYLAN_RELEASE_ROOT%\Bin;%PATH%
if "%QUIET%"=="no" echo Path=%PATH%
if "%QUIET%"=="no" echo Running: %DEBUGGER% %TEST_FULL_NAME% -progress -report log %COMMAND_ARGS%
%DEBUGGER% %TEST_FULL_NAME% -progress -report log %COMMAND_ARGS% > %LOG%
if %ERRORLEVEL% NEQ 0 goto test_suite_failed

if "%FORMATTED%"=="no" goto output_report

if not exist "%OLD_LOG%" goto log_report

:COMPARE_LOGS
if "%QUIET%"=="no" echo Running: %TESTWORKS_REPORT% /quiet %LOG% %OLD_LOG%
%TESTWORKS_REPORT% /quiet %LOG% %OLD_LOG% > %REPORT_LOG%
if %ERRORLEVEL% NEQ 0 goto test_report_diff_failed
if "%QUIET%"=="no" echo Running: %TESTWORKS_REPORT% %LOG% %OLD_LOG% -report summary
%TESTWORKS_REPORT% %LOG% %OLD_LOG% -report summary
if %ERRORLEVEL% NEQ 0 goto test_report_failed
goto success

:LOG_REPORT
if "%QUIET%"=="no" echo Running: %TESTWORKS_REPORT% /quiet %LOG% -report summary
%TESTWORKS_REPORT% /quiet %LOG% -report summary
if %ERRORLEVEL% NEQ 0 goto test_report_failed
goto success

:OUTPUT_REPORT
type %LOG%
goto success

:SUCCESS
echo.
endlocal
goto end

:fixup_PATHS
if not "%~$PATH:1"=="" set DEBUGGER=%~sf$PATH:1
goto :EOF

:NO_APP
echo Test application %TEST_FULL_NAME% does not exist
goto generate_error

:TEST_SUITE_FAILED
echo Test suite %TEST_APP% failed (status code %ERRORLEVEL%)
echo [see log %LOG%]
goto generate_error

:NO_TESTWORKS_REPORT
echo testworks-report.exe wasn't found
goto generate_error

:TESTWORKS_REPORT_FAILED
echo Test Report failed (status code %ERRORLEVEL%)
echo [see log %REPORT_LOG%]
goto generate_error

:TESTWORKS_REPORT_DIFF_FAILED
echo Test Report difference failed (status code %ERRORLEVEL%)
echo [see log %REPORT_LOG%]
goto generate_error

:GENERATE_ERROR
endlocal
bogus-command-to-cause-an-error-exit 2>nul

:END
