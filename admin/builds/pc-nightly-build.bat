@rem overnight Hope checkout and trunk build
@rem   -- by Doug Dodds
@rem   -- generous assistance from Andy Armstrong

@rem -- loop waiting for "go" signal to appear
@rem -- done this way to allow the main job to run
@rem -- truly under the logged-in user's settings

cd /d D:\Users\Dylan\admin\builds

@rem -- Arguments:
@rem --  %1 -- Date to start.  Values:
@rem --      ""  Use default time; presently 23:30
@rem --      <time of day>  a future time a which to start
@rem --      "go"  start now, without delay
@rem --  %2 -- "NoInit", whether to initialize by deleting all of the
@rem --                  build area and doing a source checkout.  Values:
@rem --      ""  Default.  Do normal checkout and build-area initialization.
@rem --      "I"  Do initialization but no checkout,
@rem --      "T"  Do no checkout and no initialization.  Use this to
@rem --           proceed with a failed build, after making some fix.
@rem --  %3 -- Mail recipient, a string that is a mail alias.
@rem --        Default is "dylan-pc-builds"
@rem -- 

set DATE=%1
if "%DATE%"=="" set DATE=23:30

set NOINIT=%2

set MAILRECIP=%3
if "%MAILRECIP%"=="" set MAILRECIP="dylan-pc-builds"

set LAST_BUILD_FINISHED=D:\Users\Dylan\Trunk\logs\last-build-finished
set SYNCSTOP=D:\Users\Dylan\admin\logs\.build_working
set GO=D:\Users\Dylan\admin\logs\pc-nightly-build.go
if exist %GO% del %GO%
if "%DATE%"=="go" goto go
at %DATE% cmd /c echo ">" %GO%

:WAITFORGO
if exist %GO% goto go
sleep 60
echo off
goto waitforgo

:GO
echo on

if "%NOINIT%"=="T" goto build
if "%NOINIT%"=="I" goto buildinit
goto checkout

:CHECKOUT
@rem -- first make sure there is no roadblock file
@rem --
if exist %SYNCSTOP% del %SYNCSTOP%

@rem -- and do the checkout
@rem --
call D:\Users\Dylan\admin\builds\pc-nightly-sync.bat

if %ERRORLEVEL% NEQ 0 goto generate_error

:BUILDINIT
cd \Users\Dylan\Trunk

@rem -- save aside the final-generation logs from the last build
@rem -- but only if the last build ran to completion
@rem --
if not exist %LAST_BUILD_FINISHED% goto logs_done
rmdir /s /q previous\logs
mkdir previous\logs
copy logs\* previous\logs\*
:LOGS_DONE

@rem -- remove all the products of previous builds
@rem --
rmdir /s /q bin Build Bootstrap.1 Bootstrap.2 Bootstrap.3 databases Examples lib logs QA Redistributable Templates
rmdir sources\bootstrap1-registry

:BUILD
@rem -- create interlock file so another checkout doesn't trip us up
@rem --
echo > %SYNCSTOP%

@rem -- now do the build
@rem --
cd \Users\Dylan\admin\builds

@rem -- 
call D:\Users\Dylan\admin\builds\build-release.bat D:\Users\Dylan\Trunk ^
  /sources D:\Users\Dylan\Trunk\sources /generations 4 ^
  /target releases /rm-early-builds /warnings ^
  /compare-timings /timings-root D:\Users\Dylan\Trunk\previous ^
  /show-failure-log /debug-failure ^
  > D:\Users\Dylan\admin\logs\nightly-build.log

set ERRLV=%ERRORLEVEL%

:SEND_MAIL
set SM=%HOMEDRIVE%%HOMEPATH%

echo Build on builder ending > %TEMP%\build-junk
date /t >> %TEMP%\build-junk
echo   -  -  -  >> %TEMP%\build-junk
echo ---------- build output (all of it; sorry) ---------- >> %TEMP%\build-junk
type D:\Users\Dylan\admin\logs\nightly-build.log >> %TEMP%\build-junk

mail -s "build on builder" %MAILRECIP% < %TEMP%\build-junk
del %TEMP%\build-junk

@rem -- if success, create marker file
@rem --
if %ERRLV% NEQ 0 goto build_error
echo > %LAST_BUILD_FINISHED%

@rem ---*** TEMPORARY: (SQL-ODBC test crashes with an error that's not trapped
@rem ---*** TEMPORARY:  by TestWorks, resulting in a dialog box that must be
@rem ---*** TEMPORARY:  manually dismissed which stops the build dead.)
goto skip_TEST

@rem -- If the build finished successfully, test it
@rem --
call D:\Users\Dylan\admin\builds\build-release.bat D:\Users\Dylan\Trunk ^
  /sources D:\Users\Dylan\Trunk\sources /generations 3 ^
  /target releases /rm-early-builds  ^
  /test /show-failure-log /debug-failure ^
  > D:\Users\Dylan\admin\logs\nightly-test.log

set ERRLV=%ERRORLEVEL%

:SEND_MAIL_2
echo Tests on builder ending > %TEMP%\build-junk
date /t >> %TEMP%\build-junk
echo   -  -  -  >> %TEMP%\build-junk
echo ---------- test output (all of it; sorry) ---------- >> %TEMP%\build-junk
type D:\Users\Dylan\admin\logs\nightly-test.log >> %TEMP%\build-junk

mail -s "testing on builder" %MAILRECIP% < %TEMP%\build-junk
del %TEMP%\build-junk

@rem ---*** TEMPORARY
:skip_TEST

@rem -- remove interlock
@rem --
del %SYNCSTOP%

if %ERRLV% NEQ 0 goto build_error
goto end

:BUILD_ERROR
echo Nightly build failed at:
call date /t
call time /t
goto generate_error

:GENERATE_ERROR
if exist %SYNCSTOP% del %SYNCSTOP%
bogus-command-to-cause-an-error-exit 2>nul

:END
