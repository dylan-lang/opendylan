@rem  perpetual overnight Hope checkout and trunk build
@rem   -- by Doug Dodds

@rem -- call pc-nightly-build.bat, then sleep for some hours,
@rem -- then loop to do it again.
@rem -- Will run the nightly build every evening at 23:30 (default hour)
@rem -- if not interrupted.

set GOCODE=%1

:START

cd /d D:\Users\Dylan\admin\builds

set DATE=23:00
set LOOPGO=D:\Users\Dylan\admin\logs\pc-build-loop.go
if exist %LOOPGO% del %LOOPGO%

if "%GOCODE%"=="go" goto go

at %DATE% cmd /c echo ">" %LOOPGO%

:WAITFORGO
if exist %LOOPGO% goto go
sleep 300
echo off
goto waitforgo

:GO
echo on

call D:\Users\Dylan\admin\builds\pc-nightly-build.bat %GOCODE%

@rem -- expected finish ~08:00, so sleep for 10 hours

sleep 36000
set GOCODE=

@rem -- now do it again for the next evening

goto start
