@rem overnight Hope checkout
@rem -- by Doug Dodds

echo off

@rem -- Use the existence of a file as an interlock so running this script
@rem -- by accident doesn't tangle up an ongoing build.  This file is
@rem -- created and removed by pc-nightly-build.bat.
@rem -- 
set SYNCSTOP=d:\Users\Dylan\admin\logs\.build_working
if exist %SYNCSTOP% goto NOCHECKOUT

echo doing hope checkout
echo on

call hope checkout -stale-unit-files delete -missing-dir force ^
 -writable-files warn -modified-files warn ^
 -branch trunk -rec -compound D-admin-builds ^
 -directory d:\Users\Dylan\admin\builds ^
 > d:\Users\Dylan\admin\logs\hope-admin-checkout.log

rem --
echo on

call hope checkout -stale-unit-files delete -missing-dir force ^
 -writable-files force -modified-files force ^
 -branch trunk -rec -compound D-sources ^
 -directory d:\Users\Dylan\Trunk\sources ^
 > d:\Users\Dylan\admin\logs\hope-checkout.log

if %ERRORLEVEL% NEQ 0 goto checkout_error

goto END

:NOCHECKOUT
echo No Hope checkout done because a system build is in progress.
goto END

:CHECKOUT_ERROR
echo Nightly checkout failed at:
call date /t
call time /t
goto generate_error

:GENERATE_ERROR
bogus-command-to-cause-an-error-exit 2>nul

:END
echo on
