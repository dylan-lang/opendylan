@echo off
rem hope-checkout.bat        gts, 11/97
rem writes hope-checkout-continue.bat
call perl check-for-remote-checkout-dir.pl %*
hope-checkout-continue.bat
