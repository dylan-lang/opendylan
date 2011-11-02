@echo off

perl find-warnings.pl -vn 1 -e "at a rate of" %OPEN_DYLAN_BUILD_LOGS%\compile-*.log
