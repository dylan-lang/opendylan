@echo off

perl find-warnings.pl -vn 1 -e "at a rate of" %FUNCTIONAL_DEVELOPER_BUILD_LOGS%\compile-*.log
