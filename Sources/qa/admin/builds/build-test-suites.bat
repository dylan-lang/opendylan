@echo off
mkdir %WEBSTER_PERSONAL_ROOT%
pushd %WEBSTER_PERSONAL_ROOT%
call hope checkout -rec -ci-date -stale-unit-files delete -missing-dir force -writable-files skip -compound D-qa %1 %2 %3 %4 %5 %6 %7 %8 %9
call nmake /f %WEBSTER_PERSONAL_ROOT%\qa\admin\builds\Makefile test-suites


