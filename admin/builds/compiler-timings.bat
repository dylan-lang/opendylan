@echo off

rem usage: compiler-timings directory-where-logs-live

set DIR=%1%
shift

perl compiler-timings.pl -c %DIR%\compile-*.log

