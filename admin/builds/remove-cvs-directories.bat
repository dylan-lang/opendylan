@echo off

rmdir /S /Q %1\CVS 2>nul
for /d %%I in (%1\*) do call %~f0 %%~sfI

echo. >nul
