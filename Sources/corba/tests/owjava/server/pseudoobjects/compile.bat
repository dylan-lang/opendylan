@echo off
set PKG_NAME=server.pseudoobjects

IF "%1"=="help" GOTO HELP
IF "%1"=="build" GOTO BUILD
IF "%1"=="clean" GOTO CLEAN
IF "%1"=="commands" GOTO COMMANDS

:BUILD
@echo Building demo source files
owjavac -verbose -echo *.java
if not errorlevel 0 goto END

@echo owjava -echo %PKG_NAME%.Server %%1 > server.bat
GOTO END

:COMMANDS
	echo Run the OrbixWeb server by typing
	echo      [ start server ] 
	GOTO END

:HELP
        echo.
	echo Compiles the pseudoobjects server java source files.
	echo.
	echo COMPILE [build] [clean] [commands] [help]
	echo.
	echo    build      Compiles the java source files.
	echo    clean      Removes all generated files.
	echo    commands   Display instructions on executing the demo.
	echo    help       Display these instructions.	
	echo.
	GOTO END

:CLEAN
	del server.bat 
	GOTO END	

:END
	set PKG_NAME=
