@echo off

IF "%1"=="help" GOTO HELP
IF "%1"=="build" GOTO BUILD
IF "%1"=="clean" GOTO CLEAN
IF "%1"=="commands" GOTO COMMANDS
IF "%1"=="run" GOTO RUN

:BUILD
cd ..\shared
call compile build
cd ..\server

@echo Building servers
cd any
call compile build
cd ..\array
call compile build
cd ..\bank
call compile build
cd ..\chat
call compile build
cd ..\enum
call compile build
cd ..\grid
call compile build
cd ..\pseudoobjects
call compile build
cd ..\sequence
call compile build
cd ..\struct
call compile build
cd ..\tree
call compile build
cd ..\union
call compile build
cd ..

@echo Building top-level server source files
owjavac -verbose -echo *.java
if not errorlevel 0 goto END

set PKG_NAME=server
@echo owjava -echo %PKG_NAME%.Server %%1 > server.bat
GOTO END

:COMMANDS
	echo Run the OrbixWeb servers by typing
	echo      [compile run] 
        echo Run the Dylan test suite by typing
        echo      [corba-tests-client-app.exe -ignore-suite collocated?]
        GOTO END

:HELP
        echo.
	echo Executes and builds the java servers for the CORBA test suite.
	echo.
	echo COMPILE [build] [clean] [commands] [run] [help]
	echo.
	echo    build      Compiles the java source files.
	echo    clean      Removes all generated files.
	echo    commands   Display instructions on executing the demo.
	echo    run        Starts the servers
	echo    help       Display these instructions.	
	echo.
	GOTO END

:CLEAN
	del server.bat
	cd ..\shared
	call compile clean
	cd ..\server
	cd any
	call compile clean
	cd ..\array
	call compile clean
	cd ..\bank
	call compile clean
	cd ..\chat
	call compile clean
	cd ..\enum
	call compile clean
	cd ..\grid
	call compile clean
	cd ..\pseudoobjects
	call compile clean
	cd ..\sequence
	call compile clean
	cd ..\struct
	call compile clean
	cd ..\tree
	call compile clean
	cd ..\union
	call compile clean
	cd ..
	GOTO END	

:RUN
	server.bat

:END
	set PKG_NAME=
