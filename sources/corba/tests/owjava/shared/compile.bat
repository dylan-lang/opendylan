@echo off

IF "%1"=="help" GOTO HELP
IF "%1"=="build" GOTO BUILD
IF "%1"=="clean" GOTO CLEAN

:BUILD
	echo Compiling IDL
	idl -N -D ORBIXWEB -jP shared -jO .. ..\..\idl\corba-tests.idl

	echo Installing patches for generated code
	cd patches
	call install.bat
	cd ..

	echo Compiling generated code
	owjavac *.java TreePackage\*.java TreeUPackage\*.java AnyTestPackage\*.java ArrayTestPackage\*.java bankPackage\*.java SequenceTestPackage\*.java PseudoObjectsTestPackage\*.java UnionTestPackage\*.java RLE_entity_4Package\*.java
	GOTO END

:HELP
        echo.
	echo Generates and compiles java code from the CORBA test suite IDL source
	echo.
	echo COMPILE [build] [clean] [help]
	echo.
	echo    build      Generate and compile the java source files.
	echo    clean      Removes all generated files.
	echo    help       Display these instructions.	
	echo.
	GOTO END

:CLEAN
	rd /S/Q accountPackage
	rd /S/Q AnyTestPackage
	rd /S/Q ArrayTestPackage
	rd /S/Q bankPackage
	rd /S/Q CallBackPackage
	rd /S/Q ChatPackage
	rd /S/Q currentAccountPackage
	rd /S/Q EnumTestPackage
	rd /S/Q gridPackage
	rd /S/Q M1
	rd /S/Q M2
	rd /S/Q M4
	rd /S/Q PseudoObjectsTestPackage
	rd /S/Q RLE_entity_4Package
	rd /S/Q SequenceTestPackage
	rd /S/Q StructTestPackage
	rd /S/Q TestObjectAPackage
	rd /S/Q TestObjectBPackage
	rd /S/Q TestObjectCPackage
	rd /S/Q TestObjectDPackage
	rd /S/Q TestObjectPackage
	rd /S/Q TestObjectXPackage
	rd /S/Q TreePackage
	rd /S/Q TreeTestPackage
	rd /S/Q TreeUPackage
	rd /S/Q UnionTestPackage
	del *.java

:END
