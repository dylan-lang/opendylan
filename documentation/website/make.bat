@ECHO OFF

REM Command file for Sphinx documentation

if "%SPHINXBUILD%" == "" (
	set SPHINXBUILD=sphinx-build
)
set BUILDDIR=build
set ALLSPHINXOPTS=-d %BUILDDIR%/doctrees %SPHINXOPTS% source
set I18NSPHINXOPTS=%SPHINXOPTS% source

if "%1" == "" goto help

if "%1" == "help" (
	:help
	echo.Please use `make ^<target^>` where ^<target^> is one of
	echo.  html       to make standalone HTML files
	echo.  gettext    to make PO message catalogs
	echo.  changes    to make an overview over all changed/added/deprecated items
	echo.  linkcheck  to check all external links for integrity
	goto end
)

if "%1" == "clean" (
	for /d %%i in (%BUILDDIR%\*) do rmdir /q /s %%i
	del /q /s %BUILDDIR%\*
	goto end
)

if "%1" == "html" (
	%SPHINXBUILD% -b html %ALLSPHINXOPTS% %BUILDDIR%/html
	if errorlevel 1 exit /b 1
	echo.
	echo.Build finished. The HTML pages are in %BUILDDIR%/html.
	goto end
)

if "%1" == "gettext" (
	%SPHINXBUILD% -b gettext %I18NSPHINXOPTS% %BUILDDIR%/locale
	if errorlevel 1 exit /b 1
	echo.
	echo.Build finished. The message catalogs are in %BUILDDIR%/locale.
	goto end
)

if "%1" == "changes" (
	%SPHINXBUILD% -b changes %ALLSPHINXOPTS% %BUILDDIR%/changes
	if errorlevel 1 exit /b 1
	echo.
	echo.The overview file is in %BUILDDIR%/changes.
	goto end
)

if "%1" == "linkcheck" (
	%SPHINXBUILD% -b linkcheck %ALLSPHINXOPTS% %BUILDDIR%/linkcheck
	if errorlevel 1 exit /b 1
	echo.
	echo.Link check complete; look for any errors in the above output ^
or in %BUILDDIR%/linkcheck/output.txt.
	goto end
)

:end
