# todo:
# - check copyrights throughout
# - add splash screen?
# - add warning about nonempty install dir 
# - check there's warning on overwrite
# - uninstaller:
#  - replace rmdir /r with smth more intelligent ///warning messagebox added, still ugly
#  - restore previous file associations


;Functional Developer 2.1 Alpha 4 Install Script
;install script version 0.2
;Written by Denis Mashkevich (oudeis)
;Date: 2004/05/30


!include "path.nsh"

;--------------------------------
;Define application defines
!define APPNAME "Functional Developer" ;Define your own software name here
!define APPNAMEANDVERSION "${APPNAME} 2.1" ;Define your own software version here

;;-------------------------------------
;helper defines



;--------------------------------
;Configuration
!include "MUI.nsh"

!define MUI_ABORTWARNING
!define MUI_FINISHPAGE_RUN "$INSTDIR\bin\with-splash-screen.exe"
!define MUI_FINISHPAGE_RUN_PARAMETERS "/k 2.1 /v $\"Version 2.1$\" /e $\"Internal Edition$\" win32-environment.exe" 
!define MUI_FINISHPAGE_RUN_CHECKED

;Check this out
#!define MUI_HEADERIMAGE
#!define MUI_HEADERIMAGE_BITMAP "???.bmp"

;;;; Install properties
!insertmacro MUI_PAGE_WELCOME
!insertmacro MUI_PAGE_LICENSE "src/License.txt"
!insertmacro MUI_PAGE_COMPONENTS
!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_INSTFILES
!insertmacro MUI_PAGE_FINISH

;;;; Uninstall properties
!insertmacro MUI_UNPAGE_CONFIRM
!insertmacro MUI_UNPAGE_INSTFILES

;;;; Language
!insertmacro MUI_LANGUAGE "English"
!insertmacro MUI_RESERVEFILE_LANGDLL

  
;;;; General Settings

OutFile "fundev-win32-20041002.exe"
Name "${APPNAMEANDVERSION}"
InstallDir "$PROGRAMFILES\Functional Objects\${APPNAME}"
InstallDirRegKey HKEY_LOCAL_MACHINE "SOFTWARE\Functional Objects\${APPNAME}\Install" "Install_Dir"
BrandingText "Functional Developer - www.functionalobjects.com, www.gwydiondylan.org"
LicenseData "src/License.txt"
ShowInstDetails show
	
	
;;;; Install Types
InstType Typical
InstType Full


;;;; Installer Sections

Section "${APPNAME} Core" SecFundevCore

  SectionIn 1 2 RO
  SetOutPath "$INSTDIR\"
	File /r "src\*.*"


  WriteRegStr HKEY_LOCAL_MACHINE "Software\Functional Objects\${APPNAME}\2.0" "Library-Packs" "0xffff"
  WriteRegStr HKEY_LOCAL_MACHINE "Software\Functional Objects\${APPNAME}\2.0" "Console-Tools" "Yes"

  WriteRegStr HKEY_LOCAL_MACHINE "Software\Functional Objects\${APPNAME}\2.1" "Library-Packs" "0xffff"
  WriteRegStr HKEY_LOCAL_MACHINE "Software\Functional Objects\${APPNAME}\2.1" "Console-Tools" "Yes"

  WriteRegStr HKEY_LOCAL_MACHINE "Software\Functional Objects\${APPNAME}\License" "User" "FunDev Hacker"
  WriteRegStr HKEY_LOCAL_MACHINE "Software\Functional Objects\${APPNAME}\License" "Expiration" "0000"
  WriteRegStr HKEY_LOCAL_MACHINE "Software\Functional Objects\${APPNAME}\License" "Serial" "FDTNG-0200"
  WriteRegStr HKEY_LOCAL_MACHINE "Software\Functional Objects\${APPNAME}\License" "Data" "74c1e46d0134432c7e8f17e2c38897c367f082a6"


		
  ;Write the installation path into the registry
  WriteRegStr HKLM "Software\Functional Objects\${APPNAME}\Install" "Install_Dir" "$INSTDIR"
	
  ;Write the uninstall keys for Windows
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\FunDev" "DisplayName" "${APPNAMEANDVERSION} (remove only)"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\FunDev" "UninstallString" '"$INSTDIR\uninstall.exe"'
	
  WriteUninstaller "uninstall.exe"

SectionEnd

  
Section "Associate .ddb files" SecAssocDDB
  
	SectionIn 1 2
	
  WriteRegStr HKEY_CLASSES_ROOT ".ddb" "" "Developer.Database.File"
  WriteRegStr HKEY_CLASSES_ROOT "Developer.Database.File" "" "Functional Developer Compiler Database"
  WriteRegStr HKEY_CLASSES_ROOT "Developer.Database.File\DefaultIcon" "" "$INSTDIR\\bin\\win32-environment.exe,3"

	WriteRegStr HKLM "Software\Functional Objects\${APPNAME}\Install" ".ddb associated" "Yes"
	
SectionEnd

Section "Associate .hdp files" SecAssocHDP
  
	SectionIn 1 2
	
  WriteRegStr HKEY_CLASSES_ROOT ".hdp" "" "Developer.Project.File"
  WriteRegStr HKEY_CLASSES_ROOT "Developer.Project.File" "" "Functional Developer Project"
  WriteRegStr HKEY_CLASSES_ROOT "Developer.Project.File\DefaultIcon" "" "$INSTDIR\\bin\\win32-environment.exe,2"
  WriteRegStr HKEY_CLASSES_ROOT "Developer.Project.File\shell\open\command" "" "$INSTDIR\\bin\\with-splash-screen.exe /k 2.1 /v $\"Version 2.1$\" /e $\"Internal Edition$\" win32-environment.exe $\"%1$\""
  WriteRegStr HKEY_CLASSES_ROOT "Developer.Project.File\shell\open\ddeexec" "" "[OpenFile($\"%1$\")]"
  WriteRegStr HKEY_CLASSES_ROOT "Developer.Project.File\shell\open\ddeexec\Application" "" "FunctionalDeveloper"
  WriteRegStr HKEY_CLASSES_ROOT "Developer.Project.File\shell\open\ddeexec\ifexec" "" "[]"
  WriteRegStr HKEY_CLASSES_ROOT "Developer.Project.File\shell\open\ddeexec\Topic" "" "FunctionalDeveloper"

	WriteRegStr HKLM "Software\Functional Objects\${APPNAME}\Install" ".hdp associated" "Yes"
	
SectionEnd

Section "Associate .spec files" SecAssocSPEC
  
	SectionIn 1 2
	
  WriteRegStr HKEY_CLASSES_ROOT ".spec" "" "Developer.ToolSpec.File"
	WriteRegStr HKEY_CLASSES_ROOT ".spec" "Content Type" "text/plain"
  WriteRegStr HKEY_CLASSES_ROOT "Developer.ToolSpec.File" "" "Functional Developer Tool Specification"
  WriteRegStr HKEY_CLASSES_ROOT "Developer.ToolSpec.File" "AlwaysShowExt" ""
  WriteRegStr HKEY_CLASSES_ROOT "Developer.ToolSpec.File\DefaultIcon" "" "$INSTDIR\\bin\\win32-environment.exe,5"
  WriteRegStr HKEY_CLASSES_ROOT "Developer.ToolSpec.File\shell\open\command" "" "$INSTDIR\\bin\\with-splash-screen.exe /k 2.1 /v $\"Version 2.1$\" /e $\"Internal Edition$\" win32-environment.exe $\"%1$\""
  WriteRegStr HKEY_CLASSES_ROOT "Developer.ToolSpec.File\shell\open\ddeexec" "" "[OpenFile($\"%1$\")]"
  WriteRegStr HKEY_CLASSES_ROOT "Developer.ToolSpec.File\shell\open\ddeexec\Application" "" "FunctionalDeveloper"
  WriteRegStr HKEY_CLASSES_ROOT "Developer.ToolSpec.File\shell\open\ddeexec\ifexec" "" "[]"
  WriteRegStr HKEY_CLASSES_ROOT "Developer.ToolSpec.File\shell\open\ddeexec\Topic" "" "FunctionalDeveloper"

	WriteRegStr HKLM "Software\Functional Objects\${APPNAME}\Install" ".spec associated" "Yes"
	
SectionEnd

Section "Associate .lid files" SecAssocLID
  
	SectionIn 1 2
	
  WriteRegStr HKEY_CLASSES_ROOT ".lid" "" "Dylan.LID.File"
  WriteRegStr HKEY_CLASSES_ROOT ".lid" "Content Type" "text/plain"
  WriteRegStr HKEY_CLASSES_ROOT "Dylan.LID.File" "" "Dylan Library Interchange Description"
  WriteRegStr HKEY_CLASSES_ROOT "Dylan.LID.File" "AlwaysShowExt" ""
  WriteRegStr HKEY_CLASSES_ROOT "Dylan.LID.File\DefaultIcon" "" "$INSTDIR\\bin\\win32-environment.exe,4"
  WriteRegStr HKEY_CLASSES_ROOT "Dylan.LID.File\shell\open\command" "" "$INSTDIR\\bin\\with-splash-screen.exe /k 2.1 /v $\"Version 2.1$\" /e $\"Internal Edition$\" win32-environment.exe $\"%1$\""
  WriteRegStr HKEY_CLASSES_ROOT "Dylan.LID.File\shell\open\ddeexec" "" "[OpenFile($\"%1$\")]"
  WriteRegStr HKEY_CLASSES_ROOT "Dylan.LID.File\shell\open\ddeexec\Application" "" "FunctionalDeveloper"
  WriteRegStr HKEY_CLASSES_ROOT "Dylan.LID.File\shell\open\ddeexec\ifexec" "" "[]"
  WriteRegStr HKEY_CLASSES_ROOT "Dylan.LID.File\shell\open\ddeexec\Topic" "" "FunctionalDeveloper"

	WriteRegStr HKLM "Software\Functional Objects\${APPNAME}\Install" ".lid associated" "Yes"
	
SectionEnd

Section "Associate .dyl, .dylan files" SecAssocDYLAN
  
	SectionIn 1 2
	
  WriteRegStr HKEY_CLASSES_ROOT ".dyl" "" "Dylan.Source.File" 
 	WriteRegStr HKEY_CLASSES_ROOT ".dyl" "Content Type" "text/plain"
  WriteRegStr HKEY_CLASSES_ROOT ".dylan" "" "Dylan.Source.File"
	WriteRegStr HKEY_CLASSES_ROOT ".dylan" "Content Type" "text/plain"
  WriteRegStr HKEY_CLASSES_ROOT "Dylan.Source.File" "" "Dylan Source File"
  WriteRegStr HKEY_CLASSES_ROOT "Dylan.Source.File" "AlwaysShowExt" ""
  WriteRegStr HKEY_CLASSES_ROOT "Dylan.Source.File\DefaultIcon" "" "$INSTDIR\\bin\\win32-environment.exe,1"
  WriteRegStr HKEY_CLASSES_ROOT "Dylan.Source.File\shell\open\command" "" "$INSTDIR\\bin\\with-splash-screen.exe /k 2.1 /v $\"Version 2.1$\" /e $\"Internal Edition$\" win32-environment.exe $\"%1$\""
  WriteRegStr HKEY_CLASSES_ROOT "Dylan.Source.File\shell\open\ddeexec" "" "[OpenFile($\"%1$\")]"
  WriteRegStr HKEY_CLASSES_ROOT "Dylan.Source.File\shell\open\ddeexec\Application" "" "FunctionalDeveloper"
  WriteRegStr HKEY_CLASSES_ROOT "Dylan.Source.File\shell\open\ddeexec\ifexec" "" "[]"
  WriteRegStr HKEY_CLASSES_ROOT "Dylan.Source.File\shell\open\ddeexec\Topic" "" "FunctionalDeveloper"

	WriteRegStr HKLM "Software\Functional Objects\${APPNAME}\Install" ".dylan associated" "Yes"
	
SectionEnd

Section "Redistributable folder" SecRedistributable
	
  SectionIn 1 2
  SetOutPath "$INSTDIR\Redistributable\"

  File "src\bin\dxbigint.dll"
  File "src\bin\dxcffi.dll" 
  File "src\bin\dxchnnls.dll" 
  File "src\bin\dxcmndyl.dll" 
  File "src\bin\dxcnsc.dll" 
  File "src\bin\dxcollns.dll" 
  File "src\bin\dxcom.dll" 
  File "src\bin\dxcommnd.dll" 
  File "src\bin\dxdb.dll" 
  File "src\bin\dxdeuce.dll" 
  File "src\bin\dxdmdce.dll" 
  File "src\bin\dxdocon.dll" 
  File "src\bin\dxdolec.dll" 
  File "src\bin\dxdoles.dll" 
  File "src\bin\dxdood.dll" 
  File "src\bin\dxduim.dll" 
  File "src\bin\dxdylan.dll" 
  File "src\bin\dxfundyl.dll" 
  File "src\bin\dxgarith.dll" 
  File "src\bin\dxguitst.dll" 
  File "src\bin\dxhtmhlp.dll" 
  File "src\bin\dxio.dll" 
  File "src\bin\dxmidi.dll" 
  File "src\bin\dxnetwrk.dll" 
  File "src\bin\dxole.dll" 
  File "src\bin\dxoleaut.dll"
  File "src\bin\dxolecfr.dll"
  File "src\bin\dxolecon.dll"
  File "src\bin\dxolectl.dll"
  File "src\bin\dxoledlg.dll"
  File "src\bin\dxolesvr.dll"
  File "src\bin\dxorb.dll" 
  File "src\bin\dxsystem.dll"
  File "src\bin\dxtstspc.dll"
  File "src\bin\dxtstwks.dll"
  File "src\bin\dxw32cmn.dll"
  File "src\bin\dxw32ctl.dll"
  File "src\bin\dxw32dde.dll"
  File "src\bin\dxw32dlg.dll"
  File "src\bin\dxw32gdi.dll"
  File "src\bin\dxw32gl.dll" 
  File "src\bin\dxw32glu.dll"
  File "src\bin\dxw32knl.dll"
  File "src\bin\dxw32mm.dll" 
  File "src\bin\dxw32red.dll"
  File "src\bin\dxw32reg.dll"
  File "src\bin\dxw32res.dll"
  File "src\bin\dxw32shl.dll"
  File "src\bin\dxw32usr.dll"
  File "src\bin\dxw32ver.dll"
  File "src\bin\dxwduim.dll" 

	WriteRegStr HKLM "Software\Functional Objects\${APPNAME}\Install" "Redistributable folder created" "Yes"
	
SectionEnd

Section "Modify path" SecModifyPath
	SectionIn 1 2
	
  Push "$INSTDIR\\bin"
  Call AddToPath
	
	WriteRegStr HKLM "Software\Functional Objects\${APPNAME}\Install" "Path modified" "Yes"
	
SectionEnd


Section "Start Menu Shortcuts" SecStartMenuShortcuts
	
  SectionIn 1 2
  
	CreateDirectory "$SMPROGRAMS\Functional Objects\Functional Developer"
  CreateShortCut "$SMPROGRAMS\Functional Objects\Functional Developer\Uninstall.lnk" "$INSTDIR\uninstall.exe" "" "$INSTDIR\uninstall.exe" 0
  Delete "$SMPROGRAMS\Functional Objects\Functional Developer\${APPNAMEANDVERSION}.lnk" ; Delete older link if exists
  CreateShortCut "$SMPROGRAMS\Functional Objects\Functional Developer\${APPNAMEANDVERSION}.lnk" "$\"$INSTDIR\bin\with-splash-screen.exe$\"" "/k 2.1 /v $\"Version 2.1$\"  /e $\"Internal Edition$\"  win32-environment.exe" "$INSTDIR\bin\win32-environment.exe" 0

SectionEnd


Section "Desktop Shortcut" SecDesktopShortcut
  
  SectionIn 1 2
  ; For past users, cleanup previous icon if still on desktop
  Delete "$DESKTOP\${APPNAMEANDVERSION}.lnk"
  CreateShortCut "$DESKTOP\${APPNAMEANDVERSION}.lnk" "$\"$INSTDIR\bin\with-splash-screen.exe$\"" "/k 2.1 /v $\"Version 2.1$\"  /e $\"Internal Edition$\"  win32-environment.exe" "$INSTDIR\bin\win32-environment.exe" 0

SectionEnd


Section "Quick Launch Shortcut" SecQuickLaunchShortcut
  
  SectionIn 2
	Delete "$QUICKLAUNCH\${APPNAMEANDVERSION}.lnk"
  CreateShortCut "$QUICKLAUNCH\${APPNAMEANDVERSION}.lnk" "$\"$INSTDIR\bin\with-splash-screen.exe$\"" "/k 2.1 /v $\"Version 2.1$\"  /e $\"Internal Edition$\"  win32-environment.exe" "$INSTDIR\bin\win32-environment.exe" 0

SectionEnd


;;;; Component Section Descriptions
!insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
  !insertmacro MUI_DESCRIPTION_TEXT ${SecFundevCore} "${APPNAME} core files (required)"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecRedistributable} "Copy redistributable files to separate folder. It is possible to run make-redistributable.bat later to do this."
	!insertmacro MUI_DESCRIPTION_TEXT ${SecModifyPath} "Add $INSTDIR to path."
	!insertmacro MUI_DESCRIPTION_TEXT ${SecAssocDDB} "Associate .ddb files (Functional Developer Compiler Database) with ${APPNAME}."
	!insertmacro MUI_DESCRIPTION_TEXT ${SecAssocHDP} "Associate .hdp files (Functional Developer Project) with ${APPNAME}."
	!insertmacro MUI_DESCRIPTION_TEXT ${SecAssocSPEC} "Associate .spec files (Functional Developer Tool Specification) with ${APPNAME}."
	!insertmacro MUI_DESCRIPTION_TEXT ${SecAssocLID} "Associate .lid files (Dylan Library Interchange Description) with ${APPNAME}."
	!insertmacro MUI_DESCRIPTION_TEXT ${SecAssocDYLAN} "Associate .dyl and .dylan files (Dylan Source File) with ${APPNAME}."
	!insertmacro MUI_DESCRIPTION_TEXT ${SecStartMenuShortcuts} "Create Start Menu shortcuts"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecDesktopShortcut} "Create Desktop shortcut"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecQuickLaunchShortcut} "Create Quick Launch shortcut"
!insertmacro MUI_FUNCTION_DESCRIPTION_END
 

;;;; Uninstaller Section
Section "Uninstall"

  MessageBox MB_YESNO|MB_ICONEXCLAMATION "Warning: If you continue, the whole installation directory ($INSTDIR) will be deleted.$\nIf you have made any changes to the contents of the directory that you would like to preserve, please backup them before proceeding.$\nAre you sure you want to continue?" IDNO cancel
  RMDir /r "$INSTDIR" 

  ; Delete start menu stuff
  RMDir /r "$SMPROGRAMS\Functional Objects"

  ; Delete desktop icons...
  Delete "$DESKTOP\${APPNAMEANDVERSION}.lnk"
	
  ; Delete quicklaunch icon
  Delete "$QUICKLAUNCH\${APPNAMEANDVERSION}.lnk"
	
	
  ;;; registry stuff
  DeleteRegKey HKEY_CLASSES_ROOT ".ddb" 
 
  DeleteRegKey HKEY_CLASSES_ROOT ".dyl" 

  DeleteRegKey HKEY_CLASSES_ROOT ".dylan" 
  
  DeleteRegKey HKEY_CLASSES_ROOT ".hdp" 

  DeleteRegKey HKEY_CLASSES_ROOT ".lid" 
  
  DeleteRegKey HKEY_CLASSES_ROOT ".spec" 
  
  DeleteRegKey HKEY_CLASSES_ROOT "Developer.Database.File" 

  DeleteRegKey HKEY_CLASSES_ROOT "Developer.Project.File" 
  
  DeleteRegKey HKEY_CLASSES_ROOT "Developer.ToolSpec.File" 
  
  DeleteRegKey HKEY_CLASSES_ROOT "Dylan.LID.File" 
  
  DeleteRegKey HKEY_CLASSES_ROOT "Dylan.Source.File" 

  DeleteRegKey HKEY_LOCAL_MACHINE "Software\Functional Objects\Functional Developer" 
  
  ;Delete the uninstall keys for Windows
  DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\FunDev" 
  
  ;Remove bin dir from path
	Push "$INSTDIR\\bin"
  Call un.RemoveFromPath
	
  cancel:
SectionEnd

;;;; eof