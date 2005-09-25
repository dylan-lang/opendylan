# todo:
# - check copyrights throughout
# - add splash screen?
# - add warning about nonempty install dir 
# - check there's warning on overwrite
# - uninstaller:
#  - replace rmdir /r with smth more intelligent ///warning messagebox added, still ugly
#  - restore previous file associations

;Open Dylan 1.0 Beta 1 Install Script
;Originally written by Denis Mashkevich (oudeis)
;Changes by Matthias Hölzl (tc)

!include "path.nsh"

;--------------------------------
;Application defines
!define APPNAME "Open Dylan" ;Define your own software name here
!define APPNAMEANDVERSION "${APPNAME} 1.0 Beta 1" ;Define your own software version here

;-------------------------------------
;helper defines
!define REGISTRY_KEY "Software\${APPNAME}"

!ifndef OUTFILE
!define OUTFILE "opendylan-win32.exe"
!endif

;--------------------------------
;Configuration
!include "MUI.nsh"

!define MUI_ABORTWARNING
!define MUI_FINISHPAGE_RUN "$INSTDIR\bin\with-splash-screen.exe"
!define MUI_FINISHPAGE_RUN_PARAMETERS "/k 1.0 /v $\"Version 1.0 Beta 1$\" /e $\"Internal Edition$\" win32-environment.exe"
!define MUI_FINISHPAGE_RUN_CHECKED

!define MUI_HEADERIMAGE
!define MUI_HEADERIMAGE_BITMAP "header.bmp"
!define MUI_WELCOMEFINISHPAGE_BITMAP "welcome.bmp"

;;;; Install properties
!insertmacro MUI_PAGE_WELCOME
!insertmacro MUI_PAGE_LICENSE "License.txt"
!insertmacro MUI_PAGE_COMPONENTS
!insertmacro MUI_PAGE_DIRECTORY
Page custom ChooseBuildScript
!insertmacro MUI_PAGE_INSTFILES
!insertmacro MUI_PAGE_FINISH

;;;; Uninstall properties
!insertmacro MUI_UNPAGE_CONFIRM
!insertmacro MUI_UNPAGE_INSTFILES

;;;; Language
!insertmacro MUI_LANGUAGE "English"
!insertmacro MUI_RESERVEFILE_LANGDLL

ReserveFile "choose-build-script.ini"
!insertmacro MUI_RESERVEFILE_INSTALLOPTIONS

;;;; Install functions

Function .onInit
  !insertmacro MUI_INSTALLOPTIONS_EXTRACT "choose-build-script.ini"
FunctionEnd

Function ChooseBuildScript
  !insertmacro MUI_HEADER_TEXT "External Build System" \
         "Choose an external build system for ${APPNAME}.$\n(The build script can be changed after installation.)"
  !insertmacro MUI_INSTALLOPTIONS_DISPLAY "choose-build-script.ini"
FunctionEnd

;;;; General Settings

OutFile "${OUTFILE}"
Name "${APPNAMEANDVERSION}"
InstallDir "$PROGRAMFILES\${APPNAME}"
InstallDirRegKey HKEY_LOCAL_MACHINE "${REGISTRY_KEY}\Install" "Install_Dir"
BrandingText "Open Dylan - www.opendylan.org, www.functionalobjects.com"
LicenseData "License.txt"
ShowInstDetails show

;;;; Install Types
InstType Typical
InstType Full

;;;; Installer Sections

Section "${APPNAME} Core" SecOpendylanCore
  SectionIn 1 2 RO

  SetOutPath "$INSTDIR\"
  File /r D:\r1\bin
  File /r D:\r1\lib
  File /r D:\r1\logs
  File /r D:\r1\databases
  File /r D:\r1\Templates
  File /r D:\r1\Examples
  File /r D:\r1\sources

  CopyFiles "$PROGRAMFILES\Debugging Tools for Windows\dbghelp.dll" "$INSTDIR\bin"

  WriteRegStr HKEY_LOCAL_MACHINE "${REGISTRY_KEY}\1.0" "Library-Packs" "0xffff"
  WriteRegStr HKEY_LOCAL_MACHINE "${REGISTRY_KEY}\1.0" "Console-Tools" "Yes"

  WriteRegStr HKEY_LOCAL_MACHINE "${REGISTRY_KEY}\License" "User" \
              "Opendylan Hacker"
  WriteRegStr HKEY_LOCAL_MACHINE "${REGISTRY_KEY}\License" "Expiration" "0000"
  WriteRegStr HKEY_LOCAL_MACHINE "${REGISTRY_KEY}\License" "Serial" \
              "FDTNG-0200"
  WriteRegStr HKEY_LOCAL_MACHINE "${REGISTRY_KEY}\License" "Data" \
              "74c1e46d0134432c7e8f17e2c38897c367f082a6"

  ;Read the build script selection
  !insertmacro MUI_INSTALLOPTIONS_READ $0 "choose-build-script.ini" \
                                          "Field 2" "State"
  !insertmacro MUI_INSTALLOPTIONS_READ $1 "choose-build-script.ini" \
                                          "Field 3" "State"
  !insertmacro MUI_INSTALLOPTIONS_READ $2 "choose-build-script.ini" \
                                          "Field 4" "State"
  !insertmacro MUI_INSTALLOPTIONS_READ $3 "choose-build-script.ini" \
                                          "Field 5" "State"
  
  ;Display a messagebox if check box was checked
  StrCmp $0 "1" "" +2
    StrCpy $R0 "x86-win32-vc6-build.jam"
  StrCmp $1 "1" "" +2
    StrCpy $R0 "x86-win32-vc7-build.jam"
  StrCmp $2 "1" "" +2
    StrCpy $R0 "x86-win32-vc7-build.jam"
  StrCmp $3 "1" "" +2
    StrCpy $R0 "x86-win32-pellesc-build.jam"

  WriteRegStr HKCU "${REGISTRY_KEY}\1.0\Build-System" "build-script" \
              "$INSTDIR\lib\$R0"
	
  ;Write the uninstall keys for Windows
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Opendylan" "DisplayName" "${APPNAMEANDVERSION} (remove only)"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Opendylan" "UninstallString" '"$INSTDIR\uninstall.exe"'
	
  WriteUninstaller "uninstall.exe"
SectionEnd

  
Section "Associate .ddb files" SecAssocDDB
  SectionIn 1 2
	
  WriteRegStr HKEY_CLASSES_ROOT ".ddb" "" "Developer.Database.File"
  WriteRegStr HKEY_CLASSES_ROOT "Developer.Database.File" "" \
              "Open Dylan Compiler Database"
  WriteRegStr HKEY_CLASSES_ROOT "Developer.Database.File\DefaultIcon" "" \
              "$INSTDIR\\bin\\win32-environment.exe,3"

  WriteRegStr HKLM "${REGISTRY_KEY}\Install" ".ddb associated" "Yes"
SectionEnd

Section "Associate .hdp files" SecAssocHDP
  SectionIn 1 2
	
  WriteRegStr HKEY_CLASSES_ROOT ".hdp" "" "Developer.Project.File"
  WriteRegStr HKEY_CLASSES_ROOT "Developer.Project.File" "" \
              "Open Dylan Project"
  WriteRegStr HKEY_CLASSES_ROOT "Developer.Project.File\DefaultIcon" "" \
              "$INSTDIR\\bin\\win32-environment.exe,2"
  WriteRegStr HKEY_CLASSES_ROOT "Developer.Project.File\shell\open\command" "" "$INSTDIR\\bin\\with-splash-screen.exe /k 1.0 /v $\"Version 1.0 Beta 1$\" /e $\"Internal Edition$\" win32-environment.exe $\"%1$\""
  WriteRegStr HKEY_CLASSES_ROOT "Developer.Project.File\shell\open\ddeexec" "" "[OpenFile($\"%1$\")]"
  WriteRegStr HKEY_CLASSES_ROOT "Developer.Project.File\shell\open\ddeexec\Application" "" "FunctionalDeveloper"
  WriteRegStr HKEY_CLASSES_ROOT "Developer.Project.File\shell\open\ddeexec\ifexec" "" "[]"
  WriteRegStr HKEY_CLASSES_ROOT "Developer.Project.File\shell\open\ddeexec\Topic" "" "FunctionalDeveloper"

  WriteRegStr HKLM "${REGISTRY_KEY}\Install" ".hdp associated" "Yes"
SectionEnd

Section "Associate .spec files" SecAssocSPEC
  SectionIn 1 2
	
  WriteRegStr HKEY_CLASSES_ROOT ".spec" "" "Developer.ToolSpec.File"
  WriteRegStr HKEY_CLASSES_ROOT ".spec" "Content Type" "text/plain"
  WriteRegStr HKEY_CLASSES_ROOT "Developer.ToolSpec.File" "" \
              "Open Dylan Tool Specification"
  WriteRegStr HKEY_CLASSES_ROOT "Developer.ToolSpec.File" "AlwaysShowExt" ""
  WriteRegStr HKEY_CLASSES_ROOT "Developer.ToolSpec.File\DefaultIcon" "" \
              "$INSTDIR\\bin\\win32-environment.exe,5"
  WriteRegStr HKEY_CLASSES_ROOT "Developer.ToolSpec.File\shell\open\command" "" "$INSTDIR\\bin\\with-splash-screen.exe /k 1.0 /v $\"Version 1.0 Beta 1$\" /e $\"Internal Edition$\" win32-environment.exe $\"%1$\""
  WriteRegStr HKEY_CLASSES_ROOT "Developer.ToolSpec.File\shell\open\ddeexec" "" "[OpenFile($\"%1$\")]"
  WriteRegStr HKEY_CLASSES_ROOT "Developer.ToolSpec.File\shell\open\ddeexec\Application" "" "FunctionalDeveloper"
  WriteRegStr HKEY_CLASSES_ROOT "Developer.ToolSpec.File\shell\open\ddeexec\ifexec" "" "[]"
  WriteRegStr HKEY_CLASSES_ROOT "Developer.ToolSpec.File\shell\open\ddeexec\Topic" "" "FunctionalDeveloper"

  WriteRegStr HKLM "${REGISTRY_KEY}\Install" ".spec associated" "Yes"
SectionEnd

Section "Associate .lid files" SecAssocLID
  SectionIn 1 2
	
  WriteRegStr HKEY_CLASSES_ROOT ".lid" "" "Dylan.LID.File"
  WriteRegStr HKEY_CLASSES_ROOT ".lid" "Content Type" "text/plain"
  WriteRegStr HKEY_CLASSES_ROOT "Dylan.LID.File" "" "Dylan Library Interchange Description"
  WriteRegStr HKEY_CLASSES_ROOT "Dylan.LID.File" "AlwaysShowExt" ""
  WriteRegStr HKEY_CLASSES_ROOT "Dylan.LID.File\DefaultIcon" "" "$INSTDIR\\bin\\win32-environment.exe,4"
  WriteRegStr HKEY_CLASSES_ROOT "Dylan.LID.File\shell\open\command" "" "$INSTDIR\\bin\\with-splash-screen.exe /k 1.0 /v $\"Version 1.0 Beta 1$\" /e $\"Internal Edition$\" win32-environment.exe $\"%1$\""
  WriteRegStr HKEY_CLASSES_ROOT "Dylan.LID.File\shell\open\ddeexec" "" "[OpenFile($\"%1$\")]"
  WriteRegStr HKEY_CLASSES_ROOT "Dylan.LID.File\shell\open\ddeexec\Application" "" "FunctionalDeveloper"
  WriteRegStr HKEY_CLASSES_ROOT "Dylan.LID.File\shell\open\ddeexec\ifexec" "" "[]"
  WriteRegStr HKEY_CLASSES_ROOT "Dylan.LID.File\shell\open\ddeexec\Topic" "" "FunctionalDeveloper"

  WriteRegStr HKLM "${REGISTRY_KEY}\Install" ".lid associated" "Yes"
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
  WriteRegStr HKEY_CLASSES_ROOT "Dylan.Source.File\shell\open\command" "" "$INSTDIR\\bin\\with-splash-screen.exe /k 1.0 /v $\"Version 1.0 Beta 1$\" /e $\"Internal Edition$\" win32-environment.exe $\"%1$\""
  WriteRegStr HKEY_CLASSES_ROOT "Dylan.Source.File\shell\open\ddeexec" "" "[OpenFile($\"%1$\")]"
  WriteRegStr HKEY_CLASSES_ROOT "Dylan.Source.File\shell\open\ddeexec\Application" "" "FunctionalDeveloper"
  WriteRegStr HKEY_CLASSES_ROOT "Dylan.Source.File\shell\open\ddeexec\ifexec" "" "[]"
  WriteRegStr HKEY_CLASSES_ROOT "Dylan.Source.File\shell\open\ddeexec\Topic" "" "FunctionalDeveloper"

  WriteRegStr HKLM "${REGISTRY_KEY}\Install" ".dylan associated" "Yes"
SectionEnd

Section "Redistributable folder" SecRedistributable
  SectionIn 1 2
  AddSize 23000
  SetOutPath "$INSTDIR\Redistributable\"

  CopyFiles "$INSTDIR\bin\dxbigint.dll" "$INSTDIR\Redistributable\"
  CopyFiles "$INSTDIR\bin\dxcffi.dll" "$INSTDIR\Redistributable\"
  CopyFiles "$INSTDIR\bin\dxchnnls.dll" "$INSTDIR\Redistributable\"
  CopyFiles "$INSTDIR\bin\dxcmndyl.dll" "$INSTDIR\Redistributable\"
  CopyFiles "$INSTDIR\bin\dxcnsc.dll" "$INSTDIR\Redistributable\"
  CopyFiles "$INSTDIR\bin\dxcollns.dll" "$INSTDIR\Redistributable\"
  CopyFiles "$INSTDIR\bin\dxcom.dll" "$INSTDIR\Redistributable\"
  CopyFiles "$INSTDIR\bin\dxcommnd.dll" "$INSTDIR\Redistributable\"
  CopyFiles "$INSTDIR\bin\dxdb.dll" "$INSTDIR\Redistributable\"
  CopyFiles "$INSTDIR\bin\dxdeuce.dll" "$INSTDIR\Redistributable\"
  CopyFiles "$INSTDIR\bin\dxdmdce.dll" "$INSTDIR\Redistributable\"
  CopyFiles "$INSTDIR\bin\dxdocon.dll" "$INSTDIR\Redistributable\"
  CopyFiles "$INSTDIR\bin\dxdolec.dll" "$INSTDIR\Redistributable\"
  CopyFiles "$INSTDIR\bin\dxdoles.dll" "$INSTDIR\Redistributable\"
  CopyFiles "$INSTDIR\bin\dxdood.dll" "$INSTDIR\Redistributable\"
  CopyFiles "$INSTDIR\bin\dxduim.dll" "$INSTDIR\Redistributable\"
  CopyFiles "$INSTDIR\bin\dxdylan.dll" "$INSTDIR\Redistributable\"
  CopyFiles "$INSTDIR\bin\dxfundyl.dll" "$INSTDIR\Redistributable\"
  CopyFiles "$INSTDIR\bin\dxgarith.dll" "$INSTDIR\Redistributable\"
  CopyFiles "$INSTDIR\bin\dxguitst.dll" "$INSTDIR\Redistributable\"
  CopyFiles "$INSTDIR\bin\dxhtmhlp.dll" "$INSTDIR\Redistributable\"
  CopyFiles "$INSTDIR\bin\dxio.dll" "$INSTDIR\Redistributable\"
  CopyFiles "$INSTDIR\bin\dxmidi.dll" "$INSTDIR\Redistributable\"
  CopyFiles "$INSTDIR\bin\dxnetwrk.dll" "$INSTDIR\Redistributable\"
  CopyFiles "$INSTDIR\bin\dxole.dll" "$INSTDIR\Redistributable\"
  CopyFiles "$INSTDIR\bin\dxoleaut.dll" "$INSTDIR\Redistributable\"
  CopyFiles "$INSTDIR\bin\dxolecfr.dll" "$INSTDIR\Redistributable\"
  CopyFiles "$INSTDIR\bin\dxolecon.dll" "$INSTDIR\Redistributable\"
  CopyFiles "$INSTDIR\bin\dxolectl.dll" "$INSTDIR\Redistributable\"
  CopyFiles "$INSTDIR\bin\dxoledlg.dll" "$INSTDIR\Redistributable\"
  CopyFiles "$INSTDIR\bin\dxolesvr.dll" "$INSTDIR\Redistributable\"
  CopyFiles "$INSTDIR\bin\dxorb.dll" "$INSTDIR\Redistributable\"
  CopyFiles "$INSTDIR\bin\dxsystem.dll" "$INSTDIR\Redistributable\"
  CopyFiles "$INSTDIR\bin\dxtstspc.dll" "$INSTDIR\Redistributable\"
  CopyFiles "$INSTDIR\bin\dxtstwks.dll" "$INSTDIR\Redistributable\"
  CopyFiles "$INSTDIR\bin\dxw32cmn.dll" "$INSTDIR\Redistributable\"
  CopyFiles "$INSTDIR\bin\dxw32ctl.dll" "$INSTDIR\Redistributable\"
  CopyFiles "$INSTDIR\bin\dxw32dde.dll" "$INSTDIR\Redistributable\"
  CopyFiles "$INSTDIR\bin\dxw32dlg.dll" "$INSTDIR\Redistributable\"
  CopyFiles "$INSTDIR\bin\dxw32gdi.dll" "$INSTDIR\Redistributable\"
  CopyFiles "$INSTDIR\bin\dxw32gl.dll"  "$INSTDIR\Redistributable\"
  CopyFiles "$INSTDIR\bin\dxw32glu.dll" "$INSTDIR\Redistributable\"
  CopyFiles "$INSTDIR\bin\dxw32knl.dll" "$INSTDIR\Redistributable\"
  CopyFiles "$INSTDIR\bin\dxw32mm.dll" "$INSTDIR\Redistributable\"
  CopyFiles "$INSTDIR\bin\dxw32red.dll" "$INSTDIR\Redistributable\"
  CopyFiles "$INSTDIR\bin\dxw32reg.dll" "$INSTDIR\Redistributable\"
  CopyFiles "$INSTDIR\bin\dxw32res.dll" "$INSTDIR\Redistributable\"
  CopyFiles "$INSTDIR\bin\dxw32shl.dll" "$INSTDIR\Redistributable\"
  CopyFiles "$INSTDIR\bin\dxw32usr.dll" "$INSTDIR\Redistributable\"
  CopyFiles "$INSTDIR\bin\dxw32ver.dll" "$INSTDIR\Redistributable\"
  CopyFiles "$INSTDIR\bin\dxwduim.dll" "$INSTDIR\Redistributable\"

  WriteRegStr HKLM "${REGISTRY_KEY}\Install" "Redistributable folder created" "Yes"
SectionEnd

Section "Modify path" SecModifyPath
  SectionIn 1 2
	
  Push "$INSTDIR\\bin"
  Call AddToPath
	
  WriteRegStr HKLM "${REGISTRY_KEY}\Install" "Path modified" "Yes"
SectionEnd

Section "Start Menu Shortcuts" SecStartMenuShortcuts
  SectionIn 1 2
  
  CreateDirectory "$SMPROGRAMS\Open Dylan"
  CreateShortCut "$SMPROGRAMS\Open Dylan\Uninstall.lnk" "$INSTDIR\uninstall.exe" "" "$INSTDIR\uninstall.exe" 0
  Delete "$SMPROGRAMS\Open Dylan\${APPNAMEANDVERSION}.lnk" ; Delete older link if exists
  CreateShortCut "$SMPROGRAMS\Open Dylan\${APPNAMEANDVERSION}.lnk" "$\"$INSTDIR\bin\with-splash-screen.exe$\"" "/k 1.0 /v $\"Version 1.0 Beta 1$\"  /e $\"Internal Edition$\"  win32-environment.exe" "$INSTDIR\bin\win32-environment.exe" 0
SectionEnd

Section "Desktop Shortcut" SecDesktopShortcut
  SectionIn 1 2
  ; For past users, cleanup previous icon if still on desktop
  Delete "$DESKTOP\${APPNAMEANDVERSION}.lnk"
  CreateShortCut "$DESKTOP\${APPNAMEANDVERSION}.lnk" "$\"$INSTDIR\bin\with-splash-screen.exe$\"" "/k 1.0 /v $\"Version 1.0 Beta 1$\"  /e $\"Internal Edition$\"  win32-environment.exe" "$INSTDIR\bin\win32-environment.exe" 0
SectionEnd

Section "Quick Launch Shortcut" SecQuickLaunchShortcut
  SectionIn 2

  Delete "$QUICKLAUNCH\${APPNAMEANDVERSION}.lnk"
  CreateShortCut "$QUICKLAUNCH\${APPNAMEANDVERSION}.lnk" "$\"$INSTDIR\bin\with-splash-screen.exe$\"" "/k 1.0 /v $\"Version 1.0 Beta 1$\"  /e $\"Internal Edition$\"  win32-environment.exe" "$INSTDIR\bin\win32-environment.exe" 0
SectionEnd

;;;; Component Section Descriptions
!insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
  !insertmacro MUI_DESCRIPTION_TEXT ${SecOpendylanCore} \
               "${APPNAME} core files (required)"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecRedistributable} \
               "Copy redistributable files to separate folder. It is possible to run make-redistributable.bat later to do this."
  !insertmacro MUI_DESCRIPTION_TEXT ${SecModifyPath} \
               "Add $INSTDIR\bin to path."
  !insertmacro MUI_DESCRIPTION_TEXT ${SecAssocDDB} \
               "Associate .ddb files (Open Dylan Compiler Database) with ${APPNAME}."
  !insertmacro MUI_DESCRIPTION_TEXT ${SecAssocHDP} \
               "Associate .hdp files (Open Dylan Project) with ${APPNAME}."
  !insertmacro MUI_DESCRIPTION_TEXT ${SecAssocSPEC} \
               "Associate .spec files (Open Dylan Tool Specification) with ${APPNAME}."
  !insertmacro MUI_DESCRIPTION_TEXT ${SecAssocLID} \
               "Associate .lid files (Dylan Library Interchange Description) with ${APPNAME}."
  !insertmacro MUI_DESCRIPTION_TEXT ${SecAssocDYLAN} \
               "Associate .dyl and .dylan files (Dylan Source File) with ${APPNAME}."
  !insertmacro MUI_DESCRIPTION_TEXT ${SecStartMenuShortcuts} \
               "Create Start Menu shortcuts"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecDesktopShortcut} \
               "Create Desktop shortcut"
  !insertmacro MUI_DESCRIPTION_TEXT ${SecQuickLaunchShortcut} \
               "Create Quick Launch shortcut"
!insertmacro MUI_FUNCTION_DESCRIPTION_END
 
;;;; Uninstaller Section
Section "Uninstall"

  MessageBox MB_YESNO|MB_ICONEXCLAMATION "Warning: If you continue, the entire installation directory ($INSTDIR) will be deleted.$\nIf you have made any changes to the contents of the directory that you would like to preserve, please back them up before proceeding.$\nAre you sure you want to continue?" IDNO cancel
  RMDir /r "$INSTDIR" 

  ; Delete start menu stuff
  RMDir /r "$SMPROGRAMS"

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

  DeleteRegKey HKEY_LOCAL_MACHINE "Software\Open Dylan" 
  
  ;Delete the uninstall keys for Windows
  DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Opendylan" 
  
  ;Remove bin dir from path
  Push "$INSTDIR\\bin"
  Call un.RemoveFromPath

cancel:
SectionEnd

;;;; eof

