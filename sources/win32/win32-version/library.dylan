module:    Dylan-user	
Synopsis:  Win32 API for version management -- "WINVER.H" and "VERSION.DLL"
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/* Automatically generated from "library.src"; do not edit. */


define library Win32-version
  use Dylan;
  use C-FFI;
  use Win32-common;
  export Win32-version;
end;

define module Win32-version
  use Dylan;
  use C-FFI;
  use Win32-common;


  // from "winver.h":
  export $VS-VERSION-INFO, $VS-USER-DEFINED, $VS-FFI-SIGNATURE,
	$VS-FFI-STRUCVERSION, $VS-FFI-FILEFLAGSMASK, $VS-FF-PRERELEASE,
	$VS-FF-PATCHED, $VS-FF-PRIVATEBUILD, $VS-FF-INFOINFERRED,
	$VS-FF-SPECIALBUILD, $VOS-UNKNOWN, $VOS-DOS, $VOS-OS216, $VOS-OS232,
	$VOS-NT, $VOS--BASE, $VOS--WINDOWS16, $VOS--PM16, $VOS--PM32,
	$VOS--WINDOWS32, $VOS-DOS-WINDOWS16, $VOS-DOS-WINDOWS32,
	$VOS-OS216-PM16, $VOS-OS232-PM32, $VOS-NT-WINDOWS32, $VFT-UNKNOWN,
	$VFT-APP, $VFT-DLL, $VFT-DRV, $VFT-FONT, $VFT-VXD, $VFT-STATIC-LIB,
	$VFT2-UNKNOWN, $VFT2-DRV-PRINTER, $VFT2-DRV-KEYBOARD,
	$VFT2-DRV-LANGUAGE, $VFT2-DRV-DISPLAY, $VFT2-DRV-MOUSE,
	$VFT2-DRV-NETWORK, $VFT2-DRV-SYSTEM, $VFT2-DRV-INSTALLABLE,
	$VFT2-DRV-SOUND, $VFT2-DRV-COMM, $VFT2-DRV-INPUTMETHOD,
	$VFT2-FONT-RASTER, $VFT2-FONT-VECTOR, $VFT2-FONT-TRUETYPE,
	$VFFF-ISSHAREDFILE, $VFF-CURNEDEST, $VFF-FILEINUSE,
	$VFF-BUFFTOOSMALL, $VIFF-FORCEINSTALL, $VIFF-DONTDELETEOLD,
	$VIF-TEMPFILE, $VIF-MISMATCH, $VIF-SRCOLD, $VIF-DIFFLANG,
	$VIF-DIFFCODEPG, $VIF-DIFFTYPE, $VIF-WRITEPROT, $VIF-FILEINUSE,
	$VIF-OUTOFSPACE, $VIF-ACCESSVIOLATION, $VIF-SHARINGVIOLATION,
	$VIF-CANNOTCREATE, $VIF-CANNOTDELETE, $VIF-CANNOTRENAME,
	$VIF-CANNOTDELETECUR, $VIF-OUTOFMEMORY, $VIF-CANNOTREADSRC,
	$VIF-CANNOTREADDST, $VIF-BUFFTOOSMALL;
  export dwSignature-value, dwSignature-value-setter,
	dwStrucVersion-value, dwStrucVersion-value-setter,
	dwFileVersionMS-value, dwFileVersionMS-value-setter,
	dwFileVersionLS-value, dwFileVersionLS-value-setter,
	dwProductVersionMS-value, dwProductVersionMS-value-setter,
	dwProductVersionLS-value, dwProductVersionLS-value-setter,
	dwFileFlagsMask-value, dwFileFlagsMask-value-setter,
	dwFileFlags-value, dwFileFlags-value-setter, dwFileOS-value,
	dwFileOS-value-setter, dwFileType-value, dwFileType-value-setter,
	dwFileSubtype-value, dwFileSubtype-value-setter, dwFileDateMS-value,
	dwFileDateMS-value-setter, dwFileDateLS-value,
	dwFileDateLS-value-setter, <VS-FIXEDFILEINFO>, <LPVS-FIXEDFILEINFO>,
	VerFindFile, VerInstallFile, GetFileVersionInfoSize,
	GetFileVersionInfo, VerLanguageName, VerQueryValue;

end module Win32-version;
