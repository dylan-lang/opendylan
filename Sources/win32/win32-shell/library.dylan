module:    Dylan-user	
Synopsis:  Interface to Win32 shell API -- "SHELLAPI.H" and "SHELL.DLL".
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/* Automatically generated from "library.src"; do not edit. */

define library Win32-shell
  use Dylan;
  use C-FFI;
  use Win32-common;
  use Win32-user;
  export Win32-shell;
end library;

define module Win32-shell
  use Dylan;
  use C-FFI;
  use Win32-common,
    // export structure accessors defined in both modules. 
    export: { cbSize-value, cbSize-value-setter,
	      u-value, u-value-setter } ;
  use Win32-user,
    // we don't actually use anything from Win32-user, but we need to 
    // add methods to these generic functions.
    export: {hWnd-value, hWnd-value-setter,
	     lParam-value, lParam-value-setter,
	     fMask-value, fMask-value-setter,
	     hIcon-value, hIcon-value-setter, pt-value, pt-value-setter};


  // from "shellapi.h":
  export <HDROP>, DragQueryFile, DragQueryPoint, DragFinish,
	DragAcceptFiles, ShellExecute, FindExecutable, ShellAbout,
	ExtractAssociatedIcon, ExtractIcon;
  export $ABM-NEW, $ABM-REMOVE, $ABM-QUERYPOS, $ABM-SETPOS,
	$ABM-GETSTATE, $ABM-GETTASKBARPOS, $ABM-ACTIVATE,
	$ABM-GETAUTOHIDEBAR, $ABM-SETAUTOHIDEBAR, $ABM-WINDOWPOSCHANGED,
	$ABN-STATECHANGE, $ABN-POSCHANGED, $ABN-FULLSCREENAPP,
	$ABN-WINDOWARRANGE, $ABS-AUTOHIDE, $ABS-ALWAYSONTOP, $ABE-LEFT,
	$ABE-TOP, $ABE-RIGHT, $ABE-BOTTOM, uCallbackMessage-value,
	uCallbackMessage-value-setter, uEdge-value, uEdge-value-setter,
	rc-value, rc-value-setter, <APPBARDATA>, <LPAPPBARDATA>,
	<PAPPBARDATA>, SHAppBarMessage, ExtractIconEx;
  export $FO-MOVE, $FO-COPY, $FO-DELETE, $FO-RENAME,
	$FOF-MULTIDESTFILES, $FOF-CONFIRMMOUSE, $FOF-SILENT,
	$FOF-RENAMEONCOLLISION, $FOF-NOCONFIRMATION, $FOF-WANTMAPPINGHANDLE,
	$FOF-ALLOWUNDO, $FOF-FILESONLY, $FOF-SIMPLEPROGRESS,
	$FOF-NOCONFIRMMKDIR, $FOF-NOERRORUI, $FOF-NOCOPYSECURITYATTRIBS,
	<FILEOP-FLAGS>, $PO-DELETE, $PO-RENAME, $PO-PORTCHANGE, $PO-REN-PORT,
	<PRINTEROP-FLAGS>, wFunc-value, wFunc-value-setter, pFrom-value,
	pFrom-value-setter, pTo-value, pTo-value-setter, fFlags-value,
	fFlags-value-setter, fAnyOperationsAborted-value,
	fAnyOperationsAborted-value-setter, hNameMappings-value,
	hNameMappings-value-setter, lpszProgressTitle-value,
	lpszProgressTitle-value-setter, <SHFILEOPSTRUCTA>,
	<LPSHFILEOPSTRUCTA>, <SHFILEOPSTRUCT>, <LPSHFILEOPSTRUCT>,
	SHFileOperation, SHFreeNameMappings;
  export pszOldPath-value, pszOldPath-value-setter, pszNewPath-value,
	pszNewPath-value-setter, cchOldPath-value, cchOldPath-value-setter,
	cchNewPath-value, cchNewPath-value-setter, <SHNAMEMAPPINGA>,
	<LPSHNAMEMAPPINGA>, <SHNAMEMAPPING>, <LPSHNAMEMAPPING>;
  export $SE-ERR-FNF, $SE-ERR-PNF, $SE-ERR-ACCESSDENIED, $SE-ERR-OOM,
	$SE-ERR-DLLNOTFOUND, $SE-ERR-SHARE, $SE-ERR-ASSOCINCOMPLETE,
	$SE-ERR-DDETIMEOUT, $SE-ERR-DDEFAIL, $SE-ERR-DDEBUSY,
	$SE-ERR-NOASSOC, $SEE-MASK-CLASSNAME, $SEE-MASK-CLASSKEY,
	$SEE-MASK-IDLIST, $SEE-MASK-INVOKEIDLIST, $SEE-MASK-ICON,
	$SEE-MASK-HOTKEY, $SEE-MASK-NOCLOSEPROCESS, $SEE-MASK-CONNECTNETDRV,
	$SEE-MASK-FLAG-DDEWAIT, $SEE-MASK-DOENVSUBST, $SEE-MASK-FLAG-NO-UI,
	$SEE-MASK-UNICODE, $SEE-MASK-NO-CONSOLE, $SEE-MASK-ASYNCOK,
	$SEE-MASK-HMONITOR, lpVerb-value, lpVerb-value-setter, lpFile-value,
	lpFile-value-setter, lpParameters-value, lpParameters-value-setter,
	lpDirectory-value, lpDirectory-value-setter, nShow-value,
	nShow-value-setter, hInstApp-value, hInstApp-value-setter,
	lpIDList-value, lpIDList-value-setter, lpClass-value,
	lpClass-value-setter, hkeyClass-value, hkeyClass-value-setter,
	dwHotKey-value, dwHotKey-value-setter, hMonitor-value,
	hMonitor-value-setter, hProcess-value, hProcess-value-setter,
	<SHELLEXECUTEINFOA>, <LPSHELLEXECUTEINFOA>, <SHELLEXECUTEINFO>,
	<LPSHELLEXECUTEINFO>, ShellExecuteEx;
  export uID-value, uID-value-setter, uFlags-value,
	uFlags-value-setter, uCallbackMessage-value,
	uCallbackMessage-value-setter, szTip-array, szTip-array-setter,
	szTip-value, <NOTIFYICONDATAA>, <LPNOTIFYICONDATAA>,
	<PNOTIFYICONDATAA>, <NOTIFYICONDATA>, <PNOTIFYICONDATA>;
  export $NIM-ADD, $NIM-MODIFY, $NIM-DELETE, $NIF-MESSAGE, $NIF-ICON,
	$NIF-TIP, Shell-NotifyIcon;
  export iIcon-value, iIcon-value-setter, dwAttributes-value,
	dwAttributes-value-setter, szDisplayName-array,
	szDisplayName-array-setter, szDisplayName-value, szTypeName-array,
	szTypeName-array-setter, szTypeName-value, <SHFILEINFOA>,
	<LPSHFILEINFOA>, <SHFILEINFO>, $SHGFI-ICON, $SHGFI-DISPLAYNAME,
	$SHGFI-TYPENAME, $SHGFI-ATTRIBUTES, $SHGFI-ICONLOCATION,
	$SHGFI-EXETYPE, $SHGFI-SYSICONINDEX, $SHGFI-LINKOVERLAY,
	$SHGFI-SELECTED, $SHGFI-ATTR-SPECIFIED, $SHGFI-LARGEICON,
	$SHGFI-SMALLICON, $SHGFI-OPENICON, $SHGFI-SHELLICONSIZE, $SHGFI-PIDL,
	$SHGFI-USEFILEATTRIBUTES, SHGetFileInfo, $SHGNLI-PIDL,
	$SHGNLI-PREFIXNAME, $SHGNLI-NOUNIQUE;

  export <LPHICON>;
end module;
