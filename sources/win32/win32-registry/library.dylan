module:    Dylan-user	
Synopsis:  Win32 API for registry functions in "WINREG.H" and "ADVAPI32.DLL"
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/* Automatically generated from "library.src"; do not edit. */


define library Win32-Registry
  use dylan;
  use C-FFI;
  use Win32-common;
  use Win32-Kernel;
  export Win32-Registry;
end;

define module Win32-Registry
  use dylan;
  use dylan-extensions, import: { false-or };
  use C-FFI;
  use Win32-common, export: { $ERROR-SUCCESS, <LPSTR>, <LPCSTR>,
			      <LPDWORD>, <PLONG>, <LPBYTE> };
  use Win32-Kernel,
    export: { <PFILETIME>, <LPSECURITY-ATTRIBUTES>,
	     $READ-CONTROL, $SYNCHRONIZE, $STANDARD-RIGHTS-ALL,
	     $STANDARD-RIGHTS-REQUIRED, $STANDARD-RIGHTS-READ,
	     $STANDARD-RIGHTS-WRITE, $STANDARD-RIGHTS-EXECUTE };


  // from "winreg.h":
  export <REGSAM>;
  export <HKEY>, <PHKEY>;
  export $HKEY-CLASSES-ROOT, $HKEY-CURRENT-USER, $HKEY-LOCAL-MACHINE,
	$HKEY-USERS, $HKEY-PERFORMANCE-DATA, $HKEY-CURRENT-CONFIG,
	$HKEY-DYN-DATA;
  export ve-valuename-value, ve-valuename-value-setter,
	ve-valuelen-value, ve-valuelen-value-setter, ve-valueptr-value,
	ve-valueptr-value-setter, ve-type-value, ve-type-value-setter,
	<VALENTA>, <LPVALENTA>, <PVALENTA>, <VALENT>, <PVALENT>;
  export RegCloseKey, RegConnectRegistry, RegCreateKey,
	RegCreateKeyEx, RegDeleteKey, RegDeleteValue, RegEnumKey,
	RegEnumKeyEx, RegEnumValue, RegFlushKey, RegGetKeySecurity,
	RegLoadKey, RegNotifyChangeKeyValue, RegOpenKey, RegOpenKeyEx,
	RegQueryInfoKey, RegQueryValue, RegQueryMultipleValues,
	RegQueryValueEx, RegReplaceKey, RegRestoreKey, RegSaveKey,
	RegSetKeySecurity, RegSetValue, RegSetValueEx, RegUnLoadKey;


  // from "winnt.h":
  export <PSECURITY-DESCRIPTOR>;
  export <ACCESS-MASK>;
  export $DELETE, $WRITE-DAC, $WRITE-OWNER, $SPECIFIC-RIGHTS-ALL;
  export <SECURITY-INFORMATION>;
  export $KEY-QUERY-VALUE, $KEY-SET-VALUE, $KEY-CREATE-SUB-KEY,
	$KEY-ENUMERATE-SUB-KEYS, $KEY-NOTIFY, $KEY-CREATE-LINK;
  export $REG-OPTION-RESERVED, $REG-OPTION-NON-VOLATILE,
	$REG-OPTION-VOLATILE, $REG-OPTION-CREATE-LINK,
	$REG-OPTION-BACKUP-RESTORE, $REG-OPTION-OPEN-LINK;
  export $REG-CREATED-NEW-KEY, $REG-OPENED-EXISTING-KEY;
  export $REG-WHOLE-HIVE-VOLATILE, $REG-REFRESH-HIVE,
	$REG-NO-LAZY-FLUSH;
  export $REG-NOTIFY-CHANGE-NAME, $REG-NOTIFY-CHANGE-ATTRIBUTES,
	$REG-NOTIFY-CHANGE-LAST-SET, $REG-NOTIFY-CHANGE-SECURITY;
  export $REG-NONE, $REG-SZ, $REG-EXPAND-SZ, $REG-BINARY, $REG-DWORD,
	$REG-DWORD-LITTLE-ENDIAN, $REG-DWORD-BIG-ENDIAN, $REG-LINK,
	$REG-MULTI-SZ, $REG-RESOURCE-LIST, $REG-FULL-RESOURCE-DESCRIPTOR,
	$REG-RESOURCE-REQUIREMENTS-LIST;

  // high-level utility functions in "regutil.dylan":
  export get-module-file-name, get-registry-item, register-item;
  export delete-key-recursively;

end module Win32-Registry;
