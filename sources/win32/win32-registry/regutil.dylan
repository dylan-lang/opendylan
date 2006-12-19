Module:    Win32-Registry
Synopsis:  High-level utility functions for using registry from Dylan.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method get-module-file-name (#key module-handle)
 => (path :: <LPTSTR>);  
  let buf-size = $MAX-PATH;
  with-stack-structure( path-buf :: <LPTSTR>, size: buf-size)
    let hmodule :: <HMODULE> = module-handle | null-handle(<HMODULE>);
    let path-length :: <integer> =
      // Unless a DLL module handle is given, get the name of the .EXE file.
      GetModuleFileName(hmodule, path-buf, buf-size);
    if ( path-length <= 0 )
      report-win32-error("GetModuleFileName");
    end if;
    copy-sequence(path-buf, end: path-length)
  end with-stack-structure
end method get-module-file-name;

// Copy `data' into `buffer', and return the buffer address following the 
// last character.  (Note: does not store terminating nul.)
define method append-c-string ( buffer :: <LPTSTR>, data :: <byte-string> )
 => (end-pointer :: <LPTSTR>);
  let i :: <integer> = 0;
  for ( char :: <character> in data )
    pointer-value(buffer, index: i) := char;
    i := i + 1;
  end for;
  pointer-value-address(buffer, index: i) // buffer + i
end append-c-string;

define method register-item(subkey1 :: <byte-string>, subkey2 :: <byte-string>,
			    subkey3 :: <byte-string>, value :: <string>)
  let max-key-size = reduce1(\+, map(size, list(subkey1, subkey2, subkey3))) + 1;
  with-stack-structure( key-buf :: <LPTSTR>, size: max-key-size)
    let start-id = append-c-string(key-buf, subkey1 );
    let end-id = append-c-string(start-id, subkey2);
    let end-key = append-c-string(end-id, subkey3);
    pointer-value(end-key) := '\0';

    with-c-string (c-value = value)
      let key :: <HKEY> = $HKEY-CLASSES-ROOT;
      let value-size :: <integer> = size(value);
      let code = RegSetValue(key, key-buf, $REG-SZ, c-value, value-size);
      unless ( code = $ERROR-SUCCESS )
	error("Registration failure;\nRegSetValue error %d", code);
      end unless;
    end with-c-string;
  end with-stack-structure;
  values()
end register-item;



define method get-registry-item(subkey1 :: <byte-string>,
				subkey2 :: <byte-string>,
				subkey3 :: <byte-string>)
 => (value :: false-or(<string>));
  let max-key-size = reduce1(\+, map(size, list(subkey1, subkey2, subkey3))) + 1;
  with-stack-structure( key-buf :: <LPTSTR>, size: max-key-size)
  with-stack-structure( value-buf :: <LPTSTR>, size: max-key-size)
  with-stack-structure( size-ptr :: <PLONG> )
    let start-id = append-c-string(key-buf, subkey1 );
    let end-id = append-c-string(start-id, subkey2);
    let end-key = append-c-string(end-id, subkey3);
    pointer-value(end-key) := '\0';
    pointer-value(size-ptr) := max-key-size;
    let hkey :: <HKEY> = $HKEY-CLASSES-ROOT;
    let code = RegQueryValue(hkey, key-buf, value-buf, size-ptr);
    if ( code = $ERROR-SUCCESS )
      // Want to do this if it were supported:
      //   copy-sequence-as(<byte-string>, value-buf,
      //                    end: pointer-value(size-ptr) - 1);
      as(<byte-string>, value-buf)
    else
      #f
    end if;
  end with-stack-structure;
  end with-stack-structure;
  end with-stack-structure;
end get-registry-item;



define method delete-key-recursively (start-key :: <HKEY>,
				      sub-key-name :: <string>)
	=> error-code :: <integer>;
  with-c-string (str = sub-key-name)
    let error-code :: <integer> = RegDeleteKey(start-key, str);
    unless ( error-code = $ERROR-SUCCESS )
      error-code := RegDeleteKeyNT(start-key, str);
    end unless;
    error-code
  end with-c-string;
end delete-key-recursively;

/*
From MSDN:
  
 PSS ID Number: Q142491
 Article last modified on 01-16-1996

...

 To delete a key and all of its subkeys in Windows NT, a recursive delete
 function is implemented using RegEnumKeyEx and RegDeleteKey. This recursive
 delete function works by: (1) traversing down each subkey branch, one
 branch at a time, enumerating each key at each subkey level, until the last
 subkey leaf is reached and (2) individually deleting each subkey in reverse
 succession, one branch at a time, until even the particular key specified
 is deleted.
*/

// The sample code makes no attempt to check or recover from partial
// deletions.
 
define constant $MAX-KEY-LENGTH = 256;

define method RegDeleteKeyNT(start-key :: <HKEY>, key-name :: <LPTSTR> )
	=> error-code :: <integer>;

  if ( empty?(key-name) )
    $ERROR-BADKEY
  else
    let ( return-code :: <integer>, hKey :: <HKEY> ) =
      RegOpenKeyEx(start-key, key-name, 0,
		   logior($KEY-ENUMERATE-SUB-KEYS, $DELETE));
    if ( return-code = $ERROR-SUCCESS )
      with-stack-structure ( sub-key :: <LPTSTR>, size: $MAX-KEY-LENGTH )
	with-stack-structure ( sub-key-length-pointer :: <LPDWORD> )
	  let $NULL-LPDWORD :: <LPDWORD> = null-pointer(<LPDWORD>);
	  block(exit-loop)
	    while( return-code = $ERROR-SUCCESS )
	      pointer-value(sub-key-length-pointer) := $MAX-KEY-LENGTH;
	      return-code :=
		RegEnumKeyEx(hKey, 0, // always index zero
			     sub-key, sub-key-length-pointer,
			     $NULL-LPDWORD, $NULL-string,
			     $NULL-LPDWORD, null-pointer(<PFILETIME>));
	      if ( return-code = $ERROR-NO-MORE-ITEMS )
		return-code := RegDeleteKey(start-key, key-name);
		exit-loop();
	      elseif ( return-code = $ERROR-SUCCESS )
		return-code := RegDeleteKeyNT(hKey, sub-key);
	      end if;
	    end while;
	  end block;
        end with-stack-structure;
      end with-stack-structure;
      RegCloseKey(hKey);
      // Do not save return code here because error has already occurred.
    end if;
    return-code   
  end if
end method RegDeleteKeyNT;

define method RegDeleteKeyNT(start-key :: <HKEY>, key-name :: <string> )
	=> error-code :: <integer>;
  let c-string = as(<LPTSTR>, key-name);
  let code = RegDeleteKeyNT(start-key, c-string);
  destroy(c-string);
  code
end method;
