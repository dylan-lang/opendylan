Module:    motley
Author:    Seth LaForge
Synopsis:  Top level program for OLE type library walker
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define macro check-winfunc
  {
    check-winfunc(?:name(?args:*))
  } => {
    let (status :: type-union(<integer>, <machine-word>), #rest rest) = 
	    ?name(?args);
    if (status ~= $ERROR-SUCCESS)
      error("Error in %s: %s", ?"name", win32-error-message(status));
    end if;
    let rest = as(<list>, rest);
    apply(values, rest)
  }

  {
    check-winfunc(?operation:expression, ?:expression)
  } => {
    let (status :: type-union(<integer>, <machine-word>), #rest rest) = 
	    ?expression;
    if (status ~= $ERROR-SUCCESS)
      error("Error while %s: %s", ?operation, win32-error-message(status));
    end if;
    let rest = as(<list>, rest);
    apply(values, rest)
  }
end macro check-winfunc;


define macro with-key
  { with-key (?:name = ?key:expression, ?subkey:expression) ?:body end } =>
  { let ?name :: <HKEY> = 
      check-winfunc(RegOpenKeyEx(?key, ?subkey, 0, 
				 $KEY-QUERY-VALUE + $KEY-ENUMERATE-SUB-KEYS));
    block () 
      ?body
    cleanup
      check-winfunc(RegCloseKey(?name));
    end; }
end macro with-key;

define function registry-key-string-value (key :: <HKEY>, 
					   value-name :: false-or(<LPDWORD>))
                                       => (r :: false-or(<string>))
  let r = make(<C-string>, size: 1024);
  with-stack-structure (length :: <LPDWORD>)
    pointer-value(length) := 1024;
    let val :: <LPDWORD> = value-name | null-pointer(<LPDWORD>);
    let (status, type) = 
            RegQueryValueEx(key, null-pointer(<LPCSTR>), val, r, length);
    if (status = $ERROR-SUCCESS & type = $REG-SZ)
      r
    end if
  end with-stack-structure
end function registry-key-string-value;

define macro with-subkeys
  { with-subkeys(?:name, ?subkey:name = ?key:expression) ?:body end } =>
  { with-stack-structure (?name :: <C-string>, size: 1024)
      for (index from 0,
	   until:
	     begin
	       let status = RegEnumKey(?key, index, ?name, 1024);
	       if (status ~= $ERROR-NO-MORE-ITEMS) 
		 check-winfunc("RegEnumKey", status) 
	       end;
	       status = $ERROR-NO-MORE-ITEMS
	     end)
	with-key (?subkey = ?key, ?name)
	  ?body
	end with-key;
      end for;
    end with-stack-structure; }
end macro with-subkeys;

/*
define macro with
  { with () ?:body end } => { ?body }
  { with (?:name ?rest:*; ?next:*) ?:body end } 
    => { "with-" ## ?name (?rest) with (?next) ?body end end }
end macro with;
*/


define function hex-to-integer (hex :: <string>) => (r :: <integer>)
  let r :: <integer> = 0;
  for (char in hex)
    r := r * 16 + 
	    if ('0' <= char & char <= '9')
	      as(<integer>, char) - as(<integer>, '0')
	    else
	      as(<integer>, as-uppercase(char)) - as(<integer>, 'A')
	    end if;
  end for;
  r
end function hex-to-integer;


define constant <registry-type-library-info> = <pair>;
define constant registry-type-library-name = head;
define constant registry-type-library-path = tail;

// Reads the list of type libraries from the registry and returns information
// about type library names and locations.
//	r: sequence of type library descriptions.
//	r[*].registry-type-library-name: a <string> describing a type library.
//	r[*].registry-type-library-path: a <string> with the path of a type
//					 library.

define function get-registry-type-libraries 
    () => (r :: <sequence>)
  let r = make(<deque>);
  let hkey :: <HKEY> = $HKEY-CLASSES-ROOT;
  with-key (typelibs = hkey, "TypeLib")

    with-subkeys (libid-name, libid = typelibs)

      // For all versions:
      with-subkeys (libver-name, libver = libid)

        // Get the library name:
	let libname = registry-key-string-value(libver, #f) | "";

	// For all languages:
	with-subkeys (langid-name, langid-key = libver)
	  // To be a langid, it must be all digits:
	  if (every?(char-is-hex?, langid-name))
	    let langid :: <integer> = hex-to-integer(langid-name);
	    with-stack-structure (c-langname :: <C-string>, size: 1024)
	      let langname :: <string> = c-langname;
	      if (0 = GetLocaleInfo(MAKELCID(langid, $SORT-DEFAULT),
				    $LOCALE-SLANGUAGE, c-langname, 1024))
		langname := "";
	      end if;

	      block ()
		with-key (win32-key = langid-key, "win32")
		  let win32-location = registry-key-string-value(win32-key, #f);
		  if (win32-location)
		    push-last(r, 
		      pair(
			concatenate-as(<string>, libname, 
				       " (", libver-name, ") ", langname),
			concatenate-as(<string>, win32-location)));
		  end if;
		end with-key;
	      exception (x :: <error>)
		// Ignore exceptions.
	      end block;

	    end with-stack-structure;
	  end if;
	end with-subkeys;

      end with-subkeys;

    end with-subkeys;

  end with-key;
  r := sort!(r, test: method (a,b) 
  			a.registry-type-library-name.as-lowercase < 
			b.registry-type-library-name.as-lowercase 
		      end method)
end function get-registry-type-libraries;


// Extract the short name of a type library.  If there are any problems 
// opening the type library, an <error> will be signalled.
// DEPRECATED in favor of get-type-library-information.

define function get-type-library-short-name 
	(path :: type-union(<string>, <locator>)) => (short-name :: <string>)
  get-type-library-information(path)
end function get-type-library-short-name;


// Extract the short name of a type library.  
// Also get the names of all of the interfaces and coclasses in a type library,
// as candidates for the interface section of a spec file.
// If there are any problems opening the type library, an <error> will be
// signalled.

define function get-type-library-information
	(path :: type-union(<string>, <locator>)) 
     => (short-name :: <string>, 
	 interfaces :: <sequence>)
  with-ole
    let lib = make(<type-library>, file: path);
    let interfaces = make(<deque>);
    for (entry in lib.contents)
      if (instance?(entry, <coclass-info>) | instance?(entry, <interface-info>))
	if (~ member?(entry.c-name, interfaces, test: \=))
	  push-last(interfaces, entry.c-name);
	end if;
      end if;
    end for;
    values(lib.dylan-name, interfaces)
  end with-ole
end function get-type-library-information;
