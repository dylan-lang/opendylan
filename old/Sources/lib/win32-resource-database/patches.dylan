Module:    win32-resource-database-internal 
Synopsis:  public interface to resource database
Author:    Roman Budzianowski
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define C-pointer-type <LPCDLGITEMTEMPLATEA> => <DLGITEMTEMPLATE>;

define C-pointer-type <WORD*> => <WORD>;

define method debug-out-internal( format :: <string>, #rest format-args ) => ();
  let message :: <string> = apply(format-to-string, format, format-args);
  let msize :: <integer> = size(message);
  let cbuf :: <C-string> = make(<C-string>, size: msize + 25);
  let j :: <integer> = 0;
  for ( i :: <integer> from 0 below msize )
    let c :: <character> = message[i];
    if ( c = '\n' )
      cbuf[j] := '\r';
      j := j + 1;
    end if;
    cbuf[j] := message[i];
    j := j + 1;
  end for;
  cbuf[j] := '\0';
  OutputDebugString(cbuf); // display the string in the debugger (if any)
  %free(cbuf);
  values()
end;

define constant debug-out = method(#rest args) apply(debug-out-internal, args) end;
/*
define method pointer+ (p :: <C-statically-typed-pointer>, off :: <integer>)
 => (new-pointer :: <C-statically-typed-pointer>);
  let p-type = referenced-type(p);
  let type-size = size-of(p-type);
  dialog-debug-out("Type %= size: %d\n", p-type, type-size);
  let raw-pointer = p.pointer-address;
  raw-pointer := \%+(raw-pointer, (off * type-size));
  make(type-for-copy(p), pointer: raw-pointer);
end;
*/

define method pointer+ (p :: <C-statically-typed-pointer>, off :: <integer>)
 => (new-pointer :: <C-statically-typed-pointer>);
  pointer-value-address(p, index: off)
end;

/*
define method referenced-type(p :: <C-UNSIGNED-SHORT*>)
  <C-UNSIGNED-SHORT>
end;
*/
/*
define method \+ (p :: <C-statically-typed-pointer>, off :: <integer>)
 => (new-pointer :: <C-statically-typed-pointer>);
  let p-type = referenced-type(p);
  let type-size = size-of(p-type);
  dialog-debug-out("Type %= size: %d\n", p-type, type-size);
  let raw-pointer = p.pointer-address;
  raw-pointer := raw-pointer + (off * type-size);
  make(class-for-copy(p), pointer: raw-pointer);
end;
*/

define method print-last-error()
  let buff = make(<LPTSTR>, size: 1024);

  FormatMessage($FORMAT-MESSAGE-FROM-SYSTEM,
		$NULL-VOID,
		GetLastError(),
		MAKELANGID($LANG-NEUTRAL, $SUBLANG-DEFAULT),
//		make(<LPTSTR>, pointer: buff.pointer-address),
		buff,
		1024,
		$NULL-VOID);

  debug-out("ERROR: %s\n", as(<byte-string>, buff));
end;
