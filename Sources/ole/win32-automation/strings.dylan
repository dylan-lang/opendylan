Module:    WIN32-Automation
Synopsis:  Dylan accessors for OLE Automation strings.
Author:    David N. Gray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//  ---  Basic Strings  ---

define sealed method make (class == <BSTR>, #rest args,
		    #key address = #f, size :: <integer> = 0, fill = #f)
 => ( string :: <BSTR>);

  if ( address )
    next-method()
  else
    let result :: <BSTR> = SysAllocStringLen($NULL-OLESTR, size);
    if ( fill )
      for (i :: <integer> from 0 below size)
	result[i] := fill;
      end;
    end if;
    result[size] := '\0';
    result
  end
end method;

define sealed method as( class == <BSTR>, data :: <C-unicode-string> ) => v :: <BSTR>;
  SysAllocString(data)
end;

define sealed method as( class == <BSTR>, data :: <BSTR> ) => v :: <BSTR>;
  data
end;

// This might become redundant if the method on <C-unicode-string> is
// defined appropriately so that it can be inherited.	???
define method as (class == <BSTR>, s :: <string>) => v :: <BSTR>;
  let length :: <integer> = size(s);
  let new :: <BSTR> = make(class, size: length, fill: #f);
  for (i from 0 below length)
    new[i] := s[i];
  end;
  new
end method;

define sealed method copy-sequence(string :: <BSTR>,
			    #key start: _start :: <integer> = 0,
			       end: _end :: <integer> = SysStringLen(string) )
 => (string :: <BSTR>)
  if ( null-pointer?(string) )
    $NULL-BSTR
  else
    let ptr = if ( zero?(_start) )
		string
	      else
		pointer-value-address(string, index: _start)
	      end if;
    SysAllocStringLen(ptr, _end - _start)
  end if
end;

define sealed method destroy( string :: <BSTR>, #key ) => ();
  SysFreeString(string); // accepts and ignores NULL
  values()
end method destroy;

define sealed method size( string :: <BSTR> ) => length :: limited(<integer>, min: 0);
  SysStringLen(string)
end;

define sealed method empty?( string :: <BSTR> ) => empty :: <boolean>;
  null-pointer?(string) | zero?(SysStringLen(string))
end;

define constant $NULL-BSTR :: <BSTR> = null-pointer(<BSTR>);


// The following function is like `as(<BSTR>,...' except that it always
// allocates a new copy.  This is needed when returning a value that will be
// deallocated by the caller.

define open generic copy-as-BSTR (data) => (string :: <BSTR>);

define sealed method copy-as-BSTR (string :: <C-unicode-string>)
 => (string :: <BSTR>)
  if ( null-pointer?(string) )
    $NULL-BSTR
  else
    SysAllocString(string)
  end if
end;

define sealed method copy-as-BSTR (string :: <BSTR>) => (string :: <BSTR>)
  if ( null-pointer?(string) )
    $NULL-BSTR
  else
    SysAllocStringLen(string, SysStringLen(string))
  end if
end;

define sealed method copy-as-BSTR (string :: <string>) => (string :: <BSTR>)
  as(<BSTR>, string)
end;

define sealed method copy-as-BSTR (data == #f) => (string :: <BSTR>)
  $NULL-BSTR
end;
