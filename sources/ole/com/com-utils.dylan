Module:    COM
Synopsis:  miscellaneous utilities
Author:    David N. Gray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant <fixnum> = <integer>; // formerly known as <small-integer>
define constant <unsigned-fixnum> = <fixnum>;

define open primary class <ole-error> (<error>)
  slot ole-error-status :: <HRESULT>, setter: #f,
    required-init-keyword: status:;
  slot ole-error-context, // usually the name of the function
    setter: #f, init-value: #f, init-keyword: context:;
  slot ole-error-instance, // the interface object
    setter: #f, init-value: #f, init-keyword: instance:;
  slot ole-error-args :: <sequence>,  // other data
    setter: #f, init-value: #[], init-keyword: args:;
end;


define function make-ole-error( status :: <HRESULT>, context,
				    instance, args )
 => ( error :: <ole-error>);
  make(<ole-error>, status: status, context: context,
       instance: instance, args: args)
end;

define method ole-error( status :: <HRESULT>, context, instance, #rest args );
  error( make-ole-error(status, context, instance, args) );
end;

define method ole-cerror( status :: <HRESULT>, context, instance, #rest args );
  cerror($Continue-message,
	 make-ole-error(status, context, instance, args));
end;

define inline function check-ole-status ( status :: <HRESULT>,
					 context, instance, #rest args ) => ();
  if ( FAILED?(status) )
    apply(ole-error, status, context, instance, args);
  end if;
  values()
end check-ole-status;

define method condition-to-string
    (condition :: <ole-error>) => (string :: <string>)
  let arg = condition.ole-error-instance;
  if ( arg == $null-interface )
    // Special case for functions taking a string or ID instead of interface.
    let first-arg = element(condition.ole-error-args, 0, default: #f);
    if ( instance?(first-arg, <string>) )
      arg := first-arg;
    elseif ( instance?(first-arg, <REFCLSID>) )
      arg := as(<string>, first-arg);
    end if;
  end if;
  let status = condition.ole-error-status;
  let message = win32-error-message(status);
  if ( message )
    format-to-string("OLE error %=: %s\nfrom %s on %=",
		     status, message,
		     condition.ole-error-context,
		     arg)
  else
    format-to-string("OLE error %= from %s on %=",
		     status,
		     condition.ole-error-context,
		     arg)
  end if
end method condition-to-string;


define inline function C-element-pointer ( ptr :: <C-statically-typed-pointer>,
					   index :: <fixnum> )
 => ( ptr :: <C-statically-typed-pointer> )

  pointer-value-address(ptr, index: index)
end;


//---- Change this to #f for the final version:		???
define constant $enable-debug-messages = #t;

// Display message in debugger (if any):
define method Output-Debug-String ( string :: <byte-string> ) => ();
  if( $enable-debug-messages )
    OutputDebugString(string);
  end if;
  values()
end method;

define method Output-Debug-String ( string :: <C-string> ) => ();
  if( $enable-debug-messages )
    OutputDebugString(string);
  end if;
  values()
end method;



// ====  command line parsing  ====

define variable *started-by-ole*  :: <boolean> = #f;
define variable *automation-option* :: <boolean> = #f;
define variable *parse-done*  :: <boolean> = #f;
define variable *register-only* = #f;
define variable *file-arg* :: false-or(<string>) = #f;

define method OLE-util-started-by-OLE? () => start-by-ole? :: <boolean>;
  unless ( *parse-done* )
    parse-args();
  end unless;
  *started-by-ole*
end method;

define method OLE-util-automation? () => automation? :: <boolean>;
  unless ( *parse-done* )
    parse-args();
  end unless;
  *automation-option*
end method;

define method OLE-util-register-only? () => (register-only?);
  unless ( *parse-done* )
    parse-args();
  end unless;
  *register-only*
end method;

define constant $unregister-flag = 'U';

define method OLE-util-unregister? () => (unregister? :: <boolean>);
  OLE-util-register-only?() == $unregister-flag
end method;

define method OLE-util-file-arg () => file-name :: false-or(<string>);
  unless ( *parse-done* )
    parse-args();
  end unless;
  *file-arg*
end method;

define method match-option( args-line :: <LPSTR>, start :: <fixnum>,
			    option :: <byte-string> )
 => ( match :: false-or(<fixnum>) );
  // case-insensitive match of `option' with `args-line' beginning at `start';
  // if match, return index of end of argument, else #f.
  let i :: <fixnum> = start;
  block (return)
    for ( c :: <character> in option )
      if ( as-uppercase(args-line[i]) = c )
	i := i + 1;
      else
	return(#f);
      end if;
    end for;
    if ( args-line[i] <= ' ' )
      i
    else #f
    end if;
  end block;
end match-option;

define method parse-args () => ();
  let args-line = application-command-line();
  unless ( null-pointer?(args-line) )
    let i :: <unsigned-fixnum> = 0;
    // skip blanks
    while( args-line[i] = ' ' )
      i := i + 1;
    end while;

    if ( args-line[i] ~= '\0' )

      // Check for "-Embedding" or "/Embedding" and set result.
      if ( (args-line[i] = '-') | (args-line[i] = '/') )
	i := i + 1;
	if ( match-option(args-line, i, "REGSERVER") )
	  *register-only* := #t;
	elseif ( match-option(args-line, i, "UNREGSERVER") )
	  *register-only* := $unregister-flag;
	else
	  let match = match-option(args-line, i, "EMBEDDING");
	  if ( match )
	    *started-by-ole* := #t;
	    i := match;
	  else
	    match := match-option(args-line, i, "AUTOMATION");
	    if ( match )
	      *automation-option* := #t;
	      i := match;
	    end if;
	  end if;
	end if;
	while( args-line[i] > ' ' )
	  i := i + 1;
	end while;
      end if;

      // skip blanks
      while( args-line[i] = ' ' )
	i := i + 1;
      end while;

      // set file-name to argument
      if ( args-line[i] > '/' )
	*file-arg* := C-element-pointer(args-line, i);
      end if;
    end if;
  end unless;
  *parse-done* := #t;
  values()
end method parse-args;



define method as( class == <string>, guid :: <REFGUID> )
 => ( string :: <byte-string> );
  // given a GUID, return a string representation in the form:
  //	"{xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx}"
  let max-size = 100;
  with-stack-structure( buffer :: <LPOLESTR>, size: max-size )
    let actual-size = StringFromGUID2(guid, buffer, max-size);
    unless ( actual-size > 32 )
      error("StringFromGUID2 failed");
    end unless;
    // the size returned includes the terminating NUL.
    local-debug-assert( buffer[actual-size - 2] = '}' );
    as(<byte-string>, buffer)
  end with-stack-structure
end method;

define method as( class == <REFGUID>, guid :: <REFGUID> )
			=> ( guid :: <REFGUID> );
  guid
end;

define method as( class == <REFGUID>, string :: <string> )
		 => ( guid :: <REFGUID> );
  // The string must be of the format:
  //	"{xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx}"
  // where each "x" represents a hexadecimal digit.
  // The braces are required; letters may be either upper or lower case.
  let guid :: <REFGUID> = make(<REFGUID>);
  let buf = as(<LPOLESTR>, string);
  let status = CLSIDFromString(buf, guid);
  unless ( buf == string )
    destroy(buf);
  end unless;
  if ( FAILED?(status) )
    destroy(guid);
    if ( status = $CO-E-CLASSSTRING )
      error("Can't parse GUID from \"%s\"\nExpected format: \"%s\"",
	    string, as(<string>, $IID-NULL));
    else
      ole-error(status, "CLSIDFromString", $null-interface, string);
    end if;
  end if;
  guid
end method;



// Include `OleInitialize' and `OleUninitialize' in the "COM" library
// instead of the "OLE" library because they are supposed to be used by
// OLE Automation programs that don't need anything else from OLE.
// In Win32, the COM and OLE functionality are both in the same file 
// ("OLE32.DLL"), so it really doesn't matter which Dylan library contains
// the declaration.

define C-function OleInitialize
  input parameter pvReserved :: <C-pointer>;  // must be NULL
  result status :: <C-HRESULT>;
  c-name: "OleInitialize", c-modifiers: "__stdcall";
end;

define C-function OleUninitialize
  c-name: "OleUninitialize", c-modifiers: "__stdcall";
end;


// This mixin class is used to cause the Microsoft OLE library to be
// automatically  initialized at the right time.
define open free abstract class <lib-init-mixin> ( <object> )
end class;

// OleInitialize needs to be called once in each thread; see Bug 4111.
define thread variable *init-count* :: <fixnum> = 0;

define method OLE-Initialize () => ();
  if ( zero?(*init-count*) )
    Output-Debug-String("calling OleInitialize\r\n");
    let status = OleInitialize($NULL-interface);
    check-ole-status(status, "OleInitialize", $NULL-interface);
  end if;
  *init-count* := *init-count* + 1;
end method;

define method initialize (this :: <lib-init-mixin>, #rest ignore, #key ) => ();
  Ole-Initialize();
  next-method();
end method initialize;

define method OLE-UnInitialize () => ();
  *init-count* := *init-count* - 1;
  if ( zero?(*init-count*) )
    Output-Debug-String("calling OleUninitialize\r\n");
    OleUninitialize();
  end if;
end method;

define method terminate (this :: <lib-init-mixin>) => ();
  next-method();

  *init-count* := *init-count* - 1;
  if ( zero?(*init-count*) )
    Output-Debug-String("calling OleUninitialize\r\n");
    OleUninitialize();
  end if;
  values();
end terminate;

define macro with-ole
  { with-ole ?body:body end }
   => { OLE-Initialize ();
	block ()
	   ?body
	 cleanup
	   OLE-UnInitialize();
	 end
	  }
end macro with-ole;



//  defined in both OLE and OLE-Controls:
define open-accessor hAccel-value;

//  defined in COM and OLE-Dialogs:
define open-accessor iid-value;
define open-accessor clsid-value;
define open-accessor cbStruct-value;

// defined in COM and OLE-Controls:
define open-accessor pUnk-value;
define open-accessor hBitmap-value;
