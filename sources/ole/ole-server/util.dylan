Module:    OLE-Server
Synopsis:  utilities for debugging and detecting and reporting errors.
Author:    David N. Gray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// For "int", allow more than 16 bits, but still fit in a fixnum:
//	(Note: can't use names <int> or <long> here because those are
//		exported by Win32-Common with a different meaning.)

// SCC version:
//define constant <fixnum> = <small-integer>;
//define constant <unsigned-fixnum> = <small-integer>;

// For "kan-970425" and later:
define constant <fixnum> = <integer>; // formerly known as <small-integer>
define constant <unsigned-fixnum> = <integer>;

// portable version:
/*
define constant <fixnum> = limited(<integer>, min: -#x800000, max: #x7FFFFF);
define constant <unsigned-fixnum> = limited(<integer>, min: 0, max: #x7FFFFF);
*/

//---- Change this to #f for the final version:		???
define constant $enable-debug-messages = #t;

define method debug-out( format :: <string>, #rest format-args ) => ();
  if( $enable-debug-messages )
    let message :: <string> = apply(format-to-string, format, format-args);
    let msize :: <fixnum> = size(message);
    let cbuf :: <C-string> = make(<C-string>, size: msize + 25);
    let j :: <fixnum> = 0;
    for ( i :: <fixnum> from 0 below msize )
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
    destroy(cbuf);
  end if;
  values()
end;



define method use-error-status ( err :: <ole-error> ) => status :: <HRESULT>;
  debug-out("Caught %s\n", err);
  ole-error-status(err)
end method;

// Evaluate the body and return $S-OK.  
// But if an OLE error is signalled, return the status code from the error.
define macro returning-error-status
 { returning-error-status ?:body end }
    => { block()
	   begin ?body end;
	   $S-OK;
	 exception ( err :: <ole-error> )
	   use-error-status(err);
	 end }
end macro;
