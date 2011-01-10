Module: checkmate
Synopsis: Test application for debugger
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// BIG-LITERAL (internal, adapted from big-literal of ~dylan/harp/coff-manager/coff-constants.dylan)
///
/// TEMPORARY: BIG-LITERAL is a function for making potentially oversized 
/// integers, at a time when the Dylan reader is unable to do this for itself.

define function big-literal
    (hi :: <integer>, lo :: <integer>) => (res :: <machine-word>)
  as(<machine-word>, generic-+(generic-ash(hi, 16), lo));
end function;

/// $EXECPTION-* (WinNT.h via WinBase.h)

// define constant $EXCEPTION-WAIT-0			= big-literal(#x0000, #x0000);
// define constant $EXCEPTION-ABANDONED-WAIT-0		= big-literal(#x0000, #x0080);
// define constant $EXCEPTION-USER-APC			= big-literal(#x0000, #x00C0);
// define constant $EXCEPTION-TIMEOUT			= big-literal(#x0000, #x0102);
// define constant $EXCEPTION-PENDING			= big-literal(#x0000, #x0103);
// define constant $EXCEPTION-SEGMENT-NOTIFICATION		= big-literal(#x4000, #x0005);
// define constant $EXCEPTION-GUARD-PAGE-VIOLATION		= big-literal(#x8000, #x0001);
define constant $EXCEPTION-DATATYPE-MISALIGNMENT	= big-literal(#x8000, #x0002);
// define constant $EXCEPTION-BREAKPOINT			= big-literal(#x8000, #x0003);
// define constant $EXCEPTION-SINGLE-STEP			= big-literal(#x8000, #x0004);
define constant $EXCEPTION-ACCESS-VIOLATION		= big-literal(#xC000, #x0005);
// define constant $EXCEPTION-IN-PAGE-ERROR		= big-literal(#xC000, #x0006);
// define constant $EXCEPTION-INVALID-HANDLE		= big-literal(#xC000, #x0008);
// define constant $EXCEPTION-NO-MEMORY			= big-literal(#xC000, #x0017);
define constant $EXCEPTION-ILLEGAL-INSTRUCTION		= big-literal(#xC000, #x001D);
define constant $EXCEPTION-NONCONTINUABLE-EXCEPTION	= big-literal(#xC000, #x0025);
// define constant $EXCEPTION-INVALID-DISPOSITION		= big-literal(#xC000, #x0026);
define constant $EXCEPTION-ARRAY-BOUNDS-EXCEEDED	= big-literal(#xC000, #x008C);
define constant $EXCEPTION-FLOAT-DENORMAL-OPERAND	= big-literal(#xC000, #x008D);
define constant $EXCEPTION-FLOAT-DIVIDE-BY-ZERO		= big-literal(#xC000, #x008E);
define constant $EXCEPTION-FLOAT-INEXACT-RESULT		= big-literal(#xC000, #x008F);
define constant $EXCEPTION-FLOAT-INVALID-OPERATION	= big-literal(#xC000, #x0090);
define constant $EXCEPTION-FLOAT-OVERFLOW		= big-literal(#xC000, #x0091);
define constant $EXCEPTION-FLOAT-STACK-CHECK		= big-literal(#xC000, #x0092);
define constant $EXCEPTION-FLOAT-UNDERFLOW		= big-literal(#xC000, #x0093);
define constant $EXCEPTION-INTEGER-DIVIDE-BY-ZERO	= big-literal(#xC000, #x0094);
// define constant $EXCEPTION-INTEGER-OVERFLOW		= big-literal(#xC000, #x0095);
define constant $EXCEPTION-PRIVILEGED-INSTRUCTION	= big-literal(#xC000, #x0096);
// define constant $EXCEPTION-STACK-OVERFLOW		= big-literal(#xC000, #x00FD);
// define constant $EXCEPTION-CONTROL-C-EXIT		= big-literal(#xC000, #x013A);


/// RAISEEXCEPTION (WinBase.h)

define C-function RaiseException
  parameter dwExceptionCode    :: <DWORD>;
  parameter dwExceptionFlags   :: <DWORD>;
  parameter nNumberOfArguments :: <DWORD>;
  parameter lpArguments        :: <DWORD>;
  c-name: "RaiseException", c-modifiers: "__stdcall";
end;

