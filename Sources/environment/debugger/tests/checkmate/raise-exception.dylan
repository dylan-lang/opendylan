Module: checkmate
Synopsis: Test application for debugger
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// $EXCEPTION-* (internal)

define constant $EXCEPTION-ACCESS-VIOLATION = 0;
define constant $EXCEPTION-ARRAY-BOUNDS-EXCEEDED = 1;
define constant $EXCEPTION-DATATYPE-MISALIGNMENT = 2;
define constant $EXCEPTION-PRIV-INSTRUCTION = 3;
define constant $EXCEPTION-DENORMAL = 4;
define constant $EXCEPTION-FLT-DIVIDE-BY-ZERO = 5;
define constant $EXCEPTION-FLT-INEXACT-RESULT = 6;
define constant $EXCEPTION-FLT-INVALID-OPERATION = 7;
define constant $EXCEPTION-FLT-OVERFLOW = 8;
define constant $EXCEPTION-FLT-UNDERFLOW = 9;
define constant $EXCEPTION-FLT-STACK-CHECK = 10;
define constant $EXCEPTION-INT-DIVIDE-BY-ZERO = 11;
define constant $EXCEPTION-NONCONTINUABLE-EXCEPTION = 12;
define constant $EXCEPTION-ILLEGAL-INSTRUCTION = 13;


/// RAISEEXCEPTION (internal)

define function RaiseException (code :: <integer>, #rest args)
 => ()
  error("fake low level exception code %d", code)
end function;
