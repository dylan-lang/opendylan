module:    idvm-harp
Synopsis:  The <idvm-hilo> class for HILO encodings for the IDVM backend
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define class <idvm-hilo> (<object>)
  slot idvm-hi, required-init-keyword: hi:;
  slot idvm-lo, required-init-keyword: lo:;
end;

define method print-object
   (object :: <idvm-hilo>, stream :: <stream>)
  format(stream, "{idvm-hilo %=, %=}", object.idvm-hi, object.idvm-lo);
end;
