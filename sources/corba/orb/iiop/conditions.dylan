Module: orb-iiop
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <invalid-encapsulation-length> (<simple-warning>)
  keyword format-string: = "Invalid encapsulation size stored as %d (really %d) while unmarshalling %= from %=";
end class;

define sealed domain make (subclass(<invalid-encapsulation-length>));
define sealed domain initialize (<invalid-encapsulation-length>);

define method make (class == <invalid-encapsulation-length>, #key length, actual-length, object, stream)
 => (condition :: <invalid-encapsulation-length>)
  next-method(class, format-arguments: vector(length, actual-length, object, stream));
end method;

define class <non-zero-string-terminator> (<simple-warning>)
  keyword format-string: = "Non-zero IIOP string terminator encountered while unmarshalling %= from %=";
end class;

define sealed domain make (subclass(<non-zero-string-terminator>));
define sealed domain initialize (<non-zero-string-terminator>);

define method make (class == <non-zero-string-terminator>, #key string, stream)
 => (condition :: <non-zero-string-terminator>)
  next-method(class, format-arguments: vector(string, stream))
end method;

