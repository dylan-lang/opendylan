Module: orb-streams
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <giop-message-error> (<simple-error>)
  keyword format-string: = "cannot recognise GIOP message on %=";
end class;

define sealed domain make (subclass(<giop-message-error>));
define sealed domain initialize (<giop-message-error>);

define method make (class == <giop-message-error>, #key stream)
 => (condition :: <giop-message-error>)
  next-method(class, format-arguments: vector(stream));
end method;

