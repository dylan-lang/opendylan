Module: orb-connections
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <orphan-response-error> (<simple-error>)
  keyword format-string: = "Found response id %= without outstanding request";
end class;

define sealed domain make (subclass(<orphan-response-error>));
define sealed domain initialize (<orphan-response-error>);

define method make (class == <orphan-response-error>, #key message-header, message-id, requests)
 => (condition :: <orphan-response-error>)
  next-method(class, format-arguments: vector(message-id,
					      message-header,
					      requests));
end method;

