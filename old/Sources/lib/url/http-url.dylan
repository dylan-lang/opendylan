Module: url-internals
Author: James Casey
Synopsis: URLs as defined in draft-fielding-url-syntax-03
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <http-url> (<generic-url>)
  inherited slot url-scheme, init-value: "http";
end class <http-url>;

define constant http-default-port :: <integer> = 80;

define method url-default-port(this :: <http-url>)
 => (retval :: false-or(<integer>))
  http-default-port
end method url-default-port;

define method url-scheme-symbol-as-class(this == #"http")
 =>(retval :: <class>)
  <http-url>
end method url-scheme-symbol-as-class;

// Override slot accessors which are not defined for this url type
//
define method url-user(this :: <http-url>)
 => (retval :: false-or(<byte-string>))
  #f
end method url-user;

define method url-password(this :: <http-url>)
 => (retval :: false-or(<byte-string>))
  #f
end method url-password;

