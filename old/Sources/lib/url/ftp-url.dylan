Module: url-internals
Author: James Casey
Synopsis: URLs as defined in draft-fielding-url-syntax-03
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <ftp-url> (<generic-url>)
  inherited slot url-scheme, init-value: "ftp";
end class <ftp-url>;

define constant ftp-default-port :: <integer> = 21;

define method url-default-port(this :: <ftp-url>)
 => (retval :: false-or(<integer>))
  ftp-default-port
end method url-default-port;

define method url-scheme-symbol-as-class(this == #"ftp")
 =>(retval :: <class>)
  <ftp-url>
end method url-scheme-symbol-as-class;
