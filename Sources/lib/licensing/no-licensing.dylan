Module:    licensing
Author:    Gary Palter
Synopsis:  License validate for Functional Developer
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define abstract class <license-validation-failure> (<error>)
end class <license-validation-failure>;

define function validate-license (console-tools? :: <boolean>) => ()
  ignore(console-tools?);
end function validate-license;

define function license-info ()
 => (serial-number :: <byte-string>, evaluation? :: <boolean>, expiration :: false-or(<date>),
     user :: false-or(<byte-string>), company :: false-or(<byte-string>))
  values("0",
	 #f,
	 #f,
	 "Internal Developer",
	 "Functional Objects, Inc.");
end function license-info;


define function unregistered-products () => (products :: <sequence>)
  #[]
end;

define function register-product
    (product, serial :: <byte-string>, key :: <byte-string>)
 => (registered? :: <boolean>)
  ignore(product, serial, key);
  #f
end;
