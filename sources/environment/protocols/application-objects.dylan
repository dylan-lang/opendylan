Module:    environment-protocols
Synopsis:  Environment Protocols
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Application objects

define sealed abstract class <application-object> (<environment-object>)
  //---*** Ideally this would be a required keyword
  sealed slot application-object-proxy = #f,
    init-keyword: application-object-proxy:;
end class <application-object>;

define sealed abstract class <application-code-object> (<application-object>)
end class <application-code-object>;

define sealed domain make (subclass(<application-object>));
define sealed domain initialize (<application-object>);

define open generic invalidate-application-proxy
    (server :: <server>, object :: <application-object>)
 => ();


/// Unbound names

define class <unbound-object> (<application-object>)
end class <unbound-object>;

define constant $unbound-object
  = make(<unbound-object>, application-object-proxy: #f);

define constant $unbound-primitive-name = "unbound";

define method environment-object-primitive-name
    (server :: <server>, object :: <unbound-object>)
 => (name :: <string>)
  $unbound-primitive-name
end method environment-object-primitive-name;
