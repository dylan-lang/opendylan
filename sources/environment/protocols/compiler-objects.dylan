Module:    environment-protocols
Synopsis:  Environment Protocols
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Program objects

define sealed abstract class <compiler-object> (<environment-object>)
  sealed slot compiler-object-proxy,
    required-init-keyword: compiler-object-proxy:;
end class <compiler-object>;

define sealed domain make (subclass(<compiler-object>));
define sealed domain initialize (<compiler-object>);

define open generic invalidate-compiler-proxy
    (server :: <server>, object :: <compiler-object>)
 => ();
