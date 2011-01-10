Module: orb-utilities
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define variable *architecture-little-endian?* :: <boolean> = $architecture-little-endian?;

define method architecture-little-endian? ()
  *architecture-little-endian?*
end method;

define method architecture-little-endian?-setter (little-endian? :: <boolean>)
  *architecture-little-endian?* := little-endian?
end method;

define macro with-architecture-little-endian?
  { with-architecture-little-endian? (?little-endian:expression) ?body:body end }
    => {
	invoke-architecture-little-endian(?little-endian, method () ?body end method)
	}
end macro;

define method invoke-architecture-little-endian (little-endian? :: <boolean>, function :: <function>)
  let old-little-endian? :: <boolean> = architecture-little-endian?();
  block ()
    architecture-little-endian?() := little-endian?;
    function();
  cleanup
    architecture-little-endian?() := old-little-endian?;
  end block;
end method;

define method operating-system-user ()
 => (user :: <string>)
  login-name() | owner-name()  | "unknown"
end method;


define locked variable *name-count* :: <integer> = 0;

define method generate-name (name :: <string>)
 => (genname :: <string>)
  format-to-string("%s-%d", name, atomic-increment!(*name-count*));
end method;

