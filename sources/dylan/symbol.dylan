Module:    internal
Author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// BOOTED: define ... class <symbol> ... end;

define sealed inline method as (class == <symbol>, string :: <string>)
 => (result :: <symbol>)
  make(<symbol>, name: as(<byte-string>, string))
end method as;

define sealed inline method as (class == <byte-string>, symbol :: <symbol>)
 => (result :: <byte-string>)
  symbol.symbol-name
end method;

define sealed inline method as (class == <string>, symbol :: <symbol>)
 => (result :: <string>)
  as(<byte-string>, symbol);
end method as;

define sealed inline method \=
    (symbol-1 :: <symbol>, symbol-2 :: <symbol>) => (well? :: <boolean>)
  as(<string>, symbol-1) = as(<string>, symbol-2)
end method \=;

define sealed inline method \<
    (symbol-1 :: <symbol>, symbol-2 :: <symbol>) => (well? :: <boolean>)
  as(<string>, symbol-1) < as(<string>, symbol-2)
end method \<;
