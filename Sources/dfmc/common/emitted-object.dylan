Module:   dfmc-common
Synopsis: Compiler objects that are emitted entities with identity. 
          This includes modeled indirect objects but not direct objects
          since multiple copies of these are emitted.
Author:   Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Modeled objects.

define compiler-open abstract dood-class <emitted-object> (<object>)
  weak slot emitted-name = #f,
    reinit-expression: #f,
    init-keyword: emitted-name:;
end;

define compiler-open generic emitted-name (object);
define compiler-open generic emitted-name-setter (name, object);

// eof
