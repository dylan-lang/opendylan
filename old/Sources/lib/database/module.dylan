module:    dylan-user
author:    jonathan bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module database
  use dylan;
  use internal, import: {allocate};
  use functional-extensions, rename: {position => hq-position};
  use mop;
  use syntax-case;
  use streams, rename: {close => es-close, <abstract-stream> => <stream>};
  use variable-search;
  use byte-vector;
end;

// eof
