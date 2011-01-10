Module:    dfmc-back-end-protocol
Author:    Jonathan Bachrach
Synopsis:  Compiler-front-end independent back-end
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define method raw-mangle (back-end :: <back-end>, name) => (res :: <byte-string>)
  mangle-name-raw(mangler(back-end), name)
end method;
