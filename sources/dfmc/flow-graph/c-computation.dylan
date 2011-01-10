Module:   dfmc-flow-graph
Author:   Jonathan Bachrach, Keith Playford, and Paul Haahr
Synopsis: computation classes for C-FFI
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define graph-class <primitive-indirect-call> (<primitive-call>)
end graph-class;

define graph-class <c-variable-pointer-call> (<primitive-call>)
  slot c-variable, init-keyword: c-variable:;
end graph-class;

define graph-class <begin-with-stack-structure> (<computation>)
  temporary slot wss-size-temp, required-init-keyword: wss-size-temp:;
  slot wss-var :: <lexical-variable>, required-init-keyword: wss-var:;
  slot end-wss :: <end-with-stack-structure>;
end graph-class;

define graph-class <end-with-stack-structure> (<computation>)
  slot begin-wss :: <begin-with-stack-structure>, init-keyword: begin-wss:;
end graph-class;
