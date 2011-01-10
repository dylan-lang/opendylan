module:    harp-instructions
Synopsis:  Output functions for the major instruction types.
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method output-error 
    (backend :: <harp-back-end>, op :: <op>, #rest args)
  error("output-instruction for %= with %= called in backend.",
        op, args);
end;

define output-function none
    (backend :: <harp-back-end>, op)
  output-instruction(backend, op);
end;

define output-function tuuu
    (backend :: <harp-back-end>, op, taag, use1, use2, use3)
  output-instruction(backend, op, taag, #f, use1, use2, use3);
  conditional-branch-windup(backend, taag);
end;


define output-function tuu
    (backend :: <harp-back-end>, op, taag, use1, use2)
  output-instruction(backend, op, taag, #f, use1, use2);
  conditional-branch-windup(backend, taag);
end;

define output-function tduu
    (backend :: <harp-back-end>, op, taag, def, use1, use2)
  output-instruction(backend, op, taag, def, use1, use2);
  conditional-branch-windup(backend, taag);
end;

define output-function tdduu
    (backend :: <harp-back-end>, op, taag, def1, def2, use1, use2)
  output-instruction(backend, op, taag, vector(def1, def2), use1, use2);
  conditional-branch-windup(backend, taag);
end;

define output-function td
    (backend :: <harp-back-end>, op, taag, def)
  output-instruction(backend, op, taag, def);
  conditional-branch-windup(backend, taag);
end;

define output-function t
    (backend :: <harp-back-end>, op, taag)
  output-instruction(backend, op, taag);
  conditional-branch-windup(backend, taag);
end;

define output-function tu
    (backend :: <harp-back-end>, op, tag, use1)
  output-instruction(backend, op, tag, #f, use1);
  conditional-branch-windup(backend, tag);
end;

define output-function du
    (backend :: <harp-back-end>, op, def, use1)
  output-instruction(backend, op, #f, def, use1);
end;

define output-function d
    (backend :: <harp-back-end>, op, def)
  output-instruction(backend, op, #f, def);
end;

define output-function u
    (backend :: <harp-back-end>, op, use1)
  output-instruction(backend, op, #f, #f, use1);
end;

define output-function uu
    (backend :: <harp-back-end>, op, u1, u2)
  output-instruction(backend, op, #f, #f, u1, u2);
end;

define output-function duu
    (backend :: <harp-back-end>, op, def, use1, use2)
  output-instruction(backend, op, #f, def, use1, use2);
end;

define output-function uuu
    (backend :: <harp-back-end>, op, u1, u2, u3)
  output-instruction(backend, op, #f, #f, u1, u2, u3);
end;

define output-function ddu
    (backend :: <harp-back-end>, op, d1, d2, u)
  output-instruction(backend, op, #f, vector(d1, d2), u);
end;

define output-function dduu
    (backend :: <harp-back-end>, op, d1, d2, u1, u2)
  output-instruction(backend, op, #f, vector(d1, d2), u1, u2);
end;

define output-function duuu
    (backend :: <harp-back-end>, op, d1, u1, u2, u3)
  output-instruction(backend, op, #f,  d1,  u1, u2, u3);
end;

define output-function duuuu
    (backend :: <harp-back-end>, op, d1, u1, u2, u3, u4)
  output-instruction(backend, op, #f,  d1,  u1, u2, u3, u4);
end;

define output-function uuuu
    (backend :: <harp-back-end>, op, d1, u1, u2, u3)
  output-instruction(backend, op, #f,  #f, d1,  u1, u2, u3);
end;

define output-function dduuu
    (backend :: <harp-back-end>, op, d1, d2, u1, u2, u3)
  output-instruction(backend, op, #f, vector(d1, d2), u1, u2, u3);
end;

