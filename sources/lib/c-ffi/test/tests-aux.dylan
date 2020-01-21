Module:    c-ffi-test
Author:    Peter Benson
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// C string tests

/// Treat X as a C-string and return the OFFSETth element
define c-callable-wrapper $c-string-value of
    method (x :: <C-string>, offset :: <integer>) => (r :: <Character>);
      pointer-value(x, index: offset);
    end
  parameter x :: <C-string>;
  parameter offset :: <C-unsigned-int>;
  result r :: <C-character>;
  c-name: "c_string_value";
end;

/// Treat X as a C-string and set the OFFSETth element to NEW
define c-callable-wrapper $c-string-value-setter of
    method (new :: <character>,
            x :: <C-string>,
            offset :: <integer>) => (r :: <character>);
      pointer-value(x, index: offset) := new;
    end
  parameter new :: <c-character>;
  parameter x :: <C-string>;
  parameter offset :: <C-unsigned-int>;
  result r :: <C-character>;
  c-name: "c_string_value_setter";
end;

// tests for bug 209

define c-callable-wrapper wilma of betty
  parameter a :: <C-int>;
  result r :: <C-int>;
  c-name: "betty";
end c-callable-wrapper;
