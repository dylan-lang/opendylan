module:    idvm-harp
Synopsis:  Printing support for IDVM references
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define method print-object
   (object :: <idvm-opcode>, stream :: <stream>)
  format(stream, "{idvm-opcode %s}", object.opcode-name);
end;


define method print-reference (stream :: <stream>, 
                               ref :: <constant-reference>, 
                               class-name :: <string>)
  format(stream, "{%s %=}", class-name, ref.cr-refers-to);
end method;


define method print-reference (stream :: <stream>,
                               ref :: <value-constant-reference>, 
                               class-name :: <string>)
  if (slot-initialized?(ref, constant-reference-value))
    format(stream, "{%s %= = %=}", class-name, 
           ref.cr-refers-to, ref.constant-reference-value);
  else
    next-method();
  end if;
end method;



define method print-object
    (ref :: <variable-reader>, stream :: <stream>)
  print-reference(stream, ref, "variable-reader");
end;


define method print-object
    (ref :: <variable-writer>, stream :: <stream>)
  print-reference(stream, ref, "variable-writer");
end;



define method print-object
    (ref :: <variable-definition>, stream :: <stream>)
  print-reference(stream, ref, "variable-definition");
end;

define method print-object
    (ref :: <constant-definition>, stream :: <stream>)
  print-reference(stream, ref, "constant-definition");
end;

define method print-object
    (ref :: <local-constant-reference>, stream :: <stream>)
  print-reference(stream, ref, "local-constant-reference");
end;


