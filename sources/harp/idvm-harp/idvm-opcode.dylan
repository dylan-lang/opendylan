module:    idvm-harp
Synopsis:  The <idvm-opcode> class for the IDVM backend
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define class <idvm-opcode> (<object>)
  slot opcode-name :: <symbol>, required-init-keyword: opcode-name:
end;


define method make-idvm-opcode (name :: <symbol>) => <idvm-opcode>;
  let full-name = as(<symbol>, concatenate("idvm-", as(<string>, name)));
  make(<idvm-opcode>, opcode-name: full-name);
end method;

