module: plugin-othello
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// for vmtether
define macro c-pointer-type-definer
  { define c-pointer-type ?new-name:name => ?ref-type:expression }
    => { define constant ?new-name = ?ref-type.pointer-type }
end;
