module: dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library debug-dispatch
  use functional-dylan;

  export debug-dispatch;
end;

define module debug-dispatch
  use dylan;
  use dylan-extensions;
  use dispatch-engine;
  use dylan-primitives;
  use simple-format;

  create
    show, wline, disp-line, disp, reset-gf, screw-gf;

end;
