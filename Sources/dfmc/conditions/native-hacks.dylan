Module:   dfmc-conditions-implementation
Author:   Paul Haahr
Synopsis: Native equivalents for emulator workarounds.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Should offer abort and retry cleanups.

// Note: see emulator-specific version.

define macro with-simple-abort-retry-restart
  { with-simple-abort-retry-restart (?abort:expression, ?retry:expression) 
      ?:body
    end }
    => { local method _loop_ ()
           block () 
             ?body
           exception (r :: <simple-restart>, 
                      init-arguments: #[format-string:, ?abort])
             #f
           exception (r :: <simple-restart>, 
                      init-arguments: #[format-string:, ?retry])
             _loop_()
           end
         end;
         _loop_() }
end macro;

// with-program-restarts is used to hide the fact that restarts aren't
// really conditions in the emulator.  In real Dylan, this can just
// become a block clause.

define macro with-program-restarts
  { with-program-restarts ?:body ?catchers end }
    => { block () ?body ?catchers end }
 catchers:
  { }
    => { }
  { restart (?spec:*) ?:body ... }
    => { exception (?spec) ?body ... }
end macro with-program-restarts;


// eof
