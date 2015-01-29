Module:   dfmc-conditions-implementation
Author:   Paul Haahr
Synopsis: Native equivalents for emulator workarounds.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
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
                      init-arguments: vector(format-string:, ?abort))
             #f
           exception (r :: <simple-restart>,
                      init-arguments: vector(format-string:, ?retry))
             _loop_()
           end
         end;
         _loop_() }
end macro;
