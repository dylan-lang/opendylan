Module:    environment-framework
Synopsis:  Environment Framework
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Update handling

define open abstract class <frame-refresh-mixin> (<frame>)
  constant slot frame-auto-refresh? = #t,
    init-keyword: auto-refresh?:;
end class <frame-refresh-mixin>;

define open generic refresh-frame (frame) => ();

define method refresh-frame (frame :: <frame>) => ()
  #f
end method refresh-frame;

define method refresh-frame (sheet :: <sheet>) => ()
  let frame = sheet-frame(sheet);
  if (frame)
    refresh-frame(frame)
  end
end method refresh-frame;

define method frame-auto-refresh? 
    (frame :: <frame>) => (auto-refresh? :: <boolean>)
  #t
end method frame-auto-refresh?;

define method auto-refresh-frame (frame :: <frame-refresh-mixin>) => ()
  if (frame-auto-refresh?(frame))
    refresh-frame(frame)
  end
end method auto-refresh-frame;
