Module:    doss
Author:    Eliot Miranda
Synopsis:  Simple DOSS load utility
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method load-doss-stream
    (stream :: <stream>) => (object :: <object>)
  block ()
    make(<doss-loader>, stream: stream).fetch-object
  cleanup
    close(stream);
  end
end method load-doss-stream;

define method load-doss-stream
    (string :: <string>) => (object :: <object>)
  load-doss-stream (make (<file-stream>, locator: string, element-type: <byte>))
end method load-doss-stream;
