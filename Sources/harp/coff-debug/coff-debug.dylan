module:    coff-debug
Synopsis:  Support for reading, writing and printing COFF files
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



define open generic write-coff-file (file, data :: <coff-file>) => ();


define method write-coff-file (file :: <string>, data :: <coff-file>) => ()
  write-coff-file(as(<file-locator>, file), data);
end method;

define method write-coff-file (file :: <locator>, data :: <coff-file>) => ()
  let stream = make(<file-stream>, locator: file, direction: output:);
  block ()
    write-coff-file(stream, data);
  cleanup
    close(stream);
  end block;
end method;


define method write-coff-file (file :: <stream>, data :: <coff-file>) => ()
  write-coff(file, data);
end method;
