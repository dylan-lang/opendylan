Module:       duim-frames-internals
Synopsis:     'print-object' methods for DUIM frames
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define method print-object
    (command-table :: <command-table>, stream :: <stream>) => ()
  printing-object (command-table, stream, type?: #t, identity?: #t)
    format(stream, "%=", command-table-name(command-table))
  end
end method print-object;
