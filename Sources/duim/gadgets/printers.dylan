Module:       duim-gadgets-internals
Synopsis:     'print-object' methods for DUIM gadgets
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define method print-object
    (gadget :: <labelled-gadget-mixin>, stream :: <stream>) => ()
  let label = gadget-label(gadget);
  if (instance?(label, <string>))
    printing-object (gadget, stream, type?: #t, identity?: #t)
      format(stream, "%=", label)
    end
  else
    next-method()
  end
end method print-object;
