Module:       format-internals
Author:       Keith Playford, Andy Armstrong
Synopsis:     Use condition-to-string to display a condition
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define sideways method print-object (c :: <condition>, s :: <stream>) => ()
  let message = condition-to-string(c);
  if (*print-escape?* | ~message)
    printing-object (c, s)
      if (message)
	format(s, ": %s", message)
      end
    end
  else
    write(s, message)
  end
end method print-object;
