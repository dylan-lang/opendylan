Module:   dfmc-modeling
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define method print-object (t :: <&singleton> , s)
  format(s, "^%=", t.^singleton-object);
end;

define method print-object (u :: <&union> , s)
  format(s, "#u[");
  for (x in u.^union-members,
       first? = #t then #f)
    unless(first?)
      format(s, ", ");
    end;
    format(s, "%s", x);
  end;
  format(s, "]");
end;

