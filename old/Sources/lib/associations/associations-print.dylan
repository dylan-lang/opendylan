Module:    associations
Author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method print
    (collection :: <associations>, #key stream = *standard-output*, verbose?)
  print-unreadable-object (collection, stream)
    format(stream, "[ASSOCS ");
    for (first? = #t then #f, association in collection.associations)
      if (first?)
        format(stream, " ")
      end if;
      format(stream, "(");
      print(association.key, stream: stream, verbose?: verbose?);
      format(stream, " -> ");
      print(association.value, stream: stream, verbose?: verbose?);
      format(stream, ")");
    end for;
    format(stream, "]")
  end print-unreadable-object
end method print;
