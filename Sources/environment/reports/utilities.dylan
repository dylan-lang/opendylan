Module:    environment-reports
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define macro increment!
  { increment! (?place:expression, ?value:expression) }
    => { ?place := ?place + ?value; }
  { increment! (?place:expression) }
    => { ?place := ?place + 1; }
end macro increment!;

define macro decrement!
  { decrement! (?place:expression, ?value:expression) }
    => { ?place := ?place - ?value; }
  { decrement! (?place:expression) }
    => { ?place := ?place - 1; }
end macro decrement!;

