Module: dfmc-java-back-end
Author: Mark Tillotson
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



define constant $initial-string-stream-contents-size = 10000;

define sealed class <java-back-end> (<back-end>)
  constant slot lambda-stream = make(<string-stream>,
                                     direction: #"output",
                                     contents:  make(<byte-string>,
                                     size:      $initial-string-stream-contents-size));
  slot current-module  = #f;
end;

register-back-end(<java-back-end>, #"java", #f, #f);

define method initialize (back-end :: <java-back-end>, #key, #all-keys) => ()
  next-method ();
  stream-contents (back-end.lambda-stream, clear-contents?: #t);
end method;

