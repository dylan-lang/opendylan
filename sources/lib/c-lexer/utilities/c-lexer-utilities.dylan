Module: c-lexer-utilities-internal
Author: Toby Weinberg
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method print-comma-separated-sequence
    (the-sequence :: <collection>, the-stream :: <stream>)
 => ();
  print-separated-sequence(the-sequence, the-stream, ", ");
end method;

define method print-separated-sequence 
    (the-sequence :: <collection>, the-stream :: <stream>,
     the-separator :: <string>, #key map-function = identity)
 => ();
  let (initial-state, limit, next-state, finished?, current-key, 
       current-element) 
    = forward-iteration-protocol(the-sequence);
  if (~empty?(the-sequence))
    print(map-function(current-element(the-sequence, initial-state)), 
	  the-stream);
    for (state = next-state(the-sequence, initial-state) 
	   then next-state(the-sequence, state),
	 until: finished?(the-sequence, state, limit))
      format(the-stream, the-separator);
      print(map-function(current-element(the-sequence, state)), the-stream);
    end for;
  end if;
end method;

// slot shared by all parsing filters, cpp streams and below
define generic source-name 
    (the-stream :: <wrapper-stream>) => (the-name :: <string>);

