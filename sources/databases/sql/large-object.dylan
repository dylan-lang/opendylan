Module: sql-implementation
Author: eec
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// $HopeName$


define open abstract class <large-object> (<inner-stream>)
end class;


define sealed class <odbc-large-object> (<large-object>)
  slot stream-element-type :: <type> = <object>,
    init-keyword: element-type:;

  slot stream-direction :: <symbol> = #"output",
    init-keyword: direction:;
end class;

define method close(large-object :: <odbc-large-object>, #rest keys, #key)
  => ()

end method;

define method stream-open?(large-object :: <odbc-large-object>)
  => (open? :: <boolean>)
end method;

define method stream-at-end?(large-object :: <odbc-large-object>)
 => (at-end? :: <boolean>)

end method;

define method initialize(large-object :: <odbc-large-object>,
			 #key start: _start, end: _end)
  => ()
  next-method();
end method;

define method read-element(large-object :: <large-object>, 
			   #key on-end-of-stream)
 => (element :: <object>)

end method;

define method unread-element(large-object :: <large-object>, 
			     element :: <object>)
  => (element :: <object>)

end method;

define method peek(large-object :: <large-object>,
		   #key on-end-of-stream)
  => (element :: <object>)

end method;

define method read(large-object :: <large-object>, n :: <integer>,
		   #key on-end-of-stream)
  => (sequence-or-eof :: <object>)

end method;

define method read-into!(large-object :: <large-object>, n :: <integer>,
			 sequence :: <mutable-sequence>,
			 #key start, on-end-of-stream)
  => (count-or-eof :: <object>)

end method;

define method discard-input(large-object :: <large-object>) => ()

end method;

define method stream-input-available?(large-object :: <large-object>)
  => (available? :: <boolean>)

end method;


define method stream-contents(large-object :: <large-object>,
			      #key clear-contents?)
  => (sequence :: <sequence>)

end method;

define method stream-contents-as(type :: <type>, 
				 large-object :: <large-object>,
				 #key clear-contents?)
  => (sequence :: <sequence>)

end method;

define method stream-sequence-class(large-object :: <large-object>)
  => (class :: <sequence>)

end method;


define method read-skip(large-object :: <large-object>, n :: <integer>)
  => ()

end method;