module: dood
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $dood-queue-initial-size = 2048;
// define constant <dood-queue-element>     = <integer>;
define constant <dood-queue-element>     = <object>;
define constant <dood-queue-data>        = limited(<vector>, of: <dood-queue-element>);

define class <dood-queue> (<object>)
  slot queue-data :: <dood-queue-data>
    = make(<dood-queue-data>, size: $dood-queue-initial-size);
  slot queue-out :: <integer> = 0;
  slot queue-in  :: <integer> = 0;
end class;

define inline method dood-queue-empty? (x :: <dood-queue>) => (well? :: <boolean>)
  queue-out(x) = queue-in(x)
end method;

// define method dood-queue-capacity (x :: <dood-queue>) => (res :: <integer>)
//   size(queue-data(x))
// end method;

define inline function next-index
    (x :: <dood-queue>, i :: <integer>) => (res :: <integer>)
  modulo(i + 1, size(queue-data(x)))
end function;

define method dood-queue-push-last 
    (x :: <dood-queue>, e :: <dood-queue-element>) => (res :: <dood-queue-element>)
  without-bounds-checks
    queue-data(x)[queue-in(x)] := e;
    queue-in(x) := next-index(x, queue-in(x));
    if (queue-in(x) = queue-out(x))
      let old-data = queue-data(x);
      let old-size = size(old-data);
      let old-out  = queue-out(x);
      let new-data :: <dood-queue-data>
	= make(<dood-queue-data>, size: old-size * 2);
      let delta = size(new-data) - old-size;
	for (i :: <integer> from 0 below old-out)
	  new-data[i] := old-data[i];
	finally
	  for (j :: <integer> from i below old-size)
	    new-data[j + delta] := old-data[j];
	  end for;
	end for;
      queue-out(x)  := old-out + delta; // MAKE ROOM IN MIDDLE
      queue-data(x) := new-data;
    end if;
  end without-bounds-checks;
  e			  
end method;

define inline method dood-queue-out (x :: <dood-queue>) => (res :: <integer>)
  queue-out(x)
end method;

define inline method dood-queue-out-setter (i :: <integer>, x :: <dood-queue>)
  queue-out(x) := i;
end method;

define inline method dood-queue-in (x :: <dood-queue>) => (res :: <integer>)
  queue-in(x)
end method;

define inline method dood-queue-in-setter (i :: <integer>, x :: <dood-queue>)
  queue-in(x) := i;
end method;

define macro with-saved-queue-frame
  { with-saved-queue-frame (?queue:expression, ?next-in:expression)
      ?:body
    end }
     => { let saved-in  = dood-queue-in(?queue);
	  let saved-out = dood-queue-out(?queue);
	  block ()
	    dood-queue-out(?queue) := ?next-in;
	    dood-queue-in(?queue)  := ?next-in;
	    ?body
          afterwards
	    dood-queue-in(?queue)  := saved-in;
	    dood-queue-out(?queue) := saved-out;
	  end block }
end macro;

define inline method dood-queue-pop 
    (x :: <dood-queue>) => (res :: <dood-queue-element>)
  without-bounds-checks
    let e = queue-data(x)[queue-out(x)];
    queue-out(x) := next-index(x, queue-out(x));
    e
  end without-bounds-checks;
end method;

