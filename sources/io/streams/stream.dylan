Module:       streams-internals
Synopsis:     Class, method, and generic function definitions for streams
Author:       Scott McKay, Eliot Miranda, Toby Weinberg
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Stream constants

define constant $input        = 1;
define constant $output       = 2;
define constant $input-output = 3;
define constant $closed       = 4;


/// The basic stream class

define open abstract primary class <basic-stream> (<stream>)
  slot outer-stream :: false-or(<stream>),
    init-keyword: outer-stream:;
  slot private-stream-element-type-value :: <type> = <object>,
    init-keyword: element-type:;
  slot private-stream-direction-value :: <integer>; //  = $input,
   //   init-keyword: direction:;
  constant slot private-stream-lock-value :: <lock> = make(<recursive-lock>);
end class <basic-stream>;

define method initialize
    (stream :: <basic-stream>,
     #key start: _start, end: _end, direction: _direction = #"input")
 => ()
  next-method();
  unless (slot-initialized?(stream, outer-stream))
    stream.outer-stream := stream
  end;
  unless (instance?(stream, <wrapper-stream>))  
    select (_direction)
      #"input" => #f;
      #"output", #"input-output" =>
	if (_start | _end)
	  error("START: and END: keywords are not allowed for output streams");
	end;
      otherwise =>
	error("%= is not one of %=, %=, or %=",
	      _direction, #"input", #"output", #"input-output");
    end select;
    stream-direction(stream) := _direction;
  end unless;
end method initialize;


define method stream-element-type 
    (the-stream :: <basic-stream>) => (result :: <type>)
  the-stream.private-stream-element-type-value
end method stream-element-type;

define sealed generic stream-element-type-setter 
    (value :: <type>, the-stream :: <stream>) => (result :: <type>);

define method stream-element-type-setter
    (the-type :: <type>, the-stream :: <basic-stream>) => (result :: <type>)
  the-stream.private-stream-element-type-value := the-type;
  the-type
end method stream-element-type-setter;

ignore(stream-element-type-setter);

define open generic stream-direction
    (stream :: <stream>) => (direction);

define sealed generic stream-direction-setter 
    (direction, the-stream :: <stream>) => (direction);

define inline method stream-direction 
    (the-stream :: <basic-stream>) => (result)  
  select (the-stream.private-stream-direction-value)
    $input-output => #"input-output";
    $input => #"input";
    $output => #"output";
    $closed => #"closed";
  end select 
end method stream-direction;

define inline method stream-direction-setter 
    (the-direction, the-stream :: <basic-stream>) => (result)
  the-stream.private-stream-direction-value
    := select (the-direction)
	 #"input-output" => $input-output;
	 #"input" => $input;
	 #"output" => $output;
	 #"closed" => $closed;
       end select;
  the-direction
end method stream-direction-setter;

// Change these to use bit fields when everything works again

define inline function readable? 
    (the-stream :: <basic-stream>) => (result :: <boolean>)
  logand(the-stream.private-stream-direction-value, $input) ~== 0
end function;

define inline function writable? 
    (the-stream :: <basic-stream>) => (result :: <boolean>)
  logand(the-stream.private-stream-direction-value, $output) ~== 0
end function;

define inline function closed? 
    (the-stream :: <basic-stream>) => (result :: <boolean>)
  (the-stream.private-stream-direction-value == $closed)
end function;

define inline function read-only? 
    (the-stream :: <basic-stream>) => (result :: <boolean>)
  (the-stream.private-stream-direction-value == $input)
end function;

define inline function write-only? 
    (the-stream :: <basic-stream>) => (result :: <boolean>)
  (the-stream.private-stream-direction-value == $output)
end function;

define inline function read-write? 
    (the-stream :: <basic-stream>) => (result :: <boolean>)
  (the-stream.private-stream-direction-value == $input-output)
end function;

define open generic stream-lock
    (stream :: <basic-stream>) => (lock);

define sealed generic stream-lock-setter 
    (value, the-stream :: <basic-stream>) => (result>);

define method stream-lock (the-stream :: <basic-stream>) => (result>)
  the-stream.private-stream-lock-value
end method stream-lock;

define method stream-lock-setter
    (the-lock, the-stream :: <basic-stream>) => (result)
  the-stream.private-stream-lock-value := the-lock;
  the-lock
end method stream-lock-setter;

/// Stream query functions, common to all streams

define method close
    (stream :: <basic-stream>,
     #rest keys, 
     #key abort? :: <boolean>, 
     wait? :: <boolean>,
     synchronize? :: <boolean>) => ()
  ignore(keys, abort?, wait?, synchronize?);
  stream.outer-stream := #f;
  stream.private-stream-direction-value := $closed;
  next-method ();
end method close;

define method stream-open? (stream :: <basic-stream>) => (open? :: <boolean>)
  ~closed?(stream)
end method stream-open?;

define method stream-at-end? (stream :: <basic-stream>) => (at-end? :: <boolean>)
  #f
end method stream-at-end?;



/// Positionable stream protocol

define open abstract class <basic-positionable-stream> (<basic-stream>,
							<positionable-stream>)
  slot initial-position :: <position-type> = 0;
  slot current-position :: <position-type> = 0;
  slot final-position :: <position-type> = 0;
end class <basic-positionable-stream>;

/* These are a problem, something in the compiler?
define open generic initial-position
    (stream :: <basic-positionable-stream>) => (result :: <position-type>);

define open generic initial-position-setter
    (the-position :: <position-type>, stream :: <basic-positionable-stream>) 
 => (result :: <position-type>);

define open generic current-position
    (stream :: <basic-positionable-stream>) => (result :: <position-type>);

define open generic current-position-setter
    (the-position :: <position-type>, stream :: <basic-positionable-stream>) 
 => (result :: <position-type>);

define open generic final-position
    (stream :: <basic-positionable-stream>) => (result :: <position-type>);

define open generic final-position-setter
    (the-position :: <position-type>, stream :: <basic-positionable-stream>) 
 => (result :: <position-type>);
*/

define open generic stream-limit (stream :: <stream>)
 => (limit :: false-or(<position-type>));

// Most streams have no limit
define method stream-limit
    (stream :: <stream>) => (limit :: singleton(#f))
  #f
end method stream-limit;

define open generic stream-limit-setter
    (limit :: false-or(<position-type>), stream :: <stream>)
 => (limit :: false-or(<position-type>));

/// Positionable stream implementation

define method stream-position
    (stream :: <basic-positionable-stream>) => (position :: <position-type>)
  stream.current-position - stream.initial-position
end method stream-position;

define method stream-position-setter
    (position :: <position-type>, stream :: <basic-positionable-stream>)
 => (position :: <position-type>)
  let limit = (stream-direction(stream) == #"input")
              & stream-limit(stream);
  let new-position = position + stream.initial-position;
  if (position < stream.initial-position
      | (limit & new-position > limit))
    error("Invalid position: %=", position);
  else
    stream.current-position := new-position
  end;
  position
end method stream-position-setter;

define method stream-position-setter
    (position == #"start", stream :: <basic-positionable-stream>)
 => (position :: <position-type>)
  stream-position(stream) := 0
end method stream-position-setter;

define method stream-position-setter
    (position == #"end", stream :: <basic-positionable-stream>)
 => (position :: <position-type>)
  stream-position(stream)
    := stream-limit-or-error(stream) - stream.initial-position
end method stream-position-setter;

define method adjust-stream-position
    (stream :: <basic-positionable-stream>, delta :: <integer>,
     #key from = #"current")
 => (position :: <position-type>)
  stream-position(stream)
    := select (from)
	 #"current" => stream-position(stream) + delta;
	 #"start"   => delta;
	 #"end"     => stream-limit-or-error(stream) + delta;
       end
end method adjust-stream-position;

/*
define method \<
    (p1 :: <stream-position>, p2 :: <stream-position>) => (result :: <boolean>)
  //---*** Implement this
end method \<;

define method \=
    (p1 :: <stream-position>, p2 :: <stream-position>) => (result :: <boolean>)
  //---*** Implement this
end method \=;
*/


/// Readable stream protocol

define method stream-input-available?
    (stream :: <basic-stream>) => (available? :: <boolean>)
  #t
end method stream-input-available?;


/// Writable stream protocol

// Everything about force-output except doesn't wait if the stream is
// asynchronous.
define generic do-force-output
    (stream :: <stream>) => ();

define method do-force-output
    (stream :: <stream>) => ()
  ignore(stream);
end method;


/// Stream contents accessing protocol

define generic stream-sequence-class
    (stream :: <stream>) => (class /* ---*** :: subclass(<sequence>) */);

define method stream-sequence-class
    (stream :: <stream>) => (class /* ---*** :: subclass(<sequence>) */)
  <vector>
end method stream-sequence-class;


/// Stream locking

define open generic stream-locked?
    (stream :: <stream>) => (locked? :: <boolean>);

define method stream-locked?
    (stream :: <stream>) => (locked? :: <boolean>)
  stream-lock(stream)
    & stream-lock(stream).owned?
end method stream-locked?;

define open generic lock-stream
    (stream :: <stream>) => ();

define method lock-stream (stream :: <stream>) => ()
  wait-for(stream-lock(stream));
end method lock-stream;

define open generic unlock-stream
    (stream :: <stream>) => ();

define method unlock-stream (stream :: <stream>) => ()
  release(stream-lock(stream))
end method unlock-stream;

define macro with-stream-locked
  { with-stream-locked (?stream:expression) ?:body end }
  => { begin
	 let _stream = ?stream;
	 block ()
	   lock-stream(_stream);
	   ?body
	 cleanup
	   unlock-stream(_stream)
	 end 
       end }
end macro with-stream-locked;


/// "High performance" functions

define open generic read-skip
    (stream :: <stream>, n :: <integer>) => ();

define method read-skip
    (stream :: <stream>, n :: <integer>) => ()
  for (i :: <integer> from 0 below n)
    read-element(stream)
  end
end method read-skip;


define open generic write-fill
    (stream :: <stream>, element :: <object>, n :: <integer>) => ();

define method write-fill
    (stream :: <stream>, elt :: <object>, n :: <integer>) => ()
  for (i :: <integer> from 0 below n)
    write-element(stream, elt)
  end
end method write-fill;
