Module:    mini-duim
Synopsis:  Mini-DUIM definitions
Author:    Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// Basic utilities

define macro swap!
  { swap! (?place1:expression, ?place2:expression) }
    => { begin
	   //--- let _value = ?place1;
	   let _value = #f;	//--- work around bug in old compiler
	   _value := ?place1;	//--- ditto
	   ?place1 := ?place2;
	   ?place2 := _value 
	 end }
end macro swap!;


define macro inc!
  { inc! (?place:expression, ?dx:expression) }
    => { ?place := ?place + ?dx; }
  { inc! (?place:expression) }
    => { ?place := ?place + 1; }
end macro inc!;

define macro dec!
  { dec! (?place:expression, ?dx:expression) }
    => { ?place := ?place - ?dx; }
  { dec! (?place:expression) }
    => { ?place := ?place - 1; }
end macro dec!;


define macro min!
  { min! (?place:expression, ?others:*) }
    => { ?place := min(?place, ?others); }
end macro min!;

define macro max!
  { max! (?place:expression, ?others:*) }
    => { ?place := max(?place, ?others); }
end macro max!;


/// Coordinates

define constant $pi   :: <single-float> = 3.14159265;
define constant $2pi  :: <single-float> = 2 * $pi;
define constant $pi/2 :: <single-float> = $pi / 2;

define function radians->degrees (radians :: <real>) => (degrees :: <float>)
  radians * (360 / $2pi)
end function radians->degrees;

define function degrees->radians (degrees :: <real>) => (radians :: <float>)
  degrees * ($2pi / 360)
end function degrees->radians;


define constant <coordinate> = <integer>;

define macro fix-coordinates!
  { fix-coordinates! (?x:expression, ?y:expression) }
    => { ?x := truncate(?x);
         ?y := truncate(?y); }
  { fix-coordinates! (?x:expression, ?y:expression, ?more:*) }
    => { ?x := truncate(?x);
         ?y := truncate(?y);
         fix-coordinates!(?more); }
end macro fix-coordinates!;

define macro convert-to-device-coordinates!
  { convert-to-device-coordinates! (?transform:expression) }
    => { }
  { convert-to-device-coordinates!
      (?transform:expression, ?x:expression, ?y:expression, ?more:*) }
    => { let (_x, _y) = transform-position(?transform, ?x, ?y);
	 ?x := truncate(_x);
         ?y := truncate(_y);
         convert-to-device-coordinates!(?transform, ?more); }
end macro convert-to-device-coordinates!;

define macro convert-to-device-distances!
  { convert-to-device-distances! (?transform:expression) }
    => { }
  { convert-to-device-distances!
      (?transform:expression, ?dx:expression, ?dy:expression, ?more:*) }
    => { let (_dx, _dy) = transform-distance(?transform, ?dx, ?dy);
	 ?dx := truncate(_dx);
         ?dy := truncate(_dy);
         convert-to-device-distances!(?transform, ?more); }
end macro convert-to-device-distances!;

define method do-coordinates
    (function :: <function>, coordinates :: <sequence>) => ()
  let length = size(coordinates);
  for (i = 0 then i + 2, until: i >= length)
    function(coordinates[i], coordinates[i + 1]);
  end
end method do-coordinates;

define method do-endpoint-coordinates
    (function :: <function>, coordinates :: <sequence>) => ()
  let length = size(coordinates);
  for (i = 0 then i + 4, until: i >= length)
    function(coordinates[i], coordinates[i + 1],
	     coordinates[i + 2], coordinates[i + 3]);
  end
end method do-endpoint-coordinates;


/// Simpler table accessors

define function gethash
    (table :: <table>, key, #key default = #f) => (value, found? :: <boolean>)
  let value = element(table, key, default: $unfound);
  if (value == $unfound)
    values(default, #f)
  else
    values(value, #t)
  end
end function gethash;

define function gethash-setter
    (value, table :: <table>, key) => (value)
  table[key] := value
end function gethash-setter;

define function remhash (table :: <table>, key) => ()
  remove-key!(table, key)
end function remhash;

define method remove-all-keys! (table :: <table>)
  for (key in key-sequence(table))
    remove-key!(table, key)
  end
end method remove-all-keys!;


/// Sequence hacking

define method insert-at!
    (sequence :: <stretchy-vector>, item, index) => (sequence :: <stretchy-vector>)
  let make-hole = method (sequence, index)
                    for (i from (size(sequence) - 1) to (index + 1) by -1)
                      sequence[i] := sequence[i - 1]
		    end
		  end method;
  select (index)
    #"start" =>
      add!(sequence, #f);	// grow the vector
      make-hole(sequence, 0);
      sequence[0] := item;
    #"end" =>
      add!(sequence, item);
    otherwise =>
      add!(sequence, #f);	// grow the vector
      make-hole(sequence, index);
      sequence[index] := item;
  end;
  sequence
end method insert-at!;

define method remove-at!
    (sequence :: <stretchy-vector>, index) => (sequence :: <stretchy-vector>)
  let move-down = method (sequence, index)
                    for (i from index to (size(sequence) - 2))
                      sequence[i] := sequence[i + 1]
		    end
		  end method;
  select (index)
    #"start" =>
      move-down(sequence, 0);
      sequence.size := sequence.size - 1;
    #"end" =>
      sequence.size := sequence.size - 1;
    otherwise =>
      move-down(sequence, 0);
      sequence.size := sequence.size - 1;
  end;
  sequence
end method remove-at!;


/// Protocols

define macro protocol-class-definer
  { define protocol-class ?:name (?supers:*) end }
    => { define open abstract class "<" ## ?name ## ">" (?supers)
	 end class;
         define protocol-predicate ?name; }
end macro protocol-class-definer;

define macro protocol-predicate-definer
  { define protocol-predicate ?:name }
    => { define open generic ?name ## "?" (x) => (true? :: <boolean>);
         define method ?name ## "?" (x :: "<" ## ?name ## ">") => (true? :: <boolean>) #t end;
         define method ?name ## "?" (x :: <object>) => (true? :: <boolean>) #f end; }
end macro protocol-predicate-definer;

define macro protocol-definer
  //--- We don't use the name or supers yet...
  { define protocol ?:name (?supers:*) ?slots-and-generics:* end }
    => { ?slots-and-generics }
 slots-and-generics:
  { } => { }
  { ?slot-or-generic:*; ... }
    => { ?slot-or-generic; ... }
 slot-or-generic:
  { getter ?getter-name:name ?getter-arglist:* => ?values:* }
    => { define open generic ?getter-name ?getter-arglist => ?values }
  { getter ?getter-name:name ?getter-arglist:* }
    => { define open generic ?getter-name ?getter-arglist }
  { setter ?setter-name:name ?setter-arglist:* => ?values:* }
    => { define open generic ?setter-name ?setter-arglist => ?values }
  { setter ?setter-name:name ?setter-arglist:* }
    => { define open generic ?setter-name ?setter-arglist }
  { function ?function-name:name ?function-arglist:* => ?values:* }
    => { define open generic ?function-name ?function-arglist => ?values }
  { function ?function-name:name ?function-arglist:* }
    => { define open generic ?function-name ?function-arglist }
end macro protocol-definer;


/// Debug support

define function warn (format-string :: <string>, #rest format-args) => ()
  apply(debug-message,
	concatenate("Warning: ", format-string), format-args)
end function warn;

define method enter-debugger (string, #rest args) => ()
  apply(cerror, "Continue anyway", string, args)
  //--- IDVM only
  //--- apply(dylan-listener-with-restarts, string, args)
end method enter-debugger;

define variable *debug* = #t;
