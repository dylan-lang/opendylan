module:    access-path-implementation
synopsis:  The true and correct definition of remote values in the access
           path
author:    Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// <REMOTE-VALUE>
//    Implemented as a <machine-word>, which is known to be able to
//    carry a C void* without any loss of data.

define constant <remote-value> = <machine-word>;


///// AS-INTEGER
//    Conversion from <remote-value> to <integer> using the coercionn
//    provided by the machine integer implementation. Note that this code
//    assumes unsigned integers (normally addresses), use as-signed-integer
//    if you need signed values.

define generic as-integer
    (x) => (i :: <abstract-integer>);

define inline method as-integer 
    (x :: <remote-value>) => (i :: <abstract-integer>)
  if (primitive-machine-word-unsigned-greater-than?
	(primitive-unwrap-machine-word(x),
	 primitive-unwrap-machine-word(coerce-integer-to-machine-word(#x1fffffff))))
    make(<double-integer>,
	 high: $minimum-unsigned-machine-word,
	 low: x);
  else
    coerce-machine-word-to-integer(x);
  end if;
end method;

define inline method as-integer
    (x :: <DESCRIPTOR-POINTER>) => (i :: <abstract-integer>)
  as-integer
  (primitive-wrap-machine-word
     (primitive-c-unsigned-long-at(x, integer-as-raw(1), integer-as-raw(0))));
end method;


///// AS-SIGNED-INTEGER
//    Conversion from remote value to signed integer

define method as-signed-integer
    (x :: <remote-value>) => (i :: <abstract-integer>)
  as(<abstract-integer>, x)
end method;


///// AS-REMOTE-VALUE
//    Conversion in the other direction. I hope this will work...

define inline method as-remote-value 
    (x :: <abstract-integer>) => (ptr :: <remote-value>)
  as (<machine-word>, x)
end method;


define inline method as-remote-pointer
    (x :: <abstract-integer>) => (ptr :: <DESCRIPTOR-POINTER>)
  let address = as (<machine-word>, x);
  make-c-pointer-internal
    (<DESCRIPTOR-POINTER>, address, vector(address: address));
end method;


///// INDEXED-REMOTE-VALUE
//    Returns the address that is a word-sized offset from the
//    given address, multiplied by the given index.

define method indexed-remote-value 
    (x :: <remote-value>, i :: <integer>) => (ptr :: <remote-value>)
  u%+(x, u%*(i, truncate/($machine-word-size, 8)))
end method;


///// BYTE-INDEXED-REMOTE-VALUE
//    Returns the address that is byte indexed from the given
//    address. The second argument is the number of bytes to
//    add.

define method byte-indexed-remote-value
    (x :: <remote-value>, i :: <integer>) => (ptr :: <remote-value>)
  u%+(x, i)
end method;


///// REMOTE-VALUE-LOW-ORDER-BITS
//    Creates an <integer> out of a specified number of bits of the
//    <remote-value>, counting from the right.
//    (This is used to detect tags)

define method remote-value-low-order-bits
    (x :: <remote-value>, bit-count :: <integer>)
       => (value :: <integer>)
  let lower = %logand(x, u%-(%shift-left(1, bit-count), 1));
  as(<integer>, lower)
end method;


///// TAGGED-REMOTE-VALUE-AS-INTEGER
//    Given a remote value that is known to have an integer tag,
//    strip the tag and return the integer.
//    This function is NOT identical to AS-INTEGER, since the
//    latter would intepret the tag bits as part of the integer
//    value when converting.

define method tagged-remote-value-as-integer (x :: <remote-value>)
    => (i :: <integer>)
  let stripped :: <remote-value> = %shift-right(x, 2);
  as(<integer>, stripped);
end method;


///// TAGGED-REMOTE-VALUE-AS-CHARACTER
//    As above, but returns a character.

define method tagged-remote-value-as-character (x :: <remote-value>)
    => (c :: <character>)
  let int-bit = %shift-right(x, 2);
  as(<character>, as(<integer>, int-bit));
end method;


///// INTEGER-AS-TAGGED-REMOTE-VALUE
//    Given an integer, this returns the integer as a correctly tagged
//    <remote-value>

define method integer-as-tagged-remote-value (i :: <integer>)
    => (x :: <remote-value>)
  %logior(%shift-left(i, 2), #b01)
end method;


//// CHARACTER-AS-TAGGED-REMOTE-VALUE
//   Given a character, this returns the character as a correctly tagged
//   <remote-value>

define method character-as-tagged-remote-value (c :: <character>)
    => (x :: <remote-value>)
  %logior(%shift-left(as(<integer>, c), 2), #b10)
end method;


///// REMOTE-VALUE-=
//    Probably needs to become more efficient.

define method remote-value-= (x :: <remote-value>, y :: <remote-value>)
    => (answer :: <boolean>)
  x = y;
end method;


///// REMOTE-VALUE-<
//    Probably needs to become more efficient.

define method remote-value-< (x :: <remote-value>, y :: <remote-value>)
    => (answer :: <boolean>)
  x < y;
end method;


///// REMOTE-VALUE-<=
//    Probably needs to become more efficient.

define method remote-value-<= (x :: <remote-value>, y :: <remote-value>)
    => (answer :: <boolean>)
  (x = y) | (x < y);
end method;


///// AS-INTEGER-LOSING-PRECISION
//    Just for the consol profiler. We'll ditch this one day, because it
//    sucks the big one.

define method as-integer-losing-precision (x :: <remote-value>)
    => (i :: <integer>)
  as(<integer>, %shift-right(x, 3))  // There goes our precision! Bollox!
end method;


///// REMOTE-VALUE-AS-STRING
//    Converts a remote value to a string on the application's machine.

define method remote-value-as-string
    (ap :: <access-path>, val :: <remote-value>, radix :: <integer>) 
       => (str :: <string>)
  // This is a bit of a hack. Pad with zero. Assume 8 digits. (Eugh!)
  let padding = 2;
  remote-value-as-string-on-connection
     (ap.connection, val, radix, padding, 8);
end method;

define open generic remote-value-as-string-on-connection
    (conn :: <access-connection>, val :: <remote-value>,
     radix :: <integer>, pad :: <integer>, sz :: <integer>)
       => (str :: <string>);


///// STRING-AS-REMOTE-VALUE
//    Converts a string to a <remote-value> on the application's machine.

define method string-as-remote-value
    (ap :: <access-path>, str :: <string>, radix :: <integer>)
       => (val :: <remote-value>)
  string-as-remote-value-on-connection
       (ap.connection, size(str), as-uppercase(str), radix)
end method;

define open generic string-as-remote-value-on-connection
    (conn :: <access-connection>, sz :: <integer>,
     str :: <string>, radix :: <integer>)
         => (val :: <remote-value>);
