Module:    c-ffi-implementation
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

///
/// C-type-cast
///

define open generic c-type-cast (type :: <type>, value :: <object>)
 => (value :: <object>);

// between boolean and integer

define sealed inline method c-type-cast
    (class :: type-union(singleton(<boolean>), singleton(<C-boolean>)),
     value :: type-union(<abstract-integer>, <machine-word>))
 => (true? :: <boolean>)
  ~ zero?(value)
end method c-type-cast;

define sealed inline method c-type-cast
    (type :: type-union(subclass(<number>), singleton(<machine-word>)),
     true? :: <boolean>) => (value :: <object>)
  c-type-cast(type, if (true?) 1 else 0 end if)
end method c-type-cast;

define sealed inline method c-type-cast
    (class :: type-union(singleton(<boolean>), singleton(<C-boolean>)),
     value :: <boolean>)
 => (true? :: <boolean>)
  value
end method c-type-cast;

// between character and integer

define sealed inline method c-type-cast
    (class :: type-union(singleton(<character>), singleton(<C-character>)),
     value :: <integer>)
 => (character :: <character>)
  as(<character>, value)
end method c-type-cast;

define sealed inline method c-type-cast
    (type :: <type>, character :: <character>) => (value :: <object>)
  c-type-cast(type, as(<integer>, character))
end method c-type-cast;

define sealed inline method c-type-cast
    (class :: type-union(singleton(<character>), singleton(<C-character>)),
     value :: <character>)
 => (character :: <character>)
  value
end method c-type-cast;

// between pointer and integer

define sealed inline method c-type-cast
    (class == <machine-word>, pointer :: <C-pointer>)
 => (address :: <machine-word>)
  pointer-address(pointer)
end method c-type-cast;

define sealed method c-type-cast
    (class :: subclass(<C-pointer>),
     address :: type-union(<abstract-integer>, <machine-word>))
 => (pointer :: <C-pointer>)
  make(class, address: address)
end method c-type-cast;

define sealed inline method c-type-cast
    (type :: subclass(<abstract-integer>), pointer :: <C-pointer>)
 => (address :: <object>)
  c-type-cast(type, pointer-address(pointer))
end method c-type-cast;

// between different pointer types

define sealed method c-type-cast
    (class :: subclass(<C-pointer>), pointer :: <C-pointer>)
 => (new-pointer :: <C-pointer>)
  if (instance?(pointer, class))
    pointer
  else
    make(class, address: pointer-address(pointer))
  end if
end method c-type-cast;

// between different integer types

define sealed inline method c-type-cast
    (type == <C-unsigned-short>, value :: <abstract-integer>)
 => (value :: <integer>)
  logand(value, #xFFFF)
end method c-type-cast;

define sealed inline method c-type-cast
    (type == <C-unsigned-short>, value :: <machine-word>)
 => (value :: <integer>)
  as(<integer>, %logand(value, #xFFFF))
end method c-type-cast;

define sealed method c-type-cast
    (type == <C-signed-short>, value :: <abstract-integer>)
 => (value :: <integer>)
  let word :: <integer> = logand(value, #xFFFF);
  if (zero?(logand(word, #x8000)))
    word
  else // extend the sign bit
    logior(word, lognot(#xFFFF));
  end if
end method c-type-cast;

define sealed inline method c-type-cast
    (type :: type-union(singleton(<C-signed-short>),
                        singleton(<C-signed-char>),
                        singleton(<C-unsigned-char>)),
     value :: <machine-word>) => (value :: <integer>)
  c-type-cast(type, c-type-cast(<C-unsigned-short>, value))
end method c-type-cast;

define sealed inline method c-type-cast
    (type == <C-unsigned-char>, value :: <abstract-integer>)
 => (value :: <integer>)
  logand(value, #xFF)
end method c-type-cast;

define sealed method c-type-cast
    (type == <C-signed-char>, value :: <abstract-integer>)
 => (value :: <integer>)
  let byte :: <integer> = logand(value, #xFF);
  if (zero?(logand(byte, #x80)))
    byte
  else // extend the sign bit
    logior(byte, lognot(#xFF));
  end if
end method c-type-cast;

define sealed inline method c-type-cast
    (type :: type-union(singleton(<C-raw-signed-int>),
                        singleton(<C-raw-unsigned-int>),
                        singleton(<C-raw-signed-long>),
                        singleton(<C-raw-unsigned-long>)),
     value :: <object>)
 => (value :: <machine-word>)
  c-type-cast(<machine-word>, value)
end method c-type-cast;

define sealed inline method c-type-cast
    (type :: type-union(singleton(<C-signed-int>),
                        singleton(<C-unsigned-int>),
                        singleton(<C-signed-long>),
                        singleton(<C-unsigned-long>)),
     value :: <object>)
 => (value :: <integer>)
  c-type-cast(<integer>, value)
end method c-type-cast;

define constant <C-both-integer> :: <type> =
  type-union(singleton(<C-both-signed-int>),
             singleton(<C-both-unsigned-int>),
             singleton(<C-both-signed-long>),
             singleton(<C-both-unsigned-long>));

define sealed inline method c-type-cast
    (type :: <C-both-integer>,
     value :: type-union(<integer>, <machine-word>))
 => (value :: type-union(<integer>, <machine-word>))
  value
end method c-type-cast;

define sealed inline method c-type-cast
    (type :: <C-both-integer>,
     value :: type-union(<boolean>, <character>))
 => (value :: <integer>)
  c-type-cast(<integer>, value)
end method c-type-cast;

define sealed inline method c-type-cast
    (type :: <C-both-integer>,
     value :: <object>)
 => (value :: <machine-word>)
  c-type-cast(<machine-word>, value)
end method c-type-cast;

define sealed inline method c-type-cast
    (class == <integer>, value :: <integer>) => (value :: <integer>)
  value
end method c-type-cast;

define sealed inline method c-type-cast
    (class == <machine-word>, value :: <machine-word>)
 => (value :: <machine-word>)
  value
end method c-type-cast;

define sealed inline method c-type-cast
    (class :: type-union(subclass(<abstract-integer>),
                         singleton(<machine-word>)),
     value :: type-union(<abstract-integer>, <machine-word>))
 => (value :: type-union(<abstract-integer>, <machine-word>))
  as(class, value)
end method c-type-cast;

// floating point

define inline method c-type-cast
    (type == <C-float>, value :: <number>) => (value :: <single-float>)
  as(<single-float>, value)
end method c-type-cast;

define inline method c-type-cast
    (type == <C-double>, value :: <number>) => (value :: <double-float>)
  as(<double-float>, value)
end method c-type-cast;

define inline method c-type-cast
    (class == <integer>, value :: <float>) => (value :: <integer>)
  // This operation is not supported by Dylan `as' for good reason,
  // but here the intent is to match the C semantics, no matter how ugly.
  truncate(value)
end method c-type-cast;

define inline method c-type-cast
    (class :: subclass(<number>), value :: <number>) => (value :: <number>)
  as(class, value)
end method c-type-cast;
