module:    harp-templates
Synopsis:  Useful predicates for HARP templates.
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define method const-ref (x :: <integer>) => (x :: <integer>)
  x;
end method;

define method const-ref (x :: <object>) => (false :: <boolean>)
  #f;
end method;


define method canonicalised-const-ref 
    (x :: <integer>) => (x :: <integer>)
  canonicalise-int(x);
end method;

define method canonicalised-const-ref 
    (x :: <object>) => (false :: <boolean>)
  #f;
end method;

define method unsigned-canonicalised-const-ref
     (x :: <integer>) => (x :: <integer>)
  unsigned-canonicalise-int(x);
end method;

define method unsigned-canonicalised-const-ref
     (x :: <object>) => (f :: <boolean>)
  #f;
end method;

define method any-spill-ref (x) => (res)
  let c = colour(x);
  if (instance?(c, <spill>)) c end;
end method;

define method ispill-ref (x) => (res)
  let c = colour(x);
  if (instance?(c, <ispill>)) c end;
end method;

define constant spill-ref = ispill-ref;

define method fspill-ref (x) => (res)
  let c = colour(x);
  if (instance?(c, <fspill>)) c end;
end method;

define method dfspill-ref (x) => (res)
  let c = colour(x);
  if (instance?(c, <dfspill>)) c end;
end method;

define method sfspill-ref (x) => (res)
  let c = colour(x);
  if (instance?(c, <sfspill>)) c end;
end method;

define inline method m-ref (x) => (res)
  let c = colour(x);
  if (instance?(c, <real-register>)) c end;
end method;

define method m/spill-ref (x)  => (res)
  m-ref(x) | spill-ref(x);
end method;

define constant <m/spill-ref> = type-union(<real-register>, <ispill>);

define method m/const-ref (x) => (res)
  m-ref(x) | const-ref(x);
end method;

define constant <m/const-ref> = type-union(<real-register>, <integer>);

define method spill/const-ref (x) => (res)
  spill-ref(x) | const-ref(x);
end method;

define constant <spill/const-ref> = type-union(<ispill>, <integer>);

define method any-constant-ref
    (ref :: <constant-reference>) => (ref :: <constant-reference>)
  ref;
end method;

define method any-constant-ref 
    (ref :: <object>) => (f :: <boolean>)
  #f;
end method;

define method i-constant-ref
    (ref :: <i-constant-reference>) => (ref :: <constant-reference>)
  ref;
end method;

define method i-constant-ref 
    (ref :: <object>) => (f :: <boolean>)
  #f;
end method;

define constant constant-ref = i-constant-ref;

define method f-constant-ref
    (ref :: <f-constant-reference>) => (ref :: <constant-reference>)
  ref;
end method;

define method f-constant-ref 
    (ref :: <object>) => (f :: <boolean>)
  #f;
end method;

define method sf-constant-ref
    (ref :: <sf-constant-reference>) => (ref :: <constant-reference>)
  ref;
end method;

define method sf-constant-ref 
    (ref :: <object>) => (f :: <boolean>)
  #f;
end method;

define method df-constant-ref
    (ref :: <df-constant-reference>) => (ref :: <constant-reference>)
  ref;
end method;

define method df-constant-ref 
    (ref :: <object>) => (f :: <boolean>)
  #f;
end method;

define method any-indirect-constant-ref 
    (ref :: <indirect-constant-reference>) => (res)
  ref;
end method;

define method any-indirect-constant-ref
     (ref :: <object>) => (f :: <boolean>)
  #f;
end method;

define method i-indirect-constant-ref 
    (ref :: <i-indirect-constant-reference>) => (res)
  ref;
end method;

define method i-indirect-constant-ref
     (ref :: <object>) => (f :: <boolean>)
  #f;
end method;

define constant indirect-constant-ref = i-indirect-constant-ref;

define method f-indirect-constant-ref
     (ref :: <object>) => (res)
  ref.sf-indirect-constant-ref | ref.df-indirect-constant-ref;
end method;

define method sf-indirect-constant-ref 
    (ref :: <sf-indirect-constant-reference>) => (res)
  ref;
end method;

define method sf-indirect-constant-ref
     (ref :: <object>) => (f :: <boolean>)
  #f;
end method;

define method df-indirect-constant-ref 
    (ref :: <df-indirect-constant-reference>) => (res)
  ref;
end method;

define method df-indirect-constant-ref
     (ref :: <object>) => (f :: <boolean>)
  #f;
end method;

define method ic/spill-ref (ref) => (res)
  spill-ref(ref) | indirect-constant-ref(ref);
end method;

define constant <ic/spill-ref> = type-union(<ispill>, <i-indirect-constant-reference>);

define method ic/m/spill-ref (ref) => (res)
  m/spill-ref(ref) | indirect-constant-ref(ref);
end method;

define constant <ic/m/spill-ref> = type-union(<ispill>, <real-register>, <i-indirect-constant-reference>);


define method f-ic/spill-ref (ref) => (res)
  fspill-ref(ref) | f-indirect-constant-ref(ref);
end method;

define constant <f-ic/spill-ref> = type-union(<fspill>, <sf-indirect-constant-reference>, <df-indirect-constant-reference>);

define method df-ic/spill-ref (ref) => (res)
  dfspill-ref(ref) | df-indirect-constant-ref(ref);
end method;

define constant <df-ic/spill-ref> = type-union(<dfspill>, <df-indirect-constant-reference>);

define method sf-ic/spill-ref (ref) => (res)
  sfspill-ref(ref) | sf-indirect-constant-ref(ref);
end method;

define constant <sf-ic/spill-ref> = type-union(<sfspill>, <sf-indirect-constant-reference>);


define method any-address-constant-ref
     (ref :: <address-constant-reference>) => (res)
  ref;
end method;

define method any-address-constant-ref (ref :: <object>) => (res)
  #f;
end method;

define method i-address-constant-ref
     (ref :: <i-address-constant-reference>) => (res)
  ref;
end method;

define method i-address-constant-ref (ref :: <object>) => (res)
  #f;
end method;

define constant address-constant-ref = i-address-constant-ref;

define method f-address-constant-ref
     (ref :: <object>) => (f :: <boolean>)
  ref.sf-address-constant-ref | ref.df-address-constant-ref;
end method;

define method sf-address-constant-ref
     (ref :: <sf-address-constant-reference>) => (res)
  ref;
end method;

define method sf-address-constant-ref (ref :: <object>) => (res)
  #f;
end method;

define method df-address-constant-ref
     (ref :: <df-address-constant-reference>) => (res)
  ref;
end method;

define method df-address-constant-ref (ref :: <object>) => (res)
  #f;
end method;


define method ac/const-ref (ref) => (res)
  const-ref(ref) | address-constant-ref(ref);
end method;

define constant <ac/const-ref> = type-union(<integer>, <i-address-constant-reference>);


define method signed-twelve-bits? (x) => (res)
  if (instance?(x, <integer>) & -#x800 <= x & x < #x800)
    x;
  end if;
end method;

define method signed-thirteen-bits? (x) => (res)
  if (instance?(x, <integer>) & -#x1000 <= x & x < #x1000)
    x;
  end if;
end method;

define inline method signed-sixteen-bits? (x :: <integer>) => (res)
  -#x8000 <= x & x < #x8000 & x
end method;

define method signed-sixteen-bits? (x) => (res)
  #f
end method;

define method unsigned-four-bits? (x) => (res)
  if (instance?(x, <integer>) & 0 <= x & x < #x10)
    x;
  end if;
end method;

define inline method signed-eight-bits? (x :: <integer>) => (res)
  -#x80 <= x & x < #x80 & x
end method;

define method signed-eight-bits? (x) => (res)
  #f
end method;

define inline method unsigned-eight-bits? (x :: <integer>) => (res)
  0 <= x & x < #x100 & x
end method;

define method unsigned-eight-bits? (x) => (res)
  #f
end method;

define constant sixteen-bit-const-ref = signed-sixteen-bits?;

define method unsigned-fifteen-bit-const-ref (x) => (res)
  if (instance?(x, <integer>) & 0 <= x & x < #x8000)
    x;
  end if;
end method;

define method signed-24bit-const-ref (x)
  // some leeway for error
  instance?(x, <integer>) & (-#x7FFFF0 <= x & x <= #x7FFFF0) & x
end method signed-24bit-const-ref;

define method signed-16bit-const-ref (x)
  // some leeway for error
  instance?(x, <integer>) & (-#x7FF0 <= x & x <= #x7FF0) & x
end method signed-16bit-const-ref;

define method unsigned-32bit-const-ref (x)
  instance?(x, <integer>) & (0 <= x & x <= #xFFFFFFFF) & x
end method unsigned-32bit-const-ref;

define method unsigned-16bit-const-ref (x)
  instance?(x, <integer>) & (0 <= x & x <= #xFFFF) & x
end method unsigned-16bit-const-ref;

define method unsigned-5bit-const-ref (x)
  instance?(x, <integer>) & (0 <= x & x <= 31) & x
end method unsigned-5bit-const-ref;

define constant eight-bit-const-ref = signed-eight-bits?;

define method canonicalise-int (int :: <integer>) => (i :: <integer>)
  int;
end method;

define method unsigned-canonicalise-int (int :: <integer>) => (i :: <integer>)
  logand(int, -1);
end method;

define inline method zero-number? (x :: <integer>) => (res)
 (x = 0) & x
end method;

define method zero-number? (x) => (res)
  #f
end method;
