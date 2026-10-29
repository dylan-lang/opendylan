Module:    c-ffi-test-suite
Author:    Peter Benson
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define interface-specification-suite c-ffi-specification-suite ()
  open generic function destroy (<C-pointer>) => ();

  function C-char-at (<C-pointer>, #"key", #"byte-index", #"scaled-index") => (<machine-word>);
  function C-char-at-setter (<machine-word>, <C-pointer>, #"key", #"byte-index", #"scaled-index") => (<machine-word>);
  function C-signed-char-at (<C-pointer>, #"key", #"byte-index", #"scaled-index") => (<machine-word>);
  function C-signed-char-at-setter (<machine-word>, <C-pointer>, #"key", #"byte-index", #"scaled-index") => (<machine-word>);
  function C-unsigned-char-at (<C-pointer>, #"key", #"byte-index", #"scaled-index") => (<machine-word>);
  function C-unsigned-char-at-setter (<machine-word>, <C-pointer>, #"key", #"byte-index", #"scaled-index") => (<machine-word>);

  function C-short-at (<C-pointer>, #"key", #"byte-index", #"scaled-index") => (<machine-word>);
  function C-short-at-setter (<machine-word>, <C-pointer>, #"key", #"byte-index", #"scaled-index") => (<machine-word>);
  function C-signed-short-at (<C-pointer>, #"key", #"byte-index", #"scaled-index") => (<machine-word>);
  function C-signed-short-at-setter (<machine-word>, <C-pointer>, #"key", #"byte-index", #"scaled-index") => (<machine-word>);
  function C-unsigned-short-at (<C-pointer>, #"key", #"byte-index", #"scaled-index") => (<machine-word>);
  function C-unsigned-short-at-setter (<machine-word>, <C-pointer>, #"key", #"byte-index", #"scaled-index") => (<machine-word>);

  function C-long-at (<C-pointer>, #"key", #"byte-index", #"scaled-index") => (<machine-word>);
  function C-long-at-setter (<machine-word>, <C-pointer>, #"key", #"byte-index", #"scaled-index") => (<machine-word>);
  function C-signed-long-at (<C-pointer>, #"key", #"byte-index", #"scaled-index") => (<machine-word>);
  function C-signed-long-at-setter (<machine-word>, <C-pointer>, #"key", #"byte-index", #"scaled-index") => (<machine-word>);
  function C-unsigned-long-at (<C-pointer>, #"key", #"byte-index", #"scaled-index") => (<machine-word>);
  function C-unsigned-long-at-setter (<machine-word>, <C-pointer>, #"key", #"byte-index", #"scaled-index") => (<machine-word>);

  function C-int-at (<C-pointer>, #"key", #"byte-index", #"scaled-index") => (<machine-word>);
  function C-int-at-setter (<machine-word>, <C-pointer>, #"key", #"byte-index", #"scaled-index") => (<machine-word>);
  function C-signed-int-at (<C-pointer>, #"key", #"byte-index", #"scaled-index") => (<machine-word>);
  function C-signed-int-at-setter (<machine-word>, <C-pointer>, #"key", #"byte-index", #"scaled-index") => (<machine-word>);
  function C-unsigned-int-at (<C-pointer>, #"key", #"byte-index", #"scaled-index") => (<machine-word>);
  function C-unsigned-int-at-setter (<machine-word>, <C-pointer>, #"key", #"byte-index", #"scaled-index") => (<machine-word>);

  function C-size-t-at (<C-pointer>, #"key", #"byte-index", #"scaled-index") => (<machine-word>);
  function c-size-t-at-setter (<machine-word>, <C-pointer>, #"key", #"byte-index", #"scaled-index") => (<machine-word>);

  function c-ssize-t-at (<C-pointer>, #"key", #"byte-index", #"scaled-index") => (<machine-word>);
  function c-ssize-t-at-setter (<machine-word>, <C-pointer>, #"key", #"byte-index", #"scaled-index") => (<machine-word>);

  function c-float-at (<C-pointer>, #"key", #"byte-index", #"scaled-index") => (<single-float>);
  function c-float-at-setter (<single-float>, <C-pointer>, #"key", #"byte-index", #"scaled-index") => (<single-float>);
  function C-double-at (<C-pointer>, #"key", #"byte-index", #"scaled-index") => (<double-float>);
  function C-double-at-setter (<double-float>, <C-pointer>, #"key", #"byte-index", #"scaled-index") => (<double-float>);

  abstract class <C-value> (<object>);
  abstract class <C-void> (<C-value>);
  abstract class <C-number> (<C-value>);

  open abstract class <C-struct> (<C-value>);
  open abstract class <C-union> (<C-value>);

  open abstract instantiable class <C-pointer> (<C-value>);
  abstract instantiable class <C-untyped-pointer> (<C-pointer>);
  open abstract instantiable class <C-void*> (<C-pointer>);
  open abstract instantiable class <C-statically-typed-pointer> (<C-void*>);
  open abstract instantiable class <C-function-pointer> (<C-statically-typed-pointer>);

  open abstract class <C-raw-unsigned-char> (<C-number>);
  open instantiable class <C-unsigned-char> (<C-raw-unsigned-char>);
  open instantiable class <C-unsafe-unsigned-char> (<C-raw-unsigned-char>);
  open instantiable class <C-both-unsigned-char> (<C-raw-unsigned-char>);
  primary instantiable class <C-unsigned-char*> (<C-statically-typed-pointer>);
  primary instantiable class <C-unsafe-unsigned-char*> (<C-statically-typed-pointer>);
  primary instantiable class <C-both-unsigned-char*> (<C-statically-typed-pointer>);

  open abstract class <C-raw-signed-char> (<C-number>);
  open instantiable class <C-signed-char> (<C-raw-signed-char>);
  open instantiable class <C-unsafe-signed-char> (<C-raw-signed-char>);
  open instantiable class <C-both-signed-char> (<C-raw-signed-char>);
  primary instantiable class <C-signed-char*> (<C-statically-typed-pointer>);
  primary instantiable class <C-unsafe-signed-char*> (<C-statically-typed-pointer>);
  primary instantiable class <C-both-signed-char*> (<C-statically-typed-pointer>);

  open abstract class <C-raw-char> (<C-number>);
  open instantiable class <C-char> (<C-raw-char>);
  open instantiable class <C-unsafe-char> (<C-raw-char>);
  open instantiable class <C-both-char> (<C-raw-char>);
  primary instantiable class <C-char*> (<C-statically-typed-pointer>);
  primary instantiable class <C-unsafe-char*> (<C-statically-typed-pointer>);
  primary instantiable class <C-both-char*> (<C-statically-typed-pointer>);

  open abstract class <C-raw-size-t> (<C-number>);
  open instantiable class <C-size-t> (<C-raw-size-t>);
  open instantiable class <C-unsafe-size-t> (<C-raw-size-t>);
  open instantiable class <C-both-size-t> (<C-raw-size-t>);
  primary instantiable class <C-size-t*> (<C-statically-typed-pointer>);

  open abstract class <C-raw-ssize-t> (<C-number>);
  open instantiable class <C-ssize-t> (<C-raw-ssize-t>);
  open instantiable class <C-unsafe-ssize-t> (<C-raw-ssize-t>);
  open instantiable class <C-both-ssize-t> (<C-raw-ssize-t>);
  primary instantiable class <C-ssize-t*> (<C-statically-typed-pointer>);

  open abstract class <C-raw-unsigned-long> (<C-number>);
  open instantiable class <C-unsigned-long> (<C-raw-unsigned-long>);
  open instantiable class <C-unsafe-unsigned-long> (<C-raw-unsigned-long>);
  open instantiable class <C-both-unsigned-long> (<C-raw-unsigned-long>);
  primary instantiable class <C-unsigned-long*> (<C-statically-typed-pointer>);
  primary instantiable class <C-unsafe-unsigned-long*> (<C-statically-typed-pointer>);
  primary instantiable class <C-both-unsigned-long*> (<C-statically-typed-pointer>);

  open abstract class <C-raw-signed-long> (<C-number>);
  open instantiable class <C-signed-long> (<C-raw-signed-long>);
  open instantiable class <C-unsafe-signed-long> (<C-raw-signed-long>);
  open instantiable class <C-both-signed-long> (<C-raw-signed-long>);
  primary instantiable class <C-signed-long*> (<C-statically-typed-pointer>);
  primary instantiable class <C-unsafe-signed-long*> (<C-statically-typed-pointer>);
  primary instantiable class <C-both-signed-long*> (<C-statically-typed-pointer>);

  open abstract class <C-raw-long> (<C-number>);
  open instantiable class <C-long> (<C-raw-long>);
  open instantiable class <C-unsafe-long> (<C-raw-long>);
  open instantiable class <C-both-long> (<C-raw-long>);
  primary instantiable class <C-long*> (<C-statically-typed-pointer>);
  primary instantiable class <C-unsafe-long*> (<C-statically-typed-pointer>);
  primary instantiable class <C-both-long*> (<C-statically-typed-pointer>);

  open abstract class <C-raw-unsigned-short> (<C-number>);
  open instantiable class <C-unsigned-short> (<C-raw-unsigned-short>);
  open instantiable class <C-unsafe-unsigned-short> (<C-raw-unsigned-short>);
  open instantiable class <C-both-unsigned-short> (<C-raw-unsigned-short>);
  primary instantiable class <C-unsigned-short*> (<C-statically-typed-pointer>);
  primary instantiable class <C-unsafe-unsigned-short*> (<C-statically-typed-pointer>);
  primary instantiable class <C-both-unsigned-short*> (<C-statically-typed-pointer>);

  open abstract class <C-raw-signed-short> (<C-number>);
  open instantiable class <C-signed-short> (<C-raw-signed-short>);
  open instantiable class <C-unsafe-signed-short> (<C-raw-signed-short>);
  open instantiable class <C-both-signed-short> (<C-raw-signed-short>);
  primary instantiable class <C-signed-short*> (<C-statically-typed-pointer>);
  primary instantiable class <C-unsafe-signed-short*> (<C-statically-typed-pointer>);
  primary instantiable class <C-both-signed-short*> (<C-statically-typed-pointer>);

  open abstract class <C-raw-short> (<C-number>);
  open instantiable class <C-short> (<C-raw-short>);
  open instantiable class <C-unsafe-short> (<C-raw-short>);
  open instantiable class <C-both-short> (<C-raw-short>);
  primary instantiable class <C-short*> (<C-statically-typed-pointer>);
  primary instantiable class <C-unsafe-short*> (<C-statically-typed-pointer>);
  primary instantiable class <C-both-short*> (<C-statically-typed-pointer>);

  open abstract class <C-raw-unsigned-int> (<C-number>);
  open instantiable class <C-unsigned-int> (<C-raw-unsigned-int>);
  open instantiable class <C-unsafe-unsigned-int> (<C-raw-unsigned-int>);
  open instantiable class <C-both-unsigned-int> (<C-raw-unsigned-int>);
  primary instantiable class <C-unsigned-int*> (<C-statically-typed-pointer>);
  primary instantiable class <C-unsafe-unsigned-int*> (<C-statically-typed-pointer>);
  primary instantiable class <C-both-unsigned-int*> (<C-statically-typed-pointer>);

  open abstract class <C-raw-signed-int> (<C-number>);
  open instantiable class <C-signed-int> (<C-raw-signed-int>);
  open instantiable class <C-unsafe-signed-int> (<C-raw-signed-int>);
  open instantiable class <C-both-signed-int> (<C-raw-signed-int>);
  primary instantiable class <C-signed-int*> (<C-statically-typed-pointer>);
  primary instantiable class <C-unsafe-signed-int*> (<C-statically-typed-pointer>);
  primary instantiable class <C-both-signed-int*> (<C-statically-typed-pointer>);

  open instantiable class <C-int> (<C-raw-signed-int>);
  open instantiable class <C-unsafe-int> (<C-raw-signed-int>);
  open instantiable class <C-both-int> (<C-raw-signed-int>);
  primary instantiable class <C-int*> (<C-statically-typed-pointer>);
  primary instantiable class <C-unsafe-int*> (<C-statically-typed-pointer>);
  primary instantiable class <C-both-int*> (<C-statically-typed-pointer>);

  open instantiable class <C-character> (<C-raw-unsigned-char>);
  primary instantiable class <C-character*> (<C-statically-typed-pointer>);

  open instantiable class <C-dylan-object> (<C-void*>);
  primary instantiable class <C-dylan-object*> (<C-statically-typed-pointer>);

  function register-c-dylan-object (<object>) => ();
  function unregister-c-dylan-object (<object>) => ();
  function export-c-dylan-object (<object>) => (<C-dylan-object>);
  function import-c-dylan-object (<C-dylan-object>) => (<object>);

  function c-pointer-at (subclass(<C-pointer>), <C-pointer>, #"key", #"byte-index", #"scaled-index") => (<C-pointer>);
  function c-pointer-at-setter (<C-pointer>, <C-pointer>, #"key", #"byte-index", #"scaled-index") => (<C-pointer>);

  function size-of (<object>) => (<integer>);
  function alignment-of (<object>) => (<integer>);
  function pointer-address (<C-pointer>) => (<machine-word>);
  function null-pointer (subclass(<C-pointer>)) => (<C-pointer>);
  function null-pointer? (<C-pointer>) => (<boolean>);

  open generic function pointer-value (<c-pointer>, #"key", #"index") => (<object>);
  open generic function pointer-value-setter (<object>, <c-pointer>, #"key", #"index") => (<object>);

  function referenced-type (<object>) => (<object>);

  open abstract instantiable class <C-string> (<C-raw-unsigned-char*>, <string>);
  function c-string-constant (<byte-string>) => (<C-string>);
  primary instantiable class <C-string*> (<C-statically-typed-pointer>);

  open instantiable class <c-unicode-string> (<c-unsigned-short*>, <string>);
  function c-unicode-string-constant (<string>) => (<C-unicode-string>);
  primary instantiable class <c-unicode-string*> (<c-statically-typed-pointer>);

  abstract instantiable class <C-boolean> (<C-raw-signed-int>);

  function pointer-cast (subclass(<C-pointer>), <C-pointer>) => (<C-pointer>);
  open generic function c-type-cast (<type>, <object>) => (<object>);

  function copy-into! (<C-pointer>, <C-pointer>, <integer>) => ();
  function copy-bytes! (<C-pointer>, <C-pointer>, <integer>) => ();
  function equal-memory? (<C-pointer>, <C-pointer>, <integer>) => (<boolean>);
  function clear-memory! (<C-pointer>, <integer>) => ();

  constant <ffi-integer> :: <type>;
  constant <ffi-integer-or-machine-word> :: <type>;
end interface-specification-suite;

define suite c-ffi-module-test-suite ()
  test c-type-import;
  test variable-address-test;
end suite;

define suite c-ffi-test-suite ()
  suite c-ffi-specification-suite;
  suite c-ffi-module-test-suite;
end suite;
