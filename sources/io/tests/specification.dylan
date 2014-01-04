Module:       io-test-suite
Synopsis:     IO library test suite
Author:       Andy Armstrong, et al...
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module-spec streams ()

  // Constants

  constant <buffer-index> :: <type>;
  constant <byte> :: <type>;
  constant <byte-character> :: <type>;
  // constant <unicode-character> :: <type>;

  // Classes

  sealed instantiable concrete class <byte-vector> (<vector>);
  open instantiable class <sequence-stream> (<positionable-stream>);
  open instantiable class <string-stream> (<sequence-stream>);
  open instantiable class <byte-string-stream> (<string-stream>);
  // open instantiable class <unicode-string-stream> (<string-stream>);

  sealed instantiable class <incomplete-read-error> (<end-of-stream-error>);
  sealed instantiable class <end-of-stream-error> (<error>);
  abstract class <stream-position> (<object>);

  // Stream convenience functions
  open generic-function read-line (<stream>, #"key", #"on-end-of-stream")
    => (<object>, <boolean>);
  open generic-function read-line-into!
    (<stream>, <string>, #"key", #"start", #"on-end-of-stream", #"grow?")
    => (<object>, <boolean>);
  open generic-function read-text (<stream>, <integer>, #"key", #"on-end-of-stream")
    => (<object>);
  open generic-function read-text-into! (<stream>, <integer>, <string>,
					 #"key", #"start", #"on-end-of-stream")
    => (<object>);
  function skip-through (<stream>, <object>, #"key", #"test")
    => (<boolean>);
  open generic-function write-line (<stream>, <string>, #"key", #"start", #"end")
    => ();
  open generic-function write-text (<stream>, <string>, #"key", #"start", #"end")
    => ();
  function read-through
    (<stream>, <object>, #"key", #"on-end-of-stream", #"test")
    => (<object>, <boolean>);
  function read-to
    (<stream>, <object>, #"key", #"on-end-of-stream", #"test")
    => (<object>, <boolean>);
  function read-to-end (<stream>)
    => (<sequence>);
  open generic-function new-line (<stream>)
    => ();

  // Miscellaneous stream functions
  function stream-lock (<stream>)
    => (<object>);
  function stream-lock-setter (<object>, <stream>)
    => (<object>);

  // Wrapper streams
  open abstract instantiable class <wrapper-stream> (<stream>);
  open generic-function inner-stream (<wrapper-stream>)
    => (<stream>);
  open generic-function inner-stream-setter (<stream>, <wrapper-stream>)
    => (<stream>);
  open generic-function outer-stream (<stream>) => (<wrapper-stream>);
  open generic-function outer-stream-setter 
       (<wrapper-stream>, <stream>)
    => (<wrapper-stream>);

  // Stream buffers
  sealed instantiable class <buffer> (<vector>);
  function buffer-next (<buffer>)
    => (<buffer-index>);
  function buffer-next-setter (<buffer-index>, <buffer>)
    => (<buffer-index>);
  function buffer-end (<buffer>)
    => (<buffer-index>);
  function buffer-end-setter (<buffer-index>, <buffer>)
    => (<buffer-index>);
  open generic-function buffer-subsequence
    (<buffer>, subclass(<mutable-sequence>), <buffer-index>, <buffer-index>)
    => (<mutable-sequence>);
  open generic-function copy-into-buffer!
    (<buffer>, <buffer-index>, <sequence>, #"key", #"start", #"end") => ();
  open generic-function copy-from-buffer!
        (<buffer>, <buffer-index>, <mutable-sequence>,
         #"key", #"start", #"end")
     => ();

  // Buffered streams
  open abstract class <buffered-stream> (<stream>);
  function get-input-buffer (<buffered-stream>, #"key", #"wait?", #"bytes")
    => (false-or(<buffer>));
  open generic-function do-get-input-buffer
    (<buffered-stream>, #"key", #"wait?", #"bytes")
    => (false-or(<buffer>));
  function get-output-buffer (<buffered-stream>, #"key", #"bytes")
    => (false-or(<buffer>));
  open generic-function do-get-output-buffer (<buffered-stream>, #"key", #"bytes")
    => (false-or(<buffer>));
  function input-available-at-source? (<buffered-stream>)
    => (<boolean>);
  open generic-function do-input-available-at-source? (<buffered-stream>)
    => (<boolean>);
  function next-input-buffer (<buffered-stream>, #"key", #"wait?", #"bytes")
    => (false-or(<buffer>));
  open generic-function do-next-input-buffer
    (<buffered-stream>, #"key", #"wait?", #"bytes")
    => (false-or(<buffer>));
  function next-output-buffer (<buffered-stream>, #"key", #"bytes")
    => ();
  open generic-function do-next-output-buffer (<buffered-stream>, #"key", #"bytes")
    => (<buffer>);
  function release-input-buffer (<buffered-stream>)
    => ();
  open generic-function do-release-input-buffer (<buffered-stream>)
    => ();
  function release-output-buffer (<buffered-stream>)
    => ();
  open generic-function do-release-output-buffer (<buffered-stream>)
    => ();

  // Macros

  macro-test with-input-buffer-test;
  macro-test with-output-buffer-test;
  macro-test with-output-to-string-test;
  macro-test with-input-from-string-test;
end module-spec streams;


define module-spec pprint ()
  variable *print-miser-width*   :: false-or(<integer>);
  variable *default-line-length* :: <integer>;

  sealed instantiable class <pretty-stream> (<stream>);

  function pprint-logical-block (<stream>) => ();
  function pprint-newline (<symbol>, <stream>) => ();
  function pprint-indent (<symbol>, <integer>, <stream>) => ();
  function pprint-tab (<symbol>, <integer>, <integer>, <stream>) => ();

  macro-test printing-logical-block-test;
end module-spec pprint;


define module-spec print ()
  variable *print-length*  :: false-or(<integer>);
  variable *print-level*   :: false-or(<integer>);
  variable *print-circle?* :: <boolean>;
  variable *print-pretty?* :: <boolean>;
  variable *print-escape?* :: <boolean>;

  function print (<object>, <stream>) => ();
  open generic-function print-object (<object>, <stream>) => ();
  function print-to-string (<object>) => (<string>);

  macro-test printing-object-test;
end module-spec print;

define library-spec io ()
  module streams;
  // module streams-internals;
  module pprint;
  module print;
  // module print-internals;
  // module format;
  // module format-internals;
  // module standard-io;
  // module format-out;
  suite universal-streams-suite;
  suite additional-streams-suite;
  suite format-test-suite;
end library-spec io;
