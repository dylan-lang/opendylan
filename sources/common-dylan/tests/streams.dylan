Module:       common-dylan-test-suite
Synopsis:     Common Dylan library test suite
Author:       Andy Armstrong, et al...
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Stream testing

define sideways method class-test-function
    (class :: subclass(<stream>)) => (function :: <function>)
  test-stream-class
end method class-test-function;


/// Stream information

define class <stream-class-info> (<object>)
  constant slot info-class-name :: <string>,
    required-init-keyword: name:;
  constant slot info-class :: subclass(<stream>),
    required-init-keyword: class:;
  constant slot info-input-stream? :: <boolean>,
    required-init-keyword: input-stream?:;
  constant slot info-output-stream? :: <boolean>,
    required-init-keyword: output-stream?:;
  constant slot info-element-type :: <type>,
    required-init-keyword: element-type:;
end class <stream-class-info>;

define constant $stream-classes :: <object-table> = make(<object-table>);

define function register-stream-class-info
    (name :: <string>, class :: subclass(<stream>),
     #key input-stream? :: <boolean> = #f,
          output-stream? :: <boolean> = #f,
          element-type :: <type> = <object>)
 => ()
  let info
    = make(<stream-class-info>,
           name: name,
           class: class,
           input-stream?: input-stream?,
           output-stream?: output-stream?,
           element-type: element-type);
  $stream-classes[class] := info
end function register-stream-class-info;

define function registered-stream-classes
    () => (classes :: <sequence>)
  key-sequence($stream-classes)
end function registered-stream-classes;

define function stream-class-info
    (class :: subclass(<stream>))
 => (info :: <stream-class-info>)
  element($stream-classes, class, default: #f)
    | error("Stream class %= not registered for testing", class)
end function stream-class-info;

define class <stream-test-info> (<object>)
  constant slot info-test-name :: <string>,
    required-init-keyword: test-name:;
  constant slot info-class-info :: <stream-class-info>,
    required-init-keyword: class-info:;
  constant slot info-contents :: <sequence>,
    required-init-keyword: contents:;
  constant slot info-direction :: <symbol>,
    required-init-keyword: direction:;
  constant slot info-make-function :: <function>,
    required-init-keyword: make-function:;
  constant slot info-destroy-function :: <function> = maybe-close-test-stream,
    init-keyword: destroy-function:;
end class <stream-test-info>;

define function maybe-close-test-stream
    (stream :: <stream>) => ()
  if (stream-open?(stream))
    close(stream)
  end
end function maybe-close-test-stream;

define open generic test-stream-class
    (class :: subclass(<stream>), #key, #all-keys) => ();

define method test-stream-class
    (class :: subclass(<stream>), #key name, instantiable?, #all-keys)
 => ()
  if (instantiable?)
    test-stream-of-size(format-to-string("Empty %s", name), class, 0);
    test-stream-of-size(format-to-string("One item %s", name), class, 1);
    test-stream-of-size(format-to-string("Multiple item %s", name), class, 5);
  end
end method test-stream-class;

define method test-stream-of-size
    (name :: <string>, class :: <class>, stream-size :: <integer>) => ()
  let info = stream-class-info(class);
  let tests = make-stream-tests-of-size(class, stream-size);
  do(test-stream, tests)
end method test-stream-of-size;

define open generic make-stream-tests-of-size
    (class :: subclass(<stream>), stream-size :: <integer>)
 => (streams :: <sequence>);

define method make-stream-tests-of-size
    (class :: subclass(<stream>), stream-size :: <integer>)
 => (streams :: <sequence>)
  #[]
end method make-stream-tests-of-size;

define method find-stream-test-info
    (class :: subclass(<stream>)) => (info :: false-or(<stream-test-info>))
  let tests = make-stream-tests-of-size(class, 2);
  if (~tests.empty?)
    tests[0]
  end
end method find-stream-test-info;

define sideways method make-test-instance
    (class :: subclass(<stream>)) => (stream :: <stream>)
  let info = find-stream-test-info(class);
  assert(info, "Making test instance of unregistered stream class %=", class);
  let make-function :: <function> = info.info-make-function;
  make-function()
end method make-test-instance;

define sideways method destroy-test-instance
    (class :: subclass(<stream>), stream :: <stream>) => ()
  let info = find-stream-test-info(class);
  assert(info, "Destroying test instance of unregistered stream class %=", class);
  let destroy-function :: <function> = info.info-destroy-function;
  destroy-function(stream)
end method destroy-test-instance;

define constant $stream-tests :: <object-table> = make(<object-table>);

define class <stream-test-function-info> (<object>)
  constant slot info-class :: subclass(<stream>),
    required-init-keyword: class:;
  constant slot info-test-function :: <function>,
    required-init-keyword: test-function:;
  constant slot info-direction :: false-or(<symbol>) = #f,
    init-keyword: direction:;
end class <stream-test-function-info>;

define function register-stream-test
    (class :: subclass(<stream>), test-function :: <function>,
     #key direction :: false-or(<symbol>) = #f)
 => ()
  let tests :: <stretchy-object-vector>
    = element($stream-tests, class, default: make(<stretchy-object-vector>));
  add!(tests,
       make(<stream-test-function-info>,
            class: class,
            test-function: test-function,
            direction: direction));
  $stream-tests[class] := tests
end function register-stream-test;

define method test-stream
    (test-info :: <stream-test-info>) => ()
  let class-info = test-info.info-class-info;
  for (class :: subclass(<stream>) in key-sequence($stream-tests))
    if (subtype?(class-info.info-class, class))
      let tests = $stream-tests[class];
      for (test-function-info :: <stream-test-function-info> in tests)
        let test-function = test-function-info.info-test-function;
        let test-function-direction = test-function-info.info-direction;
        if (select (test-info.info-direction)
              #"input"  =>
                class-info.info-input-stream?
                  & (~test-function-direction | test-function-direction == #"input");
              #"output" =>
                class-info.info-output-stream?
                  & (~test-function-direction | test-function-direction == #"output");
            end)
          let stream = #f;
          block ()
            stream := test-info.info-make-function();
            unless (instance?(stream, class))
              error("Make function for stream class %s returns wrong class of object: %=",
                    class-info.info-class-name,
                    stream)
            end;
            if (~test-function-direction | test-function-direction == test-info.info-direction)
              test-function(test-info, stream);
            end
          cleanup
            if (stream)
              test-info.info-destroy-function(stream)
            end
          end
        end
      end
    end
  end
end method test-stream;


/// Test stream classes

define abstract class <test-stream> (<positionable-stream>)
  slot stream-closed? :: <boolean> = #f;
  slot stream-test-position :: <integer> = 0;
end class <test-stream>;

define method close
    (stream :: <test-stream>, #key) => ()
  stream.stream-closed? := #t
end method close;

define method stream-element-type
    (stream :: <test-stream>) => (type :: <type>)
  <character>
end method stream-element-type;

define method stream-position
    (stream :: <test-stream>) => (position :: <integer>)
  stream.stream-test-position
end method stream-position;

define method stream-position-setter
    (position :: <integer>, stream :: <test-stream>) => (position :: <integer>)
  stream.stream-test-position := position
end method stream-position-setter;

define method stream-position-setter
    (position == #"start", stream :: <test-stream>)
 => (position :: <integer>)
  stream.stream-test-position := 0
end method stream-position-setter;

define method stream-position-setter
    (position == #"end", stream :: <test-stream>)
 => (position :: <integer>)
  stream.stream-test-position := stream.stream-size
end method stream-position-setter;

define method adjust-stream-position
    (stream :: <test-stream>, delta :: <integer>,
     #key from = #"current")
 => (position :: <integer>)
  stream-position(stream)
    := select (from)
         #"current" => stream-position(stream) + delta;
         #"start"   => delta;
         #"end"     => stream-size(stream) + delta;
       end
end method adjust-stream-position;


// Test input stream

define class <test-input-stream> (<test-stream>)
  slot stream-test-sequence :: <byte-string>,
    required-init-keyword: test-sequence:;
end class <test-input-stream>;

register-stream-class-info("<test-input-stream>", <test-input-stream>,
                           input-stream?: #t,
                           output-stream?: #f,
                           element-type: <object>);

define method make-test-instance
    (class == <test-input-stream>) => (stream :: <test-input-stream>)
  make(<test-input-stream>, test-sequence: "Test")
end method make-test-instance;

define method stream-contents
    (stream :: <test-input-stream>,
     #key clear-contents? :: <boolean> = #t)
 => (contents :: <sequence>)
  let sequence = stream.stream-test-sequence;
  if (clear-contents?)
    // clear-contents(stream)
  end;
  sequence
end method stream-contents;

define method stream-contents-as
    (type :: subclass(<sequence>), stream :: <test-input-stream>,
     #key clear-contents? :: <boolean> = #t)
 => (contents :: <sequence>)
  as(type, stream-contents(stream, clear-contents?: clear-contents?))
end method stream-contents-as;


define method stream-size
    (stream :: <test-input-stream>) => (size :: <integer>)
  stream.stream-test-sequence.size
end method stream-size;

define method stream-at-end?
    (stream :: <test-input-stream>) => (at-end? :: <boolean>)
  stream.stream-test-position = stream.stream-size
end method stream-at-end?;

define method stream-input-available?
    (stream :: <test-input-stream>) => (input? :: <boolean>)
  ~stream.stream-at-end?
    & ~stream.stream-closed?
end method stream-input-available?;

define method peek
    (stream :: <test-input-stream>,
     #key on-end-of-stream = unsupplied())
 => (element)
  let sequence = stream.stream-test-sequence;
  if (stream.stream-test-position < stream.stream-size)
    sequence[stream.stream-test-position]
  else
    let error = make(<end-of-stream-error>, stream: stream);
    signal(error)
  end
end method peek;

define method read-element
    (stream :: <test-input-stream>,
     #key on-end-of-stream = unsupplied())
 => (element :: <object>)
  let value = peek(stream, on-end-of-stream: on-end-of-stream);
  stream.stream-test-position := stream.stream-test-position + 1;
  value
end method read-element;

define method unread-element
    (stream :: <test-input-stream>, object :: <object>)
 => (element :: <object>)
  if (stream.stream-test-position > 0)
    stream.stream-test-position := stream.stream-test-position - 1
  else
    // Should we signal an error here?
    #f
  end;
  object
end method unread-element;

define method read-into!
    (stream :: <test-input-stream>, count :: <integer>, result :: <mutable-sequence>,
     #key on-end-of-stream = unsupplied(), start :: <integer> = 0)
 => (n-read)
  for (i from 0 below count)
    result[i + start] := read-element(stream, on-end-of-stream: on-end-of-stream)
  end;
  count
end method read-into!;

define method read
    (stream :: <test-input-stream>, count :: <integer>,
     #key on-end-of-stream = unsupplied())
 => (elements)
  let result :: <vector> = make(<vector>, size: count);
  read-into!(stream, count, result, on-end-of-stream: on-end-of-stream);
  result
end method read;

define method discard-input
    (stream :: <test-input-stream>) => ()
  stream.stream-test-sequence := ""
end method discard-input;


// Test output stream

define class <test-output-stream> (<test-stream>)
  constant slot stream-test-result :: <stretchy-object-vector> = make(<stretchy-object-vector>);
end class <test-output-stream>;

register-stream-class-info("<test-output-stream>", <test-output-stream>,
                           input-stream?: #f,
                           output-stream?: #t,
                           element-type: <object>);

define method write-element
    (stream :: <test-output-stream>, char :: <character>) => ()
  stream.stream-test-result[stream.stream-test-position] := char;
  stream.stream-test-position := stream.stream-test-position + 1
end method write-element;

define method write
    (stream :: <test-output-stream>, sequence :: <sequence>,
     #key start :: <integer> = 0,
          end: _end :: <integer> = sequence.size)
 => ()
  for (i :: <integer> from start below _end)
    write(stream, sequence[i])
  end
end method write;


// Make test streams

define constant $default-string  = "abcdefghijklmnopqrstuvwxyz";

define method make-stream-tests-of-size
    (class == <test-input-stream>, stream-size :: <integer>)
 => (streams :: <sequence>)
  let class-info = stream-class-info(class);
  let tests :: <stretchy-object-vector> = make(<stretchy-object-vector>);
  let character-sequence = copy-sequence($default-string, end: stream-size);
  add!(tests,
       make(<stream-test-info>,
            test-name: format-to-string("<test-input-stream> size %d", stream-size),
            class-info: class-info,
            contents: character-sequence,
            direction: #"input",
            make-function: method () => (stream :: <test-input-stream>)
                             make(<test-input-stream>, test-sequence: character-sequence)
                           end));
  tests
end method make-stream-tests-of-size;

define method make-stream-tests-of-size
    (class == <test-output-stream>, stream-size :: <integer>)
 => (streams :: <sequence>)
  let class-info = stream-class-info(class);
  let tests :: <stretchy-object-vector> = make(<stretchy-object-vector>);
  let character-sequence = copy-sequence($default-string, end: stream-size);
  add!(tests,
       make(<stream-test-info>,
            test-name: format-to-string("<test-output-stream> size %d", stream-size),
            class-info: class-info,
            contents: character-sequence,
            direction: #"output",
            make-function: method () => (stream :: <test-output-stream>)
                             make(<test-output-stream>)
                           end));
  tests
end method make-stream-tests-of-size;

define test test-stream-test ()
  test-stream-class(<test-input-stream>, name: "<test-input-stream>", instantiable?: #t);
  test-stream-class(<test-output-stream>, name: "<test-output-stream>", instantiable?: #t)
end test test-stream-test;

define suite test-stream-suite ()
  test test-stream-test;
end suite test-stream-suite;


/// Core stream tests

register-stream-test(<stream>, test-close);
register-stream-test(<stream>, test-stream-open?);
register-stream-test(<stream>, test-stream-element-type);

// Don't test the functions we're already testing... there must be a better way!
define streams-protocol function-test close () end;
define streams-protocol function-test stream-open? () end;
define streams-protocol function-test stream-element-type () end;

define method test-close
    (info :: <stream-test-info>, stream :: <stream>) => ()
  // Can't think of anything interesting to test for this
  #f
end method test-close;

define method test-stream-open?
    (info :: <stream-test-info>, stream :: <stream>) => ()
  let name = info.info-test-name;
  check-true(format-to-string("%s: stream-open? initially", name),
             stream-open?(stream))
end method test-stream-open?;

define method test-stream-element-type
    (info :: <stream-test-info>, stream :: <stream>) => ()
  let name = info.info-test-name;
  check-true(format-to-string("%s: stream-element-type", name),
             subtype?(stream-element-type(stream),
                      info.info-class-info.info-element-type))
end method test-stream-element-type;


/// Stream reading tests

register-stream-test(<stream>, test-stream-size, direction: #"input");
register-stream-test(<stream>, test-stream-at-end?, direction: #"input");
register-stream-test(<stream>, test-read-element, direction: #"input");
register-stream-test(<stream>, test-unread-element, direction: #"input");
register-stream-test(<stream>, test-peek, direction: #"input");
register-stream-test(<stream>, test-read, direction: #"input");
register-stream-test(<stream>, test-read-into!, direction: #"input");
register-stream-test(<stream>, test-discard-input, direction: #"input");
register-stream-test(<stream>, test-stream-input-available?, direction: #"input");
register-stream-test(<stream>, test-stream-contents, direction: #"input");
register-stream-test(<stream>, test-stream-contents-as, direction: #"input");

// Don't test the functions we're already testing... there must be a better way!
define streams-protocol function-test stream-size () end;
define streams-protocol function-test stream-at-end? () end;
define streams-protocol function-test read-element () end;
define streams-protocol function-test unread-element () end;
define streams-protocol function-test peek () end;
define streams-protocol function-test read () end;
define streams-protocol function-test read-into! () end;
define streams-protocol function-test discard-input () end;
define streams-protocol function-test stream-input-available? () end;
define streams-protocol function-test stream-contents () end;
define streams-protocol function-test stream-contents-as () end;

define method test-stream-size
    (info :: <stream-test-info>, stream :: <stream>) => ()
  let name = info.info-test-name;
  check-equal(format-to-string("%s: stream-size", name),
              info.info-contents.size,
              stream-size(stream))
end method test-stream-size;

define method test-stream-at-end?
    (info :: <stream-test-info>, stream :: <stream>) => ()
  let name = info.info-test-name;
  if (~empty?(info.info-contents))
    check-false(format-to-string("%s: stream-at-end? not true initially", name),
                stream-at-end?(stream))
  else
    check-true(format-to-string("%s: stream-at-end? true initially", name),
               stream-at-end?(stream))
  end
end method test-stream-at-end?;

define method test-read-element
    (info :: <stream-test-info>, stream :: <stream>) => ()
  let name = info.info-test-name;
  for (expected-element in info.info-contents,
       i from 0)
    check-equal(format-to-string("%s: read element %d", name, i),
                expected-element,
                read-element(stream))
  end;
  check-condition(format-to-string("%s: read-element off end signals <end-of-stream-error>", name),
                  <end-of-stream-error>,
                  read-element(stream));
  check-at-end-of-stream(name, "read-element", stream)
end method test-read-element;

define method test-unread-element
    (info :: <stream-test-info>, stream :: <stream>) => ()
  let name = info.info-test-name;
  for (expected-element in info.info-contents,
       i from 0)
    check-true(format-to-string("%s: read element %d and then unread it", name, i),
               begin
                 let element = read-element(stream);
                 unread-element(stream, element);
                 read-element(stream)
               end)
  end;
  check-at-end-of-stream(name, "unread-element", stream)
end method test-unread-element;

define method test-peek
    (info :: <stream-test-info>, stream :: <stream>) => ()
  let name = info.info-test-name;
  for (expected-element in info.info-contents,
       i from 0)
    check-true(format-to-string("%s: peek element %d", name, i),
               begin
                 let element = peek(stream);
                 read-element(stream)
               end)
  end;
  check-condition(format-to-string("%s: peek off end signals <end-of-stream-error>", name),
                  <end-of-stream-error>,
                  peek(stream));
  check-at-end-of-stream(name, "peek", stream)
end method test-peek;

define method test-read
    (info :: <stream-test-info>, stream :: <stream>) => ()
  let name = info.info-test-name;
  check-equal(format-to-string("%s: read whole stream", name),
              info.info-contents,
              read(stream, info.info-contents.size));
  check-condition(format-to-string("%s: read off end signals <end-of-stream-error>", name),
                  <end-of-stream-error>,
                  read(stream, 1));
  check-at-end-of-stream(name, "read", stream)
end method test-read;

define method test-read-into!
    (info :: <stream-test-info>, stream :: <stream>) => ()
  //---*** Fill this in...
end method test-read-into!;

define method test-discard-input
    (info :: <stream-test-info>, stream :: <stream>) => ()
  //---*** Fill this in...
end method test-discard-input;

define method test-stream-input-available?
    (info :: <stream-test-info>, stream :: <stream>) => ()
  let name = info.info-test-name;
  if (~info.info-contents.empty?)
    check-true(format-to-string("%s: stream-input-available? is true", name),
               stream-input-available?(stream));
    check-no-errors(format-to-string("%s: read to the end", name),
                    while (~stream-at-end?(stream))
                      read-element(stream)
                    end)
  end;
  check-false(format-to-string("%s: stream-input-available? at end is false", name),
              stream-input-available?(stream));
  check-at-end-of-stream(name, "stream-contents", stream)
end method test-stream-input-available?;

define method test-stream-contents
    (info :: <stream-test-info>, stream :: <stream>) => ()
  let name = info.info-test-name;
  check-equal(format-to-string("%s: stream-contents correct", name),
              info.info-contents,
              stream-contents(stream));
  check-at-end-of-stream(name, "stream-contents", stream)
end method test-stream-contents;

define method test-stream-contents-as
    (info :: <stream-test-info>, stream :: <stream>) => ()
  let name = info.info-test-name;
  let contents = #f;
  check-equal(format-to-string("%s: stream-contents-as correct", name),
              info.info-contents,
              contents := stream-contents-as(<list>, stream));
  check-instance?(format-to-string("%s: stream-contents-as returns sequence of specified type", name),
                  <list>,
                  contents);
  check-at-end-of-stream(name, "stream-contents-as", stream)
end method test-stream-contents-as;

define function check-at-end-of-stream
    (name :: <string>, function-name :: <string>, stream :: <stream>) => ()
  check-true(format-to-string("%s: %s: reached end of stream", name, function-name),
             stream-at-end?(stream));
  check-condition(format-to-string("%s: %s: <end-of-stream-error> signalled reading off end",
                                   name, function-name),
                  <end-of-stream-error>,
                  read-element(stream))
end function check-at-end-of-stream;


/// Stream writing tests

register-stream-test(<stream>, test-write-element, direction: #"output");
register-stream-test(<stream>, test-write, direction: #"output");
register-stream-test(<stream>, test-force-output, direction: #"output");
register-stream-test(<stream>, test-wait-for-io-completion, direction: #"output");
register-stream-test(<stream>, test-synchronize-output, direction: #"output");
register-stream-test(<stream>, test-discard-output, direction: #"output");

// Don't test the functions we're already testing... there must be a better way!
define streams-protocol function-test write-element () end;
define streams-protocol function-test write () end;
define streams-protocol function-test force-output () end;
define streams-protocol function-test wait-for-io-completion () end;
define streams-protocol function-test synchronize-output () end;
define streams-protocol function-test discard-output () end;

define method test-write-element
    (info :: <stream-test-info>, stream :: <stream>) => ()
  //---*** Fill this in...
end method test-write-element;

define method test-write
    (info :: <stream-test-info>, stream :: <stream>) => ()
  //---*** Fill this in...
end method test-write;

define method test-force-output
    (info :: <stream-test-info>, stream :: <stream>) => ()
  //---*** Fill this in...
end method test-force-output;

define method test-wait-for-io-completion
    (info :: <stream-test-info>, stream :: <stream>) => ()
  //---*** Fill this in...
end method test-wait-for-io-completion;

define method test-synchronize-output
    (info :: <stream-test-info>, stream :: <stream>) => ()
  //---*** Fill this in...
end method test-synchronize-output;

define method test-discard-output
    (info :: <stream-test-info>, stream :: <stream>) => ()
  //---*** Fill this in...
end method test-discard-output;


/// Positionable stream tests

register-stream-test(<positionable-stream>, test-stream-position);
register-stream-test(<positionable-stream>, test-stream-position-setter);
register-stream-test(<positionable-stream>, test-adjust-stream-position);

// Don't test the functions we're already testing... there must be a better way!
define streams-protocol function-test stream-position () end;
define streams-protocol function-test stream-position-setter () end;
define streams-protocol function-test adjust-stream-position () end;

define method test-stream-position
    (info :: <stream-test-info>, stream :: <positionable-stream>) => ()
  //---*** Fill this in...
end method test-stream-position;

define method test-stream-position-setter
    (info :: <stream-test-info>, stream :: <positionable-stream>) => ()
  //---*** Fill this in...
end method test-stream-position-setter;

define method test-adjust-stream-position
    (info :: <stream-test-info>, stream :: <positionable-stream>) => ()
  //---*** Fill this in...
end method test-adjust-stream-position;


/// Stream conditions

define sideways method test-condition-class
    (class :: subclass(<stream-error>), #key name, abstract?, #all-keys) => ()
  unless (abstract?)
    test-stream-condition(name, make-test-instance(class))
  end
end method test-condition-class;

define sideways method make-test-instance
    (class :: subclass(<stream-error>))
 => (error :: <stream-error>)
  make(class, stream: make(<test-output-stream>))
end method make-test-instance;

define sideways method make-test-instance
    (class :: subclass(<incomplete-read-error>))
 => (error :: <incomplete-read-error>)
  make(class,
       stream: make(<test-output-stream>),
       count: 1,
       sequence: #[1, 2, 3]);
end method make-test-instance;

define sideways method make-test-instance
    (class :: subclass(<incomplete-write-error>))
 => (error :: <incomplete-write-error>)
  make(class,
       stream: make(<test-output-stream>),
       count: 1);
end method make-test-instance;

define method test-stream-condition
    (name :: <string>, error :: <stream-error>) => ()
  //---*** Fill this in...
end method test-stream-condition;


define streams-protocol function-test stream-error-stream ()
  //---*** Fill this in...
end function-test stream-error-stream;

define streams-protocol function-test stream-error-sequence ()
  //---*** Fill this in...
end function-test stream-error-sequence;

define streams-protocol function-test stream-error-count ()
  //---*** Fill this in...
end function-test stream-error-count;

define streams-protocol function-test open-file-stream ()
  //---*** Fill this in...
end function-test open-file-stream;

