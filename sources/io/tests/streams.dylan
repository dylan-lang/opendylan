Module:       io-test-suite
Synopsis:     IO library test suite
Author:       Andy Armstrong, et al...
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Useful constants

define constant $default-string  = "abcdefghijklmnopqrstuvwxyz";

define sideways method make-test-instance(cls == <buffer>) => (object)
  make(<buffer>)
end;


/// Stream convenience function tests

register-stream-test(<stream>, do-test-read-character, direction: #"input");
register-stream-test(<stream>, do-test-read-text, direction: #"input");
register-stream-test(<stream>, do-test-read-text-into!, direction: #"input");
register-stream-test(<stream>, do-test-read-line, direction: #"input");
register-stream-test(<stream>, do-test-read-line-into!, direction: #"input");
register-stream-test(<stream>, do-test-read-to, direction: #"input");
register-stream-test(<stream>, do-test-read-through, direction: #"input");
register-stream-test(<stream>, do-test-read-to-end, direction: #"input");
register-stream-test(<stream>, do-test-skip-through, direction: #"input");
register-stream-test(<stream>, do-test-write-text, direction: #"output");
register-stream-test(<stream>, do-test-write-line, direction: #"output");
register-stream-test(<stream>, do-test-new-line, direction: #"output");

define method do-test-read-character
    (info :: <stream-test-info>, stream :: <stream>) => ()
  //---*** Fill this in...
end method;

define method do-test-read-text
    (info :: <stream-test-info>, stream :: <stream>) => ()
  //---*** Fill this in...
end method;

define method do-test-read-text-into!
    (info :: <stream-test-info>, stream :: <stream>) => ()
  //---*** Fill this in...
end method;

define method do-test-read-line
    (info :: <stream-test-info>, stream :: <stream>) => ()
  //---*** Fill this in...
end method;

define method do-test-read-line-into!
    (info :: <stream-test-info>, stream :: <stream>) => ()
  //---*** Fill this in...
end method;

define method do-test-read-to
    (info :: <stream-test-info>, stream :: <stream>) => ()
  //---*** Fill this in...
end method;

define method do-test-read-through
    (info :: <stream-test-info>, stream :: <stream>) => ()
  //---*** Fill this in...
end method;

define method do-test-read-to-end
    (info :: <stream-test-info>, stream :: <stream>) => ()
  //---*** Fill this in...
end method;

define method do-test-skip-through
    (info :: <stream-test-info>, stream :: <stream>) => ()
  //---*** Fill this in...
end method;

define method do-test-write-text
    (info :: <stream-test-info>, stream :: <stream>) => ()
  //---*** Fill this in...
end method;

define method do-test-write-line
    (info :: <stream-test-info>, stream :: <stream>) => ()
  //---*** Fill this in...
end method;

define method do-test-new-line
    (info :: <stream-test-info>, stream :: <stream>) => ()
  //---*** Fill this in...
end method;



/// Positionable stream convenience function tests

register-stream-test(<positionable-stream>, do-test-current-position);
register-stream-test(<positionable-stream>, do-test-current-position-setter);
register-stream-test(<positionable-stream>, do-test-initial-position);
register-stream-test(<positionable-stream>, do-test-final-position);

define method do-test-current-position
    (info :: <stream-test-info>, stream :: <positionable-stream>) => ()
  //---*** Fill this in...
end method;

define method do-test-current-position-setter
    (info :: <stream-test-info>, stream :: <positionable-stream>) => ()
  //---*** Fill this in...
end method;

define method do-test-initial-position
    (info :: <stream-test-info>, stream :: <positionable-stream>) => ()
  //---*** Fill this in...
end method;

define method do-test-final-position
    (info :: <stream-test-info>, stream :: <positionable-stream>) => ()
  //---*** Fill this in...
end method;



/// Buffered stream tests

register-stream-test(<buffered-stream>, do-test-get-input-buffer, direction: #"input");
register-stream-test(<buffered-stream>, do-test-release-input-buffer, direction: #"input");
register-stream-test(<buffered-stream>, do-test-with-input-buffer, direction: #"input");
register-stream-test(<buffered-stream>, do-test-input-available-at-source?, direction: #"input");
register-stream-test(<buffered-stream>, do-test-get-output-buffer, direction: #"output");
register-stream-test(<buffered-stream>, do-test-release-output-buffer, direction: #"output");
register-stream-test(<buffered-stream>, do-test-next-output-buffer, direction: #"output");
register-stream-test(<buffered-stream>, do-test-with-output-buffer, direction: #"output");
register-stream-test(<buffered-stream>, do-test-force-output-buffers, direction: #"output");

define method do-test-get-input-buffer
    (info :: <stream-test-info>, stream :: <buffered-stream>) => ()
  //---*** Fill this in...
end method;

define method do-test-release-input-buffer
    (info :: <stream-test-info>, stream :: <buffered-stream>) => ()
  //---*** Fill this in...
end method;

define method do-test-with-input-buffer
    (info :: <stream-test-info>, stream :: <buffered-stream>) => ()
  //---*** Fill this in...
end method;

define method do-test-input-available-at-source?
    (info :: <stream-test-info>, stream :: <buffered-stream>) => ()
  //---*** Fill this in...
end method;

define method do-test-get-output-buffer
    (info :: <stream-test-info>, stream :: <buffered-stream>) => ()
  //---*** Fill this in...
end method;

define method do-test-release-output-buffer
    (info :: <stream-test-info>, stream :: <buffered-stream>) => ()
  //---*** Fill this in...
end method;

define method do-test-next-output-buffer
    (info :: <stream-test-info>, stream :: <buffered-stream>) => ()
  //---*** Fill this in...
end method;

define method do-test-with-output-buffer
    (info :: <stream-test-info>, stream :: <buffered-stream>) => ()
  //---*** Fill this in...
end method;

define method do-test-force-output-buffers
    (info :: <stream-test-info>, stream :: <buffered-stream>) => ()
  //---*** Fill this in...
end method;


/// Sequence stream tests

register-stream-test(<sequence-stream>, do-test-type-for-sequence-stream);
register-stream-test(<sequence-stream>, do-test-stream-limit);
register-stream-test(<sequence-stream>, do-test-stream-limit-setter);

define method do-test-type-for-sequence-stream
    (info :: <stream-test-info>, stream :: <sequence-stream>) => ()
  //---*** Fill this in...
end method;

define method do-test-stream-limit
    (info :: <stream-test-info>, stream :: <sequence-stream>) => ()
  //---*** Fill this in...
end method;

define method do-test-stream-limit-setter
    (info :: <stream-test-info>, stream :: <sequence-stream>) => ()
  //---*** Fill this in...
end method;


register-stream-class-info("<sequence-stream>", <sequence-stream>,
                           input-stream?: #t,
                           output-stream?: #t,
                           element-type: <object>);

define sideways method make-stream-tests-of-size
    (class :: subclass(<sequence-stream>), stream-size :: <integer>)
 => (streams :: <sequence>)
  let class-info = stream-class-info(class);
  let tests :: <stretchy-object-vector> = make(<stretchy-object-vector>);
  let integer-sequence = range(from: 1, to: stream-size);
  let character-sequence = copy-sequence($default-string, end: stream-size);
  local method add-stream-test-info
            (class :: subclass(<sequence>), sequence :: <sequence>, element-type :: <type>,
             direction :: <symbol>)
         => ()
          add!(tests,
               make(<stream-test-info>,
                    test-name: format-to-string("%s %s on %s size %d",
                                                direction,
                                                class-info.info-class-name,
                                                class,
                                                stream-size),
                    class-info: class-info,
                    contents: sequence,
                    direction: direction,
                    make-function: method () => (stream :: <sequence-stream>)
                                     make(<sequence-stream>,
                                          contents: as(class, sequence),
                                          element-type: element-type,
                                          direction: direction)
                                   end))
        end method add-stream-test-info;
  add-stream-test-info(<range>, integer-sequence, <integer>, #"input");
  add-stream-test-info(<range>, integer-sequence, <integer>, #"output");
  add-stream-test-info(<byte-string>, character-sequence, <character>, #"input");
  add-stream-test-info(<byte-string>, character-sequence, <character>, #"output");
  for (collection-class :: <class> in vector(<list>, <vector>, <deque>))
    let integer-contents = as(collection-class, integer-sequence);
    add-stream-test-info(collection-class, integer-sequence,   <integer>,   #"input");
    add-stream-test-info(collection-class, integer-sequence,   <integer>,   #"output");
    add-stream-test-info(collection-class, character-sequence, <character>, #"input");
    add-stream-test-info(collection-class, character-sequence, <character>, #"output");
  end;
  tests
end method make-stream-tests-of-size;

register-stream-class-info("<string-stream>", <string-stream>,
                           input-stream?: #t,
                           output-stream?: #t,
                           element-type: <character>);

register-stream-class-info("<byte-string-stream>", <byte-string-stream>,
                           input-stream?: #t,
                           output-stream?: #t,
                           element-type: <character>);

define sideways method make-stream-tests-of-size
    (class :: subclass(<string-stream>), stream-size :: <integer>)
 => (tests :: <sequence>)
  let class-info = stream-class-info(class);
  let tests :: <stretchy-object-vector> = make(<stretchy-object-vector>);
  let character-sequence = copy-sequence($default-string, end: stream-size);
  local method add-stream-test-info
            (direction :: <symbol>) => ()
          add!(tests,
               make(<stream-test-info>,
                    test-name: format-to-string("%s %s size %d",
                                                direction,
                                                class-info.info-class-name,
                                                stream-size),
                    class-info: class-info,
                    contents: character-sequence,
                    direction: direction,
                    make-function: method () => (stream :: <string-stream>)
                                     make(class,
                                          contents: character-sequence,
                                          element-type: <character>,
                                          direction: direction)
                                   end))
        end method add-stream-test-info;
  add-stream-test-info(#"input");
  add-stream-test-info(#"output");
  tests
end method make-stream-tests-of-size;

/*---*** andrewa: not yet...
register-stream-class-info("<unicode-string-stream>", <unicode-string-stream>,
                           input-stream?: #t,
                           output-stream?: #t,
                           element-type: <unicode-character>);
*/


/// Wrapper stream testing

register-stream-test(<wrapper-stream>, do-test-inner-stream);
register-stream-test(<wrapper-stream>, do-test-inner-stream-setter);

define method do-test-inner-stream
    (info :: <stream-test-info>, stream :: <wrapper-stream>) => ()
  //---*** Fill this in...
end method;

define method do-test-inner-stream-setter
    (info :: <stream-test-info>, stream :: <wrapper-stream>) => ()
  //---*** Fill this in...
end method;


register-stream-class-info("<wrapper-stream>", <wrapper-stream>,
                           input-stream?: #t,
                           output-stream?: #t,
                           element-type: <object>);

define sideways method make-stream-tests-of-size
    (class == <wrapper-stream>, stream-size :: <integer>)
 => (streams :: <sequence>)
  let class-info = stream-class-info(<wrapper-stream>);
  let tests :: <stretchy-object-vector> = make(<stretchy-object-vector>);
  local method add-wrapper-stream-test
            (test :: <stream-test-info>) => (test :: <stream-test-info>)
          let test
            = make(<stream-test-info>,
                   test-name: format-to-string("%s for %s",
                                               class-info.info-class-name,
                                               test.info-test-name),
                   class-info: class-info,
                   contents: test.info-contents,
                   direction: test.info-direction,
                   make-function: method ()
                                    let stream = test.info-make-function();
                                    make(<wrapper-stream>, inner-stream: stream)
                                  end);
          add!(tests, test);
          test
        end method add-wrapper-stream-test;
  for (class :: subclass(<stream>) in registered-stream-classes())
    if (class ~== <wrapper-stream>)
      let inner-stream-tests = make-stream-tests-of-size(<sequence-stream>, stream-size);
      // First create single layer wrappers
      let tests = map(add-wrapper-stream-test, inner-stream-tests);
      // Then create wrappers on wrappers!
      do(add-wrapper-stream-test, tests)
    end
  end;
  add!(tests,
       make(<stream-test-info>,
            test-name: "<test-wrapper-stream> for <test-input-stream>",
            class-info: stream-class-info(<test-wrapper-stream>),
            contents: as-uppercase($default-string),
            direction: #"input",
            make-function: method ()
                             let stream
                               = make(<test-input-stream>,
                                      test-sequence: $default-string);
                             make(<test-wrapper-stream>, inner-stream: stream)
                           end));
  tests
end method make-stream-tests-of-size;


/// Test wrapper stream subclass
// This subclass always converts characters read from the inner stream
// to upper case.

define class <test-wrapper-stream> (<wrapper-stream>)
end class <test-wrapper-stream>;

register-stream-class-info("<test-wrapper-stream>", <test-wrapper-stream>,
                           input-stream?: #t,
                           output-stream?: #t,
                           element-type: <object>);

define method read-element
    (stream :: <test-wrapper-stream>, #rest keys, #key on-end-of-stream = unsupplied())
 => (element :: <object>)
  let char = next-method();
  if (char == on-end-of-stream)
    on-end-of-stream
  else
    as-uppercase(char);
  end if
end method read-element;

define method read
  (stream :: <test-wrapper-stream>, n :: <integer>, #key on-end-of-stream = unsupplied())
  => (sequence)
  let sequence = next-method();
  if (sequence == on-end-of-stream)
    on-end-of-stream
  else
    map(as-uppercase, sequence);
  end if
end;

define method read-into!
  (stream :: <test-wrapper-stream>, n :: <integer>, sequence :: <mutable-sequence>,
   #key start :: <integer> = 0, on-end-of-stream = unsupplied())
  => (count-or-eof)
  let count-or-eof = next-method();
  if (count-or-eof == on-end-of-stream)
    on-end-of-stream
  else
    let count :: <integer> = count-or-eof;
    for (i from 0 below count)
      sequence[i] := as-uppercase(sequence[i]);
    end;
    count
  end
end method read-into!;

define method stream-contents
  (stream :: <test-wrapper-stream>, #key clear-contents?)
  => (sequence :: <sequence>)
  let sequence = next-method();
  map(as-uppercase, sequence)
end;

define method stream-contents-as
  (type :: <class>, stream :: <test-wrapper-stream>, #key clear-contents?)
  => (sequence :: <sequence>)
  let sequence = next-method();
  sequence := map(as-uppercase, sequence);
  as(type, sequence);
end;

define method peek
  (stream :: <test-wrapper-stream>, #key on-end-of-stream = unsupplied()) => (element-or-eof)
  let element-or-eof = next-method();
  if (on-end-of-stream == element-or-eof)
    on-end-of-stream
  else
    as-uppercase(element-or-eof)
  end
end method peek;

define method write-element
    (stream :: <test-wrapper-stream>, elt :: <character>) => ()
  write-element(stream.inner-stream, as-uppercase(elt))
end method write-element;


/// Miscellaneous stream testing

// Note:  Refs to <unicode-character> and <unicode-string-stream> have been
//           commented out since they are not yet implemented.  1997-06-19

define test test-<buffer-index> ()
  // ---*** Fill this in.
end;

define test test-<byte-character> ()
  // ---*** Fill this in.
end;

/*
define test test-<unicode-character> ()
  // ---*** Fill this in.
end;
*/

define test test-<buffer> ()
  // ---*** Fill this in.
end;

define test test-<stream-position> ()
  // ---*** Fill this in.
end;

define test test-stream-lock ()
  // ---*** Fill this in.
end;

define test test-stream-lock-setter ()
  // ---*** Fill this in.
end;

define test test-outer-stream ()
  // ---*** Fill this in.
end;

define test test-outer-stream-setter ()
  // ---*** Fill this in.
end;

define test test-buffer-next ()
  // ---*** Fill this in.
end;

define test test-buffer-next-setter ()
  // ---*** Fill this in.
end;

define test test-buffer-end ()
  // ---*** Fill this in.
end;

define test test-buffer-end-setter ()
  // ---*** Fill this in.
end;

define test test-buffer-subsequence ()
  // ---*** Fill this in.
end;

define test test-copy-into-buffer! ()
  // ---*** Fill this in.
end;

define test test-copy-from-buffer! ()
  // ---*** Fill this in.
end;

define test test-do-get-input-buffer ()
  // ---*** Fill this in.
end;

define test test-do-get-output-buffer ()
  // ---*** Fill this in.
end;

define test test-do-input-available-at-source? ()
  // ---*** Fill this in.
end;

define test test-next-input-buffer ()
  // ---*** Fill this in.
end;

define test test-do-next-input-buffer ()
  // ---*** Fill this in.
end;

define test test-do-next-output-buffer ()
  // ---*** Fill this in.
end;

define test test-do-release-input-buffer ()
  // ---*** Fill this in.
end;

define test test-do-release-output-buffer ()
  // ---*** Fill this in.
end;

define test test-with-output-to-string ()
  let test-string = "Hello world";
  check-equal("with-output-to-string test",
              with-output-to-string (stream)
                write(stream, test-string)
              end,
              test-string)
end;

define test test-with-input-from-string ()
  let test-string = "Hello world";
  check-equal("with-input-from-string test, no class spec",
              test-string,
              with-input-from-string (stream = test-string)
                read-to-end(stream)
              end);
  check-equal("with-input-from-string test, with class spec",
              test-string,
              with-input-from-string (stream :: <string-stream> = test-string)
                read-to-end(stream)
              end);
end;


/// Indenting stream tests
register-stream-class-info("<indenting-stream>", <indenting-stream>,
                           input-stream?: #f,
                           output-stream?: #t,
                           element-type: <byte-character>);

define sideways method make-stream-tests-of-size
    (class :: subclass(<indenting-stream>), stream-size :: <integer>)
 => (tests :: <sequence>)
  let class-info = stream-class-info(class);
  let tests :: <stretchy-object-vector> = make(<stretchy-object-vector>);
  let character-sequence = copy-sequence($default-string, end: stream-size);
  local method add-stream-test-info () => ()
          add!(tests,
               make(<stream-test-info>,
                    test-name: format-to-string("%s %s size %d",
                                                #"output",
                                                class-info.info-class-name,
                                                stream-size),
                    class-info: class-info,
                    contents: character-sequence,
                    direction: #"output",
                    make-function: method () => (stream :: <indenting-stream>)
                                     let string-stream = make(<byte-string-stream>);
                                     make(class, inner-stream: string-stream)
                                   end))
        end method add-stream-test-info;
  add-stream-test-info();
  tests
end method make-stream-tests-of-size;

define test test-indent ()
  check-equal("indent works",
              "   hello",
              with-output-to-string (stream)
                let is = make(<indenting-stream>, inner-stream: stream);
                indent(is, 3);
                write(is, "hello");
              end);
  check-equal("indent combines",
              "      hello",
              with-output-to-string (stream)
                let is = make(<indenting-stream>, inner-stream: stream);
                indent(is, 3);
                indent(is, 3);
                write(is, "hello");
              end);
  check-equal("indent unindents",
              "hello",
              with-output-to-string (stream)
                let is = make(<indenting-stream>, inner-stream: stream);
                indent(is, 3);
                indent(is, -3);
                write(is, "hello");
              end);
  check-equal("indent obeys indentation",
              "    hello",
              with-output-to-string (stream)
                let is = make(<indenting-stream>, inner-stream: stream, indentation: 1);
                indent(is, 3);
                write(is, "hello");
              end);
end;

define test test-with-indentation ()
  check-equal("with-indentation test, default indentation",
              "    hello",
              with-output-to-string (stream)
                let is = make(<indenting-stream>, inner-stream: stream);
                with-indentation (is)
                  write(is, "hello");
                end;
              end);
  check-equal("with-indentation test, specifying indentation",
              "   hello",
              with-output-to-string (stream)
                let is = make(<indenting-stream>, inner-stream: stream);
                with-indentation (is, 3)
                  write(is, "hello");
                end;
              end);
end;


/// Miscellaneous stream tests

// This is a hack to replace the uses of stream-contents in the existing
// test suite.  Stream contents can't be used on output file streams.
// There are problems with opening multiple streams to a single
// file on the PC.
define method stream-contents-and-close
    (stream :: <stream>)
 => (the-contents :: <sequence>)
  let the-contents :: false-or(<sequence>) = stream-contents(stream);
  close(stream);
  the-contents
end method stream-contents-and-close;

define function default-stream-setup-function
    (stream, #key direction = #"input", contents = "")
 => (stream :: <object>)
  if (contents ~= "")
    make(stream, direction: direction, contents: contents);
  else
    make(stream, direction: direction);
  end if;
end function;

define function default-stream-cleanup-function (stream :: <stream>) => ()
  ignore(stream);
end function;

define method line-test
    (class :: subclass(<stream>),
     tester-without-line-end :: <string>,
     line-end :: <string>,
     #key setup-function = default-stream-setup-function,
     cleanup-function = default-stream-cleanup-function)

  let tester = concatenate(tester-without-line-end, line-end);

  let s = setup-function(class, direction: #"input", contents: tester);
  if (member?('\n', tester))
    check("read-line", \=, read-line(s), tester-without-line-end);
  else
    check-condition("read-line condition",
                    <end-of-stream-error>, read-line(s));
  end if;
  cleanup-function(s);

  s := setup-function(class, direction: #"output");
  write-line(s, tester-without-line-end);
  if (line-end.size = 2)
    let the-contents = stream-contents-and-close(s);
    check-true("write line worked?",
               (last(the-contents) = second(line-end)) &
                 (the-contents[the-contents.size - 2] = first(line-end)));
  else
    check("write line worked?", \=, last(stream-contents-and-close(s)), line-end.last);
  end if;
  cleanup-function(s);

  // one read-write test

  s := setup-function(class, direction: #"input-output", contents: tester);
  if (size(tester) > 0)
    check("read-write read check", \=, read-element(s), first(tester));
    write-element(s, third(tester));
    second(tester) := third(tester);
    check("read-write write check", \=, stream-contents-and-close(s), tester);
  end if;
  cleanup-function(s);
end method;

define method positionable-stream-test
    (class :: subclass(<positionable-stream>), tester,
     #key setup-function = default-stream-setup-function,
     cleanup-function = default-stream-cleanup-function)
 => ()
  let s = setup-function(class, direction: #"input", contents: tester);
  check("stream position is zero?", zero?, as(<integer>, stream-position(s)));
  check("stream position is end?", \=, size(tester),
        as(<integer>, adjust-stream-position(s, size(tester), from: #"start")));
  if (size(tester) > 2)
    stream-position-setter(2, s);
    check("stream position works?", \=,
          read-element(s),
          third(tester));
  end if;
  check("stream-size works?", \=, size(tester), stream-size(s));
  // unless (instance?(s, <file-stream>))
    stream-contents(s, clear-contents?: #t);
    check("clear contents", zero?,
          stream-size(s));
  // end unless;
  cleanup-function(s);
end method positionable-stream-test;

define test test-line-functions ()
  // String streams always use the "platform independent" terminator
  line-test(<string-stream>, "hello there!", "\n");
/*
  line-test(<file-stream>, "hello there!", $line-end,
            setup-function: create-file-stream,
            cleanup-function: destroy-file-stream);
*/
end test;

define test test-position-string-streams ()
  positionable-stream-test(<string-stream>, "hello there");
  positionable-stream-test(<string-stream>, "");
end test;

define test test-position-sequence-stream ()
  positionable-stream-test(<sequence-stream>, #(1, 2, 3));
  positionable-stream-test(<sequence-stream>, #());
end test;

define test test-position-alt-string-streams ()
  positionable-stream-test(<byte-string-stream>, "yo baby!");
  /*
  positionable-stream-test(<unicode-string-stream>,
                           concatenate("here we go baby", $line-end));
  */
end test;

define test test-stretchy-stream (description: "<string-stream> stretchy vector tests")
  begin
    let sv = make(<stretchy-vector>);
    let s = make(<string-stream>, contents: sv, direction: #"output");
    write(s, #(1, 2, 3));
    write(s, #(4, 5, 6));

    check-true("stretchy vector streched", sv = #(1, 2, 3, 4, 5, 6));
    check("stretchy vector", \=, stream-contents(s),
          as(<stretchy-vector>, list(1,2,3,4,5,6))  );
  end;

 begin
    let v = make(<vector>, size: 3);
    let s = make(<string-stream>, contents: v, direction: #"output");
    write(s, #(1, 2, 3));
    write(s, #(4, 5, 6));
    check-true("test vector",v = #[1, 2, 3]);
    check("test stream with vector", \=, stream-contents(s), #[1,2,3,4,5,6]);
  end;
  begin
    let sv = make(<stretchy-vector>);
    let s = make(<sequence-stream>, contents: sv, direction: #"output");
    write(s, #(1, 2, 3));
    write(s, #(4, 5, 6));
    check("test stream with stretchy vector", \=, stream-contents(s),
            as(<stretchy-vector>, list(1,2,3,4,5,6)));
  end;
 begin
    let v = make(<vector>, size: 3);
    let s = make(<sequence-stream>, contents: v, direction: #"output");
    write(s, #(1, 2, 3));
    write(s, #(4, 5, 6));
    check-true("test vector #2",v = #[1, 2, 3]);
    check-true("test stream vector", stream-contents(s) = #[1,2,3,4,5,6]);
  end;
end test;

define test test-<sequence-stream> ()
  test-stream-class(<sequence-stream>, instantiable?: #t);
end test;

define test test-<string-stream> ()
  test-stream-class(<string-stream>, instantiable?: #t);
end test;

define test test-<byte-string-stream> ()
  test-stream-class(<byte-string-stream>, instantiable?: #t);
end test;

define test test-<wrapper-stream> ()
  test-stream-class(<wrapper-stream>, instantiable?: #t);
end test;

define test test-<buffered-stream> ()
  test-stream-class(<buffered-stream>, instantiable?: #f);
end test;

define test test-<indenting-stream> ()
  test-stream-class(<indenting-stream>, instantiable?: #t);
end test;

define test test-<pretty-stream> ()
  test-stream-class(<pretty-stream>, instantiable?: #t);
end test;

define suite streams-test-suite ()
  test test-<sequence-stream>;
  test test-<string-stream>;
  test test-<byte-string-stream>;
  test test-<wrapper-stream>;
  test test-<buffered-stream>;
  test test-<indenting-stream>;
  test test-<pretty-stream>;
  test test-<buffer-index>;
  test test-<buffer>;
  test test-<byte-character>;
  test test-<stream-position>;
  //test test-<unicode-character>;
  test test-buffer-end-setter;
  test test-buffer-end;
  test test-buffer-next-setter;
  test test-buffer-next;
  test test-buffer-subsequence;
  test test-copy-from-buffer!;
  test test-copy-into-buffer!;
  test test-do-get-input-buffer;
  test test-do-get-output-buffer;
  test test-do-input-available-at-source?;
  test test-do-next-input-buffer;
  test test-do-next-output-buffer;
  test test-do-release-input-buffer;
  test test-do-release-output-buffer;
  test test-indent;
  test test-line-functions;
  test test-next-input-buffer;
  test test-outer-stream-setter;
  test test-outer-stream;
  test test-position-alt-string-streams;
  test test-position-sequence-stream;
  test test-position-string-streams;
  test test-stream-lock-setter;
  test test-stream-lock;
  test test-stretchy-stream;
  test test-with-indentation;
  test test-with-input-from-string;
  test test-with-output-to-string;
end suite;
