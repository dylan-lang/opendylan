Module:       io-test-suite
Synopsis:     IO library test suite
Author:       Andy Armstrong, et al...
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Useful constants

define constant $default-string  = "abcdefghijklmnopqrstuvwxyz";


/// Stream convenience function tests

register-stream-test(<stream>, test-read-character, direction: #"input");
register-stream-test(<stream>, test-read-text, direction: #"input");
register-stream-test(<stream>, test-read-text-into!, direction: #"input");
register-stream-test(<stream>, test-read-line, direction: #"input");
register-stream-test(<stream>, test-read-line-into!, direction: #"input");
register-stream-test(<stream>, test-read-to, direction: #"input");
register-stream-test(<stream>, test-read-through, direction: #"input");
register-stream-test(<stream>, test-read-to-end, direction: #"input");
register-stream-test(<stream>, test-skip-through, direction: #"input");
register-stream-test(<stream>, test-write-text, direction: #"output");
register-stream-test(<stream>, test-write-line, direction: #"output");
register-stream-test(<stream>, test-new-line, direction: #"output");

// Don't test the functions we're already testing... there must be a better way!
define streams function-test read-character () end;
define streams function-test read-text () end;
define streams function-test read-text-into! () end;
define streams function-test read-line () end;
define streams function-test read-line-into! () end;
define streams function-test read-to () end;
define streams function-test read-through () end;
define streams function-test read-to-end () end;
define streams function-test skip-through () end;
define streams function-test write-text () end;
define streams function-test write-line () end;
define streams function-test new-line () end;

define method test-read-character
    (info :: <stream-test-info>, stream :: <stream>) => ()
  //---*** Fill this in...
end method test-read-character;

define method test-read-text
    (info :: <stream-test-info>, stream :: <stream>) => ()
  //---*** Fill this in...
end method test-read-text;

define method test-read-text-into!
    (info :: <stream-test-info>, stream :: <stream>) => ()
  //---*** Fill this in...
end method test-read-text-into!;

define method test-read-line
    (info :: <stream-test-info>, stream :: <stream>) => ()
  //---*** Fill this in...
end method test-read-line;

define method test-read-line-into!
    (info :: <stream-test-info>, stream :: <stream>) => ()
  //---*** Fill this in...
end method test-read-line-into!;

define method test-read-to
    (info :: <stream-test-info>, stream :: <stream>) => ()
  //---*** Fill this in...
end method test-read-to;

define method test-read-through
    (info :: <stream-test-info>, stream :: <stream>) => ()
  //---*** Fill this in...
end method test-read-through;

define method test-read-to-end
    (info :: <stream-test-info>, stream :: <stream>) => ()
  //---*** Fill this in...
end method test-read-to-end;

define method test-skip-through
    (info :: <stream-test-info>, stream :: <stream>) => ()
  //---*** Fill this in...
end method test-skip-through;

define method test-write-text
    (info :: <stream-test-info>, stream :: <stream>) => ()
  //---*** Fill this in...
end method test-write-text;

define method test-write-line
    (info :: <stream-test-info>, stream :: <stream>) => ()
  //---*** Fill this in...
end method test-write-line;

define method test-new-line
    (info :: <stream-test-info>, stream :: <stream>) => ()
  //---*** Fill this in...
end method test-new-line;



/// Positionable stream convenience function tests

register-stream-test(<stream>, test-current-position);
register-stream-test(<stream>, test-current-position-setter);
register-stream-test(<stream>, test-initial-position);
register-stream-test(<stream>, test-final-position);

// Don't test the functions we're already testing... there must be a better way!
define streams function-test current-position () end;
define streams function-test current-position-setter () end;
define streams function-test initial-position () end;
define streams function-test final-position () end;

define method test-current-position
    (info :: <stream-test-info>, stream :: <positionable-stream>) => ()
  //---*** Fill this in...
end method test-current-position;

define method test-current-position-setter
    (info :: <stream-test-info>, stream :: <positionable-stream>) => ()
  //---*** Fill this in...
end method test-current-position-setter;

define method test-initial-position
    (info :: <stream-test-info>, stream :: <positionable-stream>) => ()
  //---*** Fill this in...
end method test-initial-position;

define method test-final-position
    (info :: <stream-test-info>, stream :: <positionable-stream>) => ()
  //---*** Fill this in...
end method test-final-position;



/// Buffered stream tests

register-stream-test(<buffered-stream>, test-get-input-buffer, direction: #"input");
register-stream-test(<buffered-stream>, test-release-input-buffer, direction: #"input");
register-stream-test(<buffered-stream>, test-with-input-buffer, direction: #"input");
register-stream-test(<buffered-stream>, test-input-available-at-source?, direction: #"input");
register-stream-test(<buffered-stream>, test-get-output-buffer, direction: #"output");
register-stream-test(<buffered-stream>, test-release-output-buffer, direction: #"output");
register-stream-test(<buffered-stream>, test-next-output-buffer, direction: #"output");
register-stream-test(<buffered-stream>, test-with-output-buffer, direction: #"output");
register-stream-test(<buffered-stream>, test-force-output-buffers, direction: #"output");

// Don't test the functions we're already testing... there must be a better way!
define streams function-test get-input-buffer () end;
define streams function-test release-input-buffer () end;
define streams function-test input-available-at-source? () end;
define streams function-test get-output-buffer () end;
define streams function-test release-output-buffer () end;
define streams function-test next-output-buffer () end;
define streams function-test force-output-buffers () end;

define streams macro-test with-input-buffer-test () end;
define streams macro-test with-output-buffer-test () end;

define method test-get-input-buffer
    (info :: <stream-test-info>, stream :: <buffered-stream>) => ()
  //---*** Fill this in...
end method test-get-input-buffer;

define method test-release-input-buffer
    (info :: <stream-test-info>, stream :: <buffered-stream>) => ()
  //---*** Fill this in...
end method test-release-input-buffer;

define method test-with-input-buffer
    (info :: <stream-test-info>, stream :: <buffered-stream>) => ()
  //---*** Fill this in...
end method test-with-input-buffer;

define method test-input-available-at-source?
    (info :: <stream-test-info>, stream :: <buffered-stream>) => ()
  //---*** Fill this in...
end method test-input-available-at-source?;

define method test-get-output-buffer
    (info :: <stream-test-info>, stream :: <buffered-stream>) => ()
  //---*** Fill this in...
end method test-get-output-buffer;

define method test-release-output-buffer
    (info :: <stream-test-info>, stream :: <buffered-stream>) => ()
  //---*** Fill this in...
end method test-release-output-buffer;

define method test-next-output-buffer
    (info :: <stream-test-info>, stream :: <buffered-stream>) => ()
  //---*** Fill this in...
end method test-next-output-buffer;

define method test-with-output-buffer
    (info :: <stream-test-info>, stream :: <buffered-stream>) => ()
  //---*** Fill this in...
end method test-with-output-buffer;

define method test-force-output-buffers
    (info :: <stream-test-info>, stream :: <buffered-stream>) => ()
  //---*** Fill this in...
end method test-force-output-buffers;


/// Sequence stream tests

register-stream-test(<sequence-stream>, test-type-for-sequence-stream);
register-stream-test(<sequence-stream>, test-stream-limit);
register-stream-test(<sequence-stream>, test-stream-limit-setter);

// Don't test the functions we're already testing... there must be a better way!
define streams function-test type-for-sequence-stream () end;
define streams function-test stream-limit () end;
define streams function-test stream-limit-setter () end;

define method test-type-for-sequence-stream
    (info :: <stream-test-info>, stream :: <sequence-stream>) => ()
  //---*** Fill this in...
end method test-type-for-sequence-stream;

define method test-stream-limit
    (info :: <stream-test-info>, stream :: <sequence-stream>) => ()
  //---*** Fill this in...
end method test-stream-limit;

define method test-stream-limit-setter
    (info :: <stream-test-info>, stream :: <sequence-stream>) => ()
  //---*** Fill this in...
end method test-stream-limit-setter;


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

register-stream-test(<wrapper-stream>, test-inner-stream);
register-stream-test(<wrapper-stream>, test-inner-stream-setter);

// Don't test the functions we're already testing... there must be a better way!
define streams function-test inner-stream () end;
define streams function-test inner-stream-setter () end;

define method test-inner-stream
    (info :: <stream-test-info>, stream :: <wrapper-stream>) => ()
  //---*** Fill this in...
end method test-inner-stream;

define method test-inner-stream-setter
    (info :: <stream-test-info>, stream :: <wrapper-stream>) => ()
  //---*** Fill this in...
end method test-inner-stream-setter;


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
	    contents: $default-string,
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

define class <test-wrapper-stream> (<wrapper-stream>)
end class <test-wrapper-stream>;

register-stream-class-info("<test-wrapper-stream>", <test-wrapper-stream>,
			   input-stream?: #t,
			   output-stream?: #t,
			   element-type: <object>);

define method read-element
    (stream :: <test-wrapper-stream>, #rest keys, #key on-end-of-stream)
 => (element :: <object>)
  let char :: <character> = next-method();
  as-uppercase(char)
end method read-element;

define method write-element
    (stream :: <test-wrapper-stream>, elt :: <character>) => ()
  write-element(stream.inner-stream, as-uppercase(elt))
end method write-element;


/// Miscellaneous stream testing

// Note:  Refs to <unicode-character> and <unicode-string-stream> have been
// 	  commented out since they are not yet implemented.  1997-06-19

define streams constant-test <buffer-index> ()
  // ---*** Fill this in.
end;

define streams constant-test <byte> ()
  // ---*** Fill this in.
end;

define streams constant-test <byte-character> ()
  // ---*** Fill this in.
end;

/*
define streams constant-test <unicode-character> ()
  // ---*** Fill this in.
end;
*/

define sideways method make-test-instance
    (class == <byte-vector>) => (vector :: <byte-vector>)
  make(<byte-vector>, size: 1, fill: 0)
end method make-test-instance;


define streams class-test <byte-vector> ()
  // ---*** Fill this in.
end;

define streams class-test <buffer> ()
  // ---*** Fill this in.
end;

define streams class-test <stream-position> ()
  // ---*** Fill this in.
end;

define streams function-test stream-lock ()
  // ---*** Fill this in.
end;

define streams function-test stream-lock-setter ()
  // ---*** Fill this in.
end;

define streams function-test outer-stream ()
  // ---*** Fill this in.
end;

define streams function-test outer-stream-setter ()
  // ---*** Fill this in.
end;

define streams function-test buffer-next ()
  // ---*** Fill this in.
end;

define streams function-test buffer-next-setter ()
  // ---*** Fill this in.
end;

define streams function-test buffer-end ()
  // ---*** Fill this in.
end;

define streams function-test buffer-end-setter ()
  // ---*** Fill this in.
end;

define streams function-test buffer-subsequence ()
  // ---*** Fill this in.
end;

define streams function-test copy-into-buffer! ()
  // ---*** Fill this in.
end;

define streams function-test copy-from-buffer! ()
  // ---*** Fill this in.
end;

define streams function-test do-get-input-buffer ()
  // ---*** Fill this in.
end;

define streams function-test do-get-output-buffer ()
  // ---*** Fill this in.
end;

define streams function-test do-input-available-at-source? ()
  // ---*** Fill this in.
end;

define streams function-test next-input-buffer ()
  // ---*** Fill this in.
end;

define streams function-test do-next-input-buffer ()
  // ---*** Fill this in.
end;

define streams function-test do-next-output-buffer ()
  // ---*** Fill this in.
end;

define streams function-test do-release-input-buffer ()
  // ---*** Fill this in.
end;

define streams function-test do-release-output-buffer ()
  // ---*** Fill this in.
end;

define streams macro-test with-output-to-string-test ()
  let test-string = "Hello world";
  check-equal("with-output-to-string test",
	      with-output-to-string (stream)
		write(stream, test-string)
	      end,
	      test-string)
end;


/// Miscellaneous stream tests

/*---*** andrewa: not currently used
define constant $line-end :: <string> 
  = select ($os-name)
      #"win32" => "\r\n";
      #"carbon" => "\r";
      otherwise => "\n";
    end;

define constant $line-boundary :: <character> = $line-end.first;
*/

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
end;

define suite universal-streams-suite ()
  test test-line-functions;
end suite universal-streams-suite;

define suite additional-streams-suite ()
  test test-position-string-streams;
  test test-position-sequence-stream;
  test test-position-alt-string-streams;
  test test-stretchy-stream;
end suite additional-streams-suite;
