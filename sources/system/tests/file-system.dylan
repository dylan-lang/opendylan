Module:       system-test-suite
Synopsis:     System library test suite
Author:	      Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// File stream tests

register-stream-class-info("<file-stream>", <file-stream>,
			   input-stream?: #t,
			   output-stream?: #t,
			   element-type: <object>);

define sideways method make-stream-tests-of-size
    (class :: subclass(<file-stream>), stream-size :: <integer>)
 => (streams :: <sequence>)
  let class-info = stream-class-info(<wrapper-stream>);
  let tests :: <stretchy-object-vector> = make(<stretchy-object-vector>);
  //---*** Nothing yet...
  tests
end method make-stream-tests-of-size;

/// File system classes

define file-system class-test <file-error> ()
  // ---*** Fill this in.
end;

define file-system class-test <file-exists-error> ()
  // ---*** Fill this in.
end;

define file-system class-test <file-does-not-exist-error> ()
  // ---*** Fill this in.
end;

define file-system class-test <invalid-file-permissions-error> ()
  // ---*** Fill this in.
end;

define file-system function-test type-for-file-stream ()
  // ---*** Fill this in.
end;

define file-system class-test <pathname> ()
  check-instance?("A string is a pathname?",
		  <pathname>, "foo");
  check-instance?("A file locator is a pathname?",
		  <pathname>, as(<file-locator>, "test.dylan"));
end class-test <pathname>;

define file-system class-test <file-type> ()
  //---*** Fill this in.
end;

define file-system class-test <copy/rename-disposition> ()
  //---*** Fill this in.
end;


/// File-system function test cases

define file-system function-test file-exists? ()
  //---*** Fill this in.
end;

define file-system function-test file-type ()
  //---*** Fill this in.
end;

define file-system function-test delete-file ()
  //---*** Fill this in.
end;

define file-system function-test copy-file ()
  //---*** Fill this in.
end;

define file-system function-test rename-file ()
  //---*** Fill this in.
end;

define file-system function-test file-properties ()
  //---*** Fill this in.
end;

define file-system function-test file-property ()
  //---*** Fill this in.
end;

define file-system function-test file-property-setter ()
  //---*** Fill this in.
end;

define file-system function-test do-directory ()
  //---*** Fill this in.
end;

/// directory-contents is NYI currently ...
define file-system function-test directory-contents ()
  //---*** Fill this in.
end;

define file-system function-test create-directory ()
  //---*** Fill this in.
end;

define file-system function-test delete-directory ()
  //---*** Fill this in.
end;

define file-system function-test ensure-directories-exist ()
  //---*** Fill this in.
end;

define file-system function-test home-directory ()
  //---*** Fill this in.
end;

define file-system function-test temp-directory ()
  //---*** Fill this in.
end;

define file-system function-test root-directories ()
  //---*** Fill this in.
end;


/// Macro tests

define file-system macro-test with-open-file-test ()
  // ---*** Fill this in.
end macro-test with-open-file-test;


/// File stream test utilities

define variable *tbs* = 4;	// buffer-size for testing purposes

define variable *hello-file*
  = temp-file-pathname(initial-substring: "dylan-streams-test-hello");

define method stream-contents-and-close
    (stream :: <file-stream>)
 => (the-contents :: <sequence>)
  let the-contents :: false-or(<sequence>) = #f;
  let original-element-type = stream-element-type(stream);
  let the-locator = stream.stream-locator;
  close(stream);
  let read-stream = #f;
  the-contents :=
    block ()
      read-stream := make(<file-stream>,
			  direction: #"input",
			  locator: the-locator,
			  element-type: original-element-type);
      stream-contents(read-stream)
    cleanup
      if (read-stream) close(read-stream) end;
    end;
  the-contents
end method stream-contents-and-close;

define method create-file-stream 
    (stream-class :: <class>, // subclass(<file-stream>)  
     #key direction: direction :: one-of(#"input", #"output",
					 #"input-output") = #"input",
     contents: contents :: <sequence> = "") 
 => (result-stream :: <file-stream>)
  let temp-file-locator =  temp-file-pathname(initial-substring: "d-s-t");
  if (contents = "")
    make(stream-class, direction: direction, locator: temp-file-locator, 
	 if-exists: #"replace", if-does-not-exist: #"create");
  else
    let temporary-output-stream =
      make(stream-class, direction: #"output", locator: temp-file-locator, 
	   if-exists: #"replace", if-does-not-exist: #"create");
    write(temporary-output-stream, contents);
    close(temporary-output-stream);
    make(stream-class, direction: direction, locator: temp-file-locator);
  end if;
end method;

define method create-file-stream-exclusive 
    (stream-class :: <class>, // subclass(<file-stream>)  
     #key direction: direction :: one-of(#"input", #"output",
					 #"input-output") = #"input",
     contents: contents :: <sequence> = "") 
 => (result-stream :: <file-stream>)
  let temp-file-locator =  temp-file-pathname(initial-substring: "d-s-t");
  if (contents = "")
    make(stream-class, direction: direction, locator: temp-file-locator, 
	 if-exists: #"replace", if-does-not-exist: #"create", share?: #f);
  else
    let temporary-output-stream =
      make(stream-class, direction: #"output", locator: temp-file-locator, 
	   if-exists: #"replace", if-does-not-exist: #"create");
    write(temporary-output-stream, contents);
    close(temporary-output-stream);
    make(stream-class, direction: direction, locator: temp-file-locator,
	 share?: #f);
  end if;
end method;

// This is the equivalent of the old create file stream which always used
// input-output streams and reset the location.  Kept to diagnose bugs not
// found with other tests.
define method create-input-output-file-stream 
    (stream-class :: <class>, // subclass(<file-stream>)  
     #key direction :: one-of(#"input", #"output",
			      #"input-output") = #"input",
     contents :: <sequence> = "") 
 => (result-stream :: <file-stream>)
  ignore(direction);
  let temp-file-locator = temp-file-pathname(initial-substring: "ciofs");
  if (contents = "")
    make(stream-class, direction: #"input-output", locator: temp-file-locator, 
	 if-exists: #"replace", if-does-not-exist: #"create");
  else
    let result-stream =
      make(stream-class, direction: #"input-output", 
	   locator: temp-file-locator, 
	   if-exists: #"replace", if-does-not-exist: #"create");
    write(result-stream, contents);
    stream-position(result-stream) := #"start"; 
    result-stream
  end if
end method;

define method create-multi-buffered-file-stream 
    (stream-class :: <class>, // subclass(<file-stream>)  
     #key direction: direction = #"input-output",
	  buffer-size = 4, number-of-buffers = 4,
	  contents: contents :: <sequence> = "")
 => (result-stream :: <file-stream>)
  ignore(direction);
  let temp-file-locator =  temp-file-pathname(initial-substring: "cmbfs");
  if (contents = "")
    make(stream-class, locator: temp-file-locator, 
	 buffer-size: buffer-size, number-of-buffers: number-of-buffers,
	 if-exists: #"replace", if-does-not-exist: #"create");
  else
    let temporary-output-stream =
      make(<file-stream>, direction: #"output", locator: temp-file-locator, 
	   if-exists: #"replace", if-does-not-exist: #"create");
    write(temporary-output-stream, contents);
    close(temporary-output-stream);
    make(stream-class, locator: temp-file-locator,
	 buffer-size: buffer-size, number-of-buffers: number-of-buffers);
  end if;
end method;

define method create-multi-buffered-file-stream-exclusive 
    (stream-class :: <class>, // subclass(<file-stream>)  
     #key direction: direction = #"input-output",
	  buffer-size = 4, number-of-buffers = 4,
	  contents: contents :: <sequence> = "")
 => (result-stream :: <file-stream>)
  ignore(direction);
  let temp-file-locator =  temp-file-pathname(initial-substring: "cmbfs");
  if (contents = "")
    make(stream-class, locator: temp-file-locator, 
	 buffer-size: buffer-size, number-of-buffers: number-of-buffers,
	 if-exists: #"replace", if-does-not-exist: #"create", share?: #f);
  else
    let temporary-output-stream =
      make(<file-stream>, direction: #"output", locator: temp-file-locator, 
	   if-exists: #"replace", if-does-not-exist: #"create");
    write(temporary-output-stream, contents);
    close(temporary-output-stream);
    make(stream-class, locator: temp-file-locator,
	 buffer-size: buffer-size, number-of-buffers: number-of-buffers,
	 share?: #f);
  end if;
end method;

define method destroy-file-stream (the-stream :: <file-stream>) => ()
  let the-file-locator = the-stream.stream-locator;
  if (stream-open?(the-stream))
    close(the-stream);
  end if;
  delete-file(the-file-locator);
end method;


/// Miscellaneous tests

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

define method universal-stream-test 
    (class :: subclass(<stream>), tester, 
     #key setup-function = default-stream-setup-function,
     cleanup-function = default-stream-cleanup-function)
  let s = setup-function(class, direction: #"input", contents: tester);
  if (size(tester) > 0)
    check("read first element", \=, read-element(s), first(tester));
    check("unread first element", \=, unread-element(s, first(tester)), first(tester));
    check("peeks first element", \=, peek(s), first(tester));
    check("read first element again", \=, read-element(s), first(tester));
    // discard input
    //discard-input(s);
    //check("stream should be at end", stream-at-end?, s);
  else
    check-condition("end of stream should be reached", <end-of-stream-error>, read-element(s));
    end if;
  if (size(tester) > 1)
    check("should be on second element", \=, read-element(s), second(tester));
  else
    check-condition("end of stream should be reached", <end-of-stream-error>, read-element(s));
  end if;
  cleanup-function(s);
  
  s := setup-function(class, direction: #"input", contents: tester);
  if (size(tester) > 2)
    let result = read(s, 3);
    check("first element matched?", \=, first(result), first(tester));
    check("second element matched?", \=, second(result), second(tester));
    check("third element matched?", \=, third(result), third(tester));
  elseif (size(tester) > 0)
    check-condition("expect <incomplete-read-error>", <incomplete-read-error>,
		   read(tester, 3));
  else
    check-condition("empty error", <end-of-stream-error>, read(s, 3));
  end if;
  cleanup-function(s);

  s := setup-function(class, direction: #"input", contents: tester);
  let my-list = list(1, 2, 3);
  if (size(tester)> 2)
    read-into!(s, 3, my-list);
    check("first element matched?", \=, as(<integer>, first(my-list)), 
	  as(<integer>, first(tester)));
    check("second element matched?", \=, as(<integer>, second(my-list)), 
	  as(<integer>, second(tester)));
    check("third element matched?", \=, as(<integer>, third(my-list)), 
	  as(<integer>,third(tester)));
   elseif (size(tester) > 0)
    check-condition("expect <incomplete-read-error>", <incomplete-read-error>,
		   read-into!(tester, 3, my-list));
  else
    check-condition("empty error", <end-of-stream-error>, read-into!(s, 3, my-list));
  end if;
  cleanup-function(s);
  
  s := setup-function(class, direction: #"input", contents: tester);  
  if (size(tester) > 3)
    // read-to second of the tester should return a one element sequence
    check("read-to returns correct result", \=,
	  copy-sequence(tester, end: 1), read-to(s, second(tester)));
    check("read should be on third after read-to second",
	  \=, read-element(s), third(tester));
  end if;
  cleanup-function(s);
  
  s := setup-function(class, direction: #"input", contents: tester);
  if (size(tester) > 3)
    // read-through second of the tester should return a two element sequence
    check("read-through returns correct result", \=,
	  copy-sequence(tester, end:  2), read-through(s, second(tester)));
    check("read should be on third after read-through second",
	  \=, read-element(s), third(tester));
  end if;
  cleanup-function(s);
  
  s := setup-function(class, direction: #"input", contents: tester);
  // querying streams !!
  check("is stream open?", stream-open?, s);
  check("read-to-end", \=, read-to-end(s), tester);
  check("stream-at-end?", stream-at-end?, s);
  cleanup-function(s);

  s := setup-function(class, direction: #"input", contents: tester);
  if (size(tester) > 2)
    check("find second element", skip-through, s, second(tester));
    check("should be on third element", \=, read-element(s), third(tester));
    //if (~member?('\\', tester))
    //  check-false("find non existing element", skip-through(s, '\\'));
    //end if;
  end if;
  cleanup-function(s);

  // write tests
  // First test if the setup function works:
//   check("setup function with direction #\"output\" works",
// 	stream-open?, s := setup-function(stream, direction: #"output"));
//   cleanup-function(s);

  if (size(tester) > 0)
    s := setup-function(class, direction: #"output");
    write-element(s, first(tester));
    check("write first element?", \=, first(tester), 
	  first(stream-contents-and-close(s)));
    cleanup-function(s);
    
    s := setup-function(class, direction: #"output");
    write(s, tester);
    check("write work?", \=, tester, stream-contents-and-close(s));
    cleanup-function(s);
  end if;
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
  unless (instance?(s, <file-stream>))
    stream-contents(s, clear-contents?: #t);
    check("clear contents", zero?, 
	  stream-size(s));
  end;
  cleanup-function(s);
end method positionable-stream-test;

define test test-file-stream ()
   universal-stream-test(<file-stream>, "check it out",
			setup-function: create-file-stream,
			cleanup-function: destroy-file-stream);
end test;

define test test-empty-file-stream ()
  universal-stream-test(<file-stream>, "",
			setup-function: create-file-stream,
			cleanup-function: destroy-file-stream);
end test;


define test test-file-stream-exclusive ()
   universal-stream-test(<file-stream>, "check it out",
			setup-function: create-file-stream-exclusive,
			cleanup-function: destroy-file-stream);
end test;

define test test-empty-file-stream-exclusive ()
  universal-stream-test(<file-stream>, "",
			setup-function: create-file-stream-exclusive,
			cleanup-function: destroy-file-stream);
end test;

define test bogus-test-file-stream-using-input-output-streams ()
   universal-stream-test(<file-stream>, "check it out",
			setup-function: create-input-output-file-stream,
			cleanup-function: destroy-file-stream);
end test;

define test bogus-test-empty-file-stream-using-input-output-streams ()
  universal-stream-test(<file-stream>, "",
			setup-function: create-input-output-file-stream,
			cleanup-function: destroy-file-stream);
end test;


define test test-multi-buffered-file-stream ()
   universal-stream-test(<multi-buffered-stream>, "check it out",
			setup-function: create-multi-buffered-file-stream,
			cleanup-function: destroy-file-stream);
end test;

define test test-empty-multi-buffered-file-stream ()
  universal-stream-test(<multi-buffered-stream>, "",
			setup-function: create-multi-buffered-file-stream,
			cleanup-function: destroy-file-stream);
end test;


define test test-multi-buffered-file-stream-with-long-input ()
   universal-stream-test(<multi-buffered-stream>, 
			 "check out multi buffered streams with a long string",
			setup-function: create-multi-buffered-file-stream,
			cleanup-function: destroy-file-stream);
end test;

define test test-multi-buffered-file-stream-exclusive ()
   universal-stream-test(<multi-buffered-stream>, "check it out",
			setup-function: 
			   create-multi-buffered-file-stream-exclusive,
			cleanup-function: destroy-file-stream);
end test;

define test test-empty-multi-buffered-file-stream-exclusive ()
  universal-stream-test(<multi-buffered-stream>, "",
			setup-function: 
			  create-multi-buffered-file-stream-exclusive,
			cleanup-function: destroy-file-stream);
end test;


define test test-multi-buffered-file-stream-with-long-input-exclusive ()
   universal-stream-test(<multi-buffered-stream>, 
			 "check out multi buffered streams with a long string",
			setup-function: 
			   create-multi-buffered-file-stream-exclusive,
			cleanup-function: destroy-file-stream);
end test;

define test test-position-file-streams ()
  positionable-stream-test(<file-stream>, "check it out",
			   setup-function: create-file-stream,
			   cleanup-function: destroy-file-stream);
  positionable-stream-test(<file-stream>, "",
			   setup-function: create-file-stream,
			   cleanup-function: destroy-file-stream);
end test;


define test bogus-test-position-file-streams-using-input-output-streams ()
  positionable-stream-test(<file-stream>, "check it out",
			   setup-function: create-input-output-file-stream,
			   cleanup-function: destroy-file-stream);
  positionable-stream-test(<file-stream>, "",
			   setup-function: create-input-output-file-stream,
			   cleanup-function: destroy-file-stream);
end test;

define test test-position-multi-buffered-file-streams ()
  positionable-stream-test(<multi-buffered-stream>, "check it out",
			   setup-function: create-multi-buffered-file-stream,
			   cleanup-function: destroy-file-stream);
  positionable-stream-test(<multi-buffered-stream>, "",
			   setup-function: create-multi-buffered-file-stream,
			   cleanup-function: destroy-file-stream);
end test;

define test test-file-stream-creation (title: "<file-stream> make tests")
  let path = *hello-file*;
  check-true("make <file-stream> - create or overwrite", begin
	  let s = make(<file-stream>, locator: path, if-exists: #"overwrite", 
		       if-does-not-exist: #"create", 
		       element-type: <byte-character>, 
		       buffer-size: 1);
	  close(s);
	  #t;
	end);
  check-true("make <file-stream> - replace or signal",begin
	  let s = make(<file-stream>, locator: path, if-exists: #"replace", 
		       if-does-not-exist: #"signal", element-type: <character>, 
		       buffer-size: 12);
	  close(s);
	  #t;
	end);
  check-true("make <file-stream> - append or signal",begin
	  let s = make(<file-stream>, locator: path, if-exists: #"append", 
		       if-does-not-exist: #"signal", element-type: <byte>, 
		       buffer-size: 50);
	  close(s);
	  #t;
	end);
  check-true("make <file-stream> - truncate or signal",begin
	  let s = make(<file-stream>, locator: path, if-exists: #"truncate", 
		       if-does-not-exist: #"signal", buffer-size: 5);
	  close(s);
	  #t;
	end);
  check-condition("file does not exist error", <file-exists-error>, 
		  make(<file-stream>, locator: path, direction: #"output",
		       if-exists: #"signal", if-does-not-exist: #"signal", 
		       buffer-size: 5));
end test;

				 
define constant $line-end :: <string> 
  = select ($os-name)
      #"win32" => "\r\n";
      #"carbon" => "\r";
      otherwise => "\n";
    end;

define constant $line-end-size :: <integer> = $line-end.size;
  
define test test-file-read-stream (title: "<file-stream> read tests")
  let path = *hello-file*;
 //check-false("make hello-file",condition(make-hello-file()));
  begin
    let s = make(<file-stream>, direction: #"input-output", 
		 locator: path, buffer-size: *tbs*);
    write-line(s, "hello world");
    check-true("Is stream size correct?", 
	       (11 + $line-end-size) = stream-size(s));
    check-true("Is stream hello world?",
	       concatenate("hello world", $line-end) = 
		 stream-contents-and-close(s));
  end;
  begin
    let s = make(<file-stream>, locator: path, buffer-size: *tbs*);
    check-false("<file-stream> should not be at end",stream-at-end?(s));
    check-true("read hello world a stream of 11","hello world" = read(s, 11));
    check-false("Stream should STILL not be at end",stream-at-end?(s));
    close(s);
  end;
  begin
    let n = (11 + $line-end-size);
    let s = make(<file-stream>, locator: path, buffer-size: *tbs*);
    let str = make(<string>, size: n);
    check-false("<string> should not be at end",stream-at-end?(s));
    check-true("read chars into string str", n = read-into!(s, n, str));
    check-true("Str should be old hello world",
	       concatenate("hello world", $line-end) = str);
    check-true("Now stream should be-at-end",stream-at-end?(s));
    close(s);
  end;
/* 
  begin
    let s = make(<file-stream>, locator: path, buffer-size: *tbs*);
    let str = make(<string>, size: 13);
    let ire = condition(read-into!(s, 14, str));
    check-instance?("signals an <incomplete-read-error>",
		    <incomplete-read-error>, ire);
    check-instance?("The stream-error-stream should be a <file-stream>",
		    <file-stream>, ire.stream-error-stream);
    check-true("hello world should be the error sequence",
	       "hello world\n  " = ire.stream-error-sequence);
    check-true("the error-count should be 12",12 = ire.stream-error-count);
    close(s);
  end;
*/
 begin
    let s = make(<file-stream>, locator: path, buffer-size: *tbs*);
    let str = shallow-copy("=>.....|.....<=");
    check-false("This file-stream should not be at end",stream-at-end?(s));
    check-true("Read hello world into the string",11 = read-into!(s, 11, str, start: 2));
    check-true("Is the string hello world","=>hello world<=" = str);
    check-false("The stream should still not be at end",stream-at-end?(s));
    close(s);
  end;
  begin
    let s = make(<file-stream>, locator: path, buffer-size: *tbs*);
    check-true("is hello world read-to-end(s)",
	       concatenate("hello world", $line-end) = read-to-end(s));
    close(s);
  end;
  begin
    let s = make(<file-stream>, locator: path, buffer-size: *tbs*);
    check-true("The first element is h",'h' = read-element(s));
    check-true("The peek is e",'e' = peek(s));
    check-true("The next element is e",'e' = read-element(s));
    check-true("The unread element is e",'e' = unread-element(s, 'e'));
    check-true("The next element is now again e",'e' = read-element(s));
// This check is wrong.  The specification says that unreading an element
// which wasn't read is an error.  It doesn't say that the error has to be
// detected and a condition raised.
//     check-condition("Unread element which wasn't read", 
// 		    <error>, unread-element(s, 'z'));
    //discard-input(s);
    //check-true("The stream should now be at the end",stream-at-end?(s));
    close(s);
  end; 
  begin
    let s = make(<file-stream>, locator: path, buffer-size: *tbs*);
    check-true("skip the 'o'",skip-through(s, 'o'));
    check-true("The next element should be a space",' ' = read-element(s));
    close(s);
 end;
end test test-file-read-stream;

 

define test test-file-write-stream (title: "<file-stream> write tests")
  let path = temp-file-pathname(initial-substring: "tfws");
  //unix-unlink(asstring(path));
  begin
    let s = make(<file-stream>, locator: path, buffer-size: *tbs*,
		 direction: #"output", if-exists: #"replace",
		 element-type: <byte-character>);
    write(s, "hello there");
    write-line(s, " world");
    close(s);
    let s = make(<file-stream>, locator: path, buffer-size: *tbs*,
		 direction: #"input", element-type: <byte-character>);
    check-true("Stream contents = hello there world",
	       concatenate("hello there world", $line-end) =
		 stream-contents-and-close(s));
    destroy-file-stream(s);
  end;
  begin
    let s = make(<file-stream>, locator: path, buffer-size: *tbs*,
		 direction: #"output", if-exists: #"replace",
		 element-type: <byte-character>);
    write(s, "hello there");
    close(s);
    let s = make(<file-stream>, locator: path, buffer-size: *tbs*,
		 direction: #"output", if-exists: #"append",
		 element-type: <byte-character>);
    write-line(s, " sailor");
    close(s);
    let s = make(<file-stream>, locator: path, buffer-size: *tbs*,
		 direction: #"input", element-type: <byte-character>);
    check-true("Open output file with if-exists = append and write",
	       concatenate("hello there sailor", $line-end) =
		 stream-contents-and-close(s));
    destroy-file-stream(s);
  end;
  begin
    // This is a read-write test but the read-write tests are such a mess
    // that I don't even want to touch them!!
    let s = make(<file-stream>, locator: path, buffer-size: *tbs*,
		 direction: #"output", if-exists: #"replace",
		 element-type: <byte-character>);
    write(s, "hello there");
    close(s);
    let s = make(<file-stream>, locator: path, buffer-size: *tbs*,
		 direction: #"input-output", if-exists: #"append",
		 element-type: <byte-character>);
    write-line(s, " sailor");
    close(s);
    let s = make(<file-stream>, locator: path, buffer-size: *tbs*,
		 direction: #"input", element-type: <byte-character>);
    check-true("Open output file with if-exists = append and write",
	       concatenate("hello there sailor", $line-end) =
		 stream-contents-and-close(s));
    destroy-file-stream(s);
  end;
  begin
    let s = make(<file-stream>, locator: path, buffer-size: *tbs*,
		 direction: #"output", if-exists: #"replace",
		 element-type: <character>);
    write(s, "hello there");
    write-line(s, " world");
    close(s);
    let s = make(<file-stream>, locator: path, buffer-size: *tbs*,
		 direction: #"input", element-type: <character>);
    check-true("Contents hello there world",
	       concatenate("hello there world", $line-end) = stream-contents-and-close(s));
  end;
  begin
    let c = concatenate("hello there world", $line-end);
    let n = size(c);
    let s = make(<file-stream>, locator: path, buffer-size: *tbs*,
		 direction: #"output", if-exists: #"replace");
    write(s, c);
    check-true("Stream position should be n", n = stream-position(s));
    check-true("Contents still hello there world", c =
		 stream-contents-and-close(s));
    destroy-file-stream(s);
  end;
end test test-file-write-stream;

define test test-write-to-multi-buffered-file-stream (title: "<multi-buffered--stream> write tests")
  let path = temp-file-pathname(initial-substring: "tfws");
  begin
    let s = make(<multi-buffered-stream>, locator: path, buffer-size: 4,
		 number-of-buffers: 2, if-exists: #"replace",
		 element-type: <byte-character>);
    write(s, "hello there");
    write-line(s, " world");
    close(s);
    let s = make(<file-stream>, locator: path, buffer-size: *tbs*,
		 direction: #"input", element-type: <byte-character>);
    check-true("Stream contents = hello there world",
	       concatenate("hello there world", $line-end) =
		 stream-contents-and-close(s));
    destroy-file-stream(s);
  end;
  begin
    let s = make(<file-stream>, locator: path, buffer-size: *tbs*,
		 direction: #"output", if-exists: #"replace",
		 element-type: <byte-character>);
    write(s, "hello there");
    close(s);
    let s = make(<multi-buffered-stream>, locator: path, buffer-size: 4,
		 number-of-buffers: 2, if-exists: #"append",
		 element-type: <byte-character>);
    write-line(s, " sailor");
    close(s);
    let s = make(<file-stream>, locator: path, buffer-size: *tbs*,
		 direction: #"input", element-type: <byte-character>);
    check-true("Open output file with if-exists = append and write",
	       concatenate("hello there sailor", $line-end) =
		 stream-contents-and-close(s));
    destroy-file-stream(s);
  end;
  begin
    let s = make(<multi-buffered-stream>, locator: path, buffer-size: 4,
		 number-of-buffers: 2, if-exists: #"replace",
		 element-type: <character>);
    write(s, "hello there");
    write-line(s, " world");
    close(s);
    let s = make(<file-stream>, locator: path, buffer-size: *tbs*,
		 direction: #"input", element-type: <character>);
    check-true("Contents hello there world",
	       concatenate("hello there world", $line-end) = stream-contents-and-close(s));
  end;
  begin
    let c = concatenate("hello there world", $line-end);
    let n = size(c);
    let s = make(<multi-buffered-stream>, locator: path, buffer-size: 4,
		 number-of-buffers: 2, if-exists: #"replace");
    write(s, c);
    check-true("Stream position should be n", n = stream-position(s));
    check-true("Contents still hello there world", c =
		 stream-contents-and-close(s));
    destroy-file-stream(s);
  end;
end test test-write-to-multi-buffered-file-stream;

// This uses the existing *hello-file* gross
define test test-file-read-write-stream (title: "<file-stream> read-write tests")
  let path = *hello-file*;
  let c = concatenate("hello world", $line-end);
  let n = size(c);
  begin
    let s = make(<file-stream>, locator: path, buffer-size: *tbs*,
		 direction: #"input-output",
		 if-exists: #"overwrite", if-does-not-exist: #"signal");
    check-true("Stream size should now be n",n = stream-size(s));
    check-true("Contents are still hello world", 
	       c = stream-contents-and-close(s));
  end;
  begin
    let s = make(<file-stream>, locator: path, buffer-size: *tbs*,
		 direction: #"input-output",
		 if-exists: #"overwrite", if-does-not-exist: #"signal");
    check-false("Stream cant be at the end",stream-at-end?(s));
    check-true("Read 11 chars of hello world","hello world" = read(s, 11));
    check-false("Stream still cant be at the end",stream-at-end?(s));
    close(s);
  end;
  begin
    let s = make(<file-stream>, locator: path, buffer-size: *tbs*,
		 direction: #"input-output",
		 if-exists: #"overwrite", if-does-not-exist: #"signal");
    let str = make(<string>, size: n);
    check-false("String stream isnt at the end",stream-at-end?(s));
    check-true("Reading 12 chars into it", n = read-into!(s, n, str));
    check-true("String is same as hello world", c = str);
    check-true("Now at the end of stream",stream-at-end?(s));
    close(s);
  end;
  /*
  begin
    let s = make(<file-stream>, locator: path, buffer-size: *tbs*,
		 direction: #"input-output",
		 if-exists: #"overwrite", if-does-not-exist: #"signal");
    let str = make(<string>,size: 14);
    let ire = condition(read-into!(s, 14, str));
    check-instance?("should signal an incomplete-read-error",
		    <incomplete-read-error>, ire);
    check-instance?("The stream error stream is <file-stream>",
		    <file-stream>, ire.stream-error-stream);
    check-true("hello world is the error sequence","hello world\n  " = ire.stream-error-sequence);
    check-true("The error count is 12",12 = ire.stream-error-count);
    close(s);
  end;
  */
  begin
    let s = make(<file-stream>, locator: path, buffer-size: *tbs*,
		 direction: #"input-output",
		 if-exists: #"overwrite", if-does-not-exist: #"signal");
    let str = shallow-copy("=>.....|.....<=");
    check-false("Stream should not be at the end",stream-at-end?(s));
    check-true("Reading 11 chars into str",11 = read-into!(s, 11, str, start: 2));
    check-true("Hello world should be the str","=>hello world<=" = str);  
    check-false("Still not at the end",stream-at-end?(s));
    close(s);
  end;
  begin
    let s = make(<file-stream>, locator: path, buffer-size: *tbs*,
		 direction: #"input-output",
		 if-exists: #"overwrite", if-does-not-exist: #"signal");
    check-true("read-to-end hello world",
	       concatenate("hello world", $line-end) = read-to-end(s));
    close(s);
    destroy-file-stream(s);
  end;

  path := temp-file-pathname(initial-substring: "tfrws-new");
  let new-c = concatenate("hello there world", $line-end);
  let new-n = size(new-c);

  begin
    let s = make(<file-stream>, locator: path, buffer-size: *tbs*,
		 direction: #"input-output", if-exists: #"replace");
    write(s, "hello there");
    write-line(s, " world");
    check-true("The position is now new-n", new-n = stream-position(s));
    check-true("The contents are hello there world",
	       new-c = stream-contents-and-close(s));
  end;
  begin
    let s = make(<file-stream>, locator: path, buffer-size: *tbs*,
		 direction: #"input-output", if-exists: #"replace");
    write(s, new-c);
    check-true("The position is still new-n", new-n = stream-position(s));
    check-true("The contents are still hello there world",
	       new-c = stream-contents-and-close(s));
  end;
  with-open-file (s = path)
  end;
  with-open-file (s = path, buffer-size: *tbs*,
		  direction: #"input-output", if-exists: #"replace")
    write(s, "01234567890123456789");
    stream-position(s) := 5;
    write(s, "ABCDEFG");
    check-true("contents: 01234ABCDEFG23456789",
	       "01234ABCDEFG23456789" = stream-contents-and-close(s));
    destroy-file-stream(s);
  end;
end test test-file-read-write-stream;

define test test-file-stream-functions (title: "<file-stream> function tests")
  let path = temp-file-pathname(initial-substring: "tfsf1");

  begin
    let s = make(<file-stream>, locator: path, buffer-size: (*tbs* + 1),
		 direction: #"output", if-exists: #"replace", 
		 element-type: <byte>);
    write-line(s, "hello there");
    close(s);
    let s = make(<file-stream>, locator: path, buffer-size: *tbs*,
		 direction: #"output", if-exists: #"overwrite",
		 element-type: <byte>);
    write(s, "yummy");
    close(s);
    let s = make(<file-stream>, locator: path, buffer-size: *tbs*,
		 direction: #"input", element-type: <byte>);
    check-true("coercing contents to string yummy there",
	       concatenate("yummy there", $line-end) = 
		 as(<string>, stream-contents-and-close(s)));
  end;
  begin
    let s = make(<file-stream>, locator: path, buffer-size: (*tbs* + 1),
		 direction: #"output", if-exists: #"replace", 
		 element-type: <byte>);
    write-line(s, "hello there");
    close(s);
    let s = make(<file-stream>, locator: path, buffer-size: *tbs*,
		 direction: #"output", if-exists: #"overwrite",
		 element-type: <byte>);
    write(s, "yummy");
    close(s);
    let s = make(<file-stream>, locator: path, buffer-size: *tbs*,
		 direction: #"input", element-type: <byte>);
    check-true("yummy there is the contents as a <string>",
	       concatenate("yummy there", $line-end) = 
		 (as(<string>, stream-contents-and-close(s))));
  end;
/*
 begin
    let s = make(<file-stream>, locator: path, buffer-size: *tbs*,
		 direction: #"output", if-exists: #"replace", 
		 element-type: <byte>);
    write-line(s, "hello there");
    close(s);
    let s = make(<file-stream>, locator: path, buffer-size: *tbs*,
		 direction: #"output", if-exists: #"append", 
                                        // #"append" is not implemented?
		 element-type: <character>); // <character> and
                                        // <byte-character>  not 
 	                                // implemented yet?
    write-line(s, "yummy");
    close(s);
    let s = make(<file-stream>, locator: path, buffer-size: *tbs*,
		 direction: #"input");
    check-true("hello there yummy contents on 2 lines",
	       "hello there\nyummy\n" = stream-contents-and-close(s));
  end;
*/
  begin
    let s = make(<file-stream>, locator: path, buffer-size: (*tbs* + 1),
		 direction: #"output", if-exists: #"replace");
    write-line(s, "hello there");
    close(s);
    check-condition("Signals file exists error", <file-exists-error>,
		    make(<file-stream>, locator: path, buffer-size: *tbs*,
			 direction: #"output", if-exists: #"signal"));
    check-condition("Signals file does not exist error", <file-does-not-exist-error>,
		    make(<file-stream>, 
                         locator: temp-file-pathname(initial-substring: "dst-helloeeee"),
			 buffer-size: *tbs* + 1,
			 direction: #"output", 
			 if-does-not-exist: #"signal")); 
    destroy-file-stream(s);
  end;
end test;

define method closed-external-stream-test-2
    (stream-class, the-direction, the-element-type, file-to-open)
  let title-suffix = 
    concatenate(select (stream-class)
		  <file-stream> => ", file-stream";
		  <multi-buffered-stream> => ", multi-buffered-stream";
		  otherwise => ", unknown stream type";
		end select,
		", ",
		as(<string>, the-direction),
		select (the-element-type)
		  <byte-character> => ", byte-character";
		  <byte> => ", byte";
		  otherwise => ", unknown element type";
		end select);
  let s = make(stream-class, direction: the-direction, 
	       element-type: the-element-type, locator: file-to-open);
  let the-stream-size = s.stream-size;
  close(s);
  check-condition(concatenate("read-element from closed stream", 
			      title-suffix), 
		  <stream-closed-error>,
		  read-element(s));
  check-condition(concatenate("unread-element from closed stream",
			      title-suffix), 
		  <stream-closed-error>,
		  unread-element(s, as(the-element-type, 'a')));
  check-condition(concatenate("read-line from closed stream", 
			      title-suffix), 
		  <stream-closed-error>,
		  read-line(s));
  check-condition(concatenate("read from closed stream", title-suffix), 
		  <stream-closed-error>,
		  read(s, 37));
  check-condition(concatenate("read-to-end from closed stream", 
			      title-suffix), 
		  <stream-closed-error>,
		  read-to-end(s));
  check-condition(concatenate("peek from closed stream", title-suffix), 
		  <stream-closed-error>,
		  peek(s));
  check-condition(concatenate("write-element to closed stream",
			      title-suffix), 
		  <stream-closed-error>,
		  write-element(s, as(the-element-type, 'b')));
  check-condition(concatenate("write-line to closed stream", title-suffix), 
		  <stream-closed-error>,
		  write-line(s, "something"));
  check-condition(concatenate("write to closed stream", title-suffix), 
		  <stream-closed-error>,
		  write(s, "something-else"));
  check-condition(concatenate("set position in closed stream", 
			      title-suffix), 
		  <stream-closed-error>,
		  stream-position-setter(the-stream-size + 23, s));
  check-condition(concatenate("new-line position in closed stream",
			      title-suffix), 
		  <stream-closed-error>,
		  new-line(s));
 // should be benign
  check-true(concatenate("close closed file", title-suffix), 
	     block() close(s); #t end block);
  check-true(concatenate("force-output on closed file", title-suffix),
	block() force-output(s); #t end block);
  check-false(concatenate("stream-open? on closed file",
			  title-suffix),
	      stream-open?(s)); 
  check-false(concatenate("stream-input-available? on closed file", 
			  title-suffix), 
	      stream-input-available?(s)); 
end method;

define method closed-external-stream-test-1(the-element-type, file-to-open)
  closed-external-stream-test-2(<file-stream>, #"input", the-element-type,
				file-to-open); 
  closed-external-stream-test-2(<file-stream>, #"output", the-element-type,
				file-to-open); 
  closed-external-stream-test-2(<file-stream>, #"input-output",
				the-element-type, file-to-open); 
  closed-external-stream-test-2(<multi-buffered-stream>,
				#"input-output", the-element-type, file-to-open);
  closed-external-stream-test-2(<multi-buffered-stream>, #"input",
				the-element-type, file-to-open); 
end method;

define method closed-external-stream-test()
  let temp-file-locator =  temp-file-pathname(initial-substring:
						"d-s-t");
  let temporary-output-stream =
    make(<file-stream>, direction: #"output", locator: temp-file-locator, 
	   if-exists: #"replace", if-does-not-exist: #"create");
  write(temporary-output-stream, "closed streams tests" );
  close(temporary-output-stream);
  closed-external-stream-test-1(<byte-character>, temp-file-locator); 
  closed-external-stream-test-1(<byte>, temp-file-locator); 
  delete-file(temp-file-locator);
end method;

define test test-closed-external-streams
  (title: "Test operations on closed external streams")  
     closed-external-stream-test();
end test;  

define test test-multi-buffered-read-only (title: "<multi-buffered-stream> readonly tests")
  let path = *hello-file*;
 //check-false("make hello-file",condition(make-hello-file()));
  begin
    let s = make(<multi-buffered-stream>, direction: #"input-output", 
		 locator: path, buffer-size: 4, number-of-buffers: 2);
    write-line(s, "hello world");
    check-true("Is stream size correct?", 
	       (11 + $line-end-size) = stream-size(s));
    check-true("Is stream hello world?",
	       concatenate("hello world", $line-end) = 
		 stream-contents-and-close(s));
  end;
  begin
    let s = make(<multi-buffered-stream>, locator: path, buffer-size:
		   4, number-of-buffers: 2, direction: #"input"); 
    check-false("<multi-buffered-stream> should not be at end",stream-at-end?(s));
    check-true("read hello world a stream of 11","hello world" = read(s, 11));
    check-false("Stream should STILL not be at end",stream-at-end?(s));
    close(s);
  end;
  begin
    let n = (11 + $line-end-size);
    let s = make(<multi-buffered-stream>, locator: path, buffer-size:
		   4, number-of-buffers: 2, direction: #"input"); 
    let str = make(<string>, size: n);
    check-false("<string> should not be at end",stream-at-end?(s));
    check-true("read chars into string str", n = read-into!(s, n, str));
    check-true("Str should be old hello world",
	       concatenate("hello world", $line-end) = str);
    check-true("Now stream should be-at-end",stream-at-end?(s));
    close(s);
  end;
/* 
  begin
    let s = make(<multi-buffered-stream>, locator: path, buffer-size: 4, number-of-buffers: 2, direction: #"input");
    let str = make(<string>, size: 13);
    let ire = condition(read-into!(s, 14, str));
    check-instance?("signals an <incomplete-read-error>",
		    <incomplete-read-error>, ire);
    check-instance?("The stream-error-stream should be a <multi-buffered-stream>",
		    <multi-buffered-stream>, ire.stream-error-stream);
    check-true("hello world should be the error sequence",
	       "hello world\n  " = ire.stream-error-sequence);
    check-true("the error-count should be 12",12 = ire.stream-error-count);
    close(s);
  end;
*/
 begin
    let s = make(<multi-buffered-stream>, locator: path, buffer-size:
		   4, number-of-buffers: 2, direction: #"input"); 
    let str = shallow-copy("=>.....|.....<=");
    check-false("This file-stream should not be at end",stream-at-end?(s));
    check-true("Read hello world into the string",11 = read-into!(s, 11, str, start: 2));
    check-true("Is the string hello world","=>hello world<=" = str);
    check-false("The stream should still not be at end",stream-at-end?(s));
    close(s);
  end;
  begin
    let s = make(<multi-buffered-stream>, locator: path, buffer-size:
		   4, number-of-buffers: 2, direction: #"input"); 
    check-true("is hello world read-to-end(s)",
	       concatenate("hello world", $line-end) = read-to-end(s));
    close(s);
  end;
  begin
    let s = make(<multi-buffered-stream>, locator: path, buffer-size:
		   4, number-of-buffers: 2, direction: #"input"); 
    check-true("The first element is h",'h' = read-element(s));
    check-true("The peek is e",'e' = peek(s));
    check-true("The next element is e",'e' = read-element(s));
    check-true("The unread element is e",'e' = unread-element(s, 'e'));
    check-true("The next element is now again e",'e' = read-element(s));
// This check is wrong.  The specification says that unreading an element
// which wasn't read is an error.  It doesn't say that the error has to be
// detected and a condition raised.
//     check-condition("Unread element which wasn't read", 
// 		    <error>, unread-element(s, 'z'));
    //discard-input(s);
    //check-true("The stream should now be at the end",stream-at-end?(s));
    close(s);
  end; 
  begin
    let s = make(<multi-buffered-stream>, locator: path, buffer-size:
		   4, number-of-buffers: 2, direction: #"input"); 
    check-true("skip the 'o'",skip-through(s, 'o'));
    check-true("The next element should be a space",' ' = read-element(s));
    close(s);
 end;
end test test-multi-buffered-read-only;

// This test tests the type of things that might fail with an async stream,
// but is still valid applied to a syncronous stream.  
// stream should be input-output.
define function async-stream-test (stream :: <file-stream>) => ()
  let rand :: <random> = make(<random>, seed: 0);
  let num-buffers :: <integer> = 8;
  let buffer-size :: <integer> = 16 * 1024;  // == $preferred-buffer-size;
  let data :: <byte-vector> = make(<byte-vector>, 
				   size: num-buffers * buffer-size);

  for (index from 0 below num-buffers * buffer-size)
    data[index] := random(256, random: rand);
  end for;

  write(stream, data);
   
  begin
    stream.stream-position := 6 * buffer-size;
    let buffer-6 :: <byte-vector> = read(stream, buffer-size);
    check("buffer read after writes", \=, buffer-6, 
	  copy-sequence(data, start: 6 * buffer-size, end: 7 * buffer-size));
  end;

  let half-buffer-size = floor/(buffer-size, 2);
  for (index from 2 * buffer-size + half-buffer-size below 3 * buffer-size)
    data[index] := random(256, random: rand);
  end for;
  stream.stream-position := 2 * buffer-size + half-buffer-size;
  write(stream, data, start: 2 * buffer-size + half-buffer-size, 
  	end: 3 * buffer-size);
  
  begin
    stream.stream-position := 2 * buffer-size;
    let buffer-2 :: <byte-vector> = read(stream, buffer-size);
    check("changed buffer read", \=, buffer-2, 
	  copy-sequence(data, start: 2 * buffer-size, end: 3 * buffer-size));
  end;

  begin
    stream.stream-position := 0;
    let buffers :: <byte-vector> = read(stream, num-buffers * buffer-size);
    check("all buffer read", \=, buffers, data);
  end;
end function async-stream-test;

define function open-stream-for-async-test (#rest keywords) 
					=> (r :: <file-stream>)
  let temp-file-locator =  temp-file-pathname(initial-substring: "a-s-t");
  apply(make, <file-stream>, direction: #"input-output", 
	locator: temp-file-locator, if-exists: #"replace", 
	if-does-not-exist: #"create", element-type: <byte>, keywords);
end function open-stream-for-async-test;

define test test-sync-stream-async-tests ()
  let s = open-stream-for-async-test(asynchronous?: #f);
  async-stream-test(s);
  close(s);
end test test-sync-stream-async-tests;

define test test-async-stream-async-tests ()
  let s = open-stream-for-async-test(asynchronous?: #t);
  async-stream-test(s);
  close(s);
end test test-async-stream-async-tests;


/// Test suites

define suite universal-streams-suite ()
  test test-file-stream;
  test test-empty-file-stream;
  test test-closed-external-streams;
  test test-file-stream-exclusive;
  test test-empty-file-stream-exclusive;
  test bogus-test-file-stream-using-input-output-streams;
  test bogus-test-empty-file-stream-using-input-output-streams;
  test test-multi-buffered-file-stream;
  test test-empty-multi-buffered-file-stream;
  test test-multi-buffered-file-stream-with-long-input;
  test test-sync-stream-async-tests;
  test test-async-stream-async-tests;
  test test-multi-buffered-file-stream-exclusive;
  test test-empty-multi-buffered-file-stream-exclusive;
  test test-multi-buffered-file-stream-with-long-input-exclusive;
end suite universal-streams-suite;

define suite additional-streams-suite ()
  test test-position-file-streams;
  test bogus-test-position-file-streams-using-input-output-streams;
  test test-position-multi-buffered-file-streams;

  test test-file-stream-creation;
  test test-file-read-stream;
  test test-file-write-stream;
  test test-file-read-write-stream;
  test test-file-stream-functions;
  test test-write-to-multi-buffered-file-stream;
  test test-multi-buffered-read-only;
end suite additional-streams-suite;


/// File system locators

define sideways method make-test-instance
    (class == <microsoft-volume-locator>)
 => (instance :: <microsoft-volume-locator>)
  make(<microsoft-volume-locator>, drive: 'a')
end method make-test-instance;

define sideways method make-test-instance
    (class == <microsoft-unc-locator>)
 => (instance :: <microsoft-unc-locator>)
  make(<microsoft-unc-locator>, host: "host")
end method make-test-instance;

define sideways method make-test-instance
    (class :: subclass(<directory-locator>))
 => (instance :: <directory-locator>)
  make(class, name: "directory")
end method make-test-instance;

define sideways method make-test-instance
    (class :: subclass(<file-locator>))
 => (instance :: <file-locator>)
  make(class, name: "hello")
end method make-test-instance;

define file-system function-test file-system-separator ()
  check-instance?("file-system-separator() returns a character",
                  <character>,
                  file-system-separator());
end function-test file-system-separator;

define file-system class-test <file-system-locator> ()
  //---*** Fill this in...
end class-test <file-system-locator>;

define file-system class-test <native-file-system-locator> ()
  //---*** Fill this in...
end class-test <native-file-system-locator>;

define file-system class-test <file-system-directory-locator> ()
  //---*** Fill this in...
end class-test <file-system-directory-locator>;

define file-system class-test <file-system-file-locator> ()
  //---*** Fill this in...
end class-test <file-system-file-locator>;

define method test-file-system-locator-class
    (class-name :: <string>, class :: subclass(<file-system-locator>), 
     pathnames :: <sequence>,
     #key case-sensitive? :: <boolean> = #t,
          separator :: false-or(<character>) = '/',
          alternate-separator :: false-or(<character>) = separator)
 => ()
  local method switch-separators
	    (string :: <string>, old :: <character>, new :: <character>)
	 => (new-string :: <string>)
	  if (old == new)
	    string
	  else
	    map(method (char :: <character>)
		  if (char == old) new else char end
		end,
		string)
	  end
	end method switch-separators,

        method canonicalize-pathname
	    (pathname :: <string>) => (canonical-pathname :: <string>)
	  let pathname = switch-separators(pathname, alternate-separator, separator);
	  let pathname-size = pathname.size;
	  if (subtype?(class, <directory-locator>)
		& (pathname-size == 0
		     | pathname[pathname-size - 1] ~== separator))
	    concatenate(pathname, vector(separator))
	  else
	    pathname
	  end
	end method canonicalize-pathname;

  for (pathname :: <string> in pathnames)
    let locator = as(class, pathname);
    check-instance?(format-to-string
		      ("as(%s, %=) returns valid locator",
		       class-name, pathname),
		    class, locator);
    check-equal(format-to-string("as(<string>, as(%s, %=)) = %=",
				 class-name, pathname, pathname),
		as(<string>, locator),
		canonicalize-pathname(pathname));
    if (case-sensitive?)
      check-false(format-to-string("Locator %s sensitive to case", pathname),
		  as(class, as-lowercase(pathname))
		    = as(class, as-uppercase(pathname)))
    else
      check-equal(format-to-string("Locator %s insensitive to case", pathname),
		  as(class, as-lowercase(pathname)),
		  as(class, as-uppercase(pathname)))
    
    end;
    if (alternate-separator ~== separator)
      let pathname = switch-separators(pathname, alternate-separator, separator);
      check-equal(format-to-string("locator %= = locator %=",
				   pathname, pathname),
		  locator,
		  as(class, pathname));
      check-equal(format-to-string("as(<string>, as(%s, %=)) = %=",
				   class-name, pathname, pathname),
		  as(<string>, locator),
		  canonicalize-pathname(pathname))
    end
  end
end method test-file-system-locator-class;

/// Microsoft locator classes

define constant $microsoft-directories
  = #["relative/directory",
      "/users/dylan/hello",
      "/directory/with/a/short/name",
      "c:/Program Files/Functional Objects/Dylan/bin",
      "//machine/users/dylan"];

define constant $microsoft-filenames
  = #["hello",
      "hello.txt",
      "relative/directory/test.txt",
      "/users/dylan/hello/hello-world",
      "/file/with/a/short/directory/name/test.txt",
      "/file/with/a/short/name/b",
      "c:/Program Files/Functional Objects/Functional Developer/bin/functional-dylan.exe",
      "//machine/users/dylan/test.dylan"];

define file-system class-test <microsoft-server-locator> ()
  //---*** Fill this in...
end class-test <microsoft-server-locator>;

define file-system class-test <microsoft-unc-locator> ()
  check-equal("as(<string>, microsoft-unc-locator)",
	      as(<string>, make(<microsoft-unc-locator>, host: "host")),
	      "\\\\host");
  check-equal("microsoft-unc-locator case insensitive",
	      make(<microsoft-unc-locator>, host: "host"),
	      make(<microsoft-unc-locator>, host: "HOST"))
end class-test <microsoft-unc-locator>;

define file-system class-test <microsoft-volume-locator> ()
  check-equal("as(<string>, microsoft-volume-locator)",
	      as(<string>, make(<microsoft-volume-locator>, volume: "a")),
	      "a:");
  check-equal("microsoft-volume-locator case insensitive",
	      make(<microsoft-volume-locator>, volume: "a"),
	      make(<microsoft-volume-locator>, volume: "A"))
end class-test <microsoft-volume-locator>;

define file-system class-test <microsoft-file-system-locator> ()
  test-file-system-locator-class
    ("<microsoft-file-system-locator>", <microsoft-file-system-locator>,
     concatenate(map(rcurry(concatenate, "/"), $microsoft-directories),
		 $microsoft-filenames),
     case-sensitive?: #f,
     separator:           '\\',
     alternate-separator: '/')
end class-test <microsoft-file-system-locator>;

define file-system class-test <microsoft-directory-locator> ()
  test-file-system-locator-class
    ("<microsoft-directory-locator>", <microsoft-directory-locator>,
     $microsoft-directories,
     case-sensitive?: #f,
     separator:           '\\',
     alternate-separator: '/')
end class-test <microsoft-directory-locator>;

define file-system class-test <microsoft-file-locator> ()
  test-file-system-locator-class
    ("<microsoft-file-locator>", <microsoft-file-locator>,
     $microsoft-filenames,
     case-sensitive?: #f,
     separator:           '\\',
     alternate-separator: '/')
end class-test <microsoft-file-locator>;

/// Posix locators

define constant $posix-directories
  = #["relative/directory",
      "/users/dylan/hello",
      "/directory/with/a/short/name"];

define constant $posix-filenames
  = #["hello",
      "hello.txt",
      "relative/directory/test.txt",
      "/users/dylan/hello/hello-world",
      "/file/with/a/short/directory/name/test.txt",
      "/file/with/a/short/name/b"];

define file-system class-test <posix-file-system-locator> ()
  test-file-system-locator-class
    ("<posix-file-system-locator>", <posix-file-system-locator>,
     concatenate(map(rcurry(concatenate, "/"), $posix-directories),
		 $posix-filenames))
end class-test <posix-file-system-locator>;

define file-system class-test <posix-directory-locator> ()
  test-file-system-locator-class
    ("<posix-directory-locator>", <posix-directory-locator>,
     $posix-directories)
end class-test <posix-directory-locator>;

define file-system class-test <posix-file-locator> ()
  test-file-system-locator-class
    ("<posix-file-locator>", <posix-file-locator>,
     $posix-filenames)
end class-test <posix-file-locator>;
