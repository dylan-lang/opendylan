Module:       io-test-suite
Synopsis:     IO library test suite
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// Pretty stream testing

register-stream-class-info("<pretty-stream>", <pretty-stream>,
			   input-stream?: #f,
			   output-stream?: #t,
			   element-type: <object>);

define constant $pretty-printing-test-string
  = "This is an extremely long paragraph that should be"
    " pretty printed into a number of lines of text. This"
    " is some padding to the paragraph to ensure that it"
    " is long enough.";

define sideways method make-stream-tests-of-size
    (class :: subclass(<pretty-stream>), stream-size :: <integer>)
 => (streams :: <sequence>)
  let class-info = stream-class-info(class);
  let character-sequence = copy-sequence($pretty-printing-test-string, end: stream-size);
  let tests :: <stretchy-object-vector> = make(<stretchy-object-vector>);
  local method add-pretty-stream-test
	    (contents :: <sequence>)
	 => (test :: <stream-test-info>)
	  let test
	    = make(<stream-test-info>,
		   test-name: format-to-string("%s size %d",
					       class-info.info-class-name,
					       stream-size),
		   class-info: class-info,
		   contents: contents,
		   direction: #"output",
		   make-function: method ()
				    let target-stream
				      = make(<sequence-stream>, direction: #"output");
				    make(<pretty-stream>,
					 sequence: contents,
					 target: target-stream);
				  end);
	  add!(tests, test);
	  test
	end method add-pretty-stream-test;
  add-pretty-stream-test(character-sequence);
  tests
end method make-stream-tests-of-size;


/// Pretty printing interface testing

define pprint variable-test *print-miser-width* ()
  //---*** Fill this in...
end variable-test *print-miser-width*;

define pprint variable-test *default-line-length* ()
  //---*** Fill this in...
end variable-test *default-line-length*;

define pprint function-test pprint-logical-block ()
  //---*** Fill this in...
end function-test pprint-logical-block;

define pprint function-test pprint-newline ()
  //---*** Fill this in...
end function-test pprint-newline;

define pprint function-test pprint-indent ()
  //---*** Fill this in...
end function-test pprint-indent;

define pprint function-test pprint-tab ()
  //---*** Fill this in...
end function-test pprint-tab;

define pprint macro-test printing-logical-block-test ()
  //---*** Fill this in...
end macro-test printing-logical-block-test;
