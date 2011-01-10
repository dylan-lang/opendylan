module:       streams-internals
Synopsis:     Copy-down methods for speed
Author:       Toby Weinberg, Scott McKay, Marc Ferguson, Eliot Miranda
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define macro copy-down-stream-definer
  { define copy-down-stream ?:name element ?elt:name sequence ?seq:name }
    => { define sealed domain do-get-input-buffer (?name);
	 define sealed domain do-next-input-buffer (?name);
	 define sealed domain do-release-input-buffer (?name);
	 define sealed domain do-get-output-buffer (?name);
	 define sealed domain do-next-output-buffer (?name);
	 define sealed domain do-release-output-buffer (?name);
	 define sealed domain coerce-to-element
	   (?name, <buffer>, <integer>);
	 define sealed domain coerce-from-element
	   (?name, <buffer>, <integer>, <object>);
	 define sealed domain coerce-to-sequence
	   (?name, <buffer>, <integer>, ?seq, <integer>, <integer>);
	 define sealed domain coerce-from-sequence
	   (?name, <buffer>, <integer>, ?seq, <integer>, <integer>);
         define sealed domain write (?name, ?seq);
	 define sealed domain peek (?name);
	 define sealed domain read (?name, <integer>);
	 define sealed domain read-into!(?name, <integer>, ?seq);

	 define sealed copy-down-method write-element
	   (stream :: ?name, elt :: ?elt) => ();

	 define sealed copy-down-method write
	   (stream :: ?name, elements :: ?seq,
	    #key start: _start :: <integer> = 0, end: _end = #f) => ();

	 define sealed copy-down-method write-line
	   (stream :: ?name, elements :: ?seq,
	    #key start: _start :: <integer> = 0, end: _end = #f) => ();

	 define sealed copy-down-method write-text
	   (stream :: ?name, elements :: ?seq,
	    #key start: _start :: <integer> = 0, end: _end = #f) => ();

	 define sealed copy-down-method read-element
	     (stream :: ?name,
	      #key on-end-of-stream = unsupplied())
	  => (element-or-eof);

	 define sealed copy-down-method peek
	     (stream :: ?name,
	      #key on-end-of-stream = unsupplied())
	  => (element-or-eof);

	 define sealed copy-down-method read
	     (stream :: ?name, n :: <integer>,
	      #key on-end-of-stream = unsupplied())
	  => (elements);

	 define sealed copy-down-method read-into!
	     (stream :: ?name, n :: <integer>, seq :: ?seq,
	      #key start :: <integer> = 0, on-end-of-stream = unsupplied())
	  => (n-read);

	 define sealed copy-down-method read-text
	     (stream :: ?name, n :: <integer>,
	      #key on-end-of-stream = unsupplied())
	  => (elements);

	 define sealed copy-down-method read-text-into!
	     (stream :: ?name, n :: <integer>, seq :: ?seq,
	      #key start :: <integer> = 0, on-end-of-stream = unsupplied())
	  => (n-read) }
end macro;

define copy-down-stream <byte-string-stream> 
   element <byte-character> sequence <byte-string>;

/*
	 define copy-down-method coerce-to-element
  	      (stream :: ?name, sb :: <buffer>, index :: <integer>)
           => (res :: ?elt);

	 define copy-down-method coerce-from-element
	     (stream :: ?name, sb :: <buffer>, 
              index :: <integer>, elt :: <object>)
           => (res :: ?elt);

	 define copy-down-method coerce-to-sequence
	     (stream :: ?name, sb :: <buffer>, buf-start :: <integer>, 
              sequence :: ?seq, seq-start :: <integer>, count :: <integer>)
           => ();

	 define copy-down-method coerce-from-sequence
	     (stream :: ?name, sb :: <buffer>, buf-start :: <integer>, 
              sequence :: ?seq, seq-start :: <integer>, count :: <integer>)
           => ();

*/
