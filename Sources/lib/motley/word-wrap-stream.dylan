Module:    motley
Author:    Seth LaForge
Synopsis:  A generic <wrapper-stream> to provide word-wrapping
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define constant <character-vector> :: <type> = 
	limited(<stretchy-vector>, of: <byte-character>);


// This class implements a generic word-wrapping wrapper stream.  It wraps
// only at spaces or tabs, when the line would be over line-length
// characters in length.  Any line which is wrapped from the previous is
// indented by wrap-offset spaces.
//
// Current limitations:
//   - Can only wrap at spaces and tabs - should allow for user-defined
//     word boundaries.
//   - Tabs are not handled properly - they're considered to be a single
//     column for now.
//   - Efficiency could probably be improved by not treating a
//     multi-character write as a series of single-character writes.
//   - A line containing only spaces followed by a very long word will
//     result in a line break being added before the word.  This is
//     questionable behaviour.
//   - Seeking will produce unpredictable results.  Don't do that.

define class <word-wrap-stream> (<wrapper-stream>)
  slot line-length :: <integer> = 75, init-keyword: line-length:;
  slot wrap-offset :: <integer> = 0, init-keyword: wrap-offset:;

  slot line-position :: <integer> = 0;
  slot word-buffer :: <character-vector> = make(<character-vector>);
  slot offset-string :: <byte-string>;
end class <word-wrap-stream>;


define method initialize (this :: <word-wrap-stream>, 
			  #rest rest, #key #all-keys) => ()
  ignore(rest);
  next-method();
  this.offset-string := make(<byte-string>, size: this.wrap-offset, fill: ' ');
end method initialize;


define method write-element (this :: <word-wrap-stream>, elt :: <character>) 
			 => ()
  select (elt)
    ' ', '\n', '\r', '\t' =>
	// Note - tabs not handled properly!
	wrap-if-necessary(this);
	write-element(this.inner-stream, elt);
	if (elt == '\n' | elt == '\r')
	  this.line-position := 0;
	else
	  this.line-position := this.line-position + 1;
	end if;
    otherwise =>
	this.word-buffer := add!(this.word-buffer, elt);
  end select;
end method write-element;


define method write (this :: <word-wrap-stream>, elements :: <sequence>,
		     #key start: _start = 0, end: _end = -1) => ()
  if (_end < 0) _end := elements.size; end if;
  for (index from _start below _end)
    write-element(this, elements[index]);
  end for;
end method write;

define method write-line (this :: <word-wrap-stream>, elements :: <string>,
                          #key start: _start = 0, end: _end = #f) => ()
  write(this, elements, start: _start, end: _end | elements.size);
  new-line(this);
end method write-line;

define method new-line (this :: <word-wrap-stream>) => ()
  wrap-if-necessary(this);
  new-line(this.inner-stream);
  this.line-position := 0;
end method new-line;


define function wrap-if-necessary (this :: <word-wrap-stream>) => ()
  if (this.line-position > 0 &
      this.line-position + this.word-buffer.size > this.line-length)
    new-line(this.inner-stream);
    write(this.inner-stream, this.offset-string);
    this.line-position := this.wrap-offset;
  end if;
  flush-word-buffer(this);
end function wrap-if-necessary;


define function flush-word-buffer (this :: <word-wrap-stream>) => ()
  if (~this.word-buffer.empty?)
    this.line-position := this.line-position + this.word-buffer.size;
    for (elt in this.word-buffer)
      write-element(this.inner-stream, elt);
    end for;
    this.word-buffer.size := 0;
  end if;
end function flush-word-buffer;


define method close (this :: <word-wrap-stream>, #rest rest, #key #all-keys) 
		 => ()
  ignore(rest);
  flush-word-buffer(this);
  next-method();
end method close;


// Get rid of the damn 'referenced but not used' warnings:

ignorable(line-length, line-length-setter);
ignorable(wrap-offset, wrap-offset-setter);
ignorable(line-position, line-position-setter);
ignorable(word-buffer, word-buffer-setter);
ignorable(offset-string, offset-string-setter);
