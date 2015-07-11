module: streams-internals
author: William Lott, Peter S. Housel

//======================================================================
//
// Copyright (c) 1996  Carnegie Mellon University
// Copyright (c) 1998, 1999, 2000, 2004, 2005  Gwydion Dylan Maintainers
// All rights reserved.
//
// Use and copying of this software and preparation of derivative
// works based on this software are permitted, including commercial
// use, provided that the following conditions are observed:
//
// 1. This copyright notice must be retained in full on any copies
//    and on appropriate parts of any derivative works.
// 2. Documentation (paper or online) accompanying any system that
//    incorporates this software, or any part of it, must acknowledge
//    the contribution of the Gwydion Project at Carnegie Mellon
//    University, and the Gwydion Dylan Maintainers.
//
// This software is made available "as is".  Neither the authors nor
// Carnegie Mellon University make any warranty about the software,
// its performance, or its conformity to any specification.
//
// Bug reports should be sent to <gd-bugs@gwydiondylan.org>; questions,
// comments and suggestions are welcome at <gd-hackers@gwydiondylan.org>.
// Also, see http://www.gwydiondylan.org/ for updates and documentation.
//
//======================================================================

// Types:
//   <indenting-stream>
//      Wrapper stream which outputs indented text with conversions of
//      spaces into tabs.  Keywords include "inner-stream:" and
//      "indentation:".  indentation: is the initial indentation of
//      the stream (default 0); change with the indent() function.
//
// Functions:
//   indent(stream :: <indenting-stream>, delta :: <integer>)
//      Changes the current indentation for stream text.
//   inner-stream(stream :: <indenting-stream>)
//      Returns the inner-stream.



define sealed class <indenting-stream> (<wrapper-stream>)
  slot is-space-column :: false-or(<integer>) = 0;
  slot is-indentation :: <integer> = 0, init-keyword: indentation:;
  constant slot is-input-tab-width :: <integer> = 8,
    init-keyword: input-tab-width:;
  constant slot is-output-tab-width :: false-or(<integer>) = #f,
    init-keyword: output-tab-width:;
end;

define sealed domain make(singleton(<indenting-stream>));
define sealed domain initialize(<indenting-stream>);

define constant $spaces :: <byte-string>
  = "                                                                "; // 64
define constant $tabs :: <byte-string>
  = "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t"; // 32

define function write-indent
    (istream :: <indenting-stream>, count :: <integer>)
 => ();
  let stream = istream.inner-stream;

  local
    method write-spaces (count :: <integer>) => ();
      if (count > size($spaces))
        write(stream, $spaces);
        write-spaces(count - size($spaces));
      else
        write(stream, $spaces, end: count);
      end if;
    end,
    method write-tabs (count :: <integer>) => ();
      if (count > size($tabs))
        write(stream, $tabs);
        write-tabs(count - size($tabs));
      else
        write(stream, $tabs, end: count);
      end if;
    end method;

  let tab-width = istream.is-output-tab-width;
  if (tab-width & count >= tab-width)
    let (tabs, spaces) = truncate/(count, tab-width);
    write-tabs(tabs);
    write-spaces(spaces);
  else
    write-spaces(count);
  end if;
end function;

define method write-element
    (stream :: <indenting-stream>, elt :: <character>)
 => ()
  select(elt)
    ' ' =>
      if (stream.is-space-column)
        stream.is-space-column := stream.is-space-column + 1;
      else
        write-element(stream.inner-stream, elt);
      end if;
    '\t' =>
      if (stream.is-space-column)
        let tab-width = stream.is-input-tab-width;
        stream.is-space-column
          := stream.is-space-column
          + tab-width
          - remainder(stream.is-space-column, tab-width);
      else
        write-element(stream.inner-stream, elt);
      end if;
    '\n', '\r' =>
      stream.is-space-column := 0;
      write-element(stream.inner-stream, elt);
    otherwise =>
      if (stream.is-space-column)
        write-indent(stream, stream.is-indentation + stream.is-space-column);
      end if;
      stream.is-space-column := #f;
      write-element(stream.inner-stream, elt);
  end select;
end method write-element;

define method write
    (stream :: <indenting-stream>, elements :: <sequence>,
     #key start: _start :: <integer> = 0, end: _end :: <integer> = elements.size)
 => ();
  let inner = stream.inner-stream;
  let tab-width = stream.is-input-tab-width;

  iterate loop (index :: <integer> = _start,
                start :: <integer> = _start,
                space-column :: false-or(<integer>) = stream.is-space-column)
    if (index < _end)
      let elt = elements[index];
      select (elt)
        ' ' =>
          if (space-column)
            loop(index + 1, index + 1, space-column + 1);
          else
            loop(index + 1, start, #f);
          end if;
        '\t' =>
          if (space-column)
            loop(index + 1, index + 1,
                 space-column + tab-width - remainder(space-column, tab-width));
          else
            loop(index + 1, start, #f);
          end if;
        '\n', '\r' =>
          write(inner, elements, start: start, end: index + 1);
          loop(index + 1, index + 1, 0);
        otherwise =>
          if (space-column)
            write-indent(stream, stream.is-indentation + space-column);
          end if;
          loop(index + 1, start, #f);
      end select;
    else
      if (start ~= index)
        write(inner, elements, start: start, end: index);
      end if;
      stream.is-space-column := space-column;
    end if;
  end iterate;
end method write;

define sealed copy-down-method write
    (stream :: <indenting-stream>, elements :: <byte-string>,
     #key start: _start :: <integer> = 0,
          end: _end :: <integer> = elements.size) => ();

define method new-line (stream :: <indenting-stream>) => ()
  stream.is-space-column := 0;
  new-line(stream.inner-stream)
end method new-line;

define method discard-output
    (stream :: <indenting-stream>) => ()
  stream.is-space-column := 0;
  discard-output(stream.inner-stream)
end method discard-output;

define method close (stream :: <indenting-stream>, #key, #all-keys) => ();
  force-output(stream);
end;

define method indent (stream :: <indenting-stream>, delta :: <integer>)
    => ();
  stream.is-indentation := stream.is-indentation + delta;
end;

define macro with-indentation
  { with-indentation (?stream:expression) ?:body end }
  => { with-indentation (?stream, 4) ?body end }
  { with-indentation (?stream:expression, ?indentation:expression) ?:body end }
  => { begin
         let _stream = ?stream;
         let _indentation = ?indentation;
         block ()
           indent(_stream, _indentation);
           ?body
         cleanup
           indent(_stream, -_indentation)
         end
       end }
end macro with-indentation
