Module:    scepter-utilities
Author:    Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <indenting-stream> (<wrapper-stream>)
  slot indent-depth :: <integer> = 0;
  slot %indent-string :: <string> = make(<string>, size: 64);
  constant slot indent-tab :: <integer> = 2;
end class;

define method indent+ (stream :: <indenting-stream>)
  stream.indent-depth := stream.indent-depth + 1;
end method;

define method indent- (stream :: <indenting-stream>)
  stream.indent-depth := stream.indent-depth - 1;
end method;

define method indent-string (stream :: <indenting-stream>)
  stream.grow-indent-string;
  stream.%indent-string;
end method;

define method grow-indent-string (stream :: <indenting-stream>)
  if (stream.%indent-string.size < stream.indent-size)
    let new-size = 2 * stream.%indent-string.size;
    let required-size = stream.indent-size;
    while (new-size < required-size)
      new-size := 2 * new-size;
    end while;
    stream.%indent-string := make(<string>, size: new-size);
  end if;
end method;

define method indent-size (stream :: <indenting-stream>)
  stream.indent-depth * stream.indent-tab;
end method;

define method new-line (stream :: <indenting-stream>)
 => ()
  next-method();
  write(stream, stream.indent-string, end: stream.indent-size);
end method;

define macro with-indented-body
  { with-indented-body ( ?stream:expression ) ?body:body end}
  =>
  { block ()
      indent+(?stream);
      ?body;
    cleanup
      indent-(?stream);
    end block; }
end macro;

