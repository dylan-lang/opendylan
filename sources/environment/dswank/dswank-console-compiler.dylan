module:    dswank
author:    Andreas Bogk and Hannes Mehnert
copyright: Original Code is Copyright (c) 2008-2012 Dylan Hackers;
           All rights reversed.
License:   See License.txt in this distribution for details.
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND

define method start-compiler (stream)
  let input-stream = make(<string-stream>, direction: #"input");
  let output-stream = make(<emacs-output-wrapper-stream>,
                           inner-stream: stream,
                           direction: #"output");
  make-environment-command-line-server
    (input-stream:   input-stream,
     output-stream:  output-stream);
end;

define class <emacs-output-wrapper-stream> (<wrapper-stream>)
  slot buffer :: <string> = "";
end;

define method write-element (stream :: <emacs-output-wrapper-stream>, char :: <character>)
 => ()
  if (char == '\n')
    new-line(stream);
  else
    stream.buffer := add!(stream.buffer, char);
  end;
end;

define method write
    (stream :: <emacs-output-wrapper-stream>, elements :: <sequence>,
     #rest keys, #key start: _start = 0, end: _end) => ()
  let string =
    if (_end)
      copy-sequence(elements, start: _start, end: _end);
    else
      copy-sequence(elements, start: _start);
    end;
  stream.buffer := concatenate(stream.buffer, string);
end method write;

define method new-line (stream :: <emacs-output-wrapper-stream>) => ()
  write-to-emacs(stream.inner-stream, list(#":write-string", concatenate(stream.buffer, "\n")));
  stream.buffer := "";
end method new-line;

define method write-line
    (stream :: <emacs-output-wrapper-stream>, line :: <string>, #key start = 0, end: end-index)
 => ()
  write(stream, line, start: start, end: end-index);
  new-line(stream);
end;

define function run-compiler (server, string :: <string>) => ()
  execute-command-line(server, string);
end;
