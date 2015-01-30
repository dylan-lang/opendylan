Module:    environment-deuce
Synopsis:  Environment Deuce Searching Utilities
Author:    Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Searching utilities

// Find a buffer for a file, or create one in Fundamental mode
// We read it in fundamental mode for speed; if a search matches,
// we'll enter the appropriate mode and sectionize the buffer then
define function find-buffer-for-file
    (pathname :: <pathname>)
 => (buffer :: <buffer>, new-buffer? :: <boolean>)
  //--- cpage: 1998.08.06 Temporarily, coerce to a <string>. Some callers of
  //           buffer-pathname only handle <string> even though buffer-pathname
  //           returns <pathname>. buffer-pathname will be set to pathstring
  //           in do-find-file.
  let pathstring = as(<string>, pathname);
  // This will either find a buffer containing the file, or create a buffer and
  // read the file into it.
  let editor :: <editor> = $environment-editor;
  let buffer = find-buffer-from-pathname(editor, pathstring);
  if (buffer ~== #f)
    values(buffer, #f)
  else
    values(do-find-file(editor, pathstring, direction: #"input"), #t)
  end
end function find-buffer-for-file;

// Find a frame for a buffer, or create one
define function find-frame-for-buffer
    (buffer :: <buffer>) => (frame :: <frame>)
  // This will either find a frame that displays the buffer, select the
  // buffer in an existing frame, or create one for the buffer (depending
  // on reuse policies), but it doesn't return the frame (!!) ...
  find-deuce-frame(buffer: buffer);
  // ...so now, let's look for a frame showing the buffer.
  let frame = choose-environment-frame(default-port(),
                                       <environment-editor>,
                                       buffer: buffer);
  debug-assert(frame, "Buffer not found in any frame");
  frame
end function find-frame-for-buffer;
