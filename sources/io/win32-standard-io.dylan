Module:       io-internals
Synopsis:     *standard-input*, *standard-output*, *standard-error*
Author:       Gary Palter
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// From WINBASE.H
define constant $STD_INPUT_HANDLE  = -10;
define constant $STD_OUTPUT_HANDLE = -11;
define constant $STD_ERROR_HANDLE  = -12;

define class <standard-file-accessor> (<native-file-accessor>)
  constant slot std-handle :: <integer>, required-init-keyword: std-handle:;
  slot accessor-open? :: <boolean> = #t;
end class;

// Lazily allocate a console and return a console handle
define method file-handle
    (accessor :: <standard-file-accessor>)
 => (handle :: <machine-word>)
  accessor.%file-handle
    | begin
        let handle
          = if (accessor.accessor-open?)
              if (win32-alloc-console())
                win32-std-handle(accessor.std-handle)
              else
                error("cannot allocate a console");
              end if
            else
              error("cannot operate on a closed file");
            end if;
        accessor-open(accessor, handle,
                      file-descriptor: handle, file-position: 0);
        handle
      end
end method;

define method accessor-close
    (accessor :: <standard-file-accessor>, #key abort? = #f, wait? = #t)
 => (closed? :: <boolean>);
  accessor.accessor-open? := #f;
  next-method()
end method accessor-close;

define function make-std-stream
    (std-handle :: <integer>, direction)
 => (stream :: <file-stream>);
  let handle = win32-std-handle(std-handle);
  // Check to see if STD_INPUT/STD_OUTPUT/STD_ERROR is already a valid handle.
  // According to KB105305, GUI applications started from the command line
  // may have non-NULL but invalid file handles.
  if (handle
        & ~zero?(handle)
        & win32-file-type(handle) ~= $FILE_TYPE_UNKNOWN)
    make(<file-stream>,
         locator: handle, file-descriptor: handle, direction: direction)
  else
    let accessor = make(<standard-file-accessor>,
                        std-handle: std-handle, file-position: 0);
    make(<file-stream>, locator: #f, accessor: accessor, direction: direction)
  end if
end function;

define variable *standard-input* 
  = make-std-stream($STD_INPUT_HANDLE, #"input");

define variable *standard-output*
  = make-std-stream($STD_OUTPUT_HANDLE, #"output");

define variable *standard-error*
  = make-std-stream($STD_ERROR_HANDLE, #"output");
