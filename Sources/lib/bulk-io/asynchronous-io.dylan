Module:    bulk-io-internal
Author:    Toby Weinberg
Synopsis:  bulk-io, we haul <byte>s...
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// This file implements the asynchronous write-file function.

// This error represents an error which occurred during a file write initiated
// by write-file.  Before the errror is thrown, the partially written file is
// deleted.
define class <write-file-error> (<error>)
  slot write-file-error-locator, required-init-keyword: locator:;
  slot write-file-error-win32-error, required-init-keyword: win32-error:;
  slot write-file-error-thread :: <thread>, required-init-keyword: thread:;
end class <write-file-error>;

// A collection of <write-file-error>s.
define class <write-file-errors> (<error>)
  slot write-file-errors-errors :: <list>, required-init-keyword: errors:;
end class <write-file-errors>;

define method condition-to-string (e :: <write-file-error>) => (r :: <string>)
  if (e.write-file-error-win32-error)
    format-to-string("Error writing to file %s: \"%s\".",
		     e.write-file-error-locator, 
		     win32-error-message(e.write-file-error-win32-error))
  end if;
end method condition-to-string;

define method condition-to-string (e :: <write-file-errors>) => (r :: <string>)
  local method err-str (errors :: <list>, sep :: <string>) => (r :: <string>)
    if (~ empty?(errors))
      let f = errors.head;
      format-to-string("%s%s: \"%s\"%s", sep,
		       f.write-file-error-locator, 
		       win32-error-message(f.write-file-error-win32-error),
		       err-str(errors.tail, ", "))
    else
      ""
    end if;
  end method err-str;
  if (e.write-file-errors-errors.size == 1)
    condition-to-string(e.write-file-errors-errors.first)
  else
    format-to-string("Errors when writing to files: %s.", 
		     err-str(e.write-file-errors-errors, ""))
  end if
end method condition-to-string;


/* A <deque> of <pending-file>s: */
define constant *pending-files* = make(<deque>);
define constant *pending-files-change-lock* = make(<simple-lock>);
define constant *pending-files-change-notification* =
	make(<notification>, lock: *pending-files-change-lock*);

define variable *error-queue* :: <list> = #();
define constant *error-lock* = make(<lock>);
define variable *writer-thread* :: false-or(<thread>) = #f;
define constant *writer-thread-lock* = make(<lock>);


define class <pending-file> (<object>)
  slot pending-locator, required-init-keyword: locator:;
  slot pending-accessor, required-init-keyword: accessor:;
  slot pending-thread :: <thread>, required-init-keyword: thread:;
  slot pending-buffers :: <deque>, required-init-keyword: buffers:;
end class <pending-file>;


define function write-file (locator, byte-vectors :: <sequence>, 
			    #key if-exists, if-does-not-exist)
			=> ()

  // Check for previous errors:
  write-file-error-check(thread: current-thread());

  let the-accessor = new-accessor(#"file", 
				  locator: locator,
				  direction: #"output",
				  if-exists: if-exists, 
				  if-does-not-exist: if-does-not-exist); 

  with-lock (*pending-files-change-lock*)
    let file-data = make(<pending-file>, locator: locator, 
			 accessor: the-accessor, thread: current-thread(),
			 buffers: as(<deque>, byte-vectors));
    push-last(*pending-files*, file-data);
    release-all(*pending-files-change-notification*);
  end with-lock;

  with-lock (*writer-thread-lock*)
    if (~ *writer-thread*)
      *writer-thread* := make(<thread>, function: synchronous-writer,
			      name: "write-file I/O handler");
    end if;
  end with-lock;
  
end function write-file;


define function synchronous-writer () => ()
  while (#t)
    // Get a write request:
    let request = 
      with-lock (*pending-files-change-lock*)
	while (*pending-files*.empty?)
	  wait-for(*pending-files-change-notification*);
	end while;
	*pending-files*.first
      end with-lock;

    let file-handle = make(<HANDLE>, 
			   address: request.pending-accessor.accessor-fd);

    block (exit)
      while (~ request.pending-buffers.empty?)
	let buffer = pop(request.pending-buffers);
	// HACK!  This should be with-c-byte-vector:
	with-c-string (cstr-buffer = buffer)
	  let success? = WriteFile(file-handle, cstr-buffer, buffer.size,
				   null-pointer(<LPOVERLAPPED>));
	  if (~ success?)
	    with-lock (*error-lock*)
	      *error-queue* := pair(make(<write-file-error>, 
					 locator: request.pending-locator,
					 win32-error: GetLastError(), 
					 thread: request.pending-thread),
				    *error-queue*);
	      exit();	// Don't bother finishing up the file.
	    end with-lock;
	  end if;
	end with-c-string;
      end while;
    end block;

    with-lock (*pending-files-change-lock*)
      accessor-close(request.pending-accessor);
      pop(*pending-files*);
      release-all(*pending-files-change-notification*);
    end with-lock;
	
  end while;
end function synchronous-writer;


define function write-file-error-check (#key thread = #f) => ()
  with-lock (*error-lock*)
    if (*error-queue* ~= #())
      wait-for-write-completion-internal(thread);
      let errors = make(<write-file-errors>, errors: *error-queue*);
      *error-queue* := #();
      error(errors);
    end if;
  end with-lock;
end function write-file-error-check;


define function pof-has-thread(thread :: <thread>) => (r :: <boolean>)
  with-lock (*pending-files-change-lock*)
    any?(method (of :: <pending-file>) => (r :: <boolean>)
	   of.pending-thread == thread
	 end, *pending-files*)
  end with-lock;
end function pof-has-thread;

define function wait-for-write-completion-internal (thread) => ()
  while (~ (*pending-files*.size == 0 | 
	    (thread & ~ pof-has-thread(thread))))
    with-lock (*pending-files-change-lock*)
      wait-for(*pending-files-change-notification*);
    end with-lock;
  end while;
end function wait-for-write-completion-internal;


define function wait-for-write-completion (#key thread = #f) => ()
  wait-for-write-completion-internal(thread);
  write-file-error-check(thread: thread);
end function wait-for-write-completion;
