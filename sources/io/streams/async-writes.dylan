Module:       streams-internals
Synopsis:     Support for asynchronous writes.
Author:       Seth LaForge
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Number of reserve buffers in the buffer pool.
// Should this perhaps be dynamic?
// Note: I tried creating new buffers as necessary but that slowed
// performance horribly - probably from additional allocation/GC costs.
define constant $buffer-pool-size :: <integer> = 16;


// A pending operation.  Operations which require additional data should be
// subclasses, e.g. <pending-write>.
define class <pending-operation> (<object>)

  // The function to call to perform the operation.  Should accept a
  // <pending-operation> for its first argument, and signal any errors.
  constant slot pending-operation :: <function>,
    init-keyword: operation:;

  slot pending-status :: one-of(#f, #"in-queue", #"in-progress", #"complete")
                  = #f;

  // The stream on which to perform (mostly for errors):
  constant slot pending-stream :: <stream>,
    init-keyword: stream:;

  // The accessor on which to perform:
  constant slot pending-accessor :: <external-stream-accessor>,
    init-keyword: accessor:;

end class <pending-operation>;


define class <pending-write> (<pending-operation>)

  // Offset within the file at which to write the data:
  constant slot pending-file-offset :: <integer>,
    init-keyword: file-offset:;

  // Buffer of data to write:
  constant slot pending-buffer :: <buffer>,
    init-keyword: buffer:;
  slot pending-pool-buffer? :: <boolean> = #f;

  // Number of bytes to write:
  constant slot pending-count :: <integer>,
    init-keyword: count:;

  // Offset within buffer to write:
  constant slot pending-buffer-offset :: <buffer-index>,
    init-keyword: buffer-offset:;

end class <pending-write>;


// A <deque> of <pending-operation>s:
define constant *pending-operations* :: <deque> = make(<deque>);
define constant *pending-operations-lock* :: <simple-lock>
        = make(<simple-lock>);
define constant *pending-operations-add-notification* :: <notification>
        = make(<notification>, lock: *pending-operations-lock*);
define constant *pending-operations-remove-notification* :: <notification>
        = make(<notification>, lock: *pending-operations-lock*);

// Any errors which have occurred, by accessor:
define constant *async-error-table* :: <object-table> = make(<object-table>);
define constant *async-error-lock* :: <simple-lock> = make(<simple-lock>);

// The thread which actually performs the writes:
define variable *writer-thread* :: false-or(<thread>) = #f;
define constant *writer-thread-lock* :: <simple-lock> = make(<simple-lock>);

// A pool of buffers for asynchronous operations.  Buffers are lazily created.
define variable *buffer-pool* :: <list> = make(<list>, size: $buffer-pool-size);
define constant *buffer-pool-lock* :: <simple-lock> = make(<simple-lock>);


// Enqueue an operation.

define function enqueue-operation (op :: <pending-operation>) => ()
  if (~ *writer-thread*)
    with-lock (*writer-thread-lock*)
      if (~ *writer-thread*)
        *writer-thread* := make(<thread>, function: async-IO-handler,
                                name: "asynchronous I/O handler");
      end if;
    end with-lock;
  end if;
  with-lock (*pending-operations-lock*)
    push-last(*pending-operations*, op);
    op.pending-status := #"in-queue";
    release-all(*pending-operations-add-notification*);
  end with-lock;
end function enqueue-operation;


// Enqueue a write operation, perhaps returning a fresh buffer from the pool.

define function enqueue-write (op :: <pending-write>,
                               return-fresh-buffer? :: <boolean>)
                           => (fresh-buffer :: <buffer>)
  // format-out("STREAMS: enqueuing write, buffer-size == %d.\n", op.pending-buffer.size);
  let preferred-buffer-size = accessor-preferred-buffer-size(op.pending-stream.accessor);
  if (op.pending-buffer.size == preferred-buffer-size)

    // format-out("STREAMS: async write initiated.\n");

    // Make this buffer be returned to the buffer pool on completion:
    op.pending-pool-buffer? := return-fresh-buffer?;

    // Enqueue:
    enqueue-operation(op);

    if (return-fresh-buffer?)
      // Wait for a free buffer and return it:
      with-lock (*buffer-pool-lock*)
        if (empty?(*buffer-pool*))
          with-lock (*pending-operations-lock*)
            while (empty?(*buffer-pool*))
              wait-for(*pending-operations-remove-notification*);
            end while;
          end with-lock;
        end if;
        let new-buffer = *buffer-pool*.head;
        *buffer-pool* := *buffer-pool*.tail;
        // If the buffer is #f, we need to create it:
        new-buffer | make-<power-of-two-buffer>
                        (known-power-of-two-size?: #t,
                         size: preferred-buffer-size);
      end with-lock
    else
      op.pending-buffer
    end if
  else
    enqueue-operation(op);
    with-lock (*pending-operations-lock*)
      while (op.pending-status ~= #"complete")
        wait-for(*pending-operations-remove-notification*);
      end while;
    end with-lock;
    op.pending-buffer
  end if
end function enqueue-write;


// Wait for all writes on accessor which overlap the range given to complete.

define function async-wait-for-overlapping-write-completion
        (accessor :: <external-stream-accessor>, offset :: <integer>,
         size :: <integer>) => ()
  local method overlap? (op :: <pending-operation>) => (r :: <boolean>)
    instance?(op, <pending-write>) & op.pending-accessor == accessor & begin
      let low = max(offset, op.pending-file-offset);
      let high = min(offset + size, op.pending-file-offset + op.pending-count);
      high > low
    end
  end method overlap?;
  with-lock (*pending-operations-lock*)
    while (any?(overlap?, *pending-operations*))
      wait-for(*pending-operations-remove-notification*);
    end while;
  end with-lock;
end async-wait-for-overlapping-write-completion;


// Wait for all operations on accessor to complete.

define method async-wait-for-completion
        (accessor :: <external-stream-accessor>) => ()
  local method same? (op :: <pending-operation>) => (r :: <boolean>)
    op.pending-accessor == accessor
  end method same?;
  with-lock (*pending-operations-lock*)
    while (any?(same?, *pending-operations*))
      wait-for(*pending-operations-remove-notification*);
    end while;
  end with-lock;
end async-wait-for-completion;


define function async-get-error (accessor :: <external-stream-accessor>)
                             => (err :: false-or(<error>))
  with-lock (*async-error-lock*)
    element(*async-error-table*, accessor, default: #f)
  end with-lock
end function async-get-error;


// If there's an error reported for this accessor, throw it.

define function async-check-for-errors (accessor :: <external-stream-accessor>)
                                    => ()
  let err = async-get-error(accessor);
  if (err)
    error(err);
  end if;
end function async-check-for-errors;


// The actual async operation handler - runs in its own thread, and performs
// queued operations.

define function get-op () => (r :: <pending-operation>)
      with-lock (*pending-operations-lock*)
        while (*pending-operations*.empty?)
          wait-for(*pending-operations-add-notification*);
        end while;
        let op :: <pending-operation> = *pending-operations*.first;
        op.pending-status := #"in-progress";
        op
      end with-lock;
end function get-op;

define function async-IO-handler () => ()
  while (#t)

    // Get an operation:
    let operation :: <pending-operation> = get-op();

    // Perform operation:
    block ()
      operation.pending-operation(operation);
    exception (e :: <error>)
      with-lock (*async-error-lock*)
        if (~ element(*async-error-table*, operation.pending-accessor,
                      default: #f))
          *async-error-table*[operation.pending-accessor] := e;
        end if;
      end with-lock;
    end block;
    async-post-operation(operation);

    // Remove operation from queue:
    with-lock (*pending-operations-lock*)
      operation.pending-status := #"complete";
      pop(*pending-operations*);
      release-all(*pending-operations-remove-notification*);
    end with-lock;

  end while;
end function async-IO-handler;


// async-post-operation gets called after an operation has completed (even if
// there was an error).  One good use is to return buffers to the buffer pool.

define generic async-post-operation (operation :: <pending-operation>) => ();

define method async-post-operation (operation :: <pending-operation>) => ()
end method async-post-operation;

define method async-post-operation (operation :: <pending-write>) => ()
  if (operation.pending-pool-buffer?)
    // Return the buffer to the pool.  No lock necessary, since this is an
    // atomic operation.
    *buffer-pool* := pair(operation.pending-buffer, *buffer-pool*);
  end if;
end method async-post-operation;
