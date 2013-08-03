Module:       deuce-internals
Synopsis:     The Deuce editor
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Undo histories

define protocol <<undo-history>> ()
  function find-change-record
    (interval :: <interval>, class :: subclass(<change-record>),
     #rest initargs, #key, #all-keys)
 => (record :: false-or(<change-record>), history :: false-or(<undo-history>));
  function close-change-record
    (record :: <change-record>) => ();
  function extend-insertion-record
    (record :: <change-record>, #key, #all-keys) => ();
  function extend-deletion-record
    (record :: <change-record>, #key, #all-keys) => ();
  function add-change-record
    (history :: <undo-history>, record :: <change-record>) => ();
  function reset-undo-history
    (history :: <undo-history>) => ();
  function undo-history-state
    (history :: <undo-history>) => (n-undo :: <integer>, n-redo :: <integer>);
  // These all take a window so they can arrange to do redisplay
  function undo!
    (window :: <window>, buffer :: <buffer>, history :: <undo-history>) => ();
  function undo-all!
    (window :: <window>, buffer :: <buffer>, history :: <undo-history>) => ();
  function redo!
    (window :: <window>, buffer :: <buffer>, history :: <undo-history>) => ();
  function redo-all!
    (window :: <window>, buffer :: <buffer>, history :: <undo-history>) => ();
end protocol <<undo-history>>;

// Undo histories are be kept per source-container (_not_ buffer or window!)
// but the idea is to use the element's undo tick as a timestamp in order to
// sequence undo/redo operations within a buffer.
define sealed class <undo-history> (<object>)
  // We maintain a vector of changes and an index into it.  The index gives
  // the next place where a change record will be added.  The index will be
  // less than or equal to the size; when it's less than the size, all elements
  // after the index have been undone (and can now be redone).
  sealed slot %records :: <stretchy-object-vector> = make(<stretchy-vector>);
  sealed slot %index   :: <integer> = 0;
end class <undo-history>;

define sealed domain make (singleton(<undo-history>));
define sealed domain initialize (<undo-history>);


define protocol <<change-record>> ()
  // These take a window so they can arrange to do redisplay
  // Note that they also assume that *buffer* is properly bound!
  function do-undo
    (window :: <window>, record :: <change-record>) => ();
  function do-redo
    (window :: <window>, record :: <change-record>) => ();
end protocol <<change-record>>;

// Note that the BPs in change records a temporary BPs (i.e., they don't
// store their buffers), meaning that 'undo!' and 'redo!' need to be careful
// to establish a proper binding for *buffer* for composite buffers.
// 'buffer-undo-history' returns the proper buffer to use for this.
define open abstract primary class <change-record> (<object>)
  sealed slot change-record-start-bp :: false-or(<basic-bp>) = #f;
  sealed slot change-record-end-bp   :: false-or(<basic-bp>) = #f;
  //---*** What do we do about the section modification tick(s)?
  sealed slot %buffer-tick :: <integer> = 0;
end class <change-record>;

define sealed method initialize-modification-ticks
    (record :: <change-record>) => ()
  let start-bp :: <basic-bp> = change-record-start-bp(record);
  let end-bp   :: <basic-bp> = change-record-end-bp(record);
  let buffer   :: <basic-buffer> = bp-buffer(start-bp);
  record.%buffer-tick := buffer-modification-tick(buffer)
end method initialize-modification-ticks;

define sealed method restore-modification-ticks
    (record :: <change-record>) => ()
  let start-bp :: <basic-bp> = change-record-start-bp(record);
  let end-bp   :: <basic-bp> = change-record-end-bp(record);
  let buffer   :: <basic-buffer> = bp-buffer(start-bp);
  let old-modified? = buffer-modified?(buffer);
  buffer-modification-tick(buffer) := record.%buffer-tick;
  let new-modified? = buffer-modified?(buffer);
  unless (new-modified? == old-modified?)
    note-buffer-changed-everywhere(buffer, new-modified?)
  end
end method restore-modification-ticks;

define method extend-insertion-record
    (record :: <change-record>, #key) => ()
  #f
end method extend-insertion-record;

define method extend-deletion-record
    (record :: <change-record>, #key) => ()
  #f
end method extend-deletion-record;


define thread variable *change-record* :: false-or(<change-record>) = #f;

define macro with-change-recording
  { with-change-recording (?record:name = ?buffer:expression, ?class:expression, #rest ?initargs:*)
      ?:body
    end }
    => { let (?record, _history) = find-change-record(?buffer, ?class, ?initargs);
         dynamic-bind (*change-record* = ?record)
           block ()
             ?body
           afterwards
             when (_history)
               close-change-record(?record);
               add-change-record(_history, ?record)
             end
           end
         end }
  { with-change-recording (?buffer:expression, ?class:expression, #rest ?initargs:*)
      ?:body
    end }
    => { let (_record, _history) = find-change-record(?buffer, ?class, ?initargs);
         dynamic-bind (*change-record* = _record)
           block ()
             ?body
           afterwards
             when (_history)
               close-change-record(_record);
               add-change-record(_history, _record)
             end
           end
         end }
end macro with-change-recording;

define sealed method add-change-record
    (history :: <undo-history>, record :: <change-record>) => ()
  let records = history.%records;
  let index   = history.%index;
  if (index < size(records))
    // The user has done some undos; effectively cancel all of the stuff
    // following the current index, and insert the new element here
    size(records)  := index + 1;
    records[index] := record;
    history.%index := index + 1
  else
    // 'find-change-record' might have returned the previous change
    // record, in which case we don't want to add it again
    when (index = 0 | records[index - 1] ~== record)
      add!(records, record);
      history.%index := index + 1
    end
  end
end method add-change-record;

define sealed method reset-undo-history
    (history :: <undo-history>) => ()
  size(history.%records) := 0;
  history.%index := 0
end method reset-undo-history;

define sealed method undo-history-state
    (history :: <undo-history>) => (n-undo :: <integer>, n-redo :: <integer>)
  let records = history.%records;
  let index   = history.%index;
  values(index, size(records) - index)
end method undo-history-state;


/// Undo elements

// The class for simple insertions, which get merged
define sealed class <insert-change-record> (<change-record>)
  // This gets filled in when an undo gets done
  sealed slot change-record-new-text :: false-or(<string>) = #f;
end class <insert-change-record>;

define sealed domain make (singleton(<insert-change-record>));
define sealed domain initialize (<insert-change-record>);

define sealed method initialize
    (record :: <insert-change-record>,
     #key start-bp: sbp, end-bp: ebp) => ()
  next-method();
  let start-bp :: <basic-bp> = sbp;
  let end-bp   :: <basic-bp> = ebp | sbp;
  // We maintain the BP positions by hand, so they don't need to be moving
  change-record-start-bp(record)
    := make(<bp>, line: bp-line(start-bp), index: bp-index(start-bp));
  change-record-end-bp(record)
    := make(<bp>, line: bp-line(end-bp),   index: bp-index(end-bp));
  initialize-modification-ticks(record)
end method initialize;

define method find-change-record
    (buffer :: <basic-buffer>, class == <insert-change-record>,
     #rest initargs, #key start-bp, end-bp)
 => (record :: false-or(<insert-change-record>), history :: false-or(<undo-history>))
  let section = line-section(bp-line(start-bp | end-bp));
  let history = buffer-undo-history(buffer, section: section);
  if (history)
    let records = history.%records;
    let index   = history.%index;
    let record  = index > 0 & records[index - 1];
    // Try to use the previous change record if it's for an insertion
    if (record & object-class(record) == <insert-change-record>)
      case
        end-bp =>
          // Looks like we're doing a 'yank-next'
          values(record, history);
        (start-bp = change-record-end-bp(record)) =>
          // If the new insertion is contiguous with the previous one,
          // and it's not across a newline, merge the two insertions
          values(record, history);
        otherwise =>
          // Otherwise make a new record
          values(apply(make, class, initargs), history);
      end
    else
      values(apply(make, class, initargs), history)
    end
  else
    values(#f, #f)
  end
end method find-change-record;

define method close-change-record
    (record :: <insert-change-record>) => ()
  #f
end method close-change-record;

define method extend-insertion-record
    (record :: <insert-change-record>, #key end-bp) => ()
  move-bp!(change-record-end-bp(record), bp-line(end-bp), bp-index(end-bp))
end method extend-insertion-record;

define method do-undo
    (window :: <window>, record :: <insert-change-record>) => ()
  let start-bp :: <basic-bp> = change-record-start-bp(record);
  let end-bp   :: <basic-bp> = change-record-end-bp(record);
  let interval = make-interval(start-bp, end-bp, in-order?: #t);
  change-record-new-text(record) := as(<string>, interval);
  delete!(interval);
  restore-modification-ticks(record);
  move-point!(start-bp, window: window);
  queue-region-redisplay(window, start-bp, end-bp, centering: 0)
end method do-undo;

define method do-redo
    (window :: <window>, record :: <insert-change-record>) => ()
  let start-bp :: <basic-bp> = change-record-start-bp(record);
  let end-bp   :: <basic-bp> = insert!(start-bp, change-record-new-text(record));
  change-record-new-text(record) := #f;
  move-point!(end-bp, window: window);
  queue-region-redisplay(window, start-bp, end-bp, centering: 0)
end method do-redo;


// The class for "bulk" or newline insertions, which don't get merged
define sealed class <paste-change-record> (<insert-change-record>)
end class <paste-change-record>;

define sealed domain make (singleton(<paste-change-record>));
define sealed domain initialize (<paste-change-record>);

define method find-change-record
    (buffer :: <basic-buffer>, class == <paste-change-record>,
     #rest initargs, #key start-bp, end-bp)
 => (record :: false-or(<paste-change-record>), history :: false-or(<undo-history>))
  let section = line-section(bp-line(start-bp | end-bp));
  let history = buffer-undo-history(buffer, section: section);
  if (history)
    let records = history.%records;
    let index   = history.%index;
    let record  = index > 0 & records[index - 1];
    // Try to use the previous change record we're doing 'yank-next'
    if (record & object-class(record) == <paste-change-record>)
      case
        end-bp =>
          // Looks like we're doing a 'yank-next'
          values(record, history);
        otherwise =>
          // Otherwise make a new record
          values(apply(make, class, initargs), history);
      end
    else
      values(apply(make, class, initargs), history)
    end
  else
    values(#f, #f)
  end
end method find-change-record;


// The class for simple deletions, which get merged
define sealed class <delete-change-record> (<change-record>)
  // This gets filled in as the change record gets built
  sealed slot change-record-old-text :: false-or(<string>) = "";
end class <delete-change-record>;

define sealed domain make (singleton(<delete-change-record>));
define sealed domain initialize (<delete-change-record>);

define sealed method initialize
    (record :: <delete-change-record>,
     #key start-bp: sbp, end-bp: ebp, interval) => ()
  next-method();
  let (start-bp, end-bp) = order-bps(sbp | interval-start-bp(interval),
                                     ebp | sbp | interval-end-bp(interval));
  // We maintain the BP positions by hand, so they don't need to be moving
  change-record-start-bp(record)
    := make(<bp>, line: bp-line(start-bp), index: bp-index(start-bp));
  change-record-end-bp(record)
    := make(<bp>, line: bp-line(end-bp),   index: bp-index(end-bp));
  // 'extend-deletion-record' will initialize the old-text slot
  initialize-modification-ticks(record)
end method initialize;

define method find-change-record
    (buffer :: <basic-buffer>, class == <delete-change-record>,
     #rest initargs, #key start-bp, end-bp, interval)
 => (record :: false-or(<delete-change-record>), history :: false-or(<undo-history>))
  let start-bp = start-bp | interval-start-bp(interval);
  let end-bp   = end-bp   | interval-end-bp(interval);
  let section = line-section(bp-line(start-bp | end-bp));
  let history = buffer-undo-history(buffer, section: section);
  if (history)
    let records  = history.%records;
    let index    = history.%index;
    let record   = index > 0 & records[index - 1];
    // Try to use the previous change record if it's for a deletion
    if (record & object-class(record) == <delete-change-record>)
      case
        start-bp = change-record-start-bp(record) =>
          // If the start BP is not apparently moving,
          // it's probably repeated uses of 'delete-character'
          values(record, history);
        end-bp = change-record-start-bp(record) =>
          // If the start BP lines up with the saved end BP,
          // it's probably repeated uses of 'rubout-character'
          values(record, history);
        otherwise =>
          // Otherwise don't merge
          values(apply(make, class, initargs), history);
      end
    else
      values(apply(make, class, initargs), history)
    end
  else
    values(#f, #f)
  end
end method find-change-record;

define method close-change-record
    (record :: <delete-change-record>) => ()
  #f
end method close-change-record;

define method extend-deletion-record
    (record :: <delete-change-record>, #key interval) => ()
  let start-bp :: <basic-bp> = interval-start-bp(interval);
  let end-bp   :: <basic-bp> = interval-end-bp(interval);
  case
    start-bp = change-record-start-bp(record) =>
      // Finish the merged delete
      change-record-old-text(record)
        := concatenate-as(<string>,
                          change-record-old-text(record),
                          as(<string>, interval));
      move-bp!(change-record-end-bp(record), bp-line(end-bp), bp-index(end-bp));
    end-bp = change-record-start-bp(record) =>
      // Finish the merged rubout
      change-record-old-text(record)
        := concatenate-as(<string>,
                          as(<string>, interval),
                          change-record-old-text(record));
      move-bp!(change-record-start-bp(record), bp-line(start-bp), bp-index(start-bp));
    otherwise =>
      // Otherwise nothing to do
      #f
  end
end method extend-deletion-record;

define method do-undo
    (window :: <window>, record :: <delete-change-record>) => ()
  let start-bp :: <basic-bp> = change-record-start-bp(record);
  let end-bp   :: <basic-bp> = insert!(start-bp, change-record-old-text(record));
  change-record-end-bp(record) := end-bp;
  restore-modification-ticks(record);
  move-point!(end-bp, window: window);
  queue-region-redisplay(window, start-bp, end-bp, centering: 0)
end method do-undo;

define method do-redo
    (window :: <window>, record :: <delete-change-record>) => ()
  let start-bp :: <basic-bp> = change-record-start-bp(record);
  let end-bp   :: <basic-bp> = change-record-end-bp(record);
  let interval = make-interval(start-bp, end-bp, in-order?: #t);
  delete!(interval);
  move-point!(start-bp, window: window);
  queue-region-redisplay(window, start-bp, end-bp, centering: 0)
end method do-redo;


// The class for "bulk" deletions, which don't get merged
define sealed class <kill-change-record> (<delete-change-record>)
end class <kill-change-record>;

define sealed domain make (singleton(<kill-change-record>));
define sealed domain initialize (<kill-change-record>);

define method find-change-record
    (buffer :: <basic-buffer>, class == <kill-change-record>,
     #rest initargs, #key start-bp, end-bp, interval)
 => (record :: false-or(<kill-change-record>), history :: false-or(<undo-history>))
  let start-bp = start-bp | interval-start-bp(interval);
  let end-bp   = end-bp   | interval-end-bp(interval);
  let section = line-section(bp-line(start-bp | end-bp));
  let history = buffer-undo-history(buffer, section: section);
  if (history)
    // 'extend-deletion-record' will initialize the old-text slot
    let record = apply(make, class, initargs);
    values(record, history)
  else
    values(#f, #f)
  end
end method find-change-record;


// The class for any sort of replacement, which don't get merged
define sealed class <replace-change-record> (<change-record>)
  // This gets filled in as the change record gets built
  sealed slot change-record-old-text :: false-or(<string>) = "";
  // This gets filled in when an undo gets done
  sealed slot change-record-new-text :: false-or(<string>) = #f;
end class <replace-change-record>;

define sealed domain make (singleton(<replace-change-record>));
define sealed domain initialize (<replace-change-record>);

define sealed method initialize
    (record :: <replace-change-record>,
     #key start-bp: sbp, end-bp: ebp, interval, moving? = #f) => ()
  next-method();
  let (start-bp, end-bp) = order-bps(sbp | interval-start-bp(interval),
                                     ebp | sbp | interval-end-bp(interval));
  let interval = interval | make-interval(start-bp, end-bp, in-order?: #t);
  // The end BP may need to move in case the replacement changes
  // the size of the interval
  change-record-start-bp(record)
    := make(<bp>, line: bp-line(start-bp), index: bp-index(start-bp));
  change-record-end-bp(record)
    := make(<bp>, line: bp-line(end-bp),   index: bp-index(end-bp),
            moving?: moving?);
  change-record-old-text(record) := as(<string>, interval);
  initialize-modification-ticks(record)
end method initialize;

define method find-change-record
    (buffer :: <basic-buffer>, class == <replace-change-record>,
     #rest initargs, #key start-bp, end-bp, interval)
 => (record :: false-or(<replace-change-record>), history :: false-or(<undo-history>))
  let start-bp = start-bp | interval-start-bp(interval);
  let end-bp   = end-bp   | interval-end-bp(interval);
  let section = line-section(bp-line(start-bp | end-bp));
  let history = buffer-undo-history(buffer, section: section);
  if (history)
    values(apply(make, class, initargs), history)
  else
    values(#f, #f)
  end
end method find-change-record;

define method close-change-record
    (record :: <replace-change-record>) => ()
  #f
end method close-change-record;

define method do-undo
    (window :: <window>, record :: <replace-change-record>) => ()
  let start-bp :: <basic-bp> = change-record-start-bp(record);
  let end-bp   :: <basic-bp> = change-record-end-bp(record);
  let interval = make-interval(start-bp, end-bp, in-order?: #t);
  change-record-new-text(record) := as(<string>, interval);
  delete!(interval);
  let new-bp = insert!(start-bp, change-record-old-text(record));
  move-bp!(change-record-end-bp(record), bp-line(new-bp), bp-index(new-bp));
  restore-modification-ticks(record);
  move-point!(new-bp, window: window);
  queue-region-redisplay(window, start-bp, new-bp, centering: 0)
end method do-undo;

define method do-redo
    (window :: <window>, record :: <replace-change-record>) => ()
  let start-bp :: <basic-bp> = change-record-start-bp(record);
  let end-bp   :: <basic-bp> = change-record-end-bp(record);
  let interval = make-interval(start-bp, end-bp, in-order?: #t);
  delete!(interval);
  let new-bp = insert!(start-bp, change-record-new-text(record));
  move-bp!(change-record-end-bp(record), bp-line(new-bp), bp-index(new-bp));
  change-record-new-text(record) := #f;
  move-point!(new-bp, window: window);
  queue-region-redisplay(window, start-bp, new-bp, centering: 0)
end method do-redo;


// A more efficient class for indenting entire regions
define sealed class <indentation-change-record> (<change-record>)
  sealed slot %old-indentation :: <simple-object-vector> = #[];
  sealed slot %new-indentation :: <simple-object-vector> = #[];
end class <indentation-change-record>;

define sealed domain make (singleton(<indentation-change-record>));
define sealed domain initialize (<indentation-change-record>);

define sealed method initialize
    (record :: <indentation-change-record>,
     #key start-bp: sbp, end-bp: ebp, interval) => ()
  next-method();
  let (start-bp, end-bp) = order-bps(sbp | interval-start-bp(interval),
                                     ebp | sbp | interval-end-bp(interval));
  let start-bp = make(<bp>, line: bp-line(start-bp), index: 0);
  let end-bp   = make(<bp>, line: bp-line(end-bp),   index: 0);
  let interval = make-interval(start-bp, end-bp, in-order?: #t);
  change-record-start-bp(record) := start-bp;
  change-record-end-bp(record)   := end-bp;
  let n   :: <integer> = count-lines(interval);
  let i   :: <integer> = 0;
  let old :: <simple-object-vector> = make(<vector>, size: n);
  //--- Very ugly that we need a window in order to do this
  let window :: <basic-window> = frame-window(*editor-frame*);
  let mode   :: <major-mode>   = buffer-major-mode(bp-buffer(start-bp));
  let space-width = string-size(window, " ");
  local method measure-indentation (line :: <basic-line>, si, ei, last?)
          ignore(si, ei, last?);
          let indentation
            = if (text-line?(line))
                let bp = forward-over!(line-start(line), #[' ', '\t']);
                let margin = line-margin(line, mode, window);
                let indentation
                  = index->position(bp-line(bp), mode, window, bp-index(bp)) - margin;
                floor/(indentation, space-width)
              else
                0
              end;
          old[i] := indentation;
          inc!(i)
        end method;
  do-lines(measure-indentation, interval);
  record.%old-indentation := old;
  initialize-modification-ticks(record)
end method initialize;

define method find-change-record
    (buffer :: <basic-buffer>, class == <indentation-change-record>,
     #rest initargs, #key start-bp, end-bp, interval)
 => (record :: false-or(<indentation-change-record>), history :: false-or(<undo-history>))
  let start-bp = start-bp | interval-start-bp(interval);
  let end-bp   = end-bp   | interval-end-bp(interval);
  let section = line-section(bp-line(start-bp | end-bp));
  let history = buffer-undo-history(buffer, section: section);
  if (history)
    values(apply(make, class, initargs), history)
  else
    values(#f, #f)
  end
end method find-change-record;

define method close-change-record
    (record :: <indentation-change-record>) => ()
  let n   :: <integer> = size(record.%old-indentation);
  let i   :: <integer> = 0;
  let new :: <simple-object-vector> = make(<vector>, size: n);
  let start-bp :: <basic-bp> = change-record-start-bp(record);
  let end-bp   :: <basic-bp> = change-record-end-bp(record);
  let interval = make-interval(start-bp, end-bp, in-order?: #t);
  //--- Very ugly that we need a window in order to do this
  let window :: <basic-window> = frame-window(*editor-frame*);
  let mode   :: <major-mode>   = buffer-major-mode(bp-buffer(start-bp));
  let space-width = string-size(window, " ");
  local method measure-indentation (line :: <basic-line>, si, ei, last?)
          ignore(si, ei, last?);
          let indentation
            = if (text-line?(line))
                let bp = forward-over!(line-start(line), #[' ', '\t']);
                let margin = line-margin(line, mode, window);
                let indentation
                  = index->position(bp-line(bp), mode, window, bp-index(bp)) - margin;
                floor/(indentation, space-width)
              else
                0
              end;
          new[i] := indentation;
          inc!(i)
        end method;
  do-lines(measure-indentation, interval);
  record.%new-indentation := new
end method close-change-record;

define method do-undo
    (window :: <window>, record :: <indentation-change-record>) => ()
  let old :: <simple-object-vector> = record.%old-indentation;
  let i   :: <integer> = 0;
  let start-bp :: <basic-bp> = change-record-start-bp(record);
  let end-bp   :: <basic-bp> = change-record-end-bp(record);
  let interval = make-interval(start-bp, end-bp, in-order?: #t);
  local method change-indentation (line :: <basic-line>, si, ei, last?)
          ignore(si, ei, last?);
          let indentation = old[i];
          inc!(i);
          let bp1 = line-start(line);
          let bp2 = forward-over(bp1, #[' ', '\t']);
          delete!(make-interval(bp1, bp2, in-order?: #t));
          let spaces = make(<byte-string>, size: indentation, fill: ' ');
          insert!(bp1, spaces)
        end method;
  do-lines(change-indentation, interval);
  restore-modification-ticks(record);
  move-point!(end-bp, window: window);
  queue-region-redisplay(window, start-bp, end-bp, centering: 0)
end method do-undo;

define method do-redo
    (window :: <window>, record :: <indentation-change-record>) => ()
  let new :: <simple-object-vector> = record.%new-indentation;
  let i   :: <integer> = 0;
  let start-bp :: <basic-bp> = change-record-start-bp(record);
  let end-bp   :: <basic-bp> = change-record-end-bp(record);
  let interval = make-interval(start-bp, end-bp, in-order?: #t);
  local method change-indentation (line :: <basic-line>, si, ei, last?)
          ignore(si, ei, last?);
          let indentation = new[i];
          inc!(i);
          let bp1 = line-start(line);
          let bp2 = forward-over(bp1, #[' ', '\t']);
          delete!(make-interval(bp1, bp2, in-order?: #t));
          let spaces = make(<byte-string>, size: indentation, fill: ' ');
          insert!(bp1, spaces)
        end method;
  do-lines(change-indentation, interval);
  move-point!(end-bp, window: window);
  queue-region-redisplay(window, start-bp, end-bp, centering: 0)
end method do-redo;


/// Undo and redo

define sealed method undo!
    (window :: <basic-window>, buffer :: <basic-buffer>, history :: <undo-history>) => ()
  // Change records probably have temporary BPs in them, so we need to bind
  // *buffer* so that 'bp-buffer' does the right thing for composite buffers
  dynamic-bind (*buffer* = buffer)
    let records = history.%records;
    let index   = history.%index;
    when (index > 0)
      do-undo(window, records[index - 1]);
      history.%index := index - 1
    end
  end
end method undo!;

define sealed method undo-all!
    (window :: <basic-window>, buffer :: <basic-buffer>, history :: <undo-history>) => ()
  dynamic-bind (*buffer* = buffer)
    let records = history.%records;
    let index   = history.%index;
    while (index > 0)
      do-undo(window, records[index - 1]);
      index := index - 1
    end;
    history.%index := 0
  end
end method undo-all!;


define sealed method redo!
    (window :: <basic-window>, buffer :: <basic-buffer>, history :: <undo-history>) => ()
  dynamic-bind (*buffer* = buffer)
    let records = history.%records;
    let index   = history.%index;
    when (index < size(records))
      do-redo(window, records[index]);
      history.%index := index + 1
    end
  end
end method redo!;

define sealed method redo-all!
    (window :: <basic-window>, buffer :: <basic-buffer>, history :: <undo-history>) => ()
  dynamic-bind (*buffer* = buffer)
    let records = history.%records;
    let index   = history.%index;
    while (index < size(records))
      do-redo(window, records[index]);
      index := index + 1
    end;
    history.%index := size(records)
  end
end method redo-all!;
