Module:    environment-framework
Synopsis:  Environment Framework
Author:    Andy Armstrong, Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Searching commands

define constant $asynchronous-timeout = 10;

// Return whether the frame can issue Find/Replace commands at this time.
// These are used to determine when the relavent commands should be enabled.
define open generic frame-can-find?    (frame :: <frame>) => (can? :: <boolean>);
define open generic frame-can-replace? (frame :: <frame>) => (can? :: <boolean>);

// Open a frame to display/change the Find/Replace strings and other options.
define open generic frame-edit-search-options       (frame :: <frame>) => ();

// Return the class to instantiate for the search options frame. This allows
// client libraries to subclass the frame.
define open generic frame-search-frame-class        (frame :: <frame>) => (class :: <class>);

// Find the next/previous match, using the current search options.
define open generic frame-find-next                 (frame :: <frame>) => ();
define open generic frame-find-previous             (frame :: <frame>) => ();

// Find the next/previous match in the next/previous target. This skips over
// any matches in the current target.
define open generic frame-find-in-next-target       (frame :: <frame>) => ();
define open generic frame-find-in-previous-target   (frame :: <frame>) => ();

// Copy the selected text in the frame to the Find/Replace string.
define open generic frame-copy-selection-to-search  (frame :: <frame>) => ();
define open generic frame-copy-selection-to-replace (frame :: <frame>) => ();

// Copy the selected text in the frame to the Find/Replace string, then
// search for the next/previous match. This is the same as calling
// frame-copy-selection-to-search followed by frame-find-next/previous.
define open generic frame-find-selection-next       (frame :: <frame>) => ();
define open generic frame-find-selection-previous   (frame :: <frame>) => ();

// Replace the selected text in the current target if it is a match.
define open generic frame-replace-selection         (frame :: <frame>) => ();

// Replace the selected text in the current target if it is a match. Then
// search for the next/previous match. The search is performed whether or
// not the current selection is replaced.
define open generic frame-replace-and-find-next     (frame :: <frame>) => ();
define open generic frame-replace-and-find-previous (frame :: <frame>) => ();

// Find and replace all matches. Depending on the current search options,
// this may replace from the selection or throughout the current target,
// and it may replace only in the current target, or throughout all targets.
define open generic frame-replace-all               (frame :: <frame>) => ();

// Notify the frame that the searching options have been changed. This is
// usually used to update command enabling.
define open generic note-frame-searching-updated    (frame :: <frame>) => ();


// Default methods

define method frame-can-find? (frame :: <frame>) => (can? :: <boolean>)
  #f
end method frame-can-find?;

define method frame-can-replace? (frame :: <frame>) => (can? :: <boolean>)
  #f
end method frame-can-replace?;

define method note-frame-searching-updated (frame :: <frame>) => ()
  // Do nothing
end method note-frame-searching-updated;


/// Searching in a sheet/gadget

define open generic can-find-in-sheet?    (sheet :: <abstract-sheet>) => (can? :: <boolean>);
define open generic can-replace-in-sheet? (sheet :: <abstract-sheet>) => (can? :: <boolean>);

define open generic find-in-sheet
    (sheet         :: <abstract-sheet>,
     search-string :: <string>,
     #key from-selection?   :: <boolean>,
          backwards?        :: <boolean>,
          wrap?             :: <boolean>,
          match-case?       :: <boolean>,
          match-word?       :: <boolean>,
          match-regexp?     :: <boolean>,
          progress-callback :: false-or(<function>))
 => (object :: <object>);

define open generic find-all-in-sheet
    (sheet           :: <abstract-sheet>,
     register-object :: <function>,
     search-string   :: <string>,
     #key match-case?       :: <boolean>,
          match-word?       :: <boolean>,
          match-regexp?     :: <boolean>,
          progress-callback :: false-or(<function>))
 => ();

define open generic replace-in-sheet
    (sheet          :: <abstract-sheet>,
     search-string  :: <string>,
     replace-string :: <string>,
     #key match-case?       :: <boolean>,
          match-word?       :: <boolean>,
          match-regexp?     :: <boolean>,
          progress-callback :: false-or(<function>))
 => (object :: <object>);

define open generic replace-all-in-sheet
    (sheet          :: <abstract-sheet>,
     search-string  :: <string>,
     replace-string :: <string>,
     #key from-selection?   :: <boolean>,
          backwards?        :: <boolean>,
          wrap?             :: <boolean>,
          match-case?       :: <boolean>,
          match-word?       :: <boolean>,
          match-regexp?     :: <boolean>,
          progress-callback :: false-or(<function>))
 => (replace-count :: <integer>);

define open generic sheet-reveal-search-object
    (sheet :: <abstract-sheet>, object :: <object>) => (revealed? :: <boolean>);

define open generic sheet-search-object-label
    (sheet :: <abstract-sheet>, object :: <object>) => (label :: false-or(<string>));

define open generic sheet-search-object-icon
    (sheet :: <abstract-sheet>, object :: <object>) => (icon :: false-or(<image>));


// Default methods

define method can-find-in-sheet? (sheet :: <abstract-sheet>) => (can-find? :: <boolean>)
  #f
end method can-find-in-sheet?;

define method can-replace-in-sheet? (sheet :: <abstract-sheet>) => (can-replace? :: <boolean>)
  #f
end method can-replace-in-sheet?;


/// Searching in a frame

define open generic can-find-in-frame?    (frame :: <frame>) => (can? :: <boolean>);
define open generic can-replace-in-frame? (frame :: <frame>) => (can? :: <boolean>);

define open generic find-in-frame
    (frame         :: <frame>,
     search-string :: <string>,
     #key from-selection?   :: <boolean>,
          backwards?        :: <boolean>,
          wrap?             :: <boolean>,
          match-case?       :: <boolean>,
          match-word?       :: <boolean>,
          match-regexp?     :: <boolean>,
          progress-callback :: false-or(<function>))
 => (object :: <object>);

define open generic find-all-in-frame
    (frame           :: <frame>,
     register-object :: <function>,
     search-string   :: <string>,
     #key match-case?       :: <boolean>,
          match-word?       :: <boolean>,
          match-regexp?     :: <boolean>,
          progress-callback :: false-or(<function>))
 => ();

define open generic replace-in-frame
    (frame          :: <frame>,
     search-string  :: <string>,
     replace-string :: <string>,
     #key match-case?       :: <boolean>,
          match-word?       :: <boolean>,
          match-regexp?     :: <boolean>,
          progress-callback :: false-or(<function>))
 => (object :: <object>);

define open generic replace-all-in-frame
    (frame          :: <frame>,
     search-string  :: <string>,
     replace-string :: <string>,
     #key from-selection?   :: <boolean>,
          backwards?        :: <boolean>,
          wrap?             :: <boolean>,
          match-case?       :: <boolean>,
          match-word?       :: <boolean>,
          match-regexp?     :: <boolean>,
          progress-callback :: false-or(<function>))
 => (replace-count :: <integer>);

define open generic frame-reveal-search-object
    (frame :: <frame>, object :: <object>) => (revealed? :: <boolean>);

define open generic frame-search-object-label
    (frame :: <frame>, object :: <object>) => (label :: false-or(<string>));

define open generic frame-search-object-icon
    (frame :: <frame>, object :: <object>) => (icon :: false-or(<image>));


// Default methods

define method can-find-in-frame? (frame :: <frame>) => (can-find? :: <boolean>)
  #f
end method can-find-in-frame?;

define method can-replace-in-frame? (frame :: <frame>) => (can-replace? :: <boolean>)
  #f
end method can-replace-in-frame?;


/// Searching commands

define abstract class <frame-find-command> (<frame-focus-command>)
end class <frame-find-command>;

define abstract class <frame-find-selection-command> 
    (<frame-find-command>,
     <frame-selection-command>)
end class <frame-find-selection-command>;

define abstract class <frame-replace-command> (<frame-focus-command>)
end class <frame-replace-command>;

define class <frame-edit-search-options-command> (<basic-command>)
end class <frame-edit-search-options-command>;

define method do-execute-command
    (frame :: <frame>, command :: <frame-edit-search-options-command>) => ()
  frame-edit-search-options(frame)
end method do-execute-command;

define class <frame-find-next-command> (<frame-find-command>)
end class <frame-find-next-command>;

define method execute-command-for-focus
    (frame :: <frame>, command :: <frame-find-next-command>) => ()
  frame-find-next(frame)
end method execute-command-for-focus;

define class <frame-find-previous-command> (<frame-find-command>)
end class <frame-find-previous-command>;

define method execute-command-for-focus
    (frame :: <frame>, command :: <frame-find-previous-command>) => ()
  frame-find-previous(frame)
end method execute-command-for-focus;

define class <frame-find-in-next-target-command> (<frame-find-command>)
end class <frame-find-in-next-target-command>;

define method execute-command-for-focus
    (frame :: <frame>, command :: <frame-find-in-next-target-command>) => ()
  frame-find-in-next-target(frame)
end method execute-command-for-focus;

define class <frame-find-in-previous-target-command> (<frame-find-command>)
end class <frame-find-in-previous-target-command>;

define method execute-command-for-focus
    (frame :: <frame>, command :: <frame-find-in-previous-target-command>) => ()
  frame-find-in-previous-target(frame)
end method execute-command-for-focus;

define class <frame-copy-selection-to-search-command> (<frame-selection-command>)
end class <frame-copy-selection-to-search-command>;

define method execute-command-for-focus
    (frame :: <frame>, command :: <frame-copy-selection-to-search-command>) => ()
  frame-copy-selection-to-replace(frame)
end method execute-command-for-focus;

define class <frame-copy-selection-to-replace-command> (<frame-selection-command>)
end class <frame-copy-selection-to-replace-command>;

define method execute-command-for-focus
    (frame :: <frame>, command :: <frame-copy-selection-to-replace-command>) => ()
  frame-copy-selection-to-replace(frame)
end method execute-command-for-focus;

define class <frame-find-selection-next-command> (<frame-find-selection-command>)
end class <frame-find-selection-next-command>;

define method execute-command-for-focus
    (frame :: <frame>, command :: <frame-find-selection-next-command>) => ()
  frame-find-selection-next(frame)
end method execute-command-for-focus;

define class <frame-find-selection-previous-command> (<frame-find-selection-command>)
end class <frame-find-selection-previous-command>;

define method execute-command-for-focus
    (frame :: <frame>, command :: <frame-find-selection-previous-command>) => ()
  frame-find-selection-previous(frame)
end method execute-command-for-focus;

define class <frame-replace-selection-command> (<frame-replace-command>)
end class <frame-replace-selection-command>;

define method execute-command-for-focus
    (frame :: <frame>, command :: <frame-replace-selection-command>) => ()
  frame-replace-selection(frame)
end method execute-command-for-focus;

define class <frame-replace-and-find-next-command> (<frame-replace-command>)
end class <frame-replace-and-find-next-command>;

define method execute-command-for-focus
    (frame :: <frame>, command :: <frame-replace-and-find-next-command>) => ()
  frame-replace-and-find-next(frame)
end method execute-command-for-focus;

define class <frame-replace-and-find-previous-command> (<frame-replace-command>)
end class <frame-replace-and-find-previous-command>;

define method execute-command-for-focus
    (frame :: <frame>, command :: <frame-replace-and-find-previous-command>) => ()
  frame-replace-and-find-previous(frame)
end method execute-command-for-focus;

define class <frame-replace-all-command> (<frame-replace-command>)
end class <frame-replace-all-command>;

define method execute-command-for-focus
    (frame :: <frame>, command :: <frame-replace-all-command>) => ()
  frame-replace-all(frame)
end method execute-command-for-focus;


/// Searching within a domain

define open abstract class <search-domain> (<object>)
end class <search-domain>;

define open generic register-search-domain
    (domain :: <search-domain>) => ();

define open generic unregister-search-domain
    (domain :: <search-domain>) => ();

define open generic search-domain-label
    (domain :: <search-domain>) => (label :: <string>);

define open generic search-domain-targets
    (domain :: <search-domain>) => (targets :: <sequence>);

define open generic search-domain-target-label
    (domain :: <search-domain>, target :: <object>) => (label :: <string>);

define open generic search-domain-target-kind-label
    (domain :: <search-domain>, target :: <object>) => (label :: <string>);

define open generic search-domain-target-icon
    (domain :: <search-domain>, target :: <object>) => (icon :: false-or(<image>));

define open generic search-domain-target-can-find?
    (domain :: <search-domain>, target :: <object>) => (can-find? :: <boolean>);

define open generic search-domain-target-can-replace?
    (domain :: <search-domain>, target :: <object>) => (can-replace? :: <boolean>);

define open generic search-domain-find
    (domain        :: <search-domain>,
     target        :: <object>,
     search-string :: <string>,
     #key from-selection?   :: <boolean>,
          backwards?        :: <boolean>,
          wrap?             :: <boolean>,
          match-case?       :: <boolean>,
          match-word?       :: <boolean>,
          match-regexp?     :: <boolean>,
          progress-callback :: false-or(<function>))
 => (object :: <object>);

define open generic search-domain-find-all
    (domain          :: <search-domain>,
     target          :: <object>,
     register-object :: <function>,
     search-string   :: <string>,
     #key match-case?       :: <boolean>,
          match-word?       :: <boolean>,
          match-regexp?     :: <boolean>,
          progress-callback :: false-or(<function>))
 => ();

define open generic search-domain-replace-selection
    (domain         :: <search-domain>,
     target         :: <object>,
     search-string  :: <string>,
     replace-string :: <string>,
     #key match-case?       :: <boolean>,
          match-word?       :: <boolean>,
          match-regexp?     :: <boolean>,
          progress-callback :: false-or(<function>))
 => (object :: <object>);

define open generic search-domain-replace-all
    (domain          :: <search-domain>,
     target          :: <object>,
     search-string   :: <string>,
     replace-string  :: <string>,
     #key from-selection? :: <boolean>,
          backwards?      :: <boolean>,
          wrap?           :: <boolean>,
          match-case?     :: <boolean>,
          match-word?     :: <boolean>,
          match-regexp?   :: <boolean>,
          progress-callback :: false-or(<function>))
 => (replace-count :: <integer>);

define open generic search-domain-reveal-search-object
    (domain :: <search-domain>, object :: <object>) => (revealed? :: <boolean>);

define open generic search-domain-search-object-label
    (domain :: <search-domain>, object :: <object>) => (label :: false-or(<string>));

define open generic search-domain-search-object-icon
    (domain :: <search-domain>, object :: <object>) => (icon :: false-or(<image>));


// Default methods

define method search-domain-target-can-find?
    (domain :: <search-domain>, target :: <object>) => (can-find? :: <boolean>)
  #f
end method search-domain-target-can-find?;

define method search-domain-target-can-replace?
    (domain :: <search-domain>, target :: <object>) => (can-replace? :: <boolean>)
  #f
end method search-domain-target-can-replace?;


/// <search-target-frame-mixin>: Frames that can be searched

define open abstract class <search-target-frame-mixin> (<frame>)
end class <search-target-frame-mixin>;

// The current target frame

define variable *current-search-target-frame* :: false-or(<search-target-frame-mixin>) = #f;

define function current-search-target-frame
    () => (target-frame :: false-or(<search-target-frame-mixin>))
  *current-search-target-frame*
end function current-search-target-frame;

define function current-search-target-frame-setter 
    (target-frame :: false-or(<search-target-frame-mixin>))
 => (target-frame :: false-or(<search-target-frame-mixin>))
  when (*current-search-target-frame* ~== target-frame)
    *current-search-target-frame* := target-frame;
    let search-frame = current-search-options-frame();
    search-frame
      & call-in-frame(search-frame,
		      note-search-frame-current-search-target-frame-changed,
		      search-frame);
  end;
  target-frame
end function current-search-target-frame-setter;

// Set the current search target frame when a target frame gets the focus
define method handle-event
    (frame :: <search-target-frame-mixin>, event :: <frame-focus-in-event>) => ()
  next-method();
  //---*** Enabled for debugging only...
  debug-message("handle-event: Search target frame got the focus: %=", frame.frame-title);
  current-search-target-frame() := frame;
end method handle-event;

// Reset the current search target frame
define function reset-current-search-target-frame
    (old-target-frame :: <search-target-frame-mixin>) => ()
  when (old-target-frame == current-search-target-frame())
    //debug-message("handle-event:   The current target frame was exited");
    //--- cpage: 1998.08.11 Ideally, we could just reset this to #f and the next
    //           frame to get the focus would set it. However, the next frame to
    //           get the focus isn't necessarily a search target frame. This can
    //           happen if the Find window, for example is the next frame in Z
    //           order and the user closes the target frame. I suppose it might
    //           be better to hang this behavior on <frame-focus-in-event> for
    //           all non-target frames, to prevent unnecessary work, but I
    //           don't think there's an appropriate <frame> subclass to
    //           specialize on. Perhaps we need to define a <framework-frame>.
    // Search for the next available target frame
    let new-target-frame
      = block (return)
	  do-frames(method (frame :: <frame>) => ()
		      when (frame ~== old-target-frame
			    & instance?(frame, <search-target-frame-mixin>))
			return(frame)
		      end
		    end method,
		    z-order: #"top-down");
	end block;
    //debug-message("Resetting search target frame to: %=",
    //              new-target-frame & new-target-frame.frame-title);
    current-search-target-frame() := new-target-frame;
  end;
end function reset-current-search-target-frame;

// Reset the current search target frame when the current frame exits
define method handle-event
    (frame :: <search-target-frame-mixin>, event :: <frame-exited-event>) => ()
  next-method();
  //debug-message("handle-event: Search target frame was exited: %=", frame.frame-title);
  reset-current-search-target-frame(frame);
end method handle-event;

//---*** cpage: 1997.07.24 Andy believes we should use <frame-exited-event>
//              instead. This is partly based upon the belief that iconized
//              windows are unmapped. Verify, then delete these methods.
// BEGIN DELETE
define method handle-event
    (frame :: <search-target-frame-mixin>, event :: <frame-unmapped-event>) => ()
  next-method();
  //debug-message("handle-event: Search target frame was unmapped: %=", frame.frame-title);
  reset-current-search-target-frame(frame);
end method handle-event;

define method handle-event
    (frame :: <search-target-frame-mixin>, event :: <frame-destroyed-event>) => ()
  next-method();
  //debug-message("handle-event: Search target frame was destroyed: %=", frame.frame-title);
  reset-current-search-target-frame(frame);
end method handle-event;
// END DELETE


/// Searching in a sheet/gadget

define method can-find-in-sheet?
    (gadget    :: <collection-gadget>) => (can-find? :: <boolean>)
  #t
end method can-find-in-sheet?;

define function string-contains?
    (string      :: <string>,
     pattern     :: <string>,
     match-word? :: <boolean>)
 => (contains?   :: <boolean>)
  block (return)
    for (i from 1)
      let position = subsequence-position(string, pattern, count: i);
      when (position = #f)
	return(#f)
      end;
      unless (match-word?)
	return(#t);
      end;
      local method word-break? (c :: <character>) => (break? :: <boolean>)
	      // Return whether a character is a word-break character
	      //--- cpage: 1998.06.26 Do we have any library functions
	      //           for this type of thing?
	      member?(c, #[' ',',','\'','"','#','(',')','[',']'])
	    end method;
      let end-position = position + size(pattern) - 1;
      let end-string   = size(string) - 1;
      let at-start?    = position = 0;
      let at-end?      = end-position = end-string;
      when ((at-start? | word-break?(string[position - 1]))
	      & (at-end? | word-break?(string[end-position + 1])))
	return(#t);
      end when
    end for
  end block
end function string-contains?;

define method gadget-find-string
    (gadget :: <collection-gadget>, string :: <string>,
     #key start, wrap? = #f, match-case? = #t, match-word? = #f)
//--- cpage: 1998.08.07 Add support for progress-callback if some collections
//           might take a while to search.
 => (index :: false-or(<integer>))
  when (~match-case?)
    string := as-lowercase(string);
  end;
  block (return)
    let items = gadget-items(gadget);
    let start = start | 0;
    for (i from start below size(items))
      let label = gadget-label-key(gadget)(items[i]);
      when (~match-case?)
	label := as-lowercase(label);
      end;
      when (string-contains?(label, string, match-word?))
	return(i)
      end;
    end for;
    when (wrap? & (start > 0))
      gadget-find-string(gadget,
                         string,
                         wrap?: wrap?,
                         match-case?: match-case?,
                         match-word?: match-word?)
    end when
  end block
end method gadget-find-string;

define method gadget-find-previous-string
    (gadget :: <collection-gadget>, string :: <string>,
     #key start, wrap? = #t, match-case? = #t, match-word? = #f)
//--- cpage: 1998.08.07 Add support for progress-callback if some collections
//           might take a while to search.
 => (index :: false-or(<integer>))
  when (~match-case?)
    string := as-lowercase(string);
  end;
  block (return)
    let items = gadget-items(gadget);
    let last-index = size(items) - 1;
    let start = start | last-index;
    for (i from start to 0 by -1)
      let label = gadget-label-key(gadget)(items[i]);
      when (~match-case?)
	label := as-lowercase(label);
      end;
      when (string-contains?(label, string, match-word?))
	return(i)
      end;
    end for;
    when (wrap? & (start < last-index))
      gadget-find-previous-string(gadget,
                                  string,
                                  wrap?: wrap?,
                                  match-case?: match-case?,
                                  match-word?: match-word?)
    end when
  end block
end method gadget-find-previous-string;

define method find-in-sheet
    (gadget        :: <collection-gadget>,
     search-string :: <string>,
     #key from-selection?   :: <boolean>,
          backwards?        :: <boolean>,
          wrap?             :: <boolean>,
          match-case?       :: <boolean>,
          match-word?       :: <boolean>,
          match-regexp?     :: <boolean>,
          progress-callback :: false-or(<function>))
 => (index :: false-or(<integer>))
  ignore(match-regexp?, progress-callback);
  let start = when (from-selection?)
		let old-selection = gadget-selection(gadget);
		unless (empty?(old-selection))
		  let index = old-selection[0];
		  if (backwards?)
		    index - 1
		  else
		    index + 1
		  end if;
		end unless;
	      end when;
  if (backwards?)
    gadget-find-previous-string(gadget,
				search-string,
				start: start,
				wrap?: wrap?,
				match-case?: match-case?,
				match-word?: match-word?)
  else
    gadget-find-string(gadget,
		       search-string,
		       start: start,
		       wrap?: wrap?,
		       match-case?: match-case?,
		       match-word?: match-word?)
  end if
end method find-in-sheet;

define method find-all-in-sheet
    (gadget          :: <collection-gadget>,
     register-object :: <function>,
     search-string   :: <string>,
     #key match-case?       :: <boolean>,
          match-word?       :: <boolean>,
          match-regexp?     :: <boolean>,
          progress-callback :: false-or(<function>))
 => ()
  ignore(match-regexp?, progress-callback);
  let index = 0;
  while (index ~== #f)
    index := gadget-find-string(gadget,
				search-string,
				start: index,
				wrap?: #f,
				match-case?: match-case?,
				match-word?: match-word?);
    index & register-object(index);
  end while;
end find-all-in-sheet;

define method call-in-frame-synchronously
    (frame :: <frame>, function :: <function>, #rest args) => (#rest values)
  if (frame.frame-thread = current-thread())
    apply(function, args)
  else
    let v :: <sequence>
      = with-asynchronous-results (id, timeout: $asynchronous-timeout)
	  call-in-frame(frame, method () => ()
				 let (#rest _v) = apply(function, args);
				 provide-results(id, _v);
			       end);
	end;
    apply(values, v)
  end if
end method call-in-frame-synchronously;

//--- cpage: 1998.09.17 Perhaps this could be changed to apply
//           call-in-frame-synchronously to share code.
define method apply-in-frame-synchronously
    (frame :: <frame>, function :: <function>, arg, #rest args) => (#rest values)
  if (frame.frame-thread = current-thread())
    apply(apply, function, arg, args)
  else
    let v :: <sequence>
      = with-asynchronous-results (id, timeout: $asynchronous-timeout)
	  call-in-frame(frame, method () => ()
				 let (#rest _v) = apply(apply, function, arg, args);
				 provide-results(id, _v);
			       end);
	end;
    apply(values, v)
  end if
end method apply-in-frame-synchronously;

define method sheet-reveal-search-object
    (gadget :: <collection-gadget>, index :: <integer>) => (revealed? :: <boolean>)
  let frame = sheet-frame(gadget);
  when (frame & (index < size(gadget.gadget-items)))
    call-in-frame-synchronously(frame, method () => ()
					 gadget-selection(gadget) := vector(index);
					 note-frame-selection-updated(frame);
				       end);
    #t
  end
end method sheet-reveal-search-object;

define method sheet-search-object-label
    (gadget :: <collection-gadget>, index :: <integer>) => (label :: false-or(<string>))
  let _gadget-label    = gadget.gadget-label;
  let collection-label = _gadget-label & as(<string>, _gadget-label);
  let object-label     = when (index < size(gadget-items(gadget)))
			   let item  = gadget.gadget-items[index];
			   let label = gadget-item-label(gadget, item);
			   label & as(<string>, label)
			 end;
  concatenate(collection-label | "?", ": ", object-label | "?")
end method sheet-search-object-label;

define method sheet-search-object-icon
    (sheet :: <collection-gadget>, index :: <integer>) => (icon :: false-or(<image>))
  ignore(sheet, index);
  //---*** cpage: 1998.08.20 How can we get a gadget item's icon?
  //              There's a 'foo-icon-function' slot for several gadgets,
  //              but there appears to be no generic protocol for getting
  //              a 'gadget-item-icon'.
  #f
end method sheet-search-object-icon;


/// Searching in a frame

define method can-find-in-frame?
    (frame :: <search-target-frame-mixin>) => (can-find? :: <boolean>)
  // Search the sheet with the selection focus
  let sheet = frame-sheet-with-selection(frame);
  sheet & can-find-in-sheet?(sheet)
end method can-find-in-frame?;

define method find-in-frame
    (frame         :: <search-target-frame-mixin>,
     search-string :: <string>,
     #rest keys,
     #key from-selection?   :: <boolean>,
          backwards?        :: <boolean>,
          wrap?             :: <boolean>,
          match-case?       :: <boolean>,
          match-word?       :: <boolean>,
          match-regexp?     :: <boolean>,
          progress-callback :: false-or(<function>))
 => (object :: <object>)
  let sheet  = frame-sheet-with-selection(frame);
  let object = sheet & apply(find-in-sheet, sheet, search-string, keys);
  object & pair(sheet, object)
end method find-in-frame;

define method find-all-in-frame
    (frame           :: <search-target-frame-mixin>,
     register-object :: <function>,
     search-string   :: <string>,
     #rest keys,
     #key match-case?       :: <boolean>,
          match-word?       :: <boolean>,
          match-regexp?     :: <boolean>,
          progress-callback :: false-or(<function>))
 => ()
  let sheet = frame-sheet-with-selection(frame);
  when (sheet)
    local method register-sheet-object (object :: <object>)
	    register-object(pair(sheet, object))
	  end;
    apply(find-all-in-sheet, sheet, register-sheet-object, search-string, keys);
  end when;
end find-all-in-frame;

//---*** cpage: 1998.08.20 Most of this should probably be moved into
//              search-domain-replace-all so it can be reused
//              in more places.
define method replace-all-in-frame
    (frame          :: <search-target-frame-mixin>,
     search-string  :: <string>,
     replace-string :: <string>,
     #rest keys,
     #key from-selection?   :: <boolean>,
          backwards?        :: <boolean>,
          wrap?             :: <boolean>,
          match-case?       :: <boolean>,
          match-word?       :: <boolean>,
          match-regexp?     :: <boolean>,
          progress-callback :: false-or(<function>))
 => (replace-count :: <integer>)
  let replace-count :: <integer> = 0;
  // To prevent an infinite loop, if wrapping, just replace all from the start
  //---*** cpage: 1998.09.17 Ideally, we should add a protocol for getting some kind
  //              of token representing the current selection, then we'd be able to
  //              wrap to that point. Note that a simple fixed index or offset
  //              wouldn't be adequate because replacements may cause the position
  //              to move. Of course, we could just not allow wrapping with
  //              "Replace All", which is what some other environments do, but I
  //              think it's better to keep all search/replace operations as
  //              uniform as possible.
  when (wrap?)
    from-selection? := #f;
  end;
  // First, replace the current selection if replacing from the selection
  when (from-selection?)
    when (replace-in-frame(frame,
			   search-string,
			   replace-string,
			   match-case?:       match-case?,
			   match-word?:       match-word?,
			   match-regexp?:     match-regexp?,
			   progress-callback: progress-callback))
      replace-count := replace-count + 1;
    end when;
  end when;
  // Find the next match, if any
  let object = apply(find-in-frame,
		     frame, search-string, from-selection?: from-selection?, wrap?: #f, keys);
  while (object)
    // Select the found object, then replace it
    let revealed? = frame-reveal-search-object(frame, object);
    debug-assert(revealed?, "Unable to reveal found object %=", object);
    let replacement-object = replace-in-frame(frame,
					      search-string,
					      replace-string,
					      match-case?:       match-case?,
					      match-word?:       match-word?,
					      match-regexp?:     match-regexp?,
					      progress-callback: progress-callback);
    // Reveal the replacement object, if any, so we can continue searching past it
    // (There may not be one if the target turns out to be read-only)
    when (replacement-object)
      revealed? := frame-reveal-search-object(frame, replacement-object);
      debug-assert(revealed?, "Unable to reveal replacement object %=", replacement-object);
      replace-count := replace-count + 1;
    end;
    // Find the next match
    object := apply(find-in-frame, frame, search-string, from-selection?: #t, wrap?: #f, keys);
  end while;
  replace-count
end method replace-all-in-frame;

define method frame-reveal-search-object
    (frame :: <search-target-frame-mixin>, object :: <pair>) => (revealed? :: <boolean>)
  let sheet = object.head;
  when (sheet)
    call-in-frame-synchronously(frame, frame-input-focus-setter, sheet, frame);
    sheet-reveal-search-object(sheet, object.tail)
  end
end method frame-reveal-search-object;

define method frame-search-object-label
    (frame :: <search-target-frame-mixin>, object :: <pair>) => (label :: false-or(<string>))
  let frame-label  = frame.frame-title;
  let sheet        = object.head;
  let object-label = sheet & sheet-search-object-label(sheet, object.tail);
  concatenate(frame-label | "?", ": ", object-label | "?")
end method frame-search-object-label;

define method frame-search-object-icon
    (frame :: <search-target-frame-mixin>, object :: <pair>) => (icon :: false-or(<image>))
  let sheet = object.head;
  (sheet & sheet-search-object-icon(sheet, object.tail)) | frame.frame-icon
end method frame-search-object-icon;


/// <frame-search-domain>: Searches the main text of frames

define open abstract class <frame-search-domain> (<search-domain>)
end class <frame-search-domain>;

// Search domain for all frames
define class <all-frames-search-domain> (<frame-search-domain>)
end class <all-frames-search-domain>;

// Search domain for the "current" frame; the most recently activated frame.
define class <current-frame-search-domain> (<frame-search-domain>)
end class <current-frame-search-domain>;

define constant $current-frame-search-domain = make(<current-frame-search-domain>);

// The label for target kind
define constant $frame-search-domain-kind-label = "window";

// Frame search domain label
define constant $all-frames-search-domain-label = "All Windows";

// Base label for "Current Window" label
define constant $current-frame = "Current Window";

// The label for when there is no current window
define constant $no-current-frame = "(None)";

define method search-domain-label
    (domain :: <all-frames-search-domain>) => (label :: <string>)
  $all-frames-search-domain-label
end method search-domain-label;

define method search-domain-label
    (domain :: <current-frame-search-domain>) => (label :: <string>)
  let frame = current-search-target-frame();
  concatenate($current-frame, " - ",
	      (frame & search-domain-target-label(domain, frame))
		| $no-current-frame)
end method search-domain-label;

define method search-domain-targets
    (domain :: <all-frames-search-domain>) => (targets :: <sequence>)
  ignore(domain);
  let targets = make(<stretchy-vector>);
  do-frames(method (frame :: <frame>)
              when (instance?(frame, <search-target-frame-mixin>))
                add!(targets, frame)
              end
	    end method);
  targets
end method search-domain-targets;

define method search-domain-targets
    (domain :: <current-frame-search-domain>) => (targets :: <sequence>)
  ignore(domain);
  vector(#"current-frame")
end method search-domain-targets;


/// UTILITY CODE COPIED FROM ENVIRONMENT-TOOLS UTILITIES.DYLAN
//---*** cpage: 1998.04.17 We should unify this code and share it
//              between the framework and environment-tools. Of
//              course, we should have a simpler means for getting
//              the short name, without stripping off the suffix.
define variable $frame-title-optional-suffix :: false-or(<string>) = #f;

//---*** andrewa: it would be better to compute this directly, rather
//---*** than stripping off the extra string all of the time.
define method frame-short-title
    (frame :: <frame>) => (title :: <string>)
  let suffix
    = $frame-title-optional-suffix
        | begin
	    let suffix = concatenate(" - ", release-product-name());
	    $frame-title-optional-suffix := suffix
	  end;
  let title = frame-title(frame);
  let title-size = size(title);
  let suffix-size = size($frame-title-optional-suffix);
  let title-includes-suffix?
    = (title-size > suffix-size)
        & ($frame-title-optional-suffix 
	     = copy-sequence(title, start: title-size - suffix-size));
  if (title-includes-suffix?)
    copy-sequence(title, end: title-size - suffix-size)
  else
    title
  end
end method frame-short-title;


define method search-domain-target-label
    (domain :: <frame-search-domain>, target :: <search-target-frame-mixin>)
 => (label :: <string>)
  ignore(domain);
  frame-short-title(target)
end method search-domain-target-label;

define method search-domain-target-label
    (domain :: <current-frame-search-domain>, target == #"current-frame")
 => (label :: <string>)
  ignore(domain, target);
  let frame = current-search-target-frame();
  (frame & search-domain-target-label(domain, frame)) | $no-current-frame
end method search-domain-target-label;

define method search-domain-target-kind-label
    (domain :: <frame-search-domain>, target :: <object>)
 => (label :: <string>)
  ignore(domain, target);
  $frame-search-domain-kind-label
end method search-domain-target-kind-label;

define method search-domain-target-icon
    (domain :: <frame-search-domain>, target :: <search-target-frame-mixin>)
 => (icon :: false-or(<image>))
  ignore(domain);
  target.frame-icon
end method search-domain-target-icon;

define method search-domain-target-icon
    (domain :: <current-frame-search-domain>, target == #"current-frame")
 => (icon :: false-or(<image>))
  ignore(target);
  let frame = current-search-target-frame();
  frame & search-domain-target-icon(domain, frame)
end method search-domain-target-icon;

define method search-domain-target-can-find?
    (domain :: <frame-search-domain>, target :: <search-target-frame-mixin>)
 => (can-find? :: <boolean>)
  ignore(domain);
  can-find-in-frame?(target)
end method search-domain-target-can-find?;

define method search-domain-target-can-find?
    (domain :: <current-frame-search-domain>, target == #"current-frame")
 => (can-find? :: <boolean>)
  ignore(target);
  let frame = current-search-target-frame();
  frame & search-domain-target-can-find?(domain, frame)
end method search-domain-target-can-find?;

define method search-domain-target-can-replace?
    (domain :: <frame-search-domain>, target :: <search-target-frame-mixin>)
 => (can-replace? :: <boolean>)
  ignore(domain);
  can-replace-in-frame?(target)
end method search-domain-target-can-replace?;

define method search-domain-target-can-replace?
    (domain :: <current-frame-search-domain>, target == #"current-frame")
 => (can-find? :: <boolean>)
  ignore(target);
  let frame = current-search-target-frame();
  frame & search-domain-target-can-replace?(domain, frame)
end method search-domain-target-can-replace?;

define method search-domain-find
    (domain        :: <frame-search-domain>,
     target        :: <search-target-frame-mixin>,
     search-string :: <string>,
     #rest keys,
     #key from-selection?   :: <boolean>,
          backwards?        :: <boolean>,
          wrap?             :: <boolean>,
          match-case?       :: <boolean>,
          match-word?       :: <boolean>,
          match-regexp?     :: <boolean>,
          progress-callback :: false-or(<function>))
 => (object :: false-or(<pair>))
  ignore(domain);
  when (can-find-in-frame?(target))
    let object = apply(find-in-frame, target, search-string, keys);
    object & pair(target, object)
  end
end method search-domain-find;

define method search-domain-find
    (domain        :: <current-frame-search-domain>,
     target        == #"current-frame",
     search-string :: <string>,
     #rest keys,
     #key from-selection?   :: <boolean>,
          backwards?        :: <boolean>,
          wrap?             :: <boolean>,
          match-case?       :: <boolean>,
          match-word?       :: <boolean>,
          match-regexp?     :: <boolean>,
          progress-callback :: false-or(<function>))
 => (object :: <object>)
  ignore(target);
  let frame = current-search-target-frame();
  frame & apply(search-domain-find, domain, frame, search-string, keys)
end method search-domain-find;

define method search-domain-find-all
    (domain          :: <frame-search-domain>,
     target          :: <search-target-frame-mixin>,
     register-object :: <function>,
     search-string   :: <string>,
     #rest keys,
     #key match-case?       :: <boolean>,
          match-word?       :: <boolean>,
          match-regexp?     :: <boolean>,
          progress-callback :: false-or(<function>))
 => ()
  ignore(domain);
  when (can-find-in-frame?(target))
    local method register-frame-object (object :: <object>) => ()
            register-object(pair(target, object));
          end;
    apply(find-all-in-frame, target, register-frame-object, search-string, keys);
  end when;
end method search-domain-find-all;

define method search-domain-find-all
    (domain          :: <current-frame-search-domain>,
     target          == #"current-frame",
     register-object :: <function>,
     search-string   :: <string>,
     #rest keys,
     #key match-case?       :: <boolean>,
          match-word?       :: <boolean>,
          match-regexp?     :: <boolean>,
          progress-callback :: false-or(<function>))
 => ()
  ignore(target);
  let frame = current-search-target-frame();
  frame & apply(search-domain-find-all,
		domain, frame, register-object, search-string, keys)
end method search-domain-find-all;

define method search-domain-replace-selection
    (domain         :: <frame-search-domain>,
     target         :: <search-target-frame-mixin>,
     search-string  :: <string>,
     replace-string :: <string>,
     #rest keys,
     #key match-case?       :: <boolean>,
          match-word?       :: <boolean>,
          match-regexp?     :: <boolean>,
          progress-callback :: false-or(<function>))
 => (object :: false-or(<pair>))
  ignore(domain);
  when (can-replace-in-frame?(target))
    let object = apply(replace-in-frame, target, search-string, replace-string, keys);
    object & pair(target, object)
  end
end method search-domain-replace-selection;

define method search-domain-replace-selection
    (domain         :: <current-frame-search-domain>,
     target         == #"current-frame",
     search-string  :: <string>,
     replace-string :: <string>,
     #rest keys,
     #key match-case?       :: <boolean>,
          match-word?       :: <boolean>,
          match-regexp?     :: <boolean>,
          progress-callback :: false-or(<function>))
 => (object :: <object>)
  ignore(target);
  let frame = current-search-target-frame();
  frame & apply(search-domain-replace-selection,
		domain, frame, search-string, replace-string, keys)
end method search-domain-replace-selection;

define method search-domain-replace-all
    (domain          :: <frame-search-domain>,
     target          :: <search-target-frame-mixin>,
     search-string   :: <string>,
     replace-string  :: <string>,
     #rest keys,
     #key from-selection? :: <boolean>,
          backwards?      :: <boolean>,
          wrap?           :: <boolean>,
          match-case?     :: <boolean>,
          match-word?     :: <boolean>,
          match-regexp?   :: <boolean>,
          progress-callback :: false-or(<function>))
 => (replace-count :: <integer>)
  ignore(domain);
  if (can-replace-in-frame?(target))
    apply(replace-all-in-frame, target, search-string, replace-string, keys)
  else
    0
  end
end method search-domain-replace-all;

define method search-domain-replace-all
    (domain         :: <current-frame-search-domain>,
     target         == #"current-frame",
     search-string  :: <string>,
     replace-string :: <string>,
     #rest keys,
     #key from-selection?   :: <boolean>,
          backwards?        :: <boolean>,
          wrap?             :: <boolean>,
          match-case?       :: <boolean>,
          match-word?       :: <boolean>,
          match-regexp?     :: <boolean>,
          progress-callback :: false-or(<function>))
 => (replace-count :: <integer>)
  ignore(target);
  let frame = current-search-target-frame();
  if (frame)
    apply(search-domain-replace-all,
	  domain, frame, search-string, replace-string, keys)
  else
    0
  end
end method search-domain-replace-all;

define method search-domain-reveal-search-object
    (domain :: <frame-search-domain>, object :: <pair>) => (revealed? :: <boolean>)
  let frame = object.head;
  when (frame & frame.frame-state ~= #"destroyed")
    call-in-frame-synchronously(frame, method () => ()
					 deiconify-frame(frame);
					 raise-frame(frame);
				       end);
    frame-reveal-search-object(frame, object.tail)
  end
end method search-domain-reveal-search-object;

define method search-domain-search-object-label
    (domain :: <frame-search-domain>, object :: <sequence>) => (label :: false-or(<string>))
  let frame = first(object);
  when (frame & frame.frame-state ~= #"destroyed")
    let target-object = second(object);
    frame-search-object-label(frame, target-object)
  end
end method search-domain-search-object-label;

define method search-domain-search-object-icon
    (domain :: <frame-search-domain>, object :: <sequence>) => (icon :: false-or(<image>))
  let frame = first(object);
  when (frame & frame.frame-state ~= #"destroyed")
    let target-object = second(object);
    frame-search-object-icon(frame, target-object)
  end
end method search-domain-search-object-icon;


/// The set of registered search domains

define constant $search-domains :: <stretchy-object-vector> 
  = make(<stretchy-object-vector>, size: 1, fill: $current-frame-search-domain);

define method do-search-domains
    (function :: <function>, #key test :: false-or(<function>))
 => (domain :: false-or(<search-domain>))
  block (return)
    for (domain :: <search-domain> in $search-domains)
      if (~test | test(domain))
	function(domain)
      end
    end
  end
end method do-search-domains;

define method register-search-domain (domain :: <search-domain>) => ()
  add-new!($search-domains, domain);
  note-search-domain-registered(domain);
end method register-search-domain;

define method unregister-search-domain (domain :: <search-domain>) => ()
  remove!($search-domains, domain);
  note-search-domain-unregistered(domain);
end method unregister-search-domain;

// Update search frame UI
define function note-search-domains-changed () => ()
  let search-frame = current-search-options-frame();
  //---*** cpage: 1998.08.24 The set of functions for updating gadgets and
  //              search state are a bit convoluted. These names don't
  //              quite jive with how we're using them here. We really need
  //              to reorganize the set of functions and their names.
//  search-frame & note-search-frame-description-changed(search-frame);
  search-frame
    & call-in-frame(search-frame,
		    note-search-frame-current-search-target-frame-changed,
		    search-frame);
end function note-search-domains-changed;

// Update search frame UI to show new domain
define function note-search-domain-registered (domain :: <search-domain>) => ()
  note-search-domains-changed();
end function note-search-domain-registered;

// Select default domain if unregistering the current domain
define function note-search-domain-unregistered (domain :: <search-domain>) => ()
  let description = *current-search-description*;
  when (domain == description.search-description-domain)
    let new-domain = $current-frame-search-domain;
    description.search-description-domain  := new-domain;
    description.search-description-targets := search-domain-targets(new-domain);
  end;
  note-search-domains-changed();
end function note-search-domain-unregistered;

//--- cpage: 1998.07.30 This domain is no longer needed since the
//           introduction of other, more specific, domains. Keep
//           this code around, though. In particular, it may be
//           desirable to let other domains call on this one to
//           handle some of the generic behavior of searching
//           through frames.
/*
define constant $all-frames-search-domain = make(<all-frames-search-domain>);
register-search-domain($all-frames-search-domain);
*/


/// Search Descriptions

define class <search-description> (<object>)
  slot search-description-domain         :: <search-domain>,
    required-init-keyword: domain:;
  slot search-description-targets        :: <sequence>,
    required-init-keyword: targets:;
  slot search-description-search-string  :: <string> = "",
    init-keyword: search-string:;
  slot search-description-replace-string :: <string> = "",
    init-keyword: replace-string:;
  slot search-description-batch?         :: <boolean> = #f,
    init-keyword: batch?:;
  slot search-description-wrap?          :: <boolean> = #f,
    init-keyword: wrap?:;
  slot search-description-boundaries?    :: <boolean> = #f,
    init-keyword: boundaries?:;
  slot search-description-match-case?    :: <boolean> = #f,
    init-keyword: match-case?:;
  slot search-description-match-word?    :: <boolean> = #f,
    init-keyword: match-word?:;
  slot search-description-match-regexp?  :: <boolean> = #f,
    init-keyword: match-regexp?:;
end class <search-description>;

// Maybe this is just me being paranoid, but let's be sure to make
// copies when getting/setting the search description from outside
// this library. We don't want anyone inadvertently changing the
// description behind our back.
define method shallow-copy
    (description :: <search-description>)
 => (description :: <search-description>)
  make(<search-description>,
       domain:         description.search-description-domain,
       targets:        description.search-description-targets,
       search-string:  description.search-description-search-string,
       replace-string: description.search-description-replace-string,
       batch?:         description.search-description-batch?,
       wrap?:          description.search-description-wrap?,
       boundaries?:    description.search-description-boundaries?,
       match-case?:    description.search-description-match-case?,
       match-word?:    description.search-description-match-word?,
       match-regexp?:  description.search-description-match-regexp?)
end method shallow-copy;

define open generic frame-search-description
    (frame :: <frame>) => (description);

define open generic frame-search-description-setter
    (description, frame :: <frame>) => (description);


// Global Search Description

// This is the global search state. There is only one per application.
//--- cpage: 1997.09.29 Eventually, we may add support for persistently
//           storing the search state.

define variable *current-search-description* :: <search-description>
  = make(<search-description>,
         domain:  $current-frame-search-domain,
         targets: search-domain-targets($current-frame-search-domain));
define variable *current-search-target-from-selection?* :: <boolean> = #f;
define variable *current-search-target-index* :: false-or(<integer>) = #f;

define function current-search-description
    ()
 => (description :: <search-description>)
  shallow-copy(*current-search-description*)
end function current-search-description;

define function current-search-description-setter
    (description :: <search-description>)
 => (description :: <search-description>)
  *current-search-description* := shallow-copy(description);
  let search-frame = current-search-options-frame();
  search-frame & note-search-frame-description-changed(search-frame);
  description
end function current-search-description-setter;

define method current-search-target-from-selection? () => (from-selection? :: <boolean>)
  *current-search-target-from-selection?*
end method current-search-target-from-selection?;

define method current-search-target-from-selection?-setter
    (from-selection? :: <boolean>) => (from-selection? :: <boolean>)
  //debug-message("*current-search-target-from-selection?* := %=, was = %=",
  //              from-selection?, *current-search-target-from-selection?*);
  *current-search-target-from-selection?* := from-selection?;
  from-selection?
end method current-search-target-from-selection?-setter;

define method current-search-target-index () => (target-index :: false-or(<integer>))
  *current-search-target-index*
end method current-search-target-index;

define method current-search-target-index-setter
    (target-index :: false-or(<integer>)) => (target-index :: false-or(<integer>))
  //---*** cpage: 1998.07.23 I think perhaps we shouldn't touch
  //              *current-search-target-from-selection* here at all.
  //              We should probably just change it in those places
  //              where it needs to be changed.
  // Reset from-selection? only when the target is reset or is changing,
  // so that setting the target to the current target continues
  // searching from the selection.
  when ((target-index == #f) | (target-index ~== *current-search-target-index*))
    current-search-target-from-selection?() := #f;
  end;
  *current-search-target-index* := target-index;
  let search-frame = current-search-options-frame();
  search-frame & note-search-frame-status-changed(search-frame);
  target-index
end method current-search-target-index-setter;

// A utility function to reset the target index
define function reset-current-search-target-index (domain :: <search-domain>) => ()
  // Reset to 0 for "Current Window", so that we continue searching
  // from the selection in the current target, else reset to #f for
  // all other search domains to start searching from the start of
  // the first target.
  current-search-target-index()
    := when (domain == $current-frame-search-domain) 0 end;
end function reset-current-search-target-index;


/// Search history

// Maximum number of search/replace strings stored.
define constant $max-search-history = 10;

define variable *previous-search-strings*  :: <deque> = make(<deque>);
define variable *previous-replace-strings* :: <deque> = make(<deque>);

define method record-search-string (string :: <string>) => ()
  // Record a copy of the string, not the original
  string := shallow-copy(string);
  // If the element is already in the deque, 'move' it to the front
  remove!(*previous-search-strings*, string, test: \=);
  push(*previous-search-strings*, string);
  while (size(*previous-search-strings*) > $max-search-history)
    pop-last(*previous-search-strings*);
  end;
  let search-frame = current-search-options-frame();
  when (search-frame)
    search-frame.search-frame-previous-search-strings := *previous-search-strings*;
  end;
end method record-search-string;

define method record-replace-string (string :: <string>) => ()
  // Record a copy of the string, not the original
  string := shallow-copy(string);
  // If the element is already in the deque, 'move' it to the front
  remove!(*previous-replace-strings*, string, test: \=);
  push(*previous-replace-strings*, string);
  while (size(*previous-replace-strings*) > $max-search-history)
    pop-last(*previous-replace-strings*);
  end;
  // Update the UI
  let search-frame = current-search-options-frame();
  when (search-frame)
    search-frame.search-frame-previous-replace-strings := *previous-replace-strings*;
  end;
end method record-replace-string;

define function previous-search-strings
    ()
 => (strings :: <sequence>)
  // Copy the <deque>; we mutate it
  shallow-copy(*previous-search-strings*)
end function previous-search-strings;

define function previous-search-strings-setter
    (strings :: <sequence>)
 => (strings :: <sequence>)
  // Make sure the strings are in a <deque>
  *previous-search-strings* := make(<deque>);
  for (i from 0 below $max-search-history)
    *previous-search-strings*[i] := strings[i];
  end;
  // Update the UI
  let search-frame = current-search-options-frame();
  when (search-frame)
    search-frame.search-frame-previous-search-strings := *previous-search-strings*;
  end;
  strings
end function previous-search-strings-setter;

define function previous-replace-strings
    ()
 => (strings :: <sequence>)
  // Copy the <deque>; we mutate it
  shallow-copy(*previous-replace-strings*)
end function previous-replace-strings;

define function previous-replace-strings-setter
    (strings :: <sequence>)
 => (strings :: <sequence>)
  // Make sure the strings are in a <deque>
  *previous-replace-strings* := make(<deque>);
  for (i from 0 below $max-search-history)
    *previous-replace-strings*[i] := strings[i];
  end;
  // Update the UI
  let search-frame = current-search-options-frame();
  when (search-frame)
    search-frame.search-frame-previous-replace-strings := *previous-replace-strings*;
  end;
  strings
end function previous-replace-strings-setter;


/// <frame-search-mixin>: Frames that can issue search commands

define open abstract class <frame-search-mixin> (<frame>)
end class <frame-search-mixin>;

define method initialize (frame :: <frame-search-mixin>, #key) => ()
  next-method();
  note-frame-searching-updated(frame);
end method initialize;

define method frame-search-description
    (frame :: <frame-search-mixin>) => (description :: <search-description>)
  *current-search-description*
end method frame-search-description;

define method frame-search-description-setter
    (description :: <search-description>, frame :: <frame-search-mixin>)
 => (description :: <search-description>)
  *current-search-description* := description;
  let search-frame = current-search-options-frame();
  search-frame & note-search-frame-description-changed(search-frame);
  description
end method frame-search-description-setter;

define method note-frame-searching-updated (frame :: <frame-search-mixin>) => ()
  let has-selection? = ~frame-selection-empty?(frame);
  let can-find?      = frame-can-find?(frame);
  let can-replace?   = frame-can-replace?(frame);
  let description    = frame.frame-search-description;
  let targets        = description.search-description-targets;
  let target-count   = size(targets);
  let wrap?          = description.search-description-wrap?;
  let target-index   = current-search-target-index();
  let can-find-in-next-target?
    = can-find? & (wrap? | ~target-index | target-index < target-count - 1);
  let can-find-in-previous-target?
    = can-find? & (wrap? | ~target-index | target-index > 0);
  
  command-enabled?(<frame-find-next-command>, frame)                 := can-find?;
  command-enabled?(<frame-find-previous-command>, frame)             := can-find?;
  command-enabled?(<frame-find-in-next-target-command>, frame)       := can-find-in-next-target?;
  command-enabled?(<frame-find-in-previous-target-command>, frame)   := can-find-in-previous-target?;
  
  command-enabled?(<frame-copy-selection-to-search-command>, frame)  := has-selection?;
  command-enabled?(<frame-copy-selection-to-replace-command>, frame) := has-selection?;
  command-enabled?(<frame-find-selection-next-command>, frame)       := has-selection? & can-find?;
  command-enabled?(<frame-find-selection-previous-command>, frame)   := has-selection? & can-find?;
  
  command-enabled?(<frame-replace-selection-command>, frame)         := can-replace?;
  command-enabled?(<frame-replace-and-find-next-command>, frame)     := can-replace?;
  command-enabled?(<frame-replace-and-find-previous-command>, frame) := can-replace?;
  command-enabled?(<frame-replace-all-command>, frame)               := can-replace?;
end method note-frame-searching-updated;

define method note-frame-selection-updated (frame :: <frame-search-mixin>) => ()
  next-method();
  note-frame-searching-updated(frame);
end method note-frame-selection-updated;

define method frame-can-find?
    (frame :: <frame-search-mixin>) => (can-find? :: <boolean>)
  let description   = frame.frame-search-description;
  let search-string = description.search-description-search-string;
  // The search string isn't empty, and at least one target can find?
  size(search-string) > 0
    & block (return)
        let domain  = description.search-description-domain;
        let targets = description.search-description-targets;
        for (target in targets)
          when (search-domain-target-can-find?(domain, target))
            return(#t);
          end when;
        end for;
        #f
      end block
end method frame-can-find?;

define method frame-can-replace?
    (frame :: <frame-search-mixin>) => (can-replace? :: <boolean>)
  let description   = frame.frame-search-description;
  let search-string = description.search-description-search-string;
  let batch?        = description.search-description-batch?;
  // The search string isn't empty, and at least one target can replace?
  size(search-string) > 0
    & ~batch?
    & block (return)
        let domain  = description.search-description-domain;
        let targets = description.search-description-targets;
        for (target in targets)
          when (search-domain-target-can-replace?(domain, target))
            return(#t);
          end when;
        end for;
        #f
      end block
end method frame-can-replace?;

// Search failure status messages
define constant <search-result-message> = one-of(#"not-found",
						 #"stopped-between-targets",
						 #"all-targets-searched",
						 #"cannot-replace",
						 #"cannot-replace/no-match",
						 #"cannot-replace/read-only",
						 #"replace-all-completed");
define table $search-result-message-table
  = { #"not-found"                => "Not Found",
      #"stopped-between-targets"  => "Stopped between targets",
      #"all-targets-searched"     => "All targets searched",
      #"cannot-replace"           => "Cannot replace selection",
      // #"cannot-replace" is a catch-all for when there is no further information
      #"cannot-replace/no-match"  => "Selection not a match",
      #"cannot-replace/read-only" => "Selection is read-only",
      #"replace-all-completed"    => "Replaced all matches" };

// Notify the user about the result of a search command
define function frame-notify-search-message
    (frame   :: <frame>, message :: false-or(<search-result-message>)) => ()
  let status-message = frame-search-status-message(frame);
  let message-string = element($search-result-message-table, message, default: "");
  let message-text   = if (size(status-message) > 0)
			 concatenate(status-message, " - ", message-string)
		       else
			 message-string
		       end;
  //---*** cpage: 1998.04.17 Here's some code that will display "Not Found" in
  //              the status bar of both the search target frame and the Find
  //              window. However, the message doesn't go away. I think what
  //              we really need is a DUIM facility for displaying temporary
  //              messages in the status bar, as when selecting a menu item,
  //              but for a timed period, after which it reverts to the
  //              original message.
  let search-frame = current-search-options-frame();
  when (search-frame ~== #f)
    frame-status-message(search-frame) := message-text;
  end;
  //---*** cpage: 1998.08.20 Since we can't get this message to go away,
  //              just eliminate it for now. It's more of a distraction
  //              than it's worth.
  /*
  when (frame ~== search-frame)
    frame-status-message(frame) := message-text;
  end;
  */
  beep(frame);
end function frame-notify-search-message;

define function do-frame-find
    (frame :: <frame-search-mixin>, #key backwards? :: <boolean>) => ()
  let description   = frame.frame-search-description;
  let domain        = description.search-description-domain;
  let targets       = description.search-description-targets;
  let target-count  = size(targets);
  let search-string = description.search-description-search-string;
  let wrap?         = description.search-description-wrap?;
  let target-wrap?  = wrap? & (target-count = 1);
  let boundaries?   = description.search-description-boundaries?;
  let match-case?   = description.search-description-match-case?;
  let match-word?   = description.search-description-match-word?;
  let match-regexp? = description.search-description-match-regexp?;
  let target-step   = if (backwards?) -1 else 1 end;
  let target-index :: <integer>
    = current-search-target-index()
        | if (backwards?) target-count - 1 else 0 end;

  record-search-string(search-string);
  
  //--- cpage: 1997.10.14 Need to add support for progress callback.
  let (object, message :: false-or(<search-result-message>))
    = block (return)
	// Remember where we started
	let start-target-index :: <integer> = target-index;
	let wrapped?           :: <boolean> = #f;
	while (#t)
	  while ((target-index >= 0) & (target-index < target-count))
	    current-search-target-index() := target-index;
	    let object = search-domain-find(domain,
					    targets[target-index],
					    search-string,
					    from-selection?: current-search-target-from-selection?(),
					    backwards?:      backwards?,
					    wrap?:           target-wrap?,
					    match-case?:     match-case?,
					    match-word?:     match-word?,
					    match-regexp?:   match-regexp?);
	    // Stop searching if...
	    when (object           // ...match found
		    | boundaries?) // ...end of target and boundaries in effect
	      // Continue searching from the selection
	      current-search-target-from-selection?() := #t;
	      // Calculate the reason for stopping
	      let message = when (boundaries?)
			      if ((backwards? & (target-index > 0))
				    | (~backwards? & (target-index < target-count - 1)))
				#"stopped-between-targets"
			      else
				#"all-targets-searched"
			      end if
			    end when;
	      return(object, message);
	    end when;
	    when (wrapped?         // ...all targets searched
		    & (target-index = start-target-index))
	      //---*** cpage: 1998.08.11 Do we want to reset when we've wrapped?
	      //reset-current-search-target-index(domain);
	      return(#f, #"all-targets-searched");
	    end when;
	    target-index := target-index + target-step;
	  end while;
	  unless (wrap?)
	    reset-current-search-target-index(domain);
	    return(#f, if (target-count > 1)
			 #"all-targets-searched"
		       else
			 #"not-found"
		       end);
	  end unless;
	  target-index := if (backwards?) target-count - 1 else 0 end;
	  // Continue searching from the start
	  current-search-target-from-selection?() := #f;
	  wrapped? := #t;
	end while;
      end block;
  // Notify the user if the search failed
  unless (object & search-domain-reveal-search-object(domain, object))
    frame-notify-search-message(frame, message);
  end;
end function do-frame-find;

/*--- cpage: 1998.02.06 TBD: This is going to be "batch searching"
define function do-frame-find-all (frame :: <frame-search-mixin>) => ()
  // TBD
end function do-frame-find-all;
*/

// Replace selection; always operates on "Current Window"
define function do-frame-replace-selection (frame :: <frame-search-mixin>) => ()
  let description    = frame.frame-search-description;
  let search-string  = description.search-description-search-string;
  let replace-string = description.search-description-replace-string;
  let match-case?    = description.search-description-match-case?;
  let match-word?    = description.search-description-match-word?;
  let match-regexp?  = description.search-description-match-regexp?;

  record-search-string(search-string);
  record-replace-string(replace-string);
  
  if (search-domain-target-can-replace?($current-frame-search-domain,
					#"current-frame"))
    //--- cpage: 1997.10.14 Need to add support for progress callback.
    let replacement-object
      = search-domain-replace-selection($current-frame-search-domain,
					#"current-frame",
					search-string,
					replace-string,
					match-case?:   match-case?,
					match-word?:   match-word?,
					match-regexp?: match-regexp?);
    if (replacement-object)
      search-domain-reveal-search-object($current-frame-search-domain,
					 replacement-object);
    else
      frame-notify-search-message(frame, #"cannot-replace/no-match");
    end;
  else
    frame-notify-search-message(frame, #"cannot-replace/read-only");
  end if;
end function do-frame-replace-selection;

define function do-frame-replace-all
    (frame :: <frame-search-mixin>, #key backwards? :: <boolean>) => ()
  let description    = frame.frame-search-description;
  let domain         = description.search-description-domain;
  let targets        = description.search-description-targets;
  let target-count   = size(targets);
  let search-string  = description.search-description-search-string;
  let replace-string = description.search-description-replace-string;
  let wrap?          = description.search-description-wrap?;
  let target-wrap?   = wrap? & (target-count = 1);
  let boundaries?    = description.search-description-boundaries?;
  let match-case?    = description.search-description-match-case?;
  let match-word?    = description.search-description-match-word?;
  let match-regexp?  = description.search-description-match-regexp?;
  let target-step    = if (backwards?) -1 else 1 end;
  let target-index :: <integer>
    = current-search-target-index()
        | if (backwards?) target-count - 1 else 0 end;

  record-search-string(search-string);
  record-replace-string(replace-string);
  
  //--- cpage: 1997.10.14 Need to add support for progress callback.
  let message :: false-or(<search-result-message>)
    = block (return)
	// Remember where we started
	let start-target-index :: <integer> = target-index;
	let wrapped?           :: <boolean> = #f;
	while (#t)
	  while ((target-index >= 0) & (target-index < target-count))
	    current-search-target-index() := target-index;
	    let from-selection? = current-search-target-from-selection?();
	    //---*** cpage: 1998.09.17 Prevent infinite recursion when wrap? is #t by
	    //              stopping before we return to the target where we started
	    //              (unlike searching, which will wrap into the starting target).
	    //              Also, always replace from the start if wrap? is #t.
	    //
	    //              Ideally, we should add a protocol for getting some kind
	    //              of token representing the current selection, then we'd be able to
	    //              wrap to that point. Note that a simple fixed index or offset
	    //              wouldn't be adequate because replacements may cause the position
	    //              to move. Of course, we could just not allow wrapping with
	    //              "Replace All", which is what some other environments do, but I
	    //              think it's better to keep all search/replace operations as
	    //              uniform as possible.
	    when (wrapped?         // ...all targets searched
		    & (target-index = start-target-index))
	      //---*** cpage: 1998.08.11 Do we want to reset when we've wrapped?
	      //reset-current-search-target-index(domain);
	      return(#f, #"all-targets-searched");
	    end when;
	    when (wrap?)
	      from-selection? := #f;
	      target-wrap?    := #f;
	    end;
	    let replace-count
	      = search-domain-replace-all(domain,
					  targets[target-index],
					  search-string,
					  replace-string,
					  from-selection?: from-selection?,
					  backwards?:      backwards?,
					  wrap?:           target-wrap?,
					  match-case?:     match-case?,
					  match-word?:     match-word?,
					  match-regexp?:   match-regexp?);
	    // Stop replacing if...
	    when ((replace-count > 1) & boundaries?) // ...replacements made and boundaries in effect
	      // Continue searching from the selection
	      current-search-target-from-selection?() := #t;
	      // Calculate the reason for stopping
	      let message = if ((backwards? & (target-index > 0))
				  | (~backwards? & (target-index < target-count - 1)))
			      #"stopped-between-targets"
			    else
			      #"replace-all-completed"
			    end if;
	      return(message);
	    end when;
	    when (wrapped?    // ...all targets searched
		    & (target-index = start-target-index))
	      //---*** cpage: 1998.08.11 Do we want to reset when we've wrapped?
	      //reset-current-search-target-index(domain);
	      return(#"replace-all-completed");
	    end when;
	    target-index := target-index + target-step;
	  end while;
	  unless (wrap?)
	    reset-current-search-target-index(domain);
	    return(#"replace-all-completed");
	  end unless;
	  target-index := if (backwards?) target-count - 1 else 0 end;
	  // Continue searching from the start
	  current-search-target-from-selection?() := #f;
	  wrapped? := #t;
	end while;
      end block;
  
  // Notify the user that the operation has finished
  frame-notify-search-message(frame, message);
end function do-frame-replace-all;

define method frame-find-next (frame :: <frame-search-mixin>) => ()
  do-frame-find(frame);
end method frame-find-next;

define method frame-find-previous (frame :: <frame-search-mixin>) => ()
  do-frame-find(frame, backwards?: #t);
end method frame-find-previous;

define method frame-find-in-next-target (frame :: <frame-search-mixin>) => ()
  let description  = frame.frame-search-description;
  let targets      = description.search-description-targets;
  let wrap?        = description.search-description-wrap?;
  let target-index = current-search-target-index();
  if (target-index)
    target-index := target-index + 1;
  else
    target-index := 0;
  end;
  let in-range?    = target-index < size(targets);
  when (in-range? | wrap?)
    current-search-target-index() := if (in-range?) target-index else 0 end;
    current-search-target-from-selection?() := #f;
    do-frame-find(frame);
  end when;
end method frame-find-in-next-target;

define method frame-find-in-previous-target (frame :: <frame-search-mixin>) => ()
  let description  = frame.frame-search-description;
  let targets      = description.search-description-targets;
  let wrap?        = description.search-description-wrap?;
  let target-index = current-search-target-index();
  if (target-index)
    target-index := target-index - 1;
  else
    target-index := size(targets) - 1;
  end;
  let in-range?    = target-index >= 0;
  when (in-range? | wrap?)
    current-search-target-index() := if (in-range?) target-index else size(targets) - 1 end;
    current-search-target-from-selection?() := #f;
    do-frame-find(frame, backwards?: #t);
  end when;
end method frame-find-in-previous-target;

define method frame-copy-selection-to-search (frame :: <frame-search-mixin>) => ()
  let string = frame-selected-text(frame);
  when (string)
    frame.frame-search-description.search-description-search-string := string;
    let search-frame = current-search-options-frame();
    search-frame & note-search-frame-description-changed(search-frame);
  end
end method frame-copy-selection-to-search;

define method frame-copy-selection-to-replace (frame :: <frame-search-mixin>) => ()
  let string = frame-selected-text(frame);
  when (string)
    frame.frame-search-description.search-description-replace-string := string;
    let search-frame = current-search-options-frame();
    search-frame & note-search-frame-description-changed(search-frame);
  end
end method frame-copy-selection-to-replace;

define method frame-find-selection-next (frame :: <frame-search-mixin>) => ()
  frame-copy-selection-to-search(frame);
  frame-find-next(frame);
end method frame-find-selection-next;

define method frame-find-selection-previous (frame :: <frame-search-mixin>) => ()
  frame-copy-selection-to-search(frame);
  frame-find-previous(frame);
end method frame-find-selection-previous;

define method frame-replace-selection (frame :: <frame-search-mixin>) => ()
  do-frame-replace-selection(frame);
end method frame-replace-selection;

define method frame-replace-and-find-next (frame :: <frame-search-mixin>) => ()
  do-frame-replace-selection(frame);
  do-frame-find(frame);
end method frame-replace-and-find-next;

define method frame-replace-and-find-previous (frame :: <frame-search-mixin>) => ()
  do-frame-replace-selection(frame);
  do-frame-find(frame, backwards?: #t);
end method frame-replace-and-find-previous;

define method frame-replace-all (frame :: <frame-search-mixin>) => ()
  do-frame-replace-all(frame);
end method frame-replace-all;

/*---*** Not currently used
define method frame-replace-all-previous (frame :: <frame-search-mixin>) => ()
  do-frame-replace-all(frame, backwards?: #t);
end method frame-replace-all-previous;
*/


/// <search-frame>: The "Find" window

define constant $find-title                      = "Find/Replace";
define constant $find-options-title              = "Find/Replace...";
define constant $find-next-title                 = "Find Next";
define constant $find-previous-title             = "Find Previous";
define constant $find-in-next-target-title       = "Find in Next Target";
define constant $find-in-previous-target-title   = "Find in Previous Target";
define constant $enter-find-string-title         = "Enter 'Find' String";
define constant $enter-replace-string-title      = "Enter 'Replace' String";
define constant $find-selection-next-title       = "Find Selection";
define constant $find-selection-previous-title   = "Find Selection Previous";
define constant $replace-title                   = "Replace";
define constant $replace-and-find-next-title     = "Replace && Find Next";
define constant $replace-and-find-previous-title = "Replace && Find Previous";
define constant $replace-all-title               = "Replace All";

//--- hughg, 1998/09/11: Will need to be called via call-in-frame if
//--- it's ever used other than during the initialize method for
//--- <search-frame>, as there might be a race-condition between threads.
define generic note-search-frame-items-changed         (frame :: <frame>) => ();
//--- hughg, 1998/09/11: Normally called via call-in-frame.
define generic note-search-frame-current-search-target-frame-changed
                                                       (frame :: <frame>) => ();
define generic note-search-frame-gadget-values-changed (frame :: <frame>) => ();
define generic note-search-frame-description-changed   (frame :: <frame>) => ();
define generic note-search-frame-status-changed        (frame :: <frame>) => ();

define method search-frame-gadget-value-changed-callback (gadget) => ()
  let frame = sheet-frame(gadget);
  note-search-frame-gadget-values-changed(frame);
end method search-frame-gadget-value-changed-callback;

define open frame <search-frame>
  (<frame-search-mixin>,
   <frame-reuse-mixin>,
   <simple-frame>)
  keyword title:         = "Find/Replace";
  keyword width:         = 400;
  keyword fixed-height?: = #t;
  keyword reusable?:     = #t;
  slot search-frame-previous-search-strings :: <sequence> = #[],
    setter:       %search-frame-previous-search-strings-setter,
    init-keyword: previous-searches:;
  slot search-frame-previous-replace-strings :: <sequence> = #[],
    setter:       %search-frame-previous-replace-strings-setter,
    init-keyword: previous-replaces:;
  sealed slot %search-backwards? :: <boolean> = #f;
  pane %find-button (frame)
    make(<button>,
         min-width: 80,
         label: $find-next-title,
	 accelerator: make-keyboard-gesture(#"f", #"control"),
         activate-callback: method (gadget)
                              let frame = sheet-frame(gadget);
                              if (frame.%search-backwards?)
                                frame-find-previous(frame);
                              else
                                frame-find-next(frame);
                              end;
                            end);
  pane %replace-button (frame)
    make(<button>,
         label: $replace-title,
	 accelerator: make-keyboard-gesture(#"h", #"control"),
         activate-callback: method (gadget)
                              frame-replace-selection(sheet-frame(gadget));
                            end);
  pane %replace-and-find-button (frame)
    make(<button>,
         label: $replace-and-find-next-title,
	// mnemonic: 'i',  //TESTING - 'i' ends up selecting space, for some reason
	 accelerator: make-keyboard-gesture(#"j", #"control"),
         activate-callback: method (gadget)
                              let frame = sheet-frame(gadget);
                              if (frame.%search-backwards?)
                                frame-replace-and-find-previous(frame);
                              else
                                frame-replace-and-find-next(frame);
                              end;
                            end);
  pane %replace-all-button (frame)
    make(<button>,
         label: $replace-all-title,
         activate-callback: method (gadget)
                              frame-replace-all(sheet-frame(gadget));
                            end);
//--- cpage: 1997.09.18 To be implemented.
/*
  pane %stop-button (frame)
    make(<button>,
         label: "&Stop",
         activate-callback: method (gadget)
                              ignore(gadget)
                            end);
*/
  pane %search-text-pane (frame)
    make(<combo-box>,
         value-changing-callback: search-frame-gadget-value-changed-callback,
         value-changed-callback: search-frame-gadget-value-changed-callback,
         activate-callback: method (gadget)
                              let frame = sheet-frame(gadget);
                              if (frame.%search-backwards?)
                                frame-find-previous(frame);
                              else
                                frame-find-next(frame);
                              end;
                            end);
  pane %replace-text-pane (frame)
    make(<combo-box>,
         value-changing-callback: search-frame-gadget-value-changed-callback,
         value-changed-callback: search-frame-gadget-value-changed-callback,
         activate-callback: method (gadget)
                              let frame = sheet-frame(gadget);
                              if (frame.%search-backwards?)
                                frame-find-previous(frame);
                              else
                                frame-find-next(frame);
                              end;
                            end);
  pane %backwards?-button (frame)
    make(<check-button>,
         label: "Bac&kwards",
         value-changed-callback:
           method (gadget)
             let frame      = sheet-frame(gadget);
             let backwards? = gadget.gadget-value;
             frame.%search-backwards? := backwards?;
           end method);
  pane %batch?-button (frame)
    make(<check-button>,
         label: "&Batch",
         value-changed-callback: search-frame-gadget-value-changed-callback);
  pane %wrap?-button (frame)
    make(<check-button>,
         label: "Wra&p",
         value-changed-callback: search-frame-gadget-value-changed-callback);
  pane %boundaries?-button (frame)
    make(<check-button>,
         label: "S&top between targets",
         value-changed-callback: search-frame-gadget-value-changed-callback);
  pane %match-word?-button (frame)
    make(<check-button>,
         label: "Match &whole words",
         value-changed-callback: search-frame-gadget-value-changed-callback);
  pane %match-case?-button (frame)
    make(<check-button>,
         label: "Match &case",
         value-changed-callback: search-frame-gadget-value-changed-callback);
  pane %match-regexp?-button (frame)
    make(<check-button>,
         label: "Match regular e&xpression",
         value-changed-callback: search-frame-gadget-value-changed-callback);
  pane %look-in-pane (frame)
    make(<option-box>,
         label-key: search-domain-label,
         value-changed-callback: search-frame-gadget-value-changed-callback);
  pane %main-layout (frame)
    horizontally (x-spacing: 8)
      vertically (y-spacing: 8)
        make(<table-layout>,
             spacing: 8,
	     y-alignment: #"center",
             contents:
               vector(vector(make(<label>, label: "&Find:"),
                             frame.%search-text-pane),
                      vector(make(<label>, label: "&Replace:"),
                             frame.%replace-text-pane),
                      vector(make(<null-pane>),
                             make(<table-layout>,
                                  columns: 2,
				  x-spacing: 15,
                                  equalize-widths?: #t,
                                  contents:
                                    vector(vector(frame.%match-word?-button, frame.%backwards?-button),
					   vector(frame.%match-case?-button, frame.%wrap?-button),
			                   vector(#f, frame.%boundaries?-button)))),
		                        //---*** cpage: 1998.07.22 Not yet implemented.
				        // vector(frame.%match-regexp?-button, frame.%batch?-button)
		      vector(make(<null-pane>, max-height: $fill),
                             make(<null-pane>, max-height: $fill)),
                      vector(make(<label>, label: "&Look in:"),
                             frame.%look-in-pane)));
      end;
      vertically (equalize-widths?: #t, y-spacing: 2)
        frame.%find-button;
        frame.%replace-button;
        frame.%replace-and-find-button;
        frame.%replace-all-button;
//---*** cpage: To be implemented.
//        frame.%stop-button;
      end;
    end;
  layout     (frame) frame.%main-layout;
  status-bar (frame) make(<status-bar>);
end frame <search-frame>;

define method initialize
    (frame :: <search-frame>, #key search-description :: <search-description>) => ()
  next-method();
  frame.frame-input-focus        := frame.%search-text-pane;
  frame.frame-default-button     := frame.%find-button;
  frame.frame-search-description := search-description;
  note-search-frame-items-changed(frame);
  note-search-frame-description-changed(frame);
end method initialize;

define method reinitialize-frame
    (frame :: <search-frame>, #key) => ()
  next-method();
  frame.frame-input-focus := frame.%search-text-pane;
end method reinitialize-frame;

define method search-frame-previous-search-strings-setter
    (search-strings :: <sequence>, frame :: <search-frame>)
 => (search-strings :: <sequence>)
  frame.%search-frame-previous-search-strings := search-strings;
  // ---*** hughg, 1997/01/22: HACK to work around bug on \= for <deque>s
  // gadget-items(frame.%search-text-pane) := search-strings;
  gadget-items(frame.%search-text-pane) := as(<vector>, search-strings);
  search-strings
end method search-frame-previous-search-strings-setter;

define method search-frame-previous-replace-strings-setter
    (replace-strings :: <sequence>, frame :: <search-frame>)
 => (replace-strings :: <sequence>)
  frame.%search-frame-previous-replace-strings := replace-strings;
  // ---*** hughg, 1997/01/22: HACK to work around bug on \= for <deque>s
  // gadget-items(frame.%replace-text-pane) := replace-strings;
  gadget-items(frame.%replace-text-pane) := as(<vector>, replace-strings);
  replace-strings
end method search-frame-previous-replace-strings-setter;

define method note-search-frame-gadget-values-changed
    (frame :: <search-frame>) => ()
  let description    = frame.frame-search-description;
  let domain         = gadget-value(frame.%look-in-pane);
  let search-string  = gadget-value(frame.%search-text-pane);
  let replace-string = gadget-value(frame.%replace-text-pane);
  let batch?         = gadget-value(frame.%batch?-button);
  let wrap?          = gadget-value(frame.%wrap?-button);
  let boundaries?    = gadget-value(frame.%boundaries?-button);
  let match-case?    = gadget-value(frame.%match-case?-button);
  let match-word?    = gadget-value(frame.%match-word?-button);
  let match-regexp?  = gadget-value(frame.%match-regexp?-button);
  
  // Update search description from gadget values
  unless (description.search-description-domain == domain)
    description.search-description-domain  := domain;
    //---*** cpage: 1997.10.29 Temporarily, we always select all the
    //              targets. Eventually, we'll add UI to select targets.
    description.search-description-targets := search-domain-targets(domain);
  end;
  
  description.search-description-search-string  := search-string;
  description.search-description-replace-string := replace-string;
  description.search-description-batch?         := batch?;
  description.search-description-wrap?          := wrap?;
  description.search-description-boundaries?    := boundaries?;
  description.search-description-match-case?    := match-case?;
  description.search-description-match-word?    := match-word?;
  description.search-description-match-regexp?  := match-regexp?;
  
  note-search-frame-description-changed(frame);
  note-search-frame-status-changed(frame);
end method note-search-frame-gadget-values-changed;

//---*** cpage: 1997.07.24 I'm not sure why this function exists, or is
//              named the way it is. It is currently only really used for
//              initialization. It used to also update the "Look in:"
//              gadget, but I moved that elsewhere, so that we could
//              update the gadget label for #"current-frame" without
//              resetting the search state, since it only affects the
//              search state if #"current-frame" is the current
//              "Look in:" setting.
define method note-search-frame-items-changed
    (frame :: <search-frame>) => ()
  let description = frame.frame-search-description;
  let targets     = description.search-description-targets;
  let batch?      = description.search-description-batch?;

  // Update gadget items

  // ---*** hughg, 1997/01/22: HACK to work around bug on \= for <deque>s
  // gadget-items(frame.%search-text-pane)  := frame.search-frame-previous-search-strings;
  // gadget-items(frame.%replace-text-pane) := frame.search-frame-previous-replace-strings;
  gadget-items(frame.%search-text-pane)  := as(<vector>, frame.search-frame-previous-search-strings);
  gadget-items(frame.%replace-text-pane) := as(<vector>, frame.search-frame-previous-replace-strings);
  
  //--- hughg, 1998/09/11: This call is intentionally *not* done with
  //--- call-in-frame, because the enclosing methos is only ever called
  //--- during the initialize method for the <search-frame>.  If that
  //--- ever changes, we probably need to do the whole lot via call-in-frame.
  note-search-frame-current-search-target-frame-changed(frame);
end method note-search-frame-items-changed;

define method note-search-frame-current-search-target-frame-changed
    (frame :: <search-frame>) => ()
  let description = frame.frame-search-description;
  let targets     = description.search-description-targets;
  let batch?      = description.search-description-batch?;

  // Update gadgets

  let look-in-gadget = frame.%look-in-pane;
  gadget-items(look-in-gadget) := copy-sequence($search-domains);
  gadget-value(look-in-gadget) := description.search-description-domain;
  update-gadget(look-in-gadget);
  search-frame-update-gadgets(frame);

//---*** cpage: 1998.08.11 Do we need to do any of this anymore?
//              I've split out the gadget updating into search-frame-update-gadgets
//              so that we can update them without resetting the search state.
/*
  // If "Current Window" is selected, update other gadgets
  when (description.search-description-domain == $current-frame-search-domain)
    //---*** cpage: 1998.07.30 Temporary HACK to prevent resetting the
    //              search state when the user activates a new frame.
    //              I really need to reorganize these note/update functions.
    when (size(description.search-description-targets) > 0)
      current-search-target-index() := 0;
      current-search-target-from-selection?() := #t;
    end;
  end;
*/
end method note-search-frame-current-search-target-frame-changed;

define method search-frame-update-gadgets (frame :: <search-frame>) => ()
  let description    = frame.frame-search-description;
  let domain         = description.search-description-domain;
  let targets        = description.search-description-targets;
  let search-string  = description.search-description-search-string;
  let replace-string = description.search-description-replace-string;
  let batch?         = description.search-description-batch?;
  let wrap?          = description.search-description-wrap?;
  let boundaries?    = description.search-description-boundaries?;
  let match-case?    = description.search-description-match-case?;
  let match-word?    = description.search-description-match-word?;
  let match-regexp?  = description.search-description-match-regexp?;

  // Update gadget values

  gadget-value(frame.%search-text-pane)     := search-string;
  gadget-value(frame.%replace-text-pane)    := replace-string;
  gadget-value(frame.%batch?-button)        := batch?;
  gadget-value(frame.%wrap?-button)         := wrap?;
  gadget-value(frame.%boundaries?-button)   := boundaries?;
  gadget-value(frame.%match-case?-button)   := match-case?;
  gadget-value(frame.%match-word?-button)   := match-word?;
  gadget-value(frame.%match-regexp?-button) := match-regexp?;

  // Update gadget enabling

  let can-find?    = frame-can-find?(frame);
  let can-replace? = frame-can-replace?(frame);

  gadget-enabled?(frame.%find-button)             := can-find?;
  gadget-enabled?(frame.%replace-and-find-button) := can-replace?;
  gadget-enabled?(frame.%replace-all-button)      := can-replace?;
  //--- cpage: 1997.09.18 To be implemented.
  //  gadget-enabled?(frame.%stop-button)             := #f;
  
  // The "Replace" button always replaces the selection in the current
  // target frame, regardless of the currently selected search domain.
  gadget-enabled?(frame.%replace-button)
    := (size(search-string) > 0)
         & search-domain-target-can-replace?
             ($current-frame-search-domain, #"current-frame");
end method search-frame-update-gadgets;

define method note-search-frame-description-changed (frame :: <search-frame>) => ()
  let description = frame.frame-search-description;
  let domain      = description.search-description-domain;

  // Reset multiple-target searching
  reset-current-search-target-index(domain);

  search-frame-update-gadgets(frame);
  note-frame-searching-updated(frame);
end method note-search-frame-description-changed;

define method note-search-frame-status-changed (frame :: <search-frame>) => ()
  frame-status-message(frame) := frame-search-status-message(frame);
end method note-search-frame-status-changed;

define method frame-search-status-message
    (frame :: <frame-search-mixin>) => (message :: <string>)
  let index = current-search-target-index();
  if (index)
    let description = frame.frame-search-description;
    let domain      = description.search-description-domain;
    let target      = description.search-description-targets[index];
    format-to-string("Looking in %s '%s'%s",
		     search-domain-target-kind-label(domain, target),
		     search-domain-target-label(domain, target),
		     if (current-search-target-from-selection?())
		       ""
		     else
		       " from start/end"
		     end)
  else
    ""
  end if
end method frame-search-status-message;


/// Current search frame

define method frame-search-frame-class
    (frame :: <frame-search-mixin>) => (class :: <class>)
  <search-frame>
end method frame-search-frame-class;

define method frame-edit-search-options (frame :: <frame-search-mixin>) => ()
  ensure-environment-frame(frame,
                           frame-search-frame-class(frame),
                           search-description: *current-search-description*,
                           previous-searches:  *previous-search-strings*,
                           previous-replaces:  *previous-replace-strings*);
end method frame-edit-search-options;

define function current-search-options-frame () => (frame :: false-or(<search-frame>))
  block (return)
    do-frames(method (frame :: <frame>)
                when (instance?(frame, <search-frame>))
                  return(frame)
                end
              end method);
    #f
  end block
end function current-search-options-frame;


/// Searching command tables

define constant $find-doc             = "Searches for and replaces text or objects.";
define variable $find-bitmap          :: <label-type> = "Find";

define constant $find-next-doc        = "Searches for the next match.";
define variable $find-next-bitmap     :: <label-type> = "+";

define constant $find-previous-doc    = "Searches for the previous match.";
define variable $find-previous-bitmap :: <label-type> = "-";

define constant $find-in-next-target-doc
                                      = "Searches in the next file.";
define constant $find-in-previous-target-doc
                                      = "Searches in the previous file.";
define constant $copy-to-search-doc   = "Copies the selection to the search text.";
define constant $copy-to-replace-doc  = "Copies the selection to the replacement text.";
define constant $find-selection-next-doc
                                      = "Copies the selection to the search text"
                                        " and searches for the next match.";
define constant $find-selection-previous-doc
                                      = "Copies the selection to the search text"
                                        " and searches for the previous match.";

define constant $replace-doc          = "Replaces the match.";

define constant $replace-and-find-next-doc
                                      = "Replaces the match and searches for"
                                        " the next match.";
define variable $replace-and-find-next-bitmap :: <label-type>
                                      = "Replace && Find";

define constant $replace-and-find-previous-doc
                                      = "Replaces the match and searches for"
                                        " the previous match.";
define constant $replace-all-doc      = "Searches for and replaces all matches.";


define method make-search-tool-bar-buttons
    (frame :: <frame-search-mixin>) => (buttons :: <sequence>)
  vector(make(<button>, 
              label: $find-bitmap,
              documentation: $find-title,
              command: frame-edit-search-options,
              activate-callback: method (sheet)
                                   frame-edit-search-options(sheet-frame(sheet))
                                 end),
         make(<button>, 
              label: $find-next-bitmap,
              documentation: $find-next-title,
              command: frame-find-next,
              activate-callback: method (sheet)
                                   frame-find-next(sheet-frame(sheet))
                                 end),
         make(<button>, 
              label: $find-previous-bitmap,
              documentation: $find-previous-title,
              command: frame-find-next,
              activate-callback: method (sheet)
                                   frame-find-previous(sheet-frame(sheet))
                                 end))
end method make-search-tool-bar-buttons;

define command-table *searching-command-table* (*global-command-table*)
  menu-item $find-options-title = <frame-edit-search-options-command>,
    accelerator:   make-keyboard-gesture(#"f3"),
    documentation: $find-doc;
  menu-item $find-next-title = <frame-find-next-command>,
    accelerator:   make-keyboard-gesture(#"f", #"control"),
    documentation: $find-next-doc;
  menu-item $find-previous-title = <frame-find-previous-command>,
    accelerator:   make-keyboard-gesture(#"f", #"shift", #"control"),
    documentation: $find-previous-doc;
  menu-item $find-in-next-target-title = <frame-find-in-next-target-command>,
    accelerator:   make-keyboard-gesture(#"d", #"control"),
    documentation: $find-in-next-target-doc;
  menu-item $find-in-previous-target-title = <frame-find-in-previous-target-command>,
    accelerator:   make-keyboard-gesture(#"d", #"shift", #"control"),
    documentation: $find-in-previous-target-doc;
  menu-item $enter-find-string-title = <frame-copy-selection-to-search-command>,
    accelerator:   make-keyboard-gesture(#"e", #"control"),
    documentation: $copy-to-search-doc;
  menu-item $enter-replace-string-title = <frame-copy-selection-to-replace-command>,
    accelerator:   make-keyboard-gesture(#"e", #"shift", #"control"),
    documentation: $copy-to-replace-doc;
  menu-item $find-selection-next-title = <frame-find-selection-next-command>,
    accelerator:   make-keyboard-gesture(#"w", #"control"),
    documentation: $find-selection-next-doc;
  menu-item $find-selection-previous-title = <frame-find-selection-previous-command>,
    accelerator:   make-keyboard-gesture(#"w", #"shift", #"control"),
    documentation: $find-selection-previous-doc;
  menu-item $replace-title = <frame-replace-selection-command>,
    accelerator:   make-keyboard-gesture(#"h", #"control"),
    documentation: $replace-doc;
  menu-item $replace-and-find-next-title = <frame-replace-and-find-next-command>,
    accelerator:   make-keyboard-gesture(#"j", #"control"),
    documentation: $replace-and-find-next-doc;
  menu-item $replace-and-find-previous-title = <frame-replace-and-find-previous-command>,
    accelerator:   make-keyboard-gesture(#"j", #"shift", #"control"),
    documentation: $replace-and-find-previous-doc;
  menu-item $replace-all-title = <frame-replace-all-command>,
    documentation: $replace-all-doc;
end command-table *searching-command-table*;
