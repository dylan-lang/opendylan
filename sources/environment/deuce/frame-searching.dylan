Module:    environment-deuce
Synopsis:  Searching Editor Frames
Author:    Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Searching editor frames; search the frame's window

define method can-find-in-frame?
    (frame :: <environment-editor>) => (can-find? :: <boolean>)
  let window = frame-window(frame);
  (window & can-find-in-window?(window)) & #t
end method can-find-in-frame?;

define method can-replace-in-frame?
    (frame :: <environment-editor>) => (can-replace? :: <boolean>)
  let window = frame-window(frame);
  (window & can-replace-in-window?(window)) & #t
end method can-replace-in-frame?;

//---*** cpage: 1998.08.07 This method is left over from the old
//              editor-specific searching protocols. Delete eventually.
/// BEGIN DELETE
define sealed method frame-search-string-found?-setter
    (found? :: false-or(<buffer>), frame :: <environment-editor>)
 => (found? :: false-or(<buffer>))
  next-method();
  note-frame-searching-updated(frame);
  found?
end method frame-search-string-found?-setter;
/// END DELETE

define method find-in-frame
    (frame         :: <environment-editor>,
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
  let window :: <basic-window> = frame-window(frame);
  apply(find-in-window, window, search-string, keys)
end method find-in-frame;

define method replace-in-frame
    (frame          :: <environment-editor>,
     search-string  :: <string>,
     replace-string :: <string>,
     #rest keys,
     #key match-case?       :: <boolean>,
          match-word?       :: <boolean>,
          match-regexp?     :: <boolean>,
          progress-callback :: false-or(<function>))
 => (object :: <object>)
  let window :: <basic-window> = frame-window(frame);
  apply(replace-in-window, window, search-string, replace-string, keys)
end method replace-in-frame;

// Reveal and select matched/replaced text
//---*** cpage: 1998.07.29 Eventually this should be made more robust in the
//              face of user changes to text and buffers so that revealing a
//              match from a batch search fails gracefully and returns #f.
define method frame-reveal-search-object
    (frame :: <environment-editor>, object :: <object>) => (revealed? :: <boolean>)
  //--- cpage: 1998.08.20 Delete this flag eventually, as it is only
  //           used by older searching code written before the
  //           integration with environment-framework searching.
  frame-search-string-found?(frame) := frame-buffer(frame);
  // Note that we don't store the window with the object; we always reuse the
  // frame's current window.
  let window :: <basic-window> = frame-window(frame);
  window-reveal-search-object(window, object)
end method frame-reveal-search-object;
