Module:    environment-deuce
Synopsis:  Searching Editor Gadget
Author:    Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Searching editor gadgets; the gadget is an editor window

define method can-find-in-sheet?
    (window :: <environment-deuce-gadget>) => (can-find? :: <boolean>)
  can-find-in-window?(window)
end method can-find-in-sheet?;

define method can-replace-in-sheet?
    (window :: <environment-deuce-gadget>) => (can-replace? :: <boolean>)
  can-replace-in-window?(window)
end method can-replace-in-sheet?;

define method find-in-sheet
    (window        :: <environment-deuce-gadget>,
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
  apply(find-in-window, window, search-string, keys)
end method find-in-sheet;

define method replace-in-sheet
    (window         :: <environment-deuce-gadget>,
     search-string  :: <string>,
     replace-string :: <string>,
     #rest keys,
     #key match-case?       :: <boolean>,
          match-word?       :: <boolean>,
          match-regexp?     :: <boolean>,
          progress-callback :: false-or(<function>))
 => (object :: <object>)
  apply(replace-in-window, window, search-string, replace-string, keys)
end method replace-in-sheet;

define method sheet-reveal-search-object
    (window :: <environment-deuce-gadget>, object :: <object>) => (revealed? :: <boolean>)
  window-reveal-search-object(window, object)
end method sheet-reveal-search-object;
