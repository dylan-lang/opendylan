Module:    Dylan-User
Synopsis:  DUIM back-end for Microsoft Windows
Author:    David Gray, Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module win32-duim
  use dylan;

  use duim-imports;
  use duim-internals;
  use duim-gadget-panes-internals;      //---*** until we've got all native gadgets in

  use C-FFI;
  use win32-core;
  use dylan-extensions,
   import: { <abstract-integer>, <big-integer>,
            <double-integer>, %double-integer-low, %double-integer-high };

  export check-result,
         report-error,
         ensure-no-error,
         windows-debug-message;

  // Basic classes
  export <win32-port>,
         <win32-medium>,
         <win32-frame-manager>;

  // Fonts
  export <win32-font>;

  // Bitmaps and icons
  export <win32-image>,
         <win32-bitmap>,
         <win32-icon>,
         image-handle;

  // These can be used when the user wants to use the Win32 API directly
  // on a window that was created through DUIM
  export get-DC, release-DC,
         window-handle;

  // These can be used by someone who wants to import their own Win32 gadget
  export <native-color>,
         <win32-mirror>,
         <window-mirror>,
         <win32-pane-mixin>,
         <win32-gadget-mixin>,
         <win32-subclassed-gadget-mixin>,
         <win32-control-mixin>,
         color->native-color, native-color->color, native-brush->color,
         $default-face-color, $default-shadow-color, $default-highlight-color,
         make-gadget-control,
         handle-message,
         handle-command,
         handle-command-for-id,
         handle-notify,
         handle-control-message,
         handle-scrolling,
         handle-button,
         initialize-sheet-from-resource,
         update-gadget-image,
         update-mirror-label;

  // Exports for messing with Help systems
  export <winhelp>,
         <htmlhelp>;

  // Keyboard handling
  export virtual-key->keysym,
         keysym->virtual-key,
         virtual-key->character,
         character->virtual-key;

  // Exports for OLE-DUIM, possibly useful for others too
  export make-win32-menu,
         repaint-sheet-with-DC,
         repaint-in-DC-recursive,
         accelerator-table,
         make-text-style-from-font,
         mirror-registered-dialogs,
         update-frame-documentation,
         window-mirror,
         note-win32-frame-destroyed,
         shutdown-win32-duim;

  // Utilities
  export get-window-edges,
         get-window-size,
         get-client-edges,
         get-client-size;
end module win32-duim;
