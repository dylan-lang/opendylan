Module:    Dylan-User
Synopsis:  DUIM back-end for Motif
Author:    Scott McKay, Stuart Croy
	   Based on work by John Aspinall and Richard Billington
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Implementation module
define module motif-duim
  use dylan;

  use duim-imports;
  use duim-internals;
  use duim-gadget-panes-internals;	//---*** until we've got all native gadgets in

  use C-FFI;
  use Xlib,  prefix: "x/";
  use Xt,    prefix: "xt/";
  use Motif, prefix: "xm/";

  // Basic classes
  export <motif-port>,
         <motif-medium>,
         <motif-frame-manager>;

  // Fonts
  export <motif-font>;

  // Bitmaps and icons
  export <motif-bitmap>,
	 <motif-icon>;

  // These can be used by someone who wants to import their own Motif gadget
  export <motif-mirror>,
	 <window-mirror>,
         install-event-handlers;

  export <motif-pane-mixin>,
	 <motif-gadget-mixin>,
	 make-motif-widget,
	 initialize-gadget-mirror,
	 initialize-gadget-callbacks;

  export color->native-color, native-color->color;

  // Other possibly useful exports
  export make-motif-menu,
	 make-text-style-from-font,
	 shutdown-motif-duim;
end module motif-duim;
