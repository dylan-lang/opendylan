Module:       Dylan-User
Synopsis:     DUIM back-end for Macintosh
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Implementation module
define module carbon-duim
  use dylan;

  use duim-imports;
  use duim-internals;
  use duim-gadget-panes-internals;	//---*** until we've got all native gadgets in

  use C-FFI;
  use carbon-interface,			//---*** temporary
    rename: { <Cursor>  => carbon/<Cursor>,
	      <Pattern> => carbon/<Pattern>,
	      <PixMap>  => carbon/<PixMap>,
	      <Point>   => carbon/<Point>,
	      <Region>  => carbon/<Region> };

  // Basic classes
  export <carbon-port>,
         <carbon-medium>,
         <carbon-frame-manager>;

  // Gadget creation protocols
  export <carbon-mirror>,
         <widget-mirror>,
         <gadget-mirror>,
         make-carbon-mirror,
         install-event-handlers,
         install-named-handlers,
         update-mirror-attributes,
         set-mirror-parent,
         move-mirror,
         size-mirror;

  // Other possibly useful exports
  export shutdown-carbon-duim;
end module carbon-duim;
