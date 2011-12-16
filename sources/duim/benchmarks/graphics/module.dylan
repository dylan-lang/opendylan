Module:    Dylan-User
Author:    Andy Armstrong, Shri Amit
Synopsis:  Interactive benchmarks for DUIM graphics
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module duim-graphics-benchmarks
  use common-dylan;
  use simple-random;
  use threads;

  use duim;
  use duim-extended-geometry;
  //--- It would be nice not to need to do this...
  use duim-internals,
    import: { \with-abort-restart,
	      $default-text-style,
	      <basic-gadget>,
	      <oriented-gadget-mixin>,
	      collection-gadget-default-label-key,
	      <basic-frame>,
	      do-command-menu-gadgets,
	      do-copy-area,
              duim-debug-message,

              // Scrolling
	      <scrolling-sheet-mixin>,
	      update-scroll-bars,
	      line-scroll-amount,
	      page-scroll-amount,
	      sheet-scroll-range,
              sheet-visible-range, set-sheet-visible-range };

  // The start up function
  export start-benchmarks
end module duim-graphics-benchmarks;
