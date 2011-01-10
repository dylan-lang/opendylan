Module:       duim-sheets-internals
Synopsis:     DUIM sheets
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Carets

define protocol <<caret-protocol>> ()
  function make-caret
    (port :: <abstract-port>, sheet :: <abstract-sheet>, #key x, y, width, height)
 => (caret :: <caret>);
  getter caret-sheet
    (caret :: <caret>) => (sheet :: false-or(<abstract-sheet>));
  function caret-position
    (caret :: <caret>) => (x :: <integer>, y :: <integer>);
  function set-caret-position
    (caret :: <caret>, x :: <integer>, y :: <integer>, #key fast?) => ();
  function do-set-caret-position
    (caret :: <caret>, x :: <integer>, y :: <integer>) => ();
  function caret-size
    (caret :: <caret>) => (width :: <integer>, height :: <integer>);
  function set-caret-size
    (caret :: <caret>, width :: <integer>, height :: <integer>) => ();
  function do-set-caret-size
    (caret :: <caret>, width :: <integer>, height :: <integer>) => ();
  getter caret-visible?
    (caret :: <caret>) => (visible? :: <boolean>);
  setter caret-visible?-setter
    (visible?, caret :: <caret>, #key tooltip?) => (visible?);
  function do-show-caret
    (caret :: <caret>, #key tooltip?) => ();
  function do-hide-caret
    (caret :: <caret>, #key tooltip?) => ();
end protocol <<caret-protocol>>;


define open abstract primary class <basic-caret> (<caret>)
  sealed slot port :: false-or(<port>) = #f,
    init-keyword: port:,
    setter: %port-setter;
  sealed slot caret-sheet :: false-or(<sheet>) = #f,
    init-keyword: sheet:;
  sealed slot %x-position :: <integer> = 0,
    init-keyword: x:;
  sealed slot %y-position :: <integer> = 0,
    init-keyword: y:;
  sealed slot %width  :: <integer> = 0,
    init-keyword: width:;
  sealed slot %height :: <integer> = 0,
    init-keyword: height:;
  // Incremented when we hide the caret, 0 => visible
  sealed slot %hide-count :: <integer> = 0;
end class <basic-caret>;

define method display
    (caret :: <basic-caret>) => (display :: false-or(<display>))
  let sheet = caret-sheet(caret);
  sheet & display(sheet)
end method display;


// Returns X and Y in the coordinate system of the caret's sheet
define sealed inline method caret-position
    (caret :: <basic-caret>) => (x :: <integer>, y :: <integer>)
  values(caret.%x-position, caret.%y-position)
end method caret-position;

define sealed method set-caret-position
    (caret :: <basic-caret>, x :: <integer>, y :: <integer>,
     #key fast? = #f) => ()
  unless (caret.%x-position   = x
          & caret.%y-position = y)
    unless (fast?)		// do no work if we're trying to be fast
      do-set-caret-position(caret, x, y)
    end;
    caret.%x-position := x;
    caret.%y-position := y
  end
end method set-caret-position;


// Returns X and Y in the coordinate system of the caret's sheet
define sealed inline method caret-size
    (caret :: <basic-caret>) => (width :: <integer>, height :: <integer>)
  values(caret.%width, caret.%height)
end method caret-size;

define sealed method set-caret-size
    (caret :: <basic-caret>, width :: <integer>, height :: <integer>) => ()
  unless (caret.%width    = width
          & caret.%height = height)
    do-set-caret-size(caret, width, height);
    caret.%width  := width;
    caret.%height := height
  end
end method set-caret-size;


define sealed inline method caret-visible?
    (caret :: <basic-caret>) => (visible? :: <boolean>)
  caret.%hide-count = 0
end method caret-visible?;

define sealed method caret-visible?-setter
    (visible? :: <boolean>, caret :: <basic-caret>, #key tooltip?)
 => (visible? :: <boolean>)
  if (visible?)
    unless (caret.%hide-count = 0)	// don't let it go negative
      dec!(caret.%hide-count)
    end;
    do-show-caret(caret, tooltip?: tooltip?)
  else
    inc!(caret.%hide-count);
    do-hide-caret(caret, tooltip?: tooltip?)
  end;
  visible?
end method caret-visible?-setter;


/// Carets and sheets

define macro with-caret-position-saved
  { with-caret-position-saved (?sheet:expression)
      ?:body
    end }
    => { begin
	   let _caret = sheet-caret(?sheet);
	   let (_x, _y) = caret?(_caret) & caret-position(_caret);
	   block ()
	     ?body
	   cleanup
	     when (caret?(_caret))
	       set-caret-position(_caret, _x, _y)
	     end;
	   end
         end }
end macro with-caret-position-saved;

define macro with-caret-hidden
  { with-caret-hidden (?sheet:expression)
      ?:body
    end }
    => { begin
	   let _caret = sheet-caret(?sheet);
	   block ()
	     when (caret?(_caret))
	       caret-visible?(_caret, tooltip?: #t) := #f
	     end;
	     ?body
	   cleanup
	     when (caret?(_caret))
	       caret-visible?(_caret, tooltip?: #t) := #t
	     end;
	   end
         end }
end macro with-caret-hidden;
