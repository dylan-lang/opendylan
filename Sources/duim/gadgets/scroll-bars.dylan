Module:       duim-gadgets-internals
Synopsis:     DUIM gadgets
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Scroll bars

define protocol <<scrolling-protocol>> ()
  function scroll-down-line   (gadget :: <abstract-gadget>) => ();
  function scroll-up-line     (gadget :: <abstract-gadget>) => ();
  function scroll-down-page   (gadget :: <abstract-gadget>) => ();
  function scroll-up-page     (gadget :: <abstract-gadget>) => ();
  function scroll-to-position (gadget :: <abstract-gadget>, position) => ();
  function note-scroll-bar-changed (scroll-bar :: <scroll-bar>) => ();
end protocol <<scrolling-protocol>>;

// Scroll bar definition
define open abstract class <scroll-bar>
  (<oriented-gadget-mixin>,
   <slug-gadget-mixin>,
   <changing-value-gadget-mixin>,
   <basic-value-gadget>)
  sealed slot %scrolling-sheets :: <list> = #();
  keyword value-changing-callback: = scroll-attached-sheets;
  keyword value-changed-callback:  = scroll-attached-sheets;
end class <scroll-bar>;

define method attach-scroll-bar
    (sheet :: <scrolling-sheet-mixin>, scroll-bar :: <scroll-bar>) => ()
  scroll-bar.%scrolling-sheets := add-new!(scroll-bar.%scrolling-sheets, sheet)
end method attach-scroll-bar;

define method detach-scroll-bar 
    (sheet :: <scrolling-sheet-mixin>, scroll-bar :: <scroll-bar>) => ()
  scroll-bar.%scrolling-sheets := remove!(scroll-bar.%scrolling-sheets, sheet)
end method detach-scroll-bar;


/// The rest of <scrolling-sheet-mixin>...

define method sheet-horizontal-scroll-bar-setter
    (scroll-bar :: false-or(<scroll-bar>), sheet :: <scrolling-sheet-mixin>)
 => (scroll-bar :: false-or(<scroll-bar>))
  let old-scroll-bar = sheet-horizontal-scroll-bar(sheet);
  unless (scroll-bar == old-scroll-bar)
    old-scroll-bar & detach-scroll-bar(sheet, old-scroll-bar);
    sheet.%horizontal-scroll-bar := scroll-bar;
    scroll-bar & attach-scroll-bar(sheet, scroll-bar)
  end;
  scroll-bar
end method sheet-horizontal-scroll-bar-setter;

define method sheet-vertical-scroll-bar-setter
    (scroll-bar :: false-or(<scroll-bar>), sheet :: <scrolling-sheet-mixin>)
 => (scroll-bar :: false-or(<scroll-bar>))
  let old-scroll-bar = sheet-vertical-scroll-bar(sheet);
  unless (scroll-bar == old-scroll-bar)
    old-scroll-bar & detach-scroll-bar(sheet, old-scroll-bar);
    sheet.%vertical-scroll-bar := scroll-bar;
    scroll-bar & attach-scroll-bar(sheet, scroll-bar)
  end;
  scroll-bar
end method sheet-vertical-scroll-bar-setter;


/// Scroll bar updating

define sealed method update-scroll-bar
    (scroll-bar :: <scroll-bar>,
     contents-start :: <integer>, contents-end :: <integer>,
     visible-start :: <integer>, visible-end :: <integer>) => ()
  let (scroll-range, scroll-start, scroll-end, increment)
    = range-values(gadget-value-range(scroll-bar));
  ignore(scroll-range);
  let new-value = visible-start;
  let new-size  = visible-end - visible-start + increment;
  let new-range
    = if (scroll-start = contents-start
	  & scroll-end = contents-end)
	gadget-value-range(scroll-bar)
      else
	range(from: contents-start, to: contents-end)
      end;
  // We do this "the internal way" to avoid flashing the scroll bar
  // in the back end, which would happen if we individually updated
  // the range, then the value, then the slug size
  when (  gadget-value(scroll-bar)       ~= new-value
	| gadget-slug-size(scroll-bar)   ~= new-size
	| gadget-value-range(scroll-bar) ~= new-range)
    scroll-bar.%value       := new-value;
    scroll-bar.%slug-size   := new-size;
    scroll-bar.%value-range := new-range;
    note-scroll-bar-changed(scroll-bar)
  end
end method update-scroll-bar;

define method note-scroll-bar-changed
    (gadget :: <scroll-bar>) => ()
  #f
end method note-scroll-bar-changed;

define method scroll-attached-sheets
    (scroll-bar :: <scroll-bar>) => ()
  for (sheet in scroll-bar.%scrolling-sheets)
    let _size  = gadget-slug-size(scroll-bar);
    let _start = gadget-value(scroll-bar);
    let _end   = _start + _size;
    let (vleft, vtop, vright, vbottom) = sheet-visible-range(sheet);
    select (gadget-orientation(scroll-bar))
      #"vertical" =>
	set-sheet-visible-range(sheet, vleft, _start, vright, _end);
      #"horizontal" =>
	set-sheet-visible-range(sheet, _start, vtop, _end, vbottom);
    end
  end
end method scroll-attached-sheets;

define method line-scroll-amount
    (scroll-bar :: <scroll-bar>, orientation :: <gadget-orientation>)
 => (amount :: <integer>)
  let sheets = scroll-bar.%scrolling-sheets;
  case
    ~empty?(sheets) =>
      let amount :: <integer> = $maximum-integer;
      for (sheet in sheets)
	let sheet-amount = line-scroll-amount(sheet, orientation);
	amount := min(amount, sheet-amount)
      end;
      amount;
    otherwise =>
      // Scroll bar is not attached through a scroller,
      // so see if we can find a plausible client sheet
      let client = gadget-client(scroll-bar);
      if (sheet?(client) & ~instance?(client, <scroller>))
	line-scroll-amount(client, orientation)
      else
	max(floor(gadget-value-increment(scroll-bar) * 3), 10)
      end;
  end
end method line-scroll-amount;

define method page-scroll-amount 
    (scroll-bar :: <scroll-bar>, orientation :: <gadget-orientation>)
 => (amount :: <integer>)
  let sheets = scroll-bar.%scrolling-sheets;
  case
    ~empty?(sheets) =>
      let amount :: <integer> = $maximum-integer;
      for (sheet in sheets)
	let sheet-amount = page-scroll-amount(sheet, orientation);
	amount := min(amount, sheet-amount)
      end;
      amount;
    otherwise =>
      let client = gadget-client(scroll-bar);
      if (sheet?(client) & ~instance?(client, <scroller>))
	page-scroll-amount(client, orientation)
      else
	floor(gadget-slug-size(scroll-bar))
      end;
  end
end method page-scroll-amount;

define method scroll-by-pixels 
    (scroll-bar :: <scroll-bar>, pixels :: <integer>) => ()
  let old-value = gadget-value(scroll-bar);
  let increment = gadget-value-increment(scroll-bar);
  let new-pixel-position
    = floor/(old-value, if (increment = 0) 1 else increment end) + pixels;
  let new-value
    = select (increment by instance?)
	<integer> => floor(new-pixel-position * increment);
	<real>    => new-pixel-position * increment;
      end;
  gadget-value(scroll-bar, do-callback?: #t) := new-value
end method scroll-by-pixels;

define method scroll-up-line (scroll-bar :: <scroll-bar>) => ()
  scroll-by-pixels(scroll-bar, -gadget-line-scroll-amount(scroll-bar))
end method scroll-up-line;

define method scroll-down-line (scroll-bar :: <scroll-bar>) => ()
  scroll-by-pixels(scroll-bar, gadget-line-scroll-amount(scroll-bar))
end method scroll-down-line;

define method scroll-up-page (scroll-bar :: <scroll-bar>) => ()
  scroll-by-pixels(scroll-bar, -gadget-page-scroll-amount(scroll-bar))
end method scroll-up-page;

define method scroll-down-page (scroll-bar :: <scroll-bar>) => ()
  scroll-by-pixels(scroll-bar, gadget-page-scroll-amount(scroll-bar))
end method scroll-down-page;

define method scroll-to-position
    (scroll-bar :: <scroll-bar>, value :: <real>) => ()
  gadget-value(scroll-bar, do-callback?: #t) := value
end method scroll-to-position;

define method scroll-to-position
    (scroll-bar :: <scroll-bar>, value == #"top") => ()
  gadget-value(scroll-bar, do-callback?: #t) := gadget-start-value(scroll-bar)
end method scroll-to-position;

define method scroll-to-position 
    (scroll-bar :: <scroll-bar>, value == #"bottom") => ()
  let size = gadget-slug-size(scroll-bar);
  gadget-value(scroll-bar, do-callback?: #t)
    := gadget-end-value(scroll-bar) - size
end method scroll-to-position;
