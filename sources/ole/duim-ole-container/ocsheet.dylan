Module:    DUIM-OLE-Container
Synopsis:  mixin class for sheets that can contain an OLE embedded object.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define open abstract free class <container-sheet-mixin>
    ( duim/<abstract-sheet> )
  sealed slot sheet-embedded-gadgets :: <list> = #();
end <container-sheet-mixin>;

define method duim/note-child-added (parent :: <container-sheet-mixin>,
				     child :: <ole-gadget>) => ()
  parent.sheet-embedded-gadgets := add!(parent.sheet-embedded-gadgets, child);
  next-method();
end method duim/note-child-added;

define method duim/note-child-removed (parent :: <container-sheet-mixin>,
				       child :: <ole-gadget>) => ()
  parent.sheet-embedded-gadgets :=
  remove!(parent.sheet-embedded-gadgets, child);
  next-method();
end method duim/note-child-removed;

// Clicking outside of an OLE object deactivates it.
define method duim/handle-button-event
    (sheet :: <container-sheet-mixin>, 
     event :: duim/<button-press-event>, 
     button == duim/$left-button) => ()
  for ( gadget :: <ole-gadget> in sheet.sheet-embedded-gadgets )
    let obj = gadget.sheet-contained-object;
    if ( obj )
      container-UI-deactivate(obj); 
    end if;
  end for;
  next-method();
end method duim/handle-button-event;

// Double clicking on an OLE object activates it.
define method duim/handle-button-event
    (sheet :: <container-sheet-mixin>, 
     event :: duim/<double-click-event>, 
     button == duim/$left-button) => ()
  block(return)
    for ( gadget :: <ole-gadget> in sheet.sheet-embedded-gadgets )
      let obj = gadget.sheet-contained-object;
      if ( obj )
	let x :: <integer> = event.duim/event-x;
	let y :: <integer> = event.duim/event-y;
	let (left :: <integer>, top :: <integer>,
	     right :: <integer>, bottom :: <integer>)
	  = duim/sheet-edges(gadget);
	if ( x > left & x < right & y > top & y < bottom )
	  unless ( obj.document-in-place-active? )
            // deactivate any others
            container-UI-deactivate(obj.document-application);
          end unless;
	  let status = 
	    with-stack-structure ( pmsg :: w/<LPMSG> )
	      pmsg.w/hwnd-value := duim/window-handle(sheet);
	      pmsg.w/message-value := w/$WM-LBUTTONDBLCLK;
	      pmsg.w/wParam-value := 0;
	      pmsg.w/lParam-value := w/MAKELONG(x,y);
	      container-do-verb(obj, $OLEIVERB-PRIMARY, pmsg);
	    end with-stack-structure;
	  if ( SUCCEEDED?(status) )
	    return();
	  end if;
	end if;
      end if;
    end for;
    next-method();
  end block;
end method duim/handle-button-event;
