Module:    DUIM-OLE-Control
Synopsis:  Visual display of an OLE Control using DUIM.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Eventually this whole file should be moved to the `DUIM-OLE-Server' 
// library as part of supporting in-process servers.  In the meantime,
// the `sideways' adjective was added to the following methods to suppress
// warnings about ``method ... visible to sibling libraries''.

define sideways method OLE-part-draw (obj :: <basic-DUIM-OLE-server>,
				      hDC :: <HDC>, rect :: <LPRECTL>)
 => (status :: <HRESULT>);

  debug-out("OLE-part-draw left = %d, top = %d; right = %d, bottom = %d\n",
	    rect.left-value, rect.top-value,
	    rect.right-value, rect.bottom-value);

  let sheet = obj.get-doc.doc-sheet;

  unless ( duim/sheet-handles-repaint?(sheet) )
    // DUIM doesn't know how to draw this; gray out the rectangle
    // as a place-holder.
    let brush = pointer-cast(<HBRUSH>, w/GetStockObject(w/$DKGRAY-BRUSH));
    w/FillRect(hDC, rect, brush);
  end;

  let region = duim/sheet-region(sheet);
  let ( sheet-width, sheet-height ) = duim/box-size(region);
  let mx :: <real> =
    compute-scale(rect.right-value - rect.left-value, sheet-width);
  let my :: <real> =
    compute-scale(rect.bottom-value - rect.top-value, sheet-height);
  unless ( mx = 1 & my = 1 )
    debug-out("  scaling by %d%% x, %d%% y\n",
	      round(mx * 100), round(my * 100));
  end unless;
  duim/repaint-in-dc-recursive(sheet, hDC, mx, my,
			       rect.left-value, rect.top-value);
  $S-OK
end OLE-part-draw;

define function compute-scale (requested-size :: <integer>,
			       duim-size :: <integer> )
			 => (scale :: <real>);
  let scale :: <single-float> =
    as(<single-float>, requested-size) / as(<single-float>, duim-size);
  let abs-scale :: <single-float> = abs(scale);
  if ( abs-scale > 0.99 & abs-scale < 1.05 )
    if ( negative?(scale) ) -1 else 1 end if
  else
    scale
  end if
end compute-scale;
