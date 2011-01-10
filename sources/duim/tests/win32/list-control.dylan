Module:    win32-duim-gui-test-suite
Author:    Andy Armstrong, Scott McKay
Synopsis:  An interactive test-suite for Win32 DUIM
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Bitmaps within list controls

define frame <list-control-bitmaps> (<simple-frame>)
  pane %list-control (frame)
    make(<list-control>,
	 items: #[1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
	 icon-function:
	   method (x)
	     let bitmap = if (odd?(x)) $prompt-bitmap else $values-bitmap end;
	     values(bitmap, bitmap)
	   end method);
  layout (frame)
    frame.%list-control;
end frame <list-control-bitmaps>;

// Install the test
install-test(<list-control-bitmaps>, "List control bitmaps");


/// Icons within list controls

define frame <list-control-icons> (<simple-frame>)
  pane %list-control (frame)
    make(<list-control>,
	 items: #[1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
	 icon-function:
	   method (x)
	     let icon = if (odd?(x)) $cut-icon else $paste-icon end;
	     values(icon, icon)
	   end method);
  layout (frame)
    frame.%list-control;
end frame <list-control-icons>;

// Install the test
install-test(<list-control-icons>, "List control icons");


/// Bitmaps within tree controls

define frame <tree-control-bitmaps> (<simple-frame>)
  pane %tree-control (frame)
    make(<tree-control>,
	 roots: #[1],
	 children-generator:
	   method (x) vector(x * 2, 1 + x * 2) end,
	 icon-function:
	   method (x)
	     let bitmap = if (odd?(x)) $prompt-bitmap else $values-bitmap end;
	     values(bitmap, bitmap)
	   end method);
  layout (frame)
    frame.%tree-control;
end frame <tree-control-bitmaps>;

// Install the test
install-test(<tree-control-bitmaps>, "Tree control bitmaps");


/// Icons within tree controls

define frame <tree-control-icons> (<simple-frame>)
  pane %tree-control (frame)
    make(<tree-control>,
	 roots: #[1],
	 children-generator:
	   method (x) vector(x * 2, 1 + x * 2) end,
	 icon-function:
	   method (x)
	     let icon = if (odd?(x)) $cut-icon else $paste-icon end;
	     values(icon, icon)
	   end method);
  layout (frame)
    frame.%tree-control;
end frame <tree-control-icons>;

// Install the test
install-test(<tree-control-icons>, "Tree control icons");
