Module:    environment-framework
Synopsis:  Environment Framework
Author:    Andy Armstrong, Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Bar options (tool bar, status bar etc)

define command-table *bar-options-command-table* (*global-command-table*)
end command-table *bar-options-command-table*;

add-command-table-menu-item
  (*bar-options-command-table*,
   "", <check-box>, vector(#"tool-bar", #"status-bar"),
   items: #[#["Toolbar", #"tool-bar"], #["Status Bar", #"status-bar"]],
   label-key: first, value-key: second,
   callback: method (menu-box)
               frame-show-bars?(sheet-frame(menu-box), gadget-value(menu-box))
             end);

define method frame-show-bars?
    (frame :: <frame>, bars :: <sequence>) => ()
  let top-sheet   = top-level-sheet(frame);
  let tool-bar    = frame-tool-bar(frame);
  let status-bar  = frame-status-bar(frame);
  let tool-bar?   = member?(#"tool-bar",   bars);
  let status-bar? = member?(#"status-bar", bars);
  let relayout?   = #f;
  local method show-or-hide (sheet, present?) => ()
	  // Work extra hard to ensure that everything gets re-layed out,
	  // since bars can have associated "decorations"
	  when (sheet & sheet-withdrawn?(sheet) == present?)
	    sheet-withdrawn?(sheet) := ~present?;
	    for (s = sheet then sheet-parent(s),
		 until: s == top-sheet)
	      sheet-layed-out?(s) := #f
	    end;
	    relayout? := #t
	  end
	end method;
  show-or-hide(tool-bar,   tool-bar?);
  show-or-hide(status-bar, status-bar?);
  when (relayout?)
    relayout-children(top-sheet);
    relayout-parent(tool-bar | status-bar);
    sheet-mapped?(tool-bar)   := tool-bar?;
    sheet-mapped?(status-bar) := status-bar?;
  end
end method frame-show-bars?;


/// Basic View menu items

define open generic frame-edit-options
    (frame :: <frame>) => ();

define method frame-edit-options (frame :: <frame>) => ()
  //--- 'frame-undefined-callback(frame)' not implement at this level...
  notify-user("Not yet implemented!", owner: frame)
end method frame-edit-options;

define command-table *view-refresh-command-table* (*global-command-table*)
  menu-item "Refresh" = refresh-frame,
    accelerator:   make-keyboard-gesture(#"f5"),
    documentation: "Updates the display to reflect any changes.";
end command-table *view-refresh-command-table*;

define command-table *view-options-command-table* (*global-command-table*)
  menu-item "Options..." = frame-edit-options,
    documentation: "Enables you to change application options.";
end command-table *view-options-command-table*;

define command-table *basic-view-command-table* (*global-command-table*)
  include *view-refresh-command-table*;
  include *view-options-command-table*;
end command-table *basic-view-command-table*;



// View menu

define command-table *view-command-table* (*global-command-table*)
  include *bar-options-command-table*;
  include *basic-view-command-table*;
end command-table *view-command-table*;
