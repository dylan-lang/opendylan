Module:   employee-explorer
Synopsis: A tree explorer
Author:   Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// A utility macro missing from DUIM.
define macro split-horizontally
  { split-horizontally (#rest ?options:expression) ?children end }
    => { make(<row-splitter>, children: vector(?children), ?options) }
children:
  { }
    => { }
  { ?child:expression; ... }
    => { ?child, ... }
end macro;

define constant $boss-icon 
  = read-image-as(<win32-icon>, "BOSS", #"icon", width: 16, height: 16);
define constant $serf-icon 
  = read-image-as(<win32-icon>, "SERF", #"icon", width: 16, height: 16);

define frame <explorer-frame> (<simple-frame>)
  pane frame-file-menu (frame)
    make(<menu>, label: "File",
         children: vector(make(<menu-button>,
                               label: "Close",
                               activate-callback:
                                 method (sheet)
                                   exit-frame(sheet-frame(sheet))
                                 end)));
  menu-bar (frame)
    make(<menu-bar>, children: vector(frame-file-menu(frame)));  
  pane frame-tree (frame)
    make(<tree-control>, 
         roots: #[],
         children-generator:     subordinates,
         children-predicate:     subordinates?,
         icon-function:          employee-icon,
         always-show-selection?: #t,
         label-key:              full-name,
         value-changed-callback:
           method (gadget :: <tree-control>)
             note-frame-selection-changed(frame, gadget-value(gadget));
           end);
  pane frame-table (frame)
    make(<table-control>, 
         items: #[],
         headings:   
           #["Last Name", "First Name", "Extension", "Reports To"], 
         generators: 
           vector(last-name, first-name, extension, compose(full-name, boss)),
         activate-callback:
           method (gadget :: <table-control>) => ()
             note-frame-selection-changed(frame, gadget-value(gadget));
           end);
  pane frame-subordinates-label (frame)
    make(<label>, label: "", max-width: $fill);
  layout (frame)
    split-horizontally (ratios: #[1, 3])
      vertically (spacing: 2)
        with-border (type: #"sunken")
          make(<label>, label: "Employee hierarchy", max-width: $fill)
        end;
        frame-tree(frame);
      end;
      vertically (spacing: 2)
        with-border (type: #"sunken")
          frame-subordinates-label(frame)
        end;
        frame-table(frame);
      end;
    end;
  status-bar (frame)
    make(<status-bar>);
  keyword title: = "Employee Explorer";
  keyword width:  = 700;
  keyword height: = 400;
end frame;

define method note-frame-selection-changed 
    (frame :: <explorer-frame>, item :: <object>) => ()
  let subs = subordinates(item);
  gadget-value(frame-tree(frame)) := item;
  gadget-items(frame-table(frame)) := subs;
  gadget-label(frame-status-bar(frame)) 
    := format-to-string("%d subordinate(s)", size(subs));
  gadget-label(frame-subordinates-label(frame)) 
    := format-to-string("Direct subordinates of '%s'", full-name(item));
end method;

define method handle-event 
    (frame :: <simple-frame>, event :: <frame-mapped-event>) => ()
  // Install the data and send out a fake change notification after initial
  // display to get all the panes in sync.
  tree-control-roots(frame-tree(frame)) := bosses();
  note-frame-selection-changed(frame, gadget-value(frame-tree(frame)))
end method;

define method employee-icon (employee :: <employee>) => (icon)
  if (subordinates?(employee))
    $boss-icon
  else
    $serf-icon
  end;
end method;

// eof

