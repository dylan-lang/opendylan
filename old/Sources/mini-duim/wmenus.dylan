Module:    win32-duim
Synopsis:  Win32 menus implementation
Author:    Andy Armstrong, David Gray, Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// Win32 menus

define sealed class <menu-mirror> (<win32-mirror>)
  slot mirror-used-mnemonics = make(<stretchy-vector>);
  slot mirror-next-index = 0;
end class <menu-mirror>;

define method map-mirror
    (_port :: <win32-port>, sheet :: <sheet>, mirror :: <menu-mirror>) => ()
  //--- Anything we can do here?
  #f
end method map-mirror;

define method unmap-mirror
    (_port :: <win32-port>, sheet :: <sheet>, mirror :: <menu-mirror>) => ()
  //--- Anything we can do here?
  #f
end method unmap-mirror;

define method destroy-mirror 
    (_port :: <win32-port>, sheet :: <sheet>, mirror :: <menu-mirror>) => ()
  //--- Anything else to do here?
  sheet-direct-mirror(sheet) := #f
end method destroy-mirror;

define method mirror-edges
    (_port :: <win32-port>, sheet :: <sheet>, mirror :: <menu-mirror>)
 => (left :: <integer>, top :: <integer>,
     right :: <integer>, bottom :: <integer>)
  //---*** Should we do something better?
  values(0, 0, 100, 100)
end method mirror-edges;

define method set-mirror-edges
    (_port :: <win32-port>, sheet :: <sheet>, mirror :: <menu-mirror>,
     left :: <integer>, top :: <integer>,
     right :: <integer>, bottom :: <integer>)
 => ()
  //---*** Should we do something?
  #f
end method set-mirror-edges;


/// Mnemonic handling

define table $mnemonic-table :: <table>
  // This is the set defined by WIG, Appendix B, Table B.5, page 441
  #"About"                => "&About";
  #"Always on Top"        => "Always on &Top";
  #"Apply"                => "&Apply";
  #"Back"                 => "&Back";
  #"Browse"               => "&Browse";
  #"Close"                => "&Close";
  #"Copy"                 => "&Copy";
  #"Copy Here"            => "&Copy Here";
  #"Create Shortcut"      => "Create &Shortcut";
  #"Create Shortcut Here" => "Create &Shortcut Here";
  #"Cut"                  => "Cu&t";
  #"Delete"               => "&Delete";
  #"Edit"                 => "&Edit";
  #"Exit"                 => "E&xit";
  #"Explore"              => "&Explore";
  #"File"                 => "&File";
  #"Find"                 => "&Find";
  #"Help"                 => "&Help";
  #"Help Topics"          => "Help &Topics";
  #"Hide"                 => "&Hide";
  #"Insert"               => "&Insert";
  #"Insert Object"        => "Insert &Object";
  #"Link Here"            => "&Link Here";
  #"Maximize"             => "Ma&ximize";
  #"Minimize"             => "Mi&nimize";
  #"Move"                 => "&Move";
  #"Move Here"            => "&Move Here";
  #"New"                  => "&New";
  #"Next"                 => "&Next";
  #"No"                   => "&No";
  #"Open"                 => "&Open";
  #"Open With"            => "Open &With";
  #"Paste"                => "&Paste";
  #"Paste Link"           => "Paste &Link";
  #"Paste Shortcut"       => "Paste &Shortcut";
  #"Page Setup"           => "Page Set&up";
  #"Paste Special"        => "Paste &Special";
  #"Pause"                => "&Pause";
  #"Play"                 => "&Play";
  #"Print"                => "&Print";
  #"Print Here"           => "&Print Here";
  #"Properties"           => "P&roperties";
  #"Quick View"           => "&Quick View";
  #"Redo"                 => "&Redo";
  #"Repeat"               => "&Repeat";
  #"Restore"              => "&Restore";
  #"Resume"               => "&Resume";
  #"Retry"                => "&Retry";
  #"Run"                  => "&Run";
  #"Save"                 => "&Save";
  #"Save As"              => "Save &As";
  #"Select All"           => "Select &All";
  #"Send To"              => "Se&nd To";
  #"Show"                 => "&Show";
  #"Size"                 => "&Size";
  #"Split"                => "S&plit";
  #"Stop"                 => "&Stop";
  #"Undo"                 => "&Undo";
  #"View"                 => "&View";
  #"What's This?"         => "&What's This?";
  #"Window"               => "&Window";
  #"Yes"                  => "&Yes";

  // Some extras that seemed to be missing
  #"Previous"             => "&Previous";
end table $mnemonic-table;

define method compute-mnemonic-from-label
    (menu :: <win32-gadget-mixin>, label :: <string>)
 => (new-label :: <string>, mnemonic :: false-or(<character>))
  let (label, mnemonic) = next-method();
  if (mnemonic)
    values(label, mnemonic)
  else
    let new-label
      = element($mnemonic-table, as(<symbol>, label), default: #f);
    if (new-label)
      compute-mnemonic-from-label(menu, new-label)
    else
      values(label, #f)
    end
  end
end method compute-mnemonic-from-label;

define constant $vowels = "aeiou";

define method vowel? (char :: <character>)
  member?(as-lowercase(char), $vowels)
end method vowel?;

define method compute-used-mnemonics (menu :: <win32-gadget-mixin>) => ()
  let mirror = sheet-mirror(menu);
  let used-mnemonics = mirror-used-mnemonics(mirror);
  used-mnemonics.size := 0;
  for (child in sheet-children(menu))
    select (child by instance?)
      <menu>, <menu-button> =>
	let (label, mnemonic) 
	  = compute-mnemonic-from-label(child, gadget-label(child));
	add!(used-mnemonics, mnemonic);
      <menu-box> =>
	for (sub-child in sheet-children(menu))
	  select (sub-child by instance?)
	    <menu>, <menu-button> =>
	      let (label, mnemonic) 
		= compute-mnemonic-from-label(sub-child, gadget-label(sub-child));
	      add!(used-mnemonics, mnemonic);
	  end;
	end;
    end
  end;
end method compute-used-mnemonics;

define method allocate-unique-mnemonic 
    (menu :: <win32-gadget-mixin>, string :: <string>)
 => (char :: false-or(<character>))
  let mirror = sheet-mirror(menu);
  let used-mnemonics = mirror-used-mnemonics(mirror);
  let char
    = block (return)
	for (char in string)
	  if (~vowel?(char) & ~member?(char, used-mnemonics))
	    return(char)
	  end
	end;
        for (char in string)
	  unless (member?(char, used-mnemonics))
	    return(char)
	  end
	end
      end;
  add!(used-mnemonics, char);
  char
end method allocate-unique-mnemonic;


/// Menu bars

//---*** What is the multiple-child-composite-pane for?
define sealed class <win32-menu-bar>
    (<win32-gadget-mixin>,
     <menu-bar>,
     <multiple-child-composite-pane>)
end class <win32-menu-bar>;

define method class-for-make-pane
    (framem :: <win32-frame-manager>, class == <menu-bar>,
     #rest pane-options, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<win32-menu-bar>, #f)
end method class-for-make-pane;

//---*** Is this the best way to get the height of the menu-bar included,
//---*** or should we just put it into win32-top-level-sheet?
define method do-compose-space
    (sheet :: <win32-menu-bar>, #key width, height)
 => (space-req :: <space-requirement>)
  ignore(width, height);
  let top-sheet = top-level-sheet(sheet);
  if (top-sheet)
    let width  = 0;   //--- Do we want to compute this properly?
    let height = GetSystemMetrics($SM-CYMENU);
    make(<space-requirement>,
	 width: width,     height: height,
	 min-width: width, min-height: height,
	 max-width: $fill, max-height: height)
  end
end method do-compose-space;

define sealed class <menu-bar-mirror> (<menu-mirror>)
end class <menu-bar-mirror>;

define method do-make-mirror 
    (_port :: <win32-port>, menu-bar :: <win32-menu-bar>) => (mirror :: <win32-mirror>)
  let parent = top-level-sheet(menu-bar);
  let hMenu-bar :: <HMENU> = CreateMenu();
  let mirror = make(<menu-bar-mirror>, sheet: menu-bar, handle: hMenu-bar);
  do(method (menu)
       make-mirror(_port, menu)
     end,
     sheet-children(menu-bar));
  SetMenu(parent.%window-handle, hMenu-bar);
  mirror
end method do-make-mirror;


/// Menu buttons

// Menu buttons are unmirrored, but have to update themselves on their
// menu which will be mirrored.

define open abstract class <win32-menu-button-mixin> (<gadget>, <sheet>)
end class <win32-menu-button-mixin>;


/// Menu handling

define sealed class <win32-menu>
    (<win32-gadget-mixin>,
     <menu>,
     <multiple-child-composite-pane>)
end class <win32-menu>;

define method class-for-make-pane 
    (framem :: <win32-frame-manager>, class == <menu>,
     #rest pane-options, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<win32-menu>, #f)
end method class-for-make-pane;

define method do-make-mirror
    (_port :: <win32-port>, menu :: <win32-menu>) => (mirror :: <menu-mirror>)
  ignore(_port);
  let hMenu :: <HMENU> = make-win32-menu(menu);
  let (text, image, mnemonic) = text-or-image-from-gadget-label(menu);
  ignore(image); //---*** We need to handle images at some point
  unless (mnemonic)
    mnemonic := allocate-unique-mnemonic(menu, text)
  end;
  // Put the menu into its parent unless this is a popup menu
  unless (menu-owner(menu))
    let parent = sheet-mirrored-ancestor(menu);
    let label = make-win32-mnemonic-label(text, mnemonic);
    AppendMenu(parent.%window-handle, 
	       %logior($MF-POPUP, $MF-STRING),
	       pointer-address(hMenu),
	       label)
  end;
  make(<menu-mirror>, sheet: menu, handle: hMenu)
end method do-make-mirror;

define method map-mirror
    (_port :: <win32-port>, menu :: <win32-menu>, mirror :: <menu-mirror>)
 => ()
  let owner = menu-owner(menu);
  if (owner)
    let (owner-x, owner-y) = sheet-screen-position(_port, owner);
    let (menu-x, menu-y) = sheet-position(menu);
    check-result
      ("TrackPopupMenu",
       TrackPopupMenu(mirror.%window-handle,
		      0, //---*** should be logior($TPM-LEFTALIGN, $TPM-LEFTBUTTON)
		      owner-x + menu-x,
		      owner-y + menu-y,
		      0,
		      sheet-mirror(owner).%window-handle,
		      $NULL-RECT));
  end
end method map-mirror;

define method add-menu-item
    (menu :: <win32-menu>,
     hMenu :: <HMENU>,
     button :: <win32-menu-button-mixin>)
 => ()
  let enabled?  = gadget-enabled?(button);
  let selected? = gadget-value(button);
  let id = generate-next-gadget-id(button);
  let (text, image, mnemonic) = text-or-image-from-gadget-label(button);
  ignore(image); //---*** We need to handle images at some point
  unless (mnemonic)
    mnemonic := allocate-unique-mnemonic(menu, text)
  end;
  let label = make-win32-mnemonic-label(text, mnemonic);
  AppendMenu(hMenu,
	     %logior($MF-STRING,
	             if (enabled?) $MF-ENABLED else $MF-GRAYED end,
	             if (selected?) $MF-CHECKED else 0 end),
	     id,
	     label);
end method add-menu-item;

define method make-win32-menu
    (menu :: <menu>) => (hMenu :: <HMENU>)
  let hMenu :: <HMENU> = CreatePopupMenu();
  let _port = port(menu);
  let need-separator? = #f;
  let seen-item? = #f;
  for (child in sheet-children(menu))
    if (need-separator?)
      AppendMenu(hMenu, $MF-SEPARATOR, 0, "");
      need-separator? := #f;
      seen-item? := #f;
    end;
    select (child by instance?)
      <menu> =>
	make-mirror(_port, child);
      <menu-box> =>
	if (seen-item?)
	  AppendMenu(hMenu, $MF-SEPARATOR, 0, "");
	end;
	for (sub-child in sheet-children(child))
	  add-menu-item(menu, hMenu, sub-child)
	end;
	need-separator? := #t;
      <menu-button> =>
	add-menu-item(menu, hMenu, child);
    end;
    seen-item? := #t;
  end;
  hMenu
end method make-win32-menu;

define method note-child-added
    (sheet :: <sheet>, child :: <win32-menu-button-mixin>) => ()
  next-method();
  //---*** Do something smart to dynamically add the menu item
  #f
end method note-child-added;

define method note-child-removed
    (sheet :: <sheet>, child :: <win32-menu-button-mixin>) => ()
  next-method();
  //---*** Do something smart to dynamically remove the menu item
  #f
end method note-child-removed;

define method handle-menu-command
    (sheet :: <sheet>, id :: <integer>) => (handled? :: <boolean>)
  let menu-button = gadget-for-id(sheet, id);
  handle-menu-command(menu-button, id)
end method handle-menu-command;

define method handle-menu-command
    (menu-button :: <win32-menu-button-mixin>, id :: <integer>)
 => (handled? :: <boolean>)
  ignore(id);
  handle-button-gadget-click(menu-button)
end method handle-menu-command;

define method menu-button-position 
    (menu :: <menu>, button :: <win32-menu-button-mixin>)
 => (position :: <integer>)
  let position = 0;
  block (return)
    for (child in sheet-children(menu))
      select (child by instance?)
	<menu-button> =>
	  if (child = button)
	    return(position)
	  end;
	<menu-box> =>
	  for (subchild in sheet-children(child))
	    if (subchild = button)
	      return(position)
	    end;
	    position := position + 1;
	  end;
	<menu> =>
	  #f;
      end;
      position := position + 1;
    end;
    error("Cannot find menu button position for %=", button);
  end
end method menu-button-position;

define method note-gadget-value-changed
    (gadget :: <win32-menu-button-mixin>, old-value) => ()
  ignore(old-value);
  let menu = find-parent-of-class(gadget, <menu>);
  let handle = menu & menu.%window-handle;
  if (handle)
    let position = menu-button-position(menu, gadget);
    CheckMenuItem
      (handle,
       position, 
       %logior($MF-BYPOSITION,
	       if (gadget-value(gadget)) $MF-CHECKED else $MF-UNCHECKED end))
  end
end method note-gadget-value-changed;

//---*** Need to implement enabling/disabling for <menu-button> and <menu>,
//---*** and possibly <menu-box> although the front-end might handle that.
/*
define method note-gadget-enabled (client, gadget :: <gadget>) => ()
  ignore(client);
  #f
end method note-gadget-enabled;

define method note-gadget-disabled (client, gadget :: <gadget>) => ()
  ignore(client);
  #f
end method note-gadget-disabled;
*/


/// The concrete menu button classes
define sealed class <win32-push-menu-button>
    (<win32-menu-button-mixin>,
     <push-menu-button>,
     <leaf-pane>)
end class <win32-push-menu-button>;

define method class-for-make-pane
    (framem :: <win32-frame-manager>, class == <push-menu-button>,
     #rest pane-options, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<win32-push-menu-button>, #f)
end method class-for-make-pane;


define sealed class <win32-radio-menu-button>
    (<win32-menu-button-mixin>,
     <radio-menu-button>,
     <leaf-pane>)
end class <win32-radio-menu-button>;

define method class-for-make-pane 
    (framem :: <win32-frame-manager>, class == <radio-menu-button>,
     #rest pane-options, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<win32-radio-menu-button>, #f)
end method class-for-make-pane;


define sealed class <win32-check-menu-button>
    (<win32-menu-button-mixin>,
     <check-menu-button>,
     <leaf-pane>)
end class <win32-check-menu-button>;

define method class-for-make-pane 
    (framem :: <win32-frame-manager>, class == <check-menu-button>,
     #rest pane-options, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<win32-check-menu-button>, #f)
end method class-for-make-pane;
