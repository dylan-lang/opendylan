Module:    motif-duim
Synopsis:  Motif menus implementation
Author:    Scott McKay, Stuart Croy
	   Based on work by John Aspinall and Richard Billington
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// This is for popup menus, 'notify-user', 'choose-file', etc
define sealed class <callback-client-data> (<object>)
  sealed constant slot %owner-widget :: x/<Widget>,
    required-init-keyword: owner-widget:;
  sealed constant slot %x-display :: x/<Display>,
    required-init-keyword: x-display:;
  sealed constant slot %x :: <integer>,
    required-init-keyword: x:;
  sealed constant slot %y :: <integer>,
    required-init-keyword: y:;
  sealed constant slot %visible-setter :: false-or(<function>) = #f,
    init-keyword: visible-setter:;
  sealed constant slot %setter :: false-or(<function>) = #f,
    init-keyword: setter:;
  sealed constant slot %emergency-setter :: false-or(<function>) = #f,
    init-keyword: emergency-setter:;
end class <callback-client-data>;


/// Motif menus

define sealed class <menu-mirror> (<motif-mirror>)
  sealed slot window-handle :: <HMENU>,
     required-init-keyword: handle:;
  sealed slot %created? :: <boolean> = #f;
  sealed slot mirror-selection-owner :: false-or(<menu>) = #f,
    init-keyword: selection-owner:;
  sealed slot %used-mnemonics :: <stretchy-object-vector> = make(<stretchy-vector>);
end class <menu-mirror>;

define sealed method destroy-mirror 
    (_port :: <motif-port>, sheet :: <sheet>, mirror :: <menu-mirror>) => ()
  let handle :: <HMENU> = window-handle(mirror);
  unless (null-handle?(handle))  
    check-result("DestroyMenu", DestroyMenu(handle))
  end;
  sheet-direct-mirror(sheet) := #f
end method destroy-mirror;

define sealed method note-mirror-destroyed
    (sheet :: <sheet>, mirror :: <menu-mirror>) => ()
  let handle :: <HMENU> = window-handle(mirror);
  window-mirror(handle) := #f;
  window-handle(mirror) := $NULL-HMENU
end method note-mirror-destroyed;


define sealed class <popup-menu-mirror> (<menu-mirror>)
  sealed slot mirror-selected-gadget :: false-or(<gadget>) = #f;
end class <popup-menu-mirror>;


/// Mnemonic handling

define table $mnemonic-table :: <object-table>
  = { // This is the set defined by WIG, Appendix B, Table B.5, page 441
      #"About"                => "&About",
      #"Always on Top"        => "Always on &Top",
      #"Apply"                => "&Apply",
      #"Back"                 => "&Back",
      #"< Back"               => "< &Back",
      #"Browse"               => "&Browse",
      #"Close"                => "&Close",
      #"Copy"                 => "&Copy",
      #"Copy Here"            => "&Copy Here",
      #"Create Shortcut"      => "Create &Shortcut",
      #"Create Shortcut Here" => "Create &Shortcut Here",
      #"Cut"                  => "Cu&t",
      #"Delete"               => "&Delete",
      #"Edit"                 => "&Edit",
      #"Exit"                 => "E&xit",
      #"Explore"              => "&Explore",
      #"File"                 => "&File",
      #"Find"                 => "&Find",
      #"Help"                 => "&Help",
      #"Help Topics"          => "Help &Topics",
      #"Hide"                 => "&Hide",
      #"Insert"               => "&Insert",
      #"Insert Object"        => "Insert &Object",
      #"Link Here"            => "&Link Here",
      #"Maximize"             => "Ma&ximize",
      #"Minimize"             => "Mi&nimize",
      #"Move"                 => "&Move",
      #"Move Here"            => "&Move Here",
      #"New"                  => "&New",
      #"Next"                 => "&Next",
      #"Next >"               => "&Next >",
      #"No"                   => "&No",
      #"Open"                 => "&Open",
      #"Open With"            => "Open &With",
      #"Paste"                => "&Paste",
      #"Paste Link"           => "Paste &Link",
      #"Paste Shortcut"       => "Paste &Shortcut",
      #"Page Setup"           => "Page Set&up",
      #"Paste Special"        => "Paste &Special",
      #"Pause"                => "&Pause",
      #"Play"                 => "&Play",
      #"Print"                => "&Print",
      #"Print Here"           => "&Print Here",
      #"Properties"           => "P&roperties",
      #"Quick View"           => "&Quick View",
      #"Redo"                 => "&Redo",
      #"Repeat"               => "&Repeat",
      #"Restore"              => "&Restore",
      #"Resume"               => "&Resume",
      #"Retry"                => "&Retry",
      #"Run"                  => "&Run",
      #"Save"                 => "&Save",
      #"Save As"              => "Save &As",
      #"Select All"           => "Select &All",
      #"Send To"              => "Se&nd To",
      #"Show"                 => "&Show",
      #"Size"                 => "&Size",
      #"Split"                => "S&plit",
      #"Stop"                 => "&Stop",
      #"Undo"                 => "&Undo",
      #"View"                 => "&View",
      #"What's This?"         => "&What's This?",
      #"Window"               => "&Window",
      #"Yes"                  => "&Yes",

      // Some extras that seemed to be missing
      #"Find Next"            => "Find &Next",
      #"Find Previous"        => "Find &Previous",
      #"Forward"              => "&Forward",
      #"Previous"             => "&Previous",
      #"Replace"              => "R&eplace",
      #"Replace All"          => "Replace &All",
      #"Save All"             => "Save Al&l",
      #"Status Bar"           => "Status &Bar" };

define sealed method compute-mnemonic-from-label
    (gadget :: <motif-gadget-mixin>, label :: <string>, #key remove-ampersand? = #f)
 => (label, mnemonic :: false-or(<mnemonic>), index :: false-or(<integer>))
  let (label, mnemonic, index) = next-method();
  if (mnemonic)
    values(label, mnemonic, index)
  else
    compute-standard-motif-mnemonic(gadget, label, remove-ampersand?: remove-ampersand?)
  end
end method compute-mnemonic-from-label;

define sealed method compute-standard-motif-mnemonic
    (gadget :: <gadget>, label :: <string>, #key remove-ampersand? = #f)
 => (label, mnemonic :: false-or(<mnemonic>), index :: false-or(<integer>))
  let length :: <integer> = size(label);
  let dots :: <byte-string> = "...";
  let dots?
    = length > 3 & string-equal?(label, dots, start1: length - 3);
  let dotless-label
    = if (dots?) copy-sequence(label, end: length - 3)
      else label end;
  let new-label
    = element($mnemonic-table, as(<symbol>, dotless-label), default: #f);
  if (new-label)
    when (dots?)
      new-label := concatenate-as(<string>, new-label, dots)
    end;
    compute-mnemonic-from-label(gadget, new-label, remove-ampersand?: remove-ampersand?)
  else
    values(label, #f, #f)
  end
end method compute-standard-motif-mnemonic;

define inline function vowel? 
    (char :: <character>) => (vowel? :: <boolean>)
  member?(as-uppercase(char), "AEIOU")
end function vowel?;

define inline function consonant? 
    (char :: <character>) => (consonant? :: <boolean>)
  member?(as-uppercase(char), "BCDFGHJKLMNPQRSTVWXYZ")
end function consonant?;

define sealed method compute-used-mnemonics
    (gadget :: <motif-gadget-mixin>) => ()
  let mirror = sheet-mirror(gadget);
  let used-mnemonics = mirror.%used-mnemonics;
  used-mnemonics.size := 0;
  for (child in sheet-children(gadget))
    select (child by instance?)
      <menu>, <menu-button> =>
	let (label, mnemonic, index)
	  = compute-mnemonic-from-label(child, defaulted-gadget-label(child));
	ignore(label, index);
	mnemonic & add!(used-mnemonics, as-uppercase(gesture-character(mnemonic)));
      <menu-box> =>
	for (sub-child in sheet-children(child))
	  select (sub-child by instance?)
	    <menu>, <menu-button> =>
	      let (label, mnemonic, index)
		= compute-mnemonic-from-label(sub-child, defaulted-gadget-label(sub-child));
	      ignore(label, index);
	      mnemonic & add!(used-mnemonics, as-uppercase(gesture-character(mnemonic)));
	    <menu-box> =>
	      error("Found menu-box %= as child of menu-box %=",
		    sub-child, child);
	  end;
	end;
    end
  end;
end method compute-used-mnemonics;

// Mnemonics are allocated on Motif in the following order,
// according to WIG pg.34:
//   - first letter (or digit -- our own rule)
//   - any consonant
//   - any vowel
//   - any digit (our own rule)
// Note that all of the standard mnemonics and any user chosen ones
// have already been removed from consideration by 'compute-used-mnemonics'.
define sealed method allocate-unique-mnemonic 
    (gadget :: <motif-gadget-mixin>, string :: <string>)
 => (char :: false-or(<character>))
  assert(~empty?(string),
	 "Menu label for %= must have contents", gadget);
  let mirror = sheet-mirror(gadget);
  let used-mnemonics = mirror.%used-mnemonics;
  block (return)
    local method maybe-return-char (char)
	    let uppercase-char = as-uppercase(char);
	    unless (member?(uppercase-char, used-mnemonics))
	      add!(used-mnemonics, uppercase-char);
	      return(uppercase-char)
	    end
	  end;
    if (size(string) > 0)
      let first-char = string[0];
      when (consonant?(first-char)
	    | vowel?(first-char)
	    | digit-char?(first-char))
	maybe-return-char(string[0])
      end;
      for (char in string)
	when (consonant?(char))
	  maybe-return-char(char)
	end
      end;
      for (char in string)
	when (vowel?(char))
	  maybe-return-char(char)
	end
      end;
      for (char in string)
	when (digit-char?(char))
	  maybe-return-char(char)
	end
      end
    end
  end
end method allocate-unique-mnemonic;


/// Menu bars

//---*** What is the multiple-child-composite-pane for?
define sealed class <motif-menu-bar>
    (<motif-gadget-mixin>,
     <menu-bar>,
     <multiple-child-composite-pane>)
end class <motif-menu-bar>;

define sealed method class-for-make-pane
    (framem :: <motif-frame-manager>, class == <menu-bar>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<motif-menu-bar>, #f)
end method class-for-make-pane;

//--- This still doesn't account for the fact that Motif menus
//--- can span multiple lines...
define sealed method do-compose-space
    (sheet :: <motif-menu-bar>, #key width, height)
 => (space-req :: <space-requirement>)
  ignore(width, height);
  let top-sheet = top-level-sheet(sheet);
  when (top-sheet)
    let width  = compute-menu-bar-width(sheet);
    let height = GetSystemMetrics($SM-CYMENU);
    make(<space-requirement>,
	 width:     width, height:     height,
	 min-width: width, min-height: height,
	 max-width: $fill, max-height: height)
  end
end method do-compose-space;

// Sum up the widths of the menu bar labels, including inter-label spacing
//--- Is there a better way to do this?
define sealed method compute-menu-bar-width
    (menu :: <motif-menu-bar>) => (width :: <integer>)
  let _port = port(menu);
  let text-style = get-default-text-style(_port, menu);
  let width   :: <integer> = 0;
  let n-items :: <integer> = 0;
  for (child in sheet-children(menu))
    select (child by instance?)
      <menu>, <menu-button> =>
	let label = defaulted-gadget-label(child);
	select (label by instance?)
	  <string> =>
	    let ampersand = position(label, '&');
	    when (ampersand & (ampersand < size(label) - 1))
	      label := remove(label, '&', count: 1)
	    end;
	    inc!(width, text-size(_port, label, text-style: text-style));
	  <image> =>
	    inc!(width, image-width(label));
	end;
	inc!(n-items);
      <menu-box> =>
	#f;
    end
  end;
  width + $menu-bar-border-width * 2 + $menu-bar-label-spacing * (n-items - 1)
end method compute-menu-bar-width;

define sealed class <menu-bar-mirror> (<menu-mirror>)
end class <menu-bar-mirror>;

define sealed method do-make-mirror 
    (_port :: <motif-port>, menu-bar :: <motif-menu-bar>)
 => (mirror :: <motif-mirror>)
  let parent = top-level-sheet(menu-bar);
  let parent-handle = window-handle(parent);
  let resource-id = sheet-resource-id(menu-bar);
  if (resource-id)
    let handle = GetMenu(parent-handle);
    make(<menu-bar-mirror>, sheet: menu-bar, handle: handle)
  else
    // Note that we mirror the menus in the menu bar explicitly in order
    // to have closer control of the order in which things are created
    let handle :: <HMENU> = CreateMenu();
    let mirror = make(<menu-bar-mirror>, sheet: menu-bar, handle: handle);
    sheet-direct-mirror(menu-bar) := mirror;
    make-motif-menu-bar-contents(menu-bar, mirror);
    SetMenu(parent-handle, handle);
    mirror
  end
end method do-make-mirror;

define sealed method make-motif-menu-bar-contents
    (menu-bar :: <motif-menu-bar>, mirror :: <menu-bar-mirror>) => ()
  let _port = port(menu-bar);
  compute-used-mnemonics(menu-bar);
  do(method (menu)
       menu.%port := _port;		//--- normally done in 'graft-sheet'
       make-mirror(_port, menu)
     end,
     sheet-children(menu-bar));
  mirror.%created? := #t
end method make-motif-menu-bar-contents;

define sealed method refresh-menu-bar (menu-bar :: <motif-menu-bar>) => ()
  let mirror = sheet-direct-mirror(menu-bar);
  if (mirror)
    remove-motif-menu-contents(menu-bar, mirror);
    make-motif-menu-bar-contents(menu-bar, mirror);
    DrawMenuBar(mirror)
  end
end method refresh-menu-bar;


/// Menu buttons

// Menu buttons are unmirrored, but have to update themselves on their
// menu which will be mirrored.

define open abstract class <motif-menu-button-mixin>
    (<value-gadget>,
     <sheet-with-resource-mixin>,
     <standard-input-mixin>,
     <sheet>)
  slot %mirror-id :: <integer>;
end class <motif-menu-button-mixin>;

define sealed method compute-mnemonic-from-label
    (gadget :: <motif-menu-button-mixin>, label :: <string>, #key remove-ampersand? = #f)
 => (label, mnemonic :: false-or(<mnemonic>), index :: false-or(<integer>))
  let (label, mnemonic, index) = next-method();
  if (mnemonic)
    values(label, mnemonic, index)
  else
    compute-standard-motif-mnemonic(gadget, label, remove-ampersand?: remove-ampersand?)
  end
end method compute-mnemonic-from-label;

define sealed method note-gadget-label-changed
    (gadget :: <motif-menu-button-mixin>) => ()
  next-method();
  let (text, image, mnemonic, index)
    = text-or-image-from-gadget-label(gadget);
  ignore(image);
  let (handle, id) = menu-item-handle-and-id(gadget);
  when (handle)
    let label = make-motif-mnemonic-label(text, mnemonic, index);
    with-stack-structure (item-info :: <LPMENUITEMINFO>)
      item-info.cbSize-value := safe-size-of(<MENUITEMINFO>);
      item-info.fMask-value := %logior($MIIM-TYPE);
      item-info.dwTypeData-value := label;
      SetMenuItemInfo(handle, id, #f, item-info)
    end;
    unless (label = $NULL-string) destroy(label) end;
  end
end method note-gadget-label-changed;


/// Menu handling

define sealed class <motif-menu>
    (<motif-gadget-mixin>,
     <menu>,
     <multiple-child-composite-pane>)
  sealed slot menu-record-selection? = #f;
end class <motif-menu>;

define sealed method class-for-make-pane 
    (framem :: <motif-frame-manager>, class == <menu>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<motif-menu>, #f)
end method class-for-make-pane;

define sealed method do-make-mirror
    (_port :: <motif-port>, menu :: <motif-menu>) => (mirror :: <menu-mirror>)
  ignore(_port);
  let owner = menu-owner(menu);
  let owner = if (frame?(owner)) top-level-sheet(owner) else owner end;
  let handle :: <HMENU> = CreatePopupMenu();
  let mirror = make-menu-mirror-for-owner(owner, menu, handle);
  sheet-direct-mirror(menu) := mirror;
  make-motif-menu-contents(menu, mirror);
  mirror
end method do-make-mirror;

//--- Used by the OLE library, can we integrate it any better?
//--- Also doesn't have a selection-owner, does that matter?
define sealed method make-motif-menu
    (menu :: <menu>, #key frame) => (hMenu :: <HMENU>)
  menu-owner(menu) := frame;
  let handle :: <HMENU> = CreatePopupMenu();
  let mirror = make(<menu-mirror>, sheet: menu, handle: handle);
  sheet-direct-mirror(menu) := mirror;
  make-motif-menu-contents(menu, mirror);
  handle
end method make-motif-menu;

define sealed method make-menu-mirror-for-owner
    (owner :: <sheet>, menu :: <motif-menu>, handle :: <HMENU>)
 => (mirror :: <popup-menu-mirror>)
  let selection-owner = menu-record-selection?(menu) & menu;
  make(<popup-menu-mirror>, 
       selection-owner: selection-owner,
       sheet: menu, 
       handle: handle)
end method make-menu-mirror-for-owner;

define sealed method make-menu-mirror-for-owner
    (owner == #f, menu :: <motif-menu>, handle :: <HMENU>)
 => (mirror :: <menu-mirror>)
  let (text, image, mnemonic, index)
    = text-or-image-from-gadget-label(menu);
  ignore(image);	//---*** we need to handle images at some point
  unless (mnemonic)
    mnemonic := allocate-unique-mnemonic(menu, text)
  end;
  let parent = sheet-device-parent(menu);
  let label = make-motif-mnemonic-label(text, mnemonic, index);
  AppendMenu(window-handle(parent), 
	     %logior($MF-POPUP, $MF-STRING),
	     pointer-address(handle),
	     label);
  unless (label = $NULL-string) destroy(label) end;
  let selection-owner = mirror-selection-owner(sheet-direct-mirror(parent));
  make(<menu-mirror>,
       selection-owner: selection-owner,
       sheet: menu,
       handle: handle)
end method make-menu-mirror-for-owner;

//--- Motif lies about the type returned by TrackPopupMenu!
define inline-only C-function TrackPopupMenu-cmd
  parameter hMenu      :: <HMENU>;
  parameter uFlags     :: <UINT>;
  parameter x          :: <C-int>;
  parameter y          :: <C-int>;
  parameter nReserved  :: <C-int>;
  parameter hWnd       :: <HWND>;
  parameter prcRect    ::  /* const */ <LPRECT>;
  result value :: <C-int>;
  c-name: "TrackPopupMenu", c-modifiers: "__stdcall";
end;

define sealed method map-mirror
    (_port :: <motif-port>, menu :: <motif-menu>, 
     mirror :: <popup-menu-mirror>) => ()
  let owner  = menu-owner(menu);
  let owner  = if (frame?(owner)) top-level-sheet(owner) else owner end;
  let handle = window-handle(mirror);
  let (owner-x, owner-y) = sheet-screen-position(_port, owner);
  let (menu-x,  menu-y)  = sheet-position(menu);
  let result
   = TrackPopupMenu-cmd(handle,
			%logior($TPM-LEFTALIGN, $TPM-TOPALIGN,
				$TPM-LEFTBUTTON,
				$TPM-NONOTIFY, $TPM-RETURNCMD),
			owner-x + menu-x,
			owner-y + menu-y,
			0,
			window-handle(sheet-mirror(owner)),
			$NULL-RECT);
  // Ensure pop-up menu messages get handled "soon" (see article Q135788)
  SendMessage(handle, $WM-NULL, 0, 0);
  mirror-selected-gadget(mirror) := #f;
  unless (result = 0)
    block (break)
      // Find the button the user clicked on, set the result for
      // 'choose-from-menu' and then simulate the button click
      do-sheet-tree(method (button)
		      when (instance?(button, <motif-menu-button-mixin>)
			    & button.%mirror-id = result)
			handle-gadget-activation(button);
			break()
		      end
		    end method, menu)
    end
  end
end method map-mirror;

/*
//--- Old asynchronous menu code
define sealed method map-mirror
    (_port :: <motif-port>, menu :: <motif-menu>, 
     mirror :: <popup-menu-mirror>) => ()
  let owner  = menu-owner(menu);
  let owner  = if (frame?(owner)) top-level-sheet(owner) else owner end;
  let handle = window-handle(mirror);
  let (owner-x, owner-y) = sheet-screen-position(_port, owner);
  let (menu-x, menu-y)   = sheet-position(menu);
  check-result
    ("TrackPopupMenu",
     TrackPopupMenu(handle,
                    %logior($TPM-LEFTALIGN, $TPM-TOPALIGN, $TPM-LEFTBUTTON),
                    owner-x + menu-x,
                    owner-y + menu-y,
                    0,
                    window-handle(sheet-mirror(owner)),
                    $NULL-RECT));
  // Ensure pop-up menu messages get handled "soon" (see article Q135788)
  SendMessage(handle, $WM-NULL, 0, 0)
end method map-mirror;
*/

define method handle-menu-update
    (menu :: <motif-menu>) => ()
  local method update-child-menu-boxes
	    (gadget :: <gadget>) => ()
	  execute-update-callback
	    (gadget, gadget-client(gadget), gadget-id(gadget));
	  for (child in sheet-children(gadget))
	    if (instance?(child, <menu-box>))
	      update-child-menu-boxes(child)
	    end
	  end
	end method update-child-menu-boxes;
  update-child-menu-boxes(menu);
  ensure-menus-mirrored(menu)
end method handle-menu-update;

define sealed method ensure-menus-mirrored
    (gadget :: <gadget>) => ()
  let mirrored?
    = begin
	if (instance?(gadget, <motif-menu>))
	  let mirror = sheet-direct-mirror(gadget);
	  if (mirror & ~mirror.%created?)
	    make-motif-menu-contents(gadget, mirror);
	    #t
	  end
	end
      end;
  unless (mirrored?)
    do(ensure-menus-mirrored, sheet-children(gadget))
  end
end method ensure-menus-mirrored;


/// Menu handling

define sealed method make-motif-menu-contents
    (menu :: <menu>, mirror :: <menu-mirror>) => ()
  compute-used-mnemonics(menu);
  let handle :: <HMENU> = window-handle(mirror);
  let _port = port(menu);
  let need-separator? = #f;
  let seen-item? = #f;
  local method add-separator () => ()
	  AppendMenu(handle, $MF-SEPARATOR, 0, "");
	  need-separator? := #f;
	  seen-item? := #f
	end method add-separator;
  local method add-menu-children
	    (gadget :: <gadget>) => ()
	  for (child in sheet-children(gadget))
	    select (child by instance?)
	      <menu> =>
		child.%port := _port;	//--- normally done in 'graft-sheet'
		make-mirror(_port, child);
		seen-item? := #t;
	      <menu-box> =>
		if (seen-item?) need-separator? := #t end;
		add-menu-children(child);
		need-separator? := #t;
	      <menu-button> =>
		when (need-separator?) add-separator() end;
		add-menu-item(menu, handle, child);
		seen-item? := #t;
	    end
	  end;
	  mirror.%created? := #t
	end method add-menu-children;
  add-menu-children(menu);
  mirror.%created? := #t
end method make-motif-menu-contents;

define sealed method remove-motif-menu-contents
    (gadget :: <motif-gadget-mixin>, mirror :: <menu-mirror>) => ()
  let handle = window-handle(mirror);
  let count = GetMenuItemCount(handle);
  //--- Remove all mirrors, as we're going to rebuild them
  do-sheet-tree(method (g :: <gadget>)
		  if (g ~= gadget & sheet-direct-mirror(g))
		    sheet-direct-mirror(g) := #f
		  end
		end,
		gadget);
  //--- Delete items backwards, so the positions don't change
  for (i :: <integer> from (count - 1) to 0 by -1)
    DeleteMenu(handle, i, $MF-BYPOSITION)
  end
end method remove-motif-menu-contents;

define sealed method refresh-menu (menu :: <motif-menu>) => ()
  let mirror = sheet-direct-mirror(menu);
  if (mirror)
    remove-motif-menu-contents(menu, mirror);
    mirror.%created? := #f
  end
end method refresh-menu;

define sealed method add-menu-item
    (menu :: <motif-menu>,
     handle :: <HMENU>,
     button :: <motif-menu-button-mixin>) => ()
  let selection-mode = gadget-selection-mode(button);
  let radio-button?  = selection-mode == #"single";
  let id             = ensure-gadget-id(button);
  button.%mirror-id := id;
  let (text, image, mnemonic, index)
    = text-or-image-from-gadget-label(button);
  ignore(image);	//---*** we need to handle images at some point
  unless (mnemonic)
    mnemonic := allocate-unique-mnemonic(menu, text)
  end;
  let label = make-motif-mnemonic-label(text, mnemonic, index);
  with-stack-structure (item-info :: <LPMENUITEMINFO>)
    item-info.cbSize-value := safe-size-of(<MENUITEMINFO>);
    item-info.wId-value := id;
    item-info.fMask-value
      := %logior($MIIM-ID, $MIIM-TYPE, $MIIM-STATE, $MIIM-DATA);
    item-info.fType-value
      := %logior($MFT-STRING, if (radio-button?) $MFT-RADIOCHECK else 0 end);
    item-info.fState-value := menu-button-item-state(button);
    item-info.dwTypeData-value := label;
    InsertMenuItem(handle, id, #f, item-info)
  end;
  unless (label = $NULL-string) destroy(label) end;
end method add-menu-item;

define sealed method menu-button-item-state
    (button :: <motif-menu-button-mixin>)
 => (state :: <unsigned-int>)
  let selection-mode = gadget-selection-mode(button);
  let push-button?   = selection-mode == #"none";
  let enabled?       = gadget-enabled?(button);
  let selected?      = ~push-button? & gadget-value(button);
  let default?       = push-button?  & gadget-default?(button);
  %logior(if (default?)  $MFS-DEFAULT else 0 end,
	  if (enabled?)  $MFS-ENABLED else $MFS-DISABLED end,
	  if (selected?) $MFS-CHECKED else 0 end)
end method menu-button-item-state;

define sealed method note-child-added
    (menu-bar :: <motif-menu-bar>, menu :: <motif-menu>) => ()
  ignore(menu);
  next-method();
  refresh-menu-bar(menu-bar)
end method note-child-added;

define sealed method note-child-added
    (menu :: <motif-menu>, child :: <gadget>) => ()
  ignore(child);
  next-method();
  refresh-menu(menu)
end method note-child-added;

define sealed method note-child-added
    (gadget :: <menu-box>, child :: <motif-menu-button-mixin>) => ()
  ignore(child);
  next-method();
  let menu = find-ancestor-of-type(gadget, <menu>);
  menu & refresh-menu(menu)
end method note-child-added;

define sealed method note-child-removed
    (menu-bar :: <motif-menu-bar>, menu :: <motif-menu>) => ()
  ignore(menu);
  next-method();
  refresh-menu-bar(menu-bar)
end method note-child-removed;

define sealed method note-child-removed
    (menu :: <motif-menu>, child :: <gadget>) => ()
  ignore(child);
  next-method();
  refresh-menu(menu)
end method note-child-removed;

define sealed method note-child-removed
    (gadget :: <menu-box>, child :: <motif-menu-button-mixin>) => ()
  ignore(child);
  next-method();
  let menu = find-ancestor-of-type(gadget, <menu>);
  menu & refresh-menu(menu)
end method note-child-removed;

// Record the activation if necessary for popup menus, rather than
// doing it.  This is to handle 'choose-from-menu'.
define sealed method handle-gadget-activation
    (gadget :: <menu-button>) => (handled? :: <boolean>)
  let mirror = sheet-mirror(gadget);
  let selection-owner = mirror & mirror-selection-owner(mirror);
  if (selection-owner)
    let selection-mirror = sheet-direct-mirror(selection-owner);
    mirror-selected-gadget(selection-mirror) := gadget
  else
    handle-button-gadget-click(gadget)
  end
end method handle-gadget-activation;

//--- This code is getting tricky... maybe we should cache the
//--- item position in the button itself?
define sealed method menu-button-position 
    (menu :: <menu>, button :: <motif-menu-button-mixin>)
 => (position :: <integer>)
  let position = 0;
  let need-separator? = #f;
  let seen-item? = #f;
  local method add-separator-position ()
	  position := position + 1;
	  need-separator? := #f;
	  seen-item? := #f
	end;
  block (return)
    for (child in sheet-children(menu))
      when (need-separator?) add-separator-position() end;
      select (child by instance?)
	<menu-button> =>
	  when (child = button)
	    return(position)
	  end;
	<menu-box> =>
	  when (seen-item?) add-separator-position() end;
	  for (subchild in sheet-children(child))
	    when (subchild = button)
	      return(position)
	    end;
	    position := position + 1;
	    need-separator? := #t;
	  end;
	<menu> =>
	  #f;
      end;
      seen-item? := #t;
      position := position + 1;
    end;
    error("Cannot find menu button position for %=", button);
  end
end method menu-button-position;

define sealed method menu-item-handle-and-position
    (gadget :: <motif-menu-button-mixin>)
 => (handle :: false-or(<HMENU>), position :: false-or(<integer>))
  let menu   = find-ancestor-of-type(gadget, <menu>);
  let handle = menu & window-handle(menu);
  if (handle)
    values(handle, menu-button-position(menu, gadget))
  else
    values(#f, #f)
  end
end method menu-item-handle-and-position;

define sealed method menu-item-handle-and-position
    (menu :: <motif-menu>)
 => (handle :: false-or(<HMENU>), position :: false-or(<integer>))
  let handle = window-handle(menu);
  let parent = handle & find-ancestor-of-type(menu, type-union(<menu>, <menu-bar>));
  let parent-handle = parent & window-handle(parent);
  // The parent (or its handle) can be #f if the menu is parented on the display
  if (handle & parent-handle)
    let count = GetMenuItemCount(parent-handle);
    let position
      = block (return)
	  for (i :: <integer> from 0 below count)
	    let submenu = GetSubMenu(parent-handle, i);
	    if (submenu = handle) return(i) end
	  end;
	  error("Failed to find menu position for %= in %=", 
		gadget-label(menu) | menu,
		(instance?(parent, <menu>) & gadget-label(parent)) | parent)
	end;
    values(handle, position)
  else
    values(#f, #f)
  end
end method menu-item-handle-and-position;

define sealed method menu-item-handle-and-id
    (gadget :: <motif-menu-button-mixin>)
 => (handle :: false-or(<HMENU>), id :: false-or(<integer>))
  let menu   = find-ancestor-of-type(gadget, <menu>);
  let handle = menu & window-handle(menu);
  if (handle)
    values(handle, gadget.%mirror-id)
  else
    values(#f, #f)
  end
end method menu-item-handle-and-id;

define sealed method note-gadget-value-changed
    (gadget :: <motif-menu-button-mixin>) => ()
  let (handle, id) = menu-item-handle-and-id(gadget);
  when (handle)
    CheckMenuItem(handle, id, 
		  %logior($MF-BYCOMMAND,
			  if (gadget-value(gadget)) $MF-CHECKED else $MF-UNCHECKED end))
  end
end method note-gadget-value-changed;

define sealed method note-gadget-enabled 
    (client, gadget :: <motif-menu-button-mixin>) => ()
  ignore(client);
  let (handle, id) = menu-item-handle-and-id(gadget);
  when (handle)
    EnableMenuItem(handle, id, %logior($MF-BYCOMMAND, $MF-ENABLED))
  end
end method note-gadget-enabled;

define sealed method note-gadget-disabled
    (client, gadget :: <motif-menu-button-mixin>) => ()
  ignore(client);
  let (handle, id) = menu-item-handle-and-id(gadget);
  when (handle)
    EnableMenuItem(handle, id, %logior($MF-BYCOMMAND, $MF-GRAYED))
  end
end method note-gadget-disabled;

define sealed method note-gadget-enabled 
    (client, gadget :: <motif-menu>) => ()
  ignore(client);
  let (handle, position) = menu-item-handle-and-position(gadget);
  when (handle)
    EnableMenuItem(handle, position, %logior($MF-BYPOSITION, $MF-ENABLED))
  end
end method note-gadget-enabled;

define sealed method note-gadget-disabled
    (client, gadget :: <motif-menu>) => ()
  ignore(client);
  let (handle, position) = menu-item-handle-and-position(gadget);
  when (handle)
    EnableMenuItem(handle, position, %logior($MF-BYPOSITION, $MF-GRAYED))
  end
end method note-gadget-disabled;


/// The concrete menu button classes
define sealed class <motif-push-menu-button>
    (<motif-menu-button-mixin>,
     <push-menu-button>,
     <leaf-pane>)
end class <motif-push-menu-button>;

define sealed method class-for-make-pane
    (framem :: <motif-frame-manager>, class == <push-menu-button>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<motif-push-menu-button>, #f)
end method class-for-make-pane;

define sealed method gadget-default?-setter
    (default? :: <boolean>, gadget :: <motif-push-menu-button>)
 => (default? :: <boolean>)
  next-method();
  let (handle, id) = menu-item-handle-and-id(gadget);
  when (handle)
    let state = menu-button-item-state(gadget);
    with-stack-structure (item-info :: <LPMENUITEMINFO>)
      item-info.cbSize-value := safe-size-of(<MENUITEMINFO>);
      item-info.fMask-value  := %logior($MIIM-STATE);
      item-info.fState-value := state;
      SetMenuItemInfo(handle, id, #f, item-info)
    end
  end;
  default?
end method gadget-default?-setter;


define sealed class <motif-radio-menu-button>
    (<motif-menu-button-mixin>,
     <radio-menu-button>,
     <leaf-pane>)
end class <motif-radio-menu-button>;

define sealed method class-for-make-pane 
    (framem :: <motif-frame-manager>, class == <radio-menu-button>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<motif-radio-menu-button>, #f)
end method class-for-make-pane;


define sealed class <motif-check-menu-button>
    (<motif-menu-button-mixin>,
     <check-menu-button>,
     <leaf-pane>)
end class <motif-check-menu-button>;

define sealed method class-for-make-pane 
    (framem :: <motif-frame-manager>, class == <check-menu-button>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<motif-check-menu-button>, #f)
end method class-for-make-pane;


/// Choose from menu

define sealed method do-choose-from-menu
    (framem :: <motif-frame-manager>, owner :: <sheet>, menu :: <menu>,
     #key title, value,
          label-key, value-key, multiple-sets?,
     #all-keys)
 => (value, success? :: <boolean>)
  ignore(title, value, label-key, value-key, multiple-sets?);
  // record-selection? determines whether the events are distributed or
  // just recorded so that we can pick them up afterwards.
  menu-record-selection?(menu) := #t;
  menu-owner(menu) := owner;
  sheet-mapped?(menu) := #t;
  let mirror = sheet-mirror(menu);
  let selected-button = mirror-selected-gadget(mirror);
  values(selected-button & button-gadget-value(selected-button),
         selected-button & #t)
end method do-choose-from-menu;
