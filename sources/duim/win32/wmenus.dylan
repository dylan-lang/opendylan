Module:    win32-duim
Synopsis:  Win32 menus implementation
Author:    Andy Armstrong, Scott McKay, David Gray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Some magic Win32 constants

//---*** All of the following should be computed
define constant $menu-bar-border-width  :: <integer> =  8;
define constant $menu-bar-label-spacing :: <integer> = 14;


/// Win32 menus

define sealed class <menu-mirror> (<win32-mirror>)
  sealed slot window-handle :: <HMENU>,
     required-init-keyword: handle:;
  sealed slot %created? :: <boolean> = #f;
  sealed slot mirror-selection-owner :: false-or(<menu>) = #f,
    init-keyword: selection-owner:;
  sealed slot %used-mnemonics :: <stretchy-object-vector> = make(<stretchy-vector>);
end class <menu-mirror>;

define sealed domain make (singleton(<menu-mirror>));
define sealed domain initialize (<menu-mirror>);

define sealed method destroy-mirror 
    (_port :: <win32-port>, sheet :: <sheet>, mirror :: <menu-mirror>) => ()
  let handle :: <HMENU> = window-handle(mirror);
  unless (null-handle?(handle))  
    check-result("DestroyMenu", DestroyMenu(handle))
  end;
  // We have to explicitly do this here, since menus don't receive
  // WM_DESTROY events from Windows
  note-mirror-destroyed(sheet, mirror);
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

define sealed domain make (singleton(<popup-menu-mirror>));
define sealed domain initialize (<popup-menu-mirror>);


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
    (gadget :: <win32-gadget-mixin>, label :: <string>, #key remove-ampersand? = #f)
 => (label, mnemonic :: false-or(<mnemonic>), index :: false-or(<integer>))
  let (label, mnemonic, index) = next-method();
  if (mnemonic)
    values(label, mnemonic, index)
  else
    compute-standard-win32-mnemonic(gadget, label, remove-ampersand?: remove-ampersand?)
  end
end method compute-mnemonic-from-label;

define sealed method compute-standard-win32-mnemonic
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
end method compute-standard-win32-mnemonic;

define inline function vowel? 
    (char :: <character>) => (vowel? :: <boolean>)
  member?(as-uppercase(char), "AEIOU")
end function vowel?;

define inline function consonant? 
    (char :: <character>) => (consonant? :: <boolean>)
  member?(as-uppercase(char), "BCDFGHJKLMNPQRSTVWXYZ")
end function consonant?;

define sealed method compute-used-mnemonics
    (gadget :: <win32-gadget-mixin>) => ()
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

// Mnemonics are allocated on Windows in the following order,
// according to WIG pg.34:
//   - first letter (or digit -- our own rule)
//   - first character after a space (our own rule)
//   - any consonant
//   - any vowel
//   - any digit (our own rule)
// Note that all of the standard mnemonics and any user chosen ones
// have already been removed from consideration by 'compute-used-mnemonics'.
// Also note that we stop the processing at the first tab we find.
define sealed method allocate-unique-mnemonic 
    (gadget :: <gadget>, string :: <string>)
 => (index :: false-or(<integer>))
  assert(~empty?(string),
	 "Menu label for %= must have contents", gadget);
  let mirror = sheet-mirror(gadget);
  let used-mnemonics = mirror.%used-mnemonics;
  block (return)
    local method maybe-return-index (index :: <integer>)
	    let char = as-uppercase(string[index]);
	    unless (member?(char, used-mnemonics))
	      add!(used-mnemonics, char);
	      return(index)
	    end
	  end;
    let string-size = size(string);
    if (string-size > 0)
      let _end = position(string, '\t') | string-size;
      let first-char = string[0];
      when (consonant?(first-char)
	    | vowel?(first-char)
	    | digit-char?(first-char))
	maybe-return-index(0)
      end;
      for (i :: <integer> from 0 below _end - 1)
	let char = string[i];
	char == ' ' & maybe-return-index(i + 1)
      end;
      for (i :: <integer> from 0 below _end)
	let char = string[i];
	consonant?(char) & maybe-return-index(i)
      end;
      for (i :: <integer> from 0 below _end)
	let char = string[i];
	vowel?(char) & maybe-return-index(i)
      end;
      for (i :: <integer> from 0 below _end)
	let char = string[i];
	digit-char?(char) & maybe-return-index(i)
      end
    end
  end
end method allocate-unique-mnemonic;


/// Menu bars

//---*** What is the multiple-child-composite-pane for?
define sealed class <win32-menu-bar>
    (<win32-gadget-mixin>,
     <menu-bar>,
     <multiple-child-composite-pane>)
end class <win32-menu-bar>;

define method class-for-make-pane
    (framem :: <win32-frame-manager>, class == <menu-bar>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<win32-menu-bar>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<win32-menu-bar>));
define sealed domain initialize (<win32-menu-bar>);

//--- This still doesn't account for the fact that Windows menus
//--- can span multiple lines...
define sealed method do-compose-space
    (sheet :: <win32-menu-bar>, #key width, height)
 => (space-req :: <space-requirement>)
  ignore(width, height);
  let width  = compute-menu-bar-width(sheet);
  let height = GetSystemMetrics($SM-CYMENU);
  make(<space-requirement>,
       width:     width, height:     height,
       min-width: width, min-height: height,
       max-width: $fill, max-height: height)
end method do-compose-space;

// Sum up the widths of the menu bar labels, including inter-label spacing
//--- Is there a better way to do this?
define sealed method compute-menu-bar-width
    (menu :: <win32-menu-bar>) => (width :: <integer>)
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

define sealed domain make (singleton(<menu-bar-mirror>));
define sealed domain initialize (<menu-bar-mirror>);

define sealed method do-make-mirror 
    (_port :: <win32-port>, menu-bar :: <win32-menu-bar>)
 => (mirror :: <win32-mirror>)
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
    make-win32-menu-bar-contents(menu-bar, mirror);
    SetMenu(parent-handle, handle);
    mirror
  end
end method do-make-mirror;

define sealed method make-win32-menu-bar-contents
    (menu-bar :: <win32-menu-bar>, mirror :: <menu-bar-mirror>) => ()
  let _port = port(menu-bar);
  compute-used-mnemonics(menu-bar);
  do(method (menu)
       menu.%port := _port;		//--- normally done in 'graft-sheet'
       make-mirror(_port, menu)
     end,
     sheet-children(menu-bar));
  mirror.%created? := #t
end method make-win32-menu-bar-contents;

define sealed method refresh-menu-bar (menu-bar :: <win32-menu-bar>) => ()
  let mirror = sheet-direct-mirror(menu-bar);
  if (mirror)
    remove-win32-menu-contents(menu-bar, mirror);
    make-win32-menu-bar-contents(menu-bar, mirror);
    DrawMenuBar(mirror)
  end
end method refresh-menu-bar;


/// Menu buttons

// Menu buttons are unmirrored, but have to update themselves on their
// menu which will be mirrored.

define abstract class <win32-menu-button-mixin>
    (<value-gadget>,
     <sheet-with-resource-mixin>,
     <standard-input-mixin>,
     <sheet>)
  sealed slot %mirror-id :: false-or(<integer>) = #f;
end class <win32-menu-button-mixin>;

define sealed method compute-mnemonic-from-label
    (gadget :: <win32-menu-button-mixin>, label :: <string>, #key remove-ampersand? = #f)
 => (label, mnemonic :: false-or(<mnemonic>), index :: false-or(<integer>))
  let (label, mnemonic, index) = next-method();
  if (mnemonic)
    values(label, mnemonic, index)
  else
    compute-standard-win32-mnemonic(gadget, label, remove-ampersand?: remove-ampersand?)
  end
end method compute-mnemonic-from-label;

define method compute-menu-button-label
    (button :: <win32-menu-button-mixin>, #key add-mnemonic? = #t)
 => (label :: <string>, image :: false-or(<image>))
  let (text, image, mnemonic, index)
    = text-or-image-from-gadget-label(button);
  let new-index
    = add-mnemonic? & ~mnemonic & allocate-unique-mnemonic(button, text);
  let label
    = make-win32-mnemonic-label(text, mnemonic, index, new-index,
				postfix: gadget-label-postfix(button));
  values(label, image)
end method compute-menu-button-label;

define sealed method note-gadget-label-changed
    (gadget :: <win32-menu-button-mixin>) => ()
  next-method();
  let (handle, id) = menu-item-handle-and-id(gadget);
  when (handle)
    let (label, image) = compute-menu-button-label(gadget, add-mnemonic?: #f);
    ignore(image);	//---*** we need to handle images at some point
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

define sealed class <win32-menu>
    (<win32-gadget-mixin>,
     <menu>,
     <multiple-child-composite-pane>)
  sealed slot menu-record-selection? = #f;
end class <win32-menu>;

define method class-for-make-pane 
    (framem :: <win32-frame-manager>, class == <menu>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<win32-menu>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<win32-menu>));
define sealed domain initialize (<win32-menu>);

define sealed method do-make-mirror
    (_port :: <win32-port>, menu :: <win32-menu>) => (mirror :: <menu-mirror>)
  ignore(_port);
  let owner = menu-owner(menu);
  let owner = if (frame?(owner)) top-level-sheet(owner) else owner end;
  let handle :: <HMENU> = CreatePopupMenu();
  let mirror = make-menu-mirror-for-owner(owner, menu, handle);
  sheet-direct-mirror(menu) := mirror;
  make-win32-menu-contents(menu, mirror);
  mirror
end method do-make-mirror;

//--- Used by the OLE library, can we integrate it any better?
//--- Also doesn't have a selection-owner, does that matter?
define sealed method make-win32-menu
    (menu :: <menu>, #key frame) => (hMenu :: <HMENU>)
  menu-owner(menu) := frame;
  let handle :: <HMENU> = CreatePopupMenu();
  let mirror = make(<menu-mirror>, sheet: menu, handle: handle);
  sheet-direct-mirror(menu) := mirror;
  make-win32-menu-contents(menu, mirror);
  handle
end method make-win32-menu;

define sealed method make-menu-mirror-for-owner
    (owner :: <sheet>, menu :: <win32-menu>, handle :: <HMENU>)
 => (mirror :: <popup-menu-mirror>)
  let selection-owner = menu-record-selection?(menu) & menu;
  make(<popup-menu-mirror>, 
       selection-owner: selection-owner,
       sheet: menu, 
       handle: handle)
end method make-menu-mirror-for-owner;

define sealed method make-menu-mirror-for-owner
    (owner == #f, menu :: <win32-menu>, handle :: <HMENU>)
 => (mirror :: <menu-mirror>)
  let (text, image, mnemonic, index)
    = text-or-image-from-gadget-label(menu);
  ignore(image);	//---*** we need to handle images at some point
  let new-index = ~mnemonic & allocate-unique-mnemonic(menu, text);
  let parent = sheet-device-parent(menu);
  let label = make-win32-mnemonic-label(text, mnemonic, index, new-index);
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

//--- Windows lies about the type returned by TrackPopupMenu!
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
    (_port :: <win32-port>, menu :: <win32-menu>, 
     mirror :: <popup-menu-mirror>) => ()
  let owner  = menu-owner(menu);
  let owner  = if (frame?(owner)) top-level-sheet(owner) else owner end;
  let handle = window-handle(mirror);
  let (owner-x, owner-y) = sheet-screen-position(_port, owner);
  let (menu-x,  menu-y)  = sheet-position(menu);
  update-menu(menu, submenus?: #t);
  let x-align = select (gadget-x-alignment(menu))
		  #"left"   => $TPM-LEFTALIGN;
		  #"right"  => $TPM-RIGHTALIGN;
		  #"center" => $TPM-CENTERALIGN;
		end;
  let y-align = select (gadget-y-alignment(menu))
		  #"top"      => $TPM-TOPALIGN;
		  #"bottom"   => $TPM-BOTTOMALIGN;
		  #"baseline" => $TPM-BOTTOMALIGN;
		  #"center"   => $TPM-VCENTERALIGN;
		end;
  let result
   = TrackPopupMenu-cmd(handle,
			%logior(x-align, y-align,
				$TPM-RIGHTBUTTON,	// allow both left and right buttons
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
		      when (instance?(button, <win32-menu-button-mixin>)
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
    (_port :: <win32-port>, menu :: <win32-menu>, 
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
    (menu :: <win32-menu>) => ()
  let mirror = sheet-direct-mirror(menu);
  // Popup menus are run inside an event handler, so the
  // $WM-INITMENUPOPUP event isn't seen until the menu
  // pops down again. So instead we update the menu before
  // popping it up, and do nothing here.
  unless (instance?(mirror, <popup-menu-mirror>))
    update-menu(menu, submenus?: #f)
  end
end method handle-menu-update;

//---*** Should this be on update-gadget?
define method update-menu
    (menu :: <win32-menu>, #key submenus? = #t) => ()
  local method update-menus
	    (gadget :: <gadget>) => ()
	  execute-update-callback
	    (gadget, gadget-client(gadget), gadget-id(gadget));
	  for (child in sheet-children(gadget))
	    if (instance?(child, <menu-box>)
		  | (submenus? & instance?(child, <menu>)))
	      update-menus(child)
	    end
	  end
	end method update-menus;
  update-menus(menu);
  //--- Now make sure any new submenus are mirrored...
  ensure-menus-mirrored(menu)
end method update-menu;

define sealed method ensure-menus-mirrored
    (gadget :: <gadget>) => ()
  let mirrored?
    = begin
	if (instance?(gadget, <win32-menu>))
	  let mirror = sheet-direct-mirror(gadget);
	  if (mirror & ~mirror.%created?)
	    make-win32-menu-contents(gadget, mirror);
	    #t
	  end
	end
      end;
  unless (mirrored?)
    do(ensure-menus-mirrored, sheet-children(gadget))
  end
end method ensure-menus-mirrored;


/// Menu handling

define sealed method make-win32-menu-contents
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
end method make-win32-menu-contents;

define sealed method remove-win32-menu-contents
    (gadget :: <win32-gadget-mixin>, mirror :: <menu-mirror>) => ()
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
end method remove-win32-menu-contents;

define sealed method refresh-menu (menu :: <win32-menu>) => ()
  let mirror = sheet-direct-mirror(menu);
  if (mirror)
    remove-win32-menu-contents(menu, mirror);
    mirror.%created? := #f
  end
end method refresh-menu;

define sealed method add-menu-item
    (menu :: <win32-menu>,
     handle :: <HMENU>,
     button :: <win32-menu-button-mixin>) => ()
  let selection-mode = gadget-selection-mode(button);
  let radio-button?  = (selection-mode == #"single");
  let id             = ensure-gadget-id(button);
  button.%mirror-id := id;
  let (label, image) = compute-menu-button-label(button);
  ignore(image);	//---*** we need to handle images at some point
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
    (button :: <win32-menu-button-mixin>)
 => (state :: <unsigned-int>)
  let selection-mode = gadget-selection-mode(button);
  let push-button?   = (selection-mode == #"none");
  let enabled?       = gadget-enabled?(button);
  let selected?      = ~push-button? & gadget-value(button);
  let default?       = push-button?  & gadget-default?(button);
  %logior(if (default?)  $MFS-DEFAULT else 0 end,
	  if (enabled?)  $MFS-ENABLED else $MFS-DISABLED end,
	  if (selected?) $MFS-CHECKED else 0 end)
end method menu-button-item-state;

define sealed method note-child-added
    (menu-bar :: <win32-menu-bar>, menu :: <win32-menu>) => ()
  ignore(menu);
  next-method();
  refresh-menu-bar(menu-bar)
end method note-child-added;

define sealed method note-child-added
    (menu :: <win32-menu>, child :: <gadget>) => ()
  ignore(child);
  next-method();
  refresh-menu(menu)
end method note-child-added;

define sealed method note-child-added
    (gadget :: <menu-box>, child :: <win32-menu-button-mixin>) => ()
  ignore(child);
  next-method();
  let menu = find-ancestor-of-type(gadget, <menu>);
  menu & refresh-menu(menu)
end method note-child-added;

define sealed method note-child-removed
    (menu-bar :: <win32-menu-bar>, menu :: <win32-menu>) => ()
  ignore(menu);
  next-method();
  refresh-menu-bar(menu-bar)
end method note-child-removed;

define sealed method note-child-removed
    (menu :: <win32-menu>, child :: <gadget>) => ()
  ignore(child);
  next-method();
  refresh-menu(menu)
end method note-child-removed;

define sealed method note-child-removed
    (gadget :: <menu-box>, child :: <win32-menu-button-mixin>) => ()
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
    mirror-selected-gadget(selection-mirror) := gadget;
    #t
  else
    handle-button-gadget-click(gadget)
  end
end method handle-gadget-activation;

//--- This code is getting tricky... maybe we should cache the
//--- item position in the button itself?
define sealed method menu-button-position 
    (menu :: <menu>, button :: <win32-menu-button-mixin>)
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
    (gadget :: <win32-menu-button-mixin>)
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
    (menu :: <win32-menu>)
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
    (gadget :: <win32-menu-button-mixin>)
 => (handle :: false-or(<HMENU>), id :: false-or(<integer>))
  let menu      = find-ancestor-of-type(gadget, <menu>);
  let handle    = menu & window-handle(menu);
  let mirror-id = gadget.%mirror-id;
  if (handle & mirror-id)
    values(handle, mirror-id)
  else
    values(#f, #f)
  end
end method menu-item-handle-and-id;

define sealed method note-gadget-value-changed
    (gadget :: <win32-menu-button-mixin>) => ()
  let (handle, id) = menu-item-handle-and-id(gadget);
  when (handle)
    CheckMenuItem(handle, id, 
		  %logior($MF-BYCOMMAND,
			  if (gadget-value(gadget)) $MF-CHECKED else $MF-UNCHECKED end))
  end
end method note-gadget-value-changed;

define sealed method note-gadget-enabled 
    (client, gadget :: <win32-menu-button-mixin>) => ()
  ignore(client);
  let (handle, id) = menu-item-handle-and-id(gadget);
  when (handle)
    EnableMenuItem(handle, id, %logior($MF-BYCOMMAND, $MF-ENABLED))
  end
end method note-gadget-enabled;

define sealed method note-gadget-disabled
    (client, gadget :: <win32-menu-button-mixin>) => ()
  ignore(client);
  let (handle, id) = menu-item-handle-and-id(gadget);
  when (handle)
    EnableMenuItem(handle, id, %logior($MF-BYCOMMAND, $MF-GRAYED))
  end
end method note-gadget-disabled;

define sealed method note-gadget-enabled 
    (client, gadget :: <win32-menu>) => ()
  ignore(client);
  let (handle, position) = menu-item-handle-and-position(gadget);
  when (handle)
    EnableMenuItem(handle, position, %logior($MF-BYPOSITION, $MF-ENABLED))
  end
end method note-gadget-enabled;

define sealed method note-gadget-disabled
    (client, gadget :: <win32-menu>) => ()
  ignore(client);
  let (handle, position) = menu-item-handle-and-position(gadget);
  when (handle)
    EnableMenuItem(handle, position, %logior($MF-BYPOSITION, $MF-GRAYED))
  end
end method note-gadget-disabled;


/// The concrete menu button classes
define sealed class <win32-push-menu-button>
    (<win32-menu-button-mixin>,
     <push-menu-button>,
     <leaf-pane>)
end class <win32-push-menu-button>;

define method class-for-make-pane
    (framem :: <win32-frame-manager>, class == <push-menu-button>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<win32-push-menu-button>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<win32-push-menu-button>));
define sealed domain initialize (<win32-push-menu-button>);

define sealed method gadget-default?-setter
    (default? :: <boolean>, gadget :: <win32-push-menu-button>)
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


define sealed class <win32-radio-menu-button>
    (<win32-menu-button-mixin>,
     <radio-menu-button>,
     <leaf-pane>)
end class <win32-radio-menu-button>;

define method class-for-make-pane 
    (framem :: <win32-frame-manager>, class == <radio-menu-button>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<win32-radio-menu-button>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<win32-radio-menu-button>));
define sealed domain initialize (<win32-radio-menu-button>);


define sealed class <win32-check-menu-button>
    (<win32-menu-button-mixin>,
     <check-menu-button>,
     <leaf-pane>)
end class <win32-check-menu-button>;

define method class-for-make-pane 
    (framem :: <win32-frame-manager>, class == <check-menu-button>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<win32-check-menu-button>, #f)
end method class-for-make-pane;

define sealed domain make (singleton(<win32-check-menu-button>));
define sealed domain initialize (<win32-check-menu-button>);


/// Choose from menu

define sealed method do-choose-from-menu
    (framem :: <win32-frame-manager>, owner :: <sheet>, menu :: <menu>,
     #key title, value, label-key, value-key, 
          width, height, foreground, background, text-style,
	  multiple-sets?,
     #all-keys)
 => (value, success? :: <boolean>)
  ignore(title, value, label-key, value-key,
	 width, height, foreground, background, text-style, multiple-sets?);
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
