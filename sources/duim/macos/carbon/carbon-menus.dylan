Module:       carbon-duim
Synopsis:     Macintosh menus implementation
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// MAC menu mirrors

define sealed class <menu-button-mirror> (<control-mirror>)
end class <menu-button-mirror>;

define sealed class <menu-mirror> (<control-mirror>)
  sealed slot %created? :: <boolean> = #f;
  sealed slot mirror-selection-owner :: false-or(<menu>) = #f,
    init-keyword: selection-owner:;
  sealed slot %used-mnemonics :: <stretchy-object-vector> = make(<stretchy-vector>);
end class <menu-mirror>;

define method set-mirror-parent
    (child :: <menu-button-mirror>, parent :: <menu-mirror>)
 => ()
  debug-message("Adding %= to menu %=",
		gadget-label(mirror-sheet(child)),
		gadget-label(mirror-sheet(parent)));
  carbon-menu-append(CARBON-MENU(mirror-control(parent)),
		  mirror-control(child))
end method set-mirror-parent;
    
define method set-mirror-parent
    (child :: <menu-mirror>, parent :: <menu-mirror>)
 => ()
  let control = mirror-control(child);
  let menu = carbon-menu-new();
  debug-message("Creating submenu for %s",
		gadget-label(mirror-sheet(child)));
  carbon-menu-item-set-submenu(CARBON-MENU(control), menu);
  carbon-menu-append(CARBON-MENU(mirror-control(parent)),
		  control)
end method set-mirror-parent;
    
define method set-mirror-parent
    (child :: <menu-mirror>, parent :: <control-mirror>)
 => ()
  if (instance?(parent.mirror-sheet, <menu-bar>))
    let control = mirror-control(child);
    if (child.mirror-sheet.gadget-label = "Help")
      carbon-menu-item-right-justify(control)
    end;
    let menu = carbon-menu-new();
    debug-message("Creating submenu for menu bar");
    carbon-menu-item-set-submenu(CARBON-MENU(control), menu);
    carbon-menu-bar-append(CARBON-MENU-BAR(mirror-control(parent)),
			control)
  else
    next-method()
  end
end method set-mirror-parent;
    
define sealed method destroy-mirror 
    (_port :: <carbon-port>, menu :: <menu>, mirror :: <menu-mirror>) => ()
  ignoring("destroy-mirror for <menu>")
end method destroy-mirror;

define sealed method note-mirror-destroyed
    (menu :: <menu>, mirror :: <menu-mirror>) => ()
  ignoring("note-mirror-destroyed for menu")
end method note-mirror-destroyed;


define sealed class <popup-menu-mirror> (<menu-mirror>)
  sealed slot mirror-selected-gadget :: false-or(<gadget>) = #f;
end class <popup-menu-mirror>;


/// Mnemonic handling
///---*** Is this the right thing for MAC?

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
    (gadget :: <carbon-gadget-mixin>, label :: <string>, 
     #key remove-ampersand? = #f)
 => (label, mnemonic :: false-or(<mnemonic>), index :: false-or(<integer>))
  let (label, mnemonic, index) = next-method();
  if (mnemonic)
    values(label, mnemonic, index)
  else
    compute-standard-carbon-mnemonic
      (gadget, label, remove-ampersand?: remove-ampersand?)
  end
end method compute-mnemonic-from-label;

define sealed method compute-standard-carbon-mnemonic
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
    compute-mnemonic-from-label
      (gadget, new-label, remove-ampersand?: remove-ampersand?)
  else
    values(label, #f, #f)
  end
end method compute-standard-carbon-mnemonic;

define inline function vowel? 
    (char :: <character>) => (vowel? :: <boolean>)
  member?(as-uppercase(char), "AEIOU")
end function vowel?;

define inline function consonant? 
    (char :: <character>) => (consonant? :: <boolean>)
  member?(as-uppercase(char), "BCDFGHJKLMNPQRSTVWXYZ")
end function consonant?;

define sealed method compute-used-mnemonics
    (gadget :: <carbon-gadget-mixin>) => ()
  let mirror = sheet-mirror(gadget);
  let used-mnemonics = mirror.%used-mnemonics;
  used-mnemonics.size := 0;
  local method maybe-add-mnemonic
	    (mnemonic :: false-or(<character>))
	  if (mnemonic)
	    add!(used-mnemonics,
		 as-uppercase(gesture-character(mnemonic)))
	  end
	end method maybe-add-mnemonic;
  for (child in sheet-children(gadget))
    select (child by instance?)
      <menu>, <menu-button> =>
	let (label, mnemonic, index)
	  = compute-mnemonic-from-label(child, defaulted-gadget-label(child));
	ignore(label, index);
	maybe-add-mnemonic(mnemonic);
      <menu-box> =>
	for (sub-child in sheet-children(child))
	  select (sub-child by instance?)
	    <menu>, <menu-button> =>
	      let (label, mnemonic, index)
		= compute-mnemonic-from-label
		    (sub-child, defaulted-gadget-label(sub-child));
	      ignore(label, index);
	      maybe-add-mnemonic(mnemonic);
	    <menu-box> =>
	      error("Found menu-box %= as child of menu-box %=",
		    sub-child, child);
	  end;
	end;
    end
  end;
end method compute-used-mnemonics;

// Mnemonics are allocated on MAC in the following order:
//   - first letter (or digit -- our own rule)
//   - any consonant
//   - any vowel
//   - any digit (our own rule)
// Note that all of the standard mnemonics and any user chosen ones
// have already been removed from consideration by 'compute-used-mnemonics'.
define sealed method allocate-unique-mnemonic 
    (gadget :: <carbon-gadget-mixin>, string :: <string>)
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

define sealed class <carbon-menu-bar>
    (<carbon-gadget-mixin>,
     <menu-bar>,
     <multiple-child-composite-pane>)
  keyword carbon-fixed-height?: = #t;
end class <carbon-menu-bar>;

define sealed method class-for-make-pane
    (framem :: <carbon-frame-manager>, class == <menu-bar>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<carbon-menu-bar>, #f)
end method class-for-make-pane;

define sealed method make-carbon-mirror
    (gadget :: <carbon-menu-bar>)
 => (mirror :: <control-mirror>)
  let control = carbon-menu-bar-new();
  make(<control-mirror>,
       control: control,
       sheet:  gadget)
end method make-carbon-mirror;

define sealed method make-carbon-menu-bar-contents
    (menu-bar :: <carbon-menu-bar>, mirror :: <control-mirror>) => ()
  let _port = port(menu-bar);
  compute-used-mnemonics(menu-bar);
  do(method (menu)
       menu.%port := _port;		//--- normally done in 'graft-sheet'
       make-mirror(_port, menu)
     end,
     sheet-children(menu-bar));
  mirror.%created? := #t
end method make-carbon-menu-bar-contents;

define sealed method refresh-menu-bar
    (menu-bar :: <carbon-menu-bar>) => ()
  let mirror = sheet-direct-mirror(menu-bar);
  if (mirror)
    ignoring("refresh-menu-bar")
  end
end method refresh-menu-bar;


/// Menu buttons

// Menu buttons are unmirrored, but have to update themselves on their
// menu which will be mirrored.

define open abstract class <carbon-menu-button-mixin>
    (<value-gadget>,
     <carbon-gadget-mixin>)
end class <carbon-menu-button-mixin>;

define sealed method make-carbon-mirror
    (gadget :: <carbon-menu-button-mixin>)
 => (mirror :: <menu-button-mirror>)
  let selection-mode = gadget-selection-mode(gadget);
  let radio-button?  = selection-mode == #"single";
  let (text, image, mnemonic, index)
    = text-or-image-from-gadget-label(gadget);
  if (image)
    ignoring("menu button with image")
  end;
  unless (mnemonic)
    mnemonic := allocate-unique-mnemonic(gadget, text)
  end;
  with-c-string (c-string = text)
    let control = carbon-menu-item-new-with-label(c-string);
    make(<menu-button-mirror>,
	 control: control,
	 sheet:  gadget)
  end
end method make-carbon-mirror;

define method update-mirror-attributes
    (gadget :: <carbon-menu-button-mixin>, mirror :: <menu-button-mirror>) => ()
  next-method();
  let control = mirror.mirror-control;
end method update-mirror-attributes;

define sealed method note-gadget-label-changed
    (gadget :: <carbon-menu-button-mixin>) => ()
  next-method();
  ignoring("note-gadget-label-changed for menu button")
end method note-gadget-label-changed;


/// Menu handling

define sealed class <carbon-menu>
    (<carbon-gadget-mixin>,
     <menu>,
     <multiple-child-composite-pane>)
  sealed slot menu-record-selection? = #f;
end class <carbon-menu>;

define sealed method class-for-make-pane 
    (framem :: <carbon-frame-manager>, class == <menu>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<carbon-menu>, #f)
end method class-for-make-pane;

define sealed method make-carbon-mirror
    (gadget :: <carbon-menu>)
 => (mirror :: <menu-mirror>)
  let (text, image, mnemonic, index)
    = text-or-image-from-gadget-label(gadget);
  if (image)
    ignoring("menu with image")
  end;
  with-c-string (c-string = text)
    let control = carbon-menu-item-new-with-label(c-string);
    let owner = menu-owner(gadget);
    let owner = if (frame?(owner)) top-level-sheet(owner) else owner end;
    make-menu-mirror-for-owner(owner, gadget, control)
  end
end method make-carbon-mirror;

/*---*** Not yet!
define sealed method make-menu-mirror-for-owner
    (owner :: <sheet>, gadget :: <carbon-menu>, control :: <MacControl*>)
 => (mirror :: <popup-menu-mirror>)
  let selection-owner = menu-record-selection?(gadget) & gadget;
  make(<popup-menu-mirror>, 
       control: control,
       sheet:  gadget, 
       selection-owner: selection-owner)
end method make-menu-mirror-for-owner;

define sealed method make-menu-mirror-for-owner
    (owner == #f, gadget :: <carbon-menu>, control :: <MacControl*>)
 => (mirror :: <menu-mirror>)
  make(<menu-mirror>,
       control: control,
       sheet:  gadget)
end method make-menu-mirror-for-owner;
*/

define sealed method map-mirror
    (_port :: <carbon-port>, menu :: <carbon-menu>, 
     mirror :: <popup-menu-mirror>) => ()
  ignoring("map-mirror on a popup menu")
end method map-mirror;

/*---*** Should be called just before a menu pops up
define method handle-menu-update
    (menu :: <carbon-menu>) => ()
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
	if (instance?(gadget, <carbon-menu>))
	  let mirror = sheet-direct-mirror(gadget);
	  if (mirror & ~mirror.%created?)
	    make-carbon-menu-contents(gadget, mirror);
	    #t
	  end
	end
      end;
  unless (mirrored?)
    do(ensure-menus-mirrored, sheet-children(gadget))
  end
end method ensure-menus-mirrored;
*/


/// Menu handling

define sealed method make-carbon-menu-contents
    (menu :: <menu>, mirror :: <menu-mirror>) => ()
  let _port = port(menu);
  let control = mirror.mirror-control;
  let need-separator? = #f;
  let seen-item? = #f;
  local
    method add-separator () => ()
      ignoring("add-separator");
      need-separator? := #f;
      seen-item? := #f
    end method add-separator,

    method add-menu-children
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
	    make-mirror(_port, child);
	    seen-item? := #t;
	end
      end;
      mirror.%created? := #t
    end method add-menu-children;
  add-menu-children(menu)
end method make-carbon-menu-contents;

define sealed method remove-carbon-menu-contents
    (gadget :: <carbon-gadget-mixin>, mirror :: <menu-mirror>) => ()
  ignoring("remove-carbon-menu-contents")
end method remove-carbon-menu-contents;

define sealed method refresh-menu (menu :: <carbon-menu>) => ()
  let mirror = sheet-direct-mirror(menu);
  if (mirror)
    remove-carbon-menu-contents(menu, mirror);
    mirror.%created? := #f
  end
end method refresh-menu;

define sealed method note-child-added
    (menu-bar :: <carbon-menu-bar>, menu :: <carbon-menu>) => ()
  ignore(menu);
  next-method();
  refresh-menu-bar(menu-bar)
end method note-child-added;

define sealed method note-child-added
    (menu :: <carbon-menu>, child :: <gadget>) => ()
  ignore(child);
  next-method();
  refresh-menu(menu)
end method note-child-added;

define sealed method note-child-added
    (gadget :: <menu-box>, child :: <carbon-menu-button-mixin>) => ()
  ignore(child);
  next-method();
  let menu = find-ancestor-of-type(gadget, <menu>);
  menu & refresh-menu(menu)
end method note-child-added;

define sealed method note-child-removed
    (menu-bar :: <carbon-menu-bar>, menu :: <carbon-menu>) => ()
  ignore(menu);
  next-method();
  refresh-menu-bar(menu-bar)
end method note-child-removed;

define sealed method note-child-removed
    (menu :: <carbon-menu>, child :: <gadget>) => ()
  ignore(child);
  next-method();
  refresh-menu(menu)
end method note-child-removed;

define sealed method note-child-removed
    (gadget :: <menu-box>, child :: <carbon-menu-button-mixin>) => ()
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

define sealed method note-gadget-value-changed
    (gadget :: <carbon-menu-button-mixin>) => ()
  ignoring("note-gadget-value-changed for <menu-button>")
end method note-gadget-value-changed;

define sealed method note-gadget-enabled 
    (client, gadget :: <carbon-menu-button-mixin>) => ()
  ignoring("note-gadget-enabled for <menu-button>")
end method note-gadget-enabled;

define sealed method note-gadget-disabled
    (client, gadget :: <carbon-menu-button-mixin>) => ()
  ignoring("note-gadget-disabled for <menu-button>")
end method note-gadget-disabled;

define sealed method note-gadget-enabled 
    (client, gadget :: <carbon-menu>) => ()
  ignoring("note-gadget-enabled for <menu>")
end method note-gadget-enabled;

define sealed method note-gadget-disabled
    (client, gadget :: <carbon-menu>) => ()
  ignoring("note-gadget-disabled for <menu>")
end method note-gadget-disabled;


/// The concrete menu button classes

define sealed class <carbon-push-menu-button>
    (<carbon-menu-button-mixin>,
     <push-menu-button>,
     <leaf-pane>)
end class <carbon-push-menu-button>;

define sealed method class-for-make-pane
    (framem :: <carbon-frame-manager>, class == <push-menu-button>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<carbon-push-menu-button>, #f)
end method class-for-make-pane;

define sealed method gadget-default?-setter
    (default? :: <boolean>, gadget :: <carbon-push-menu-button>)
 => (default? :: <boolean>)
  next-method();
  ignoring("gadget-default?-setter for a menu button");
  default?
end method gadget-default?-setter;


define sealed class <carbon-radio-menu-button>
    (<carbon-menu-button-mixin>,
     <radio-menu-button>,
     <leaf-pane>)
end class <carbon-radio-menu-button>;

define sealed method class-for-make-pane 
    (framem :: <carbon-frame-manager>, class == <radio-menu-button>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<carbon-radio-menu-button>, #f)
end method class-for-make-pane;


define sealed class <carbon-check-menu-button>
    (<carbon-menu-button-mixin>,
     <check-menu-button>,
     <leaf-pane>)
end class <carbon-check-menu-button>;

define sealed method class-for-make-pane 
    (framem :: <carbon-frame-manager>, class == <check-menu-button>, #key)
 => (class :: <class>, options :: false-or(<sequence>))
  values(<carbon-check-menu-button>, #f)
end method class-for-make-pane;


/// Choose from menu

define sealed method do-choose-from-menu
    (framem :: <carbon-frame-manager>, owner :: <sheet>, menu :: <menu>,
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
