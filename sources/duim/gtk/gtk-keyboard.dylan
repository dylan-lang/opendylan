Module:       gtk-duim
Synopsis:     GTK keyboard mapping implementation
Author:       Andy Armstrong, Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// GTK keyboard handling

define method handle-gtk-key-press-event
    (sheet :: <sheet>, widget :: <GtkWidget*>, event :: <GdkEventKey*>)
 => (handled? :: <boolean>)
  handle-gtk-key-event(sheet, widget, event)
end method handle-gtk-key-press-event;

define method handle-gtk-key-release-event
    (sheet :: <sheet>, widget :: <GtkWidget*>, event :: <GdkEventKey*>)
 => (handled? :: <boolean>)
  handle-gtk-key-event(sheet, widget, event)
end method handle-gtk-key-release-event;

define method handle-gtk-key-event
    (sheet :: <sheet>, widget :: <GtkWidget*>, event :: <GdkEventKey*>)
 => (handled? :: <boolean>)
  ignore(widget);
  let _port = port(sheet);
  when (_port)
    ignoring("handle-key-event");
    #f
    /*
    let keycode   = event.x/keycode-value;
    let state     = event.x/state-value;
    let modifiers = gtk-state->duim-state(_port, logand(state, $key-event-modifier-mask));
    let (keysym-modifiers, keysym)
      = xt/XtTranslateKeycode(_port.%display, keycode, state);
    let char   = x-keysym->character(keysym, logand(state, $key-event-modifier-mask));
    let keysym = x-keysym->keysym(keysym);
    port-modifier-state(_port) := modifiers;
    distribute-event(_port,
		     make(event-class,
			  sheet:     sheet,
			  key-name:  keysym,
			  character: char,
			  modifier-state: modifiers));
    #t
    */
  end
end method handle-gtk-key-event;

/*---*** No keyboard handling yet!
define constant $x-keysym->character-table :: <object-table> = make(<table>);

define constant $x-keysym->keysym-table    :: <object-table> = make(<table>);

define function initialize-character-table () => ()
  //--- Fill some stuff in here
end function initialize-character-table;

define function initialize-keysym-table () => ()
  let table :: <object-table> = $x-keysym->keysym-table;
  // Fill in the alphabetic characters 
 for (code :: <integer> from as(<integer>, 'A') to as(<integer>, 'Z'))
    let keysym = make(<byte-string>, size: 1, fill: as(<character>, code));
    table[code]  := as(<symbol>, keysym)
  end;
  for (code :: <integer> from as(<integer>, 'a') to as(<integer>, 'z'))
    let keysym = make(<byte-string>, size: 1, fill: as(<character>, code));
    table[code]  := as(<symbol>, keysym)
  end;
  // Fill in the digit characters
  for (code :: <integer> from as(<integer>, '0') to as(<integer>, '9'))
    let keysym = make(<byte-string>, size: 1, fill: as(<character>, code));
    table[code]  := as(<symbol>, keysym)
  end;
  // Fill in the symbols
  table[x/$XK-parenleft]    := #"(";
  table[x/$XK-slash]        := #"/";
  table[x/$XK-ampersand]    := #"&";
  table[x/$XK-bracketleft]  := #"[";
  table[x/$XK-numbersign]   := #"#";
  table[x/$XK-percent]      := #"%";
  table[x/$XK-braceleft]    := #"{";
  table[x/$XK-bar]          := #"|";
  table[x/$XK-at]           := #"@";
  table[x/$XK-exclam]       := #"!";
  table[x/$XK-parenright]   := #")";
  table[x/$XK-minus]        := #"-";
  table[x/$XK-underscore]   := #"_";
  table[x/$XK-asciicircum]  := #"^";
  table[x/$XK-quotedbl]     := #"\"";
  table[x/$XK-apostrophe]   := #"'";
  table[x/$XK-grave]        := #"`";
  table[x/$XK-backslash]    := #"\\";
  table[x/$XK-plus]         := #"+";
  table[x/$XK-less]         := #"<";
  table[x/$XK-colon]        := #":";
  table[x/$XK-asterisk]     := #"*";
  table[x/$XK-bracketright] := #"]";
  table[x/$XK-space]        := #"space";
  table[x/$XK-semicolon]    := #";";
  table[x/$XK-greater]      := #">";
  table[x/$XK-question]     := #"?";
  table[x/$XK-asciitilde]   := #"~";
  table[x/$XK-comma]        := #",";
  table[x/$XK-period]       := #".";
  table[x/$XK-equal]        := #"=";
  table[x/$XK-dollar]       := #"$";
  table[x/$XK-return]       := #"return";
  table[x/$XK-delete]       := #"rubout";
  // table[x/$XK-osfDelete] := #"rubout";
  table[x/$XK-backspace]    := #"backspace";
  table[x/$XK-tab]          := #"tab";
  table[x/$XK-linefeed]     := #"linefeed";
  table[x/$XK-escape]       := #"escape";
end function initialize-keysym-table;

begin
  initialize-character-table();
  initialize-keysym-table();
end;

define constant $keysym-will-not-produce-character-mask :: <integer>
  = x/translate-to-modifiers-mask(#"mod1", #"mod2", #"mod3", #"mod4", #"mod5", #"control");

define function x-keysym->character
    (keysym, modifiers :: <integer>) => (char :: false-or(<character>))
  when (zero?(logand(modifiers, $keysym-will-not-produce-character-mask)))
    let value = x/KeysymValue(keysym);
    let char  = if (value >= 32 & value <= 126)
		  as(<byte-character>, value)
		else
		  gethash($x-keysym->character-table, value)
		end;
    char
  end
end function x-keysym->character;

define function x-keysym->keysym
    (keysym) => (keysym)
  let value = x/KeysymValue(keysym);
  gethash($x-keysym->keysym-table, value)
end function x-keysym->keysym;


/// Modifer maps

define function initialize-modifier-map
    (x-display :: x/<Display>) => (modifier-map :: <simple-object-vector>)
  let map :: <simple-object-vector> = make(<vector>, size: 256, fill: 0);
  let shifts   = vector(x/$XK-Shift-R,   x/$XK-Shift-L);
  let controls = vector(x/$XK-Control-R, x/$XK-Control-L);
  let metas    = vector(x/$XK-Meta-R,    x/$XK-Meta-L, x/$XK-Alt-R, x/$XK-Alt-L);
  let supers   = vector(x/$XK-Super-R,   x/$XK-Super-L);
  let hypers   = vector(x/$XK-Hyper-R,   x/$XK-Hyper-L);
  //---*** FINISH THIS
  map
end function initialize-modifier-map;

define function x-state->duim-state
    (_port :: <gtk-port>, state :: <integer>) => (duim-state :: <integer>)
  _port.%modifier-map[logand(state, #o377)]
end function x-state->duim-state;
*/
