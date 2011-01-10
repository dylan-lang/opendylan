Module:       gtk-duim
Synopsis:     GTK keyboard mapping implementation
Author:       Andy Armstrong, Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// GTK keyboard handling

define method handle-gtk-key-event
    (sheet :: <sheet>, event :: <GdkEventKey>)
 => (handled? :: <boolean>)
  let _port = port(sheet);
  when (_port)
    let char-codepoint = gdk-keyval-to-unicode(event.GdkEventkey-keyval);
    let char = when (char-codepoint > 0) as(<character>, char-codepoint) end;
    let class = if (event.GdkEventKey-type == $GDK-KEY-PRESS)
                  <key-press-event>
                else
                  <key-release-event>
                end;
    let keysym = keyval->keysym(event.GdkEventkey-keyval);
    let modifiers = map-modifiers(event.GdkEventKey-state);
    distribute-event(_port,
		     make(class,
			  sheet:     sheet,
			  key-name:  keysym,
			  character: char,
			  modifier-state: modifiers));
    #t
  end
end method handle-gtk-key-event;

define constant $modifier-map =
    vector(vector($GDK-SHIFT-MASK, $shift-key),
           vector($GDK-LOCK-MASK, $capslock-key),
           vector($GDK-CONTROL-MASK, $control-key),
           vector($GDK-MOD1-MASK, $alt-key),
           vector($GDK-SUPER-MASK, $super-key),
           vector($GDK-HYPER-MASK, $hyper-key),
           vector($GDK-META-MASK, $meta-key));

define inline function map-modifiers (modifiers)
  let result = 0;
  for (row in $modifier-map)
    if (logand(modifiers, row[0]) > 0)
      result := logior(result, row[1])
    end
  end;
  result
end;

define inline function keyval->keysym (keyval)
  let sym = as(<symbol>, gdk-keyval-name(keyval));
  element($keysym-table, sym, default: sym)
end;

define table $keysym-table = {
    #"Linefeed" => #"newline",
    #"Scroll_Lock" => #"scroll",
    #"Page_Up" => #"prior",
    #"Page_Down" => #"next",
    #"Num_Lock" => #"numlock",
    #"KP_Multiply" => #"multiply",
    #"KP_Add" => #"add",
    #"KP_Subtract" => #"subtract",
    #"KP_Divide" => #"divide",
    #"KP_Separator" => #"separator",
    #"KP_0" => #"numpad0",
    #"KP_1" => #"numpad1",
    #"KP_2" => #"numpad2",
    #"KP_3" => #"numpad3",
    #"KP_4" => #"numpad4",
    #"KP_5" => #"numpad5",
    #"KP_6" => #"numpad6",
    #"KP_7" => #"numpad7",
    #"KP_8" => #"numpad8",
    #"KP_9" => #"numpad9",
    #"Shift_L" => #"shift",
    #"Shift_R" => #"shift",
    #"Control_L" => #"control",
    #"Control_R" => #"control",
    #"Caps_Lock" => #"capital", // sic!
    #"Meta_L" => #"meta",
    #"Meta_R" => #"meta",
    #"Alt_L" => #"alt",
    #"Alt_R" => #"alt",
    #"Super_L" => #"super",
    #"Super_R" => #"super",
    #"Hyper_L" => #"hyper",
    #"Hyper_R" => #"hyper"};

