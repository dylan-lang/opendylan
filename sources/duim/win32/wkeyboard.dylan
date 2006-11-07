Module:    win32-duim
Synopsis:  Win32 keyboard mapping implementation
Author:    David Gray, Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Keysym translations

define constant $vk->character-table :: <object-table> = make(<object-table>);
define constant $vk->keysym-table    :: <object-table> = make(<object-table>);

define function initialize-keysym-translations
    (_port :: <win32-port>) => ()
  let char-table :: <object-table> = $vk->character-table;
  let sym-table  :: <object-table> = $vk->keysym-table;
  let string = make(<string>, size: 1);
  // ASCII
  for (code :: <integer> from 32 to 126)
    let char   = as(<character>, code);
    string[0] := as-uppercase(char);
    gethash(char-table, code) := char;
    gethash(sym-table,  code) := as(<symbol>, string)
  end;
  // Latin-1
  for (code :: <integer> from 160 to 255)
    let char   = as(<character>, code);
    string[0] := as-uppercase(char);
    gethash(char-table, code) := char;
    gethash(sym-table,  code) := as(<symbol>, string)
  end;
  // Miscellaneous standard characters
  gethash(sym-table, as(<integer>, ' '))  := #"space";
  gethash(sym-table, as(<integer>, '\t')) := #"tab";
  gethash(sym-table, as(<integer>, '\b')) := #"backspace";
  gethash(sym-table, as(<integer>, '\n')) := #"newline";
  gethash(sym-table, as(<integer>, '\r')) := #"return";
  gethash(sym-table, as(<integer>, '\f')) := #"form";
  gethash(sym-table, as(<integer>, '\e')) := #"escape";
  gethash(sym-table, as(<integer>, '\0')) := #"null";
  gethash(sym-table, 127)                 := #"rubout";
  // Windows function keys
  for (entry :: <simple-object-vector> in 
         vector(vector($VK-CANCEL,      #"cancel"),     // or maybe #"abort"?
                vector($VK-BACK,        #"backspace"),
                vector($VK-TAB,         #"tab"),
                vector($VK-CLEAR,       #"clear"),      // or maybe #"page"?
                vector($VK-RETURN,      #"return"),
                // vector($VK-LBUTTON,  #"lbutton"),
                // vector($VK-MBUTTON,  #"mbutton"),
                // vector($VK-RBUTTON,  #"rbutton"),
                vector($VK-SHIFT,       #"shift"),
                vector($VK-CONTROL,     #"control"),
                vector($VK-MENU,        #"meta"),
                vector($VK-PAUSE,       #"pause"),
                vector($VK-CAPITAL,     #"capital"),    // or maybe #"caps-lock"?
                vector($VK-ESCAPE,      #"escape"),
                // vector($VK-SPACE,    #"space"),
                vector($VK-PRIOR,       #"prior"),
                vector($VK-NEXT,        #"next"),
                vector($VK-END,         #"end"),
                vector($VK-HOME,        #"home"),
                vector($VK-LEFT,        #"left"),
                vector($VK-UP,          #"up"),
                vector($VK-RIGHT,       #"right"),
                vector($VK-DOWN,        #"down"),
                vector($VK-SELECT,      #"select"),
                vector($VK-PRINT,       #"print"),
                vector($VK-EXECUTE,     #"execute"),
                vector($VK-SNAPSHOT,    #"snapshot"),
                vector($VK-INSERT,      #"insert"),
                vector($VK-DELETE,      #"delete"),
                vector($VK-HELP,        #"help"),
                vector($VK-LWIN,        #"lwin"),
                vector($VK-RWIN,        #"rwin"),
                vector($VK-APPS,        #"apps"),
                vector($VK-NUMPAD0,     #"numpad0"),
                vector($VK-NUMPAD1,     #"numpad1"),
                vector($VK-NUMPAD2,     #"numpad2"),
                vector($VK-NUMPAD3,     #"numpad3"),
                vector($VK-NUMPAD4,     #"numpad4"),
                vector($VK-NUMPAD5,     #"numpad5"),
                vector($VK-NUMPAD6,     #"numpad6"),
                vector($VK-NUMPAD7,     #"numpad7"),
                vector($VK-NUMPAD8,     #"numpad8"),
                vector($VK-NUMPAD9,     #"numpad9"),
                vector($VK-ADD,         #"add"),
                vector($VK-SUBTRACT,    #"subtract"),
                vector($VK-MULTIPLY,    #"multiply"),
                vector($VK-DIVIDE,      #"divide"),
                vector($VK-SEPARATOR,   #"separator"),
                vector($VK-DECIMAL,     #"decimal"),
                vector($VK-F1,          #"f1"),
                vector($VK-F2,          #"f2"),
                vector($VK-F3,          #"f3"),
                vector($VK-F4,          #"f4"),
                vector($VK-F5,          #"f5"),
                vector($VK-F6,          #"f6"),
                vector($VK-F7,          #"f7"),
                vector($VK-F8,          #"f8"),
                vector($VK-F9,          #"f9"),
                vector($VK-F10,         #"f10"),
                vector($VK-F11,         #"f11"),
                vector($VK-F12,         #"f12"),
                vector($VK-F13,         #"f13"),
                vector($VK-F14,         #"f14"),
                vector($VK-F15,         #"f15"),
                vector($VK-F16,         #"f16"),
                vector($VK-F17,         #"f17"),
                vector($VK-F18,         #"f18"),
                vector($VK-F19,         #"f19"),
                vector($VK-F20,         #"f20"),
                vector($VK-F21,         #"f21"),
                vector($VK-F22,         #"f22"),
                vector($VK-F23,         #"f23"),
                vector($VK-F24,         #"f24"),
                vector($VK-NUMLOCK,     #"numlock"),
                vector($VK-SCROLL,      #"scroll")))
    let vk     = entry[0];
    let keysym = entry[1];
    gethash(sym-table, logior(vk, $function-key-mask)) := keysym
  end
end function initialize-keysym-translations;


define inline function virtual-key->keysym
    (vk :: <integer>) => (key :: false-or(<symbol>))
  gethash($vk->keysym-table, vk)
end function virtual-key->keysym;

define function keysym->virtual-key
    (keysym :: <symbol>) => (vk :: false-or(<integer>))
  block (return)
    for (ks keyed-by vk in $vk->keysym-table)
      when (ks == keysym)
        return(vk)
      end
    end;
    #f
  end
end function keysym->virtual-key;


// If the virtual key code represents a key which has a corresponding
// to an ASCII character, then return that character
define function virtual-key->character
    (vk :: <integer>) => (char :: false-or(<character>))
  case
    vk = $VK-SPACE          => ' ';
    // Keysyms for other printable characters are handled differently...
    vk >= #x30 & vk <= #x5A => as(<character>, vk);     // '0'..'9', 'A'..'Z'
    otherwise               => #f;
  end
end function virtual-key->character;

define function character->virtual-key
    (char :: <character>) => (vk :: false-or(<integer>))
  case
    char == ' '              => $VK-SPACE;
    alphanumeric-char?(char) => as(<integer>, as-uppercase(char));
    otherwise                => #f;
  end
end function character->virtual-key;


/// Key translations

define constant $key-translations :: <array>
  = make(<array>, dimensions: #[#x100, 3]);

define constant $function-key-mask :: <integer> = ash(1, 16);

define method read-key-translations
    (_port :: <win32-port>) => ()
  let windows-nt?  :: <boolean> = (_port.%os-name == #"Windows-NT");
  let translations :: <array>   = $key-translations;
  // 'transkey-buffer' is 2 long because 'ToAscii' might return two chars
  // when the layout indicates a dead key (accent or diacritic) and it can't
  // be composed to form a single character
  with-stack-structure (ascii-buffer :: <PTCHAR>, element-count: 2)
    with-stack-structure (unicode-buffer :: <PWCHAR>, element-count: 2)
      with-stack-structure (keystate-buffer :: <LPBYTE>, element-count: #x100)
        for (i :: <integer> from 0 below #x100)
          pointer-value(keystate-buffer, index: i) := 0
        end;
        // column 0 -- unmodified
        // column 1 -- Shift
        // column 2 -- AltGr (might be gotten via Ctrl+Alt)
        for (state :: <integer> from 0 below 3)
          select (state)
            0 =>
              #f;
            1 =>
              pointer-value(keystate-buffer, index: $VK-SHIFT)   := #x80;
            2 =>
              pointer-value(keystate-buffer, index: $VK-SHIFT)   := #x00;
              pointer-value(keystate-buffer, index: $VK-CONTROL) := #x80;
              pointer-value(keystate-buffer, index: $VK-MENU)    := #x80;
          end;
          for (vk :: <integer> from 0 below #x100)
            // First call to 'toUnicode'/'toAscii' is to flush the dead-key buffer
            if (windows-nt?)
              toUnicode(32, 0, keystate-buffer, unicode-buffer, 2, 0);
              let result = toUnicode(vk, 0, keystate-buffer, unicode-buffer, 2, 0);
              translations[vk, state]
                := if (result = 1)
                     as(<integer>, pointer-value(unicode-buffer, index: 0))
                   else
                     logior($function-key-mask, vk)
                   end;
            else
              toAscii(32, 0, keystate-buffer, ascii-buffer, 0);
              let result = toAscii(vk, 0, keystate-buffer, ascii-buffer, 0);
              translations[vk, state]
                := if (result = 1)
                     as(<integer>, pointer-value(ascii-buffer, index: 0))
                   else
                     logior($function-key-mask, vk)
                   end;
            end
          end
        end
      end
    end
  end
end method read-key-translations;


/// Keyboard event handling

// For detection of AltGr on Windows-95/98
define constant $extended-key-down :: <integer> = ash(1, 24);

define sealed method handle-key-down
    (sheet :: <sheet>, wParam :: <wparam-type>, lParam :: <lparam-type>)
 => (handled? :: <boolean>)
  let frame = sheet-frame(sheet);
  let _port = port(sheet);
  // When fExtended is on, we might have AltGr
  when (wParam == $VK-MENU
        & ~zero?(%logand(lParam, $extended-key-down)))
    _port.%extended-key-state := #t
  end;
  let modifiers :: <integer>
    = update-modifier-state(_port,
                            alt-key-is-meta?:   frame & frame-alt-key-is-meta?(frame),
                            allow-control-alt?: frame & frame-allow-control-alt?(frame));
  if (wParam == $VK-CANCEL)
    when (_port & frame
          & port-modifier-state(_port) = $control-key
          & frame-keyboard-interrupt?(frame))
      signal(make(<keyboard-interrupt>))
    end
  else
    let shift?   = ~zero?(logand(modifiers, $shift-key));
    let control? = ~zero?(logand(modifiers, $control-key));
    let alt?     = ~zero?(logand(modifiers, $alt-key)) & frame-alt-key-is-meta?(frame);
    let altgr?   = ~zero?(logand(modifiers, $altgr-key));
    let column :: <integer>
      = if (altgr?) 2 elseif (shift?) 1 else 0 end;
    let vk     :: <integer>
      = aref($key-translations, wParam, column);
    let no-char? = ~zero?(logand(vk, $function-key-mask));
    // If shift is pressed and we're in column 1 (the usual case for shift)
    // then discard the shift modifier unless columns 0 and 1 are the same.
    // This allows us to pick up, e.g., shift-Enter, but stops ! getting processed
    // as shift-!.  In the case of column 2 (AltGr), we keep the shift modifier.
    when (shift?
          & column = 1
          & (vk ~= aref($key-translations, wParam, 0)
             | vk = $VK-SPACE))
      // We also keep the shift modifier for capital letters when there are any
      // bucky bits set, e.g., control-shift-C
      unless (vk >= as(<integer>, 'A') & vk <= as(<integer>, 'Z')
              & (control? | alt?))
        modifiers := logand(modifiers, lognot($shift-key))
      end
    end;
    local method expect-wm-char? ()
            if (control? & alt?)
              // This is a special case -- we're processing this as Ctrl+Alt,
              // but the may be a mapping for AltGr on this key, in which case
              // a WM_CHAR _is_ expected
              zero?(logand(aref($key-translations, wParam, 2), $function-key-mask))
            else
              // Normal case -- expect a WM_CHAR message unless there's no-char 
              // is true (this key doesn't correspond to a character) or Alt is
              // true (this will generate a WM_SYSCHAR which we'll ignore)
              ~(no-char? | alt?)
            end
          end method,
          method vk-is-keypad-digit? ()
            let vk :: <integer> = wParam;
            let extended? = ~zero?(%logand(lParam, #x01000000));
            (vk >= $VK-NUMPAD0 & vk <= $VK-NUMPAD9)
            | (~extended?
               & ((vk >= $VK-PRIOR & vk <= $VK-DOWN)
                  | vk = $VK-INSERT
                  | vk = $VK-CLEAR))
          end method;
    // If 'no-char?' is true, then this key is a key such as cursor left that
    // doesn't correspond to a character, so we'd better handle it in the
    // WM_KEYDOWN message.  If Alt is down, we handle it here to save having to
    // worry about WM_SYSCHAR message.  If control is down, we handle it here,
    // to prevent windows translating Ctrl-A to character 1, etc.
    // If this is Shift-AltGr, we handle it here, since windows doesn't generate
    // anything for this combination.
    if (no-char? | control? | alt? | (altgr? & shift?))
      // We're going to process the key here, so if we're expecting
      // a WM_CHAR event, set the flag to ignore it
      when (expect-wm-char?())
        _port.%wm-char-state := #f
      end;
      // Don't handle this if it's Alt + a keypad number, since this
      // is used to enter keys by character code
      unless (alt? & vk-is-keypad-digit?())
        handle-key(sheet, <key-press-event>, vk, modifiers)
      end
    else
      // In this case we are going to get a WM_CHAR.  Store the modifers
      // for use when processing it
      _port.%wm-char-state := modifiers
    end
  end;
  #t
end method handle-key-down;

// Sometimes we get a WM_KEYUP rather than WM_SYSKEYUP on releasing an extended key
define sealed method handle-key-up
    (sheet :: <sheet>, wParam :: <wparam-type>, lParam :: <lparam-type>)
 => (handled? :: <boolean>)
  let frame = sheet-frame(sheet);
  let _port = port(sheet);
  when (frame-alt-key-is-meta?(frame)
        & ~zero?(%logand(lParam, $extended-key-down)))
    _port.%extended-key-state := #f
  end;
  let modifiers :: <integer>
    = update-modifier-state(_port,
                            alt-key-is-meta?:   frame & frame-alt-key-is-meta?(frame),
                            allow-control-alt?: frame & frame-allow-control-alt?(frame));
  let shift? = ~zero?(logand(modifiers, $shift-key));
  let altgr? = ~zero?(logand(modifiers, $altgr-key));
  let column :: <integer>
    = if (altgr?) 2 elseif (shift?) 1 else 0 end;
  let vk     :: <integer>
    = aref($key-translations, wParam, column);
  handle-key(sheet, <key-release-event>, vk, modifiers);
  #t
end method handle-key-up;

define sealed method handle-char
    (sheet :: <sheet>, wParam :: <wparam-type>, lParam :: <lparam-type>)
 => (handled? :: <boolean>)
  let frame = sheet-frame(sheet);
  let _port = port(sheet);
  let flag = _port.%wm-char-state;
  let modifiers :: <integer>
    = update-modifier-state(_port,
                            alt-key-is-meta?:   frame & frame-alt-key-is-meta?(frame),
                            allow-control-alt?: frame & frame-allow-control-alt?(frame));
  if (_port.%os-name == #"Windows-NT")
    // Easy case -- wParam is Unicode
    when (flag)
      let char = as(<character>, wParam);
      handle-key(sheet, <key-press-event>, #f, modifiers, character: char)
    end;
    _port.%wm-char-state := 0
  else
    // Input is encoded in the ANSI code page in Windows 95
    if (flag)
      if (flag > 0 & flag < 256 & IsDBCSLeadByte(flag))
        // Previous byte was lead byte, now we have a DBC
        let external-code = ash(flag, 8) + wParam;
        let char = find-external-character(external-code);
        when (char)
          handle-key(sheet, <key-press-event>, #f, modifiers, character: char)
        end;
        _port.%wm-char-state := 0
      else
        // Previous byte was not a lead byte.  This byte may or may not be.
        if (wParam > 0 & wParam < 256 & IsDBCSLeadByte(wParam))
          // Store this lead byte for next time around
          _port.%wm-char-state := wParam
        else
          // Otherwise wParam contains whole character
          let char = find-external-character(wParam);
          when (char)
            handle-key(sheet, <key-press-event>, #f, modifiers, character: char)
          end;
          _port.%wm-char-state := 0
        end
      end
    else
      _port.%wm-char-state := 0
    end
  end;
  #t
end method handle-char;

// This gets used only on Windows-95/98
define function find-external-character
    (code :: <integer>) => (character :: false-or(<character>))
  //---*** let charset = ef/ef-coded-character-set(w/*multibyte-code-page-ef*);
  //---*** let char    = ef/find-external-char(code, char-set);
  when (code >= 0 & code <= 255)
    as(<character>, code)
  end
end function find-external-character;

define sealed method handle-key
    (sheet :: <sheet>, class :: subclass(<keyboard-event>),
     vk :: false-or(<integer>), modifiers :: <integer>, 
     #key character :: false-or(<character>) = #f) => ()
  let frame = sheet-frame(sheet);
  let _port = port(sheet);
  let focus = sheet-input-focus(port-input-focus(_port) | sheet);
  when (sheet-handles-keyboard?(focus))
    let character
      = character | gethash($vk->character-table, vk);
    let keysym
      = if (vk & gethash($vk->keysym-table, vk)) gethash($vk->keysym-table, vk)
        elseif (character) standard-char->keysym(character)
        else #f end;
    let modifiers
      = if (character & digit-char?(character) & zero?(logand(modifiers, $bucky-keys)))
          logand(modifiers, lognot($shift-key))
        else
          modifiers
        end;
    when (character | keysym)
      distribute-event(_port,
                       make(class,
                            sheet: focus,
                            modifier-state: modifiers,
                            key-name:  keysym,
                            character: character))
    end
  end
end method handle-key;


define sealed method handle-syskey-down
    (sheet :: <sheet>, wParam :: <wparam-type>, lParam :: <lparam-type>)
 => (handled? :: <boolean>)
  let frame = sheet-frame(sheet);
  let _port = port(sheet);
  if (frame-alt-key-is-meta?(frame))
    let alt? = (wParam == $VK-MENU);
    _port.%alt-key-state := ~alt?;
    handle-key-down(sheet, wParam, lParam);
    // Pass WM_SYSKEYDOWN messages through for Alt only
    if (alt? & frame-alt-key-is-meta?(frame) ~== #"ignore-as-syskey") #f else #t end;
  else
    #f
  end
end method handle-syskey-down;

define sealed method handle-syskey-up
    (sheet :: <sheet>, wParam :: <wparam-type>, lParam :: <lparam-type>)
 => (handled? :: <boolean>)
  let frame = sheet-frame(sheet);
  let _port = port(sheet);
  if (frame-alt-key-is-meta?(frame))
    let _port = port(sheet);
    let alt? = (wParam == $VK-MENU);
    case
      ~alt? =>
        // If it's not the Alt key, don't pass it through to Windows
        handle-key-up(sheet, wParam, lParam);
        #t;
      _port.%alt-key-state =>
        // Another key was pressed after Alt.  In this case, we translate
        // the WM_SYSKEYUP to a WM_KEYUP so that the menu isn't entered.
        DefWindowProc(window-handle(sheet), $WM-KEYUP, wParam, lParam);
        #t;
      otherwise =>
        // Only in the case of pressing and releasing Alt do we pass this
        // through to Windows.  Even then,  not if user has specified they
        // don't want the standard Windows behavior.
        let modifiers :: <integer>
          = update-modifier-state(_port,
                                  alt-key-is-meta?:   frame & frame-alt-key-is-meta?(frame),
                                  allow-control-alt?: frame & frame-allow-control-alt?(frame));
        //---*** The VK surely isn't wParam, is it?
        handle-key(sheet, <key-release-event>, wParam, modifiers);
        frame-alt-key-is-meta?(frame) == #"ignore-as-syskey";
    end
  else
    #f
  end
end method handle-syskey-up;

define sealed method handle-syschar
    (sheet :: <sheet>, wParam :: <wparam-type>, lParam :: <lparam-type>)
 => (handled? :: <boolean>)
  ignore(wParam, lParam);
  let frame = sheet-frame(sheet);
  frame-alt-key-is-meta?(frame)
end method handle-syschar;


/// Modifier state handling

define constant $vk->modifier :: <simple-object-vector>
  = vector(vector($VK-SHIFT,   $shift-key,    #f),
           vector($VK-CONTROL, $control-key,  #f),
           vector($VK-CAPITAL, $capslock-key, #t),
           vector($VK-MENU,    $alt-key,      #f));

define constant $control+alt-key :: <integer> = logior($control-key, $alt-key);

define sealed method update-modifier-state 
    (_port :: <win32-port>, #key alt-key-is-meta? = #t, allow-control-alt? = #t)
 => (state :: <integer>)
  let modifiers :: <integer> = 0;
  for (entry :: <simple-object-vector> in $vk->modifier)
    let vk :: <integer> = entry[0];
    let mk :: <integer> = entry[1];
    let toggle?         = entry[2];
    when (get-key-state(vk, toggle?: toggle?))
      modifiers := logior(modifiers, mk)
    end
  end;
  // If control and alt are pressed on a keyboard which has an AltGr key,
  // we can try to destinguish Ctrl+Alt from AltGr.  Make this distinction
  // if 'alt-key-is-meta?' and 'allow-control-alt?' are both true, otherwise
  // any Ctrl+Alt will act like AltGr.
  when (_port.%altgr-key?
        & logand(modifiers, $control+alt-key) = $control+alt-key
        & (~(alt-key-is-meta? & allow-control-alt?)
           | get-altgr-state(_port)))
    modifiers := logior(logand(modifiers, lognot($control+alt-key)), $altgr-key)
  end;
  port-modifier-state(_port) := modifiers;
  modifiers
end method update-modifier-state;
          
// When the keyboard has an AltGr key rather than a right hand Alt key, this
// key generates RMENU and LCONTROL.  Unfortunately, these L vs. R VK's are not
// recognized on Windows-95/98 -- see the entry for GetAsynchKeyState.  In Win95/98,
// the WM_KEYDOWN message for AltGr has VK_MENU and fExtended attribute 1; hopefully
// that will enable detection of AltGr on that OS.
define function get-altgr-state
    (_port :: <win32-port>) => (altgr? :: <boolean>)
  if (_port.%os-name == #"Windows-NT")
    get-key-state($VK-RMENU) & get-key-state($VK-LCONTROL)
  else
    _port.%extended-key-state
    & get-key-state($VK-MENU) & get-key-state($VK-CONTROL)
  end
end function get-altgr-state;

define inline function get-key-state
    (vk :: <integer>, #key toggle? :: <boolean> = #f)
 => (pressed? :: <boolean>)
  ~zero?(logand(GetKeyState(vk), if (toggle?) #x0001 else #x8000 end))
end function get-key-state;


/// Keyboard interrupt handling

define sealed method maybe-handle-keyboard-interrupt
    (sheet :: <sheet>, message :: <message-type>, wParam :: <wparam-type>) => ()
  when (message == $WM-KEYDOWN & wParam == $VK-CANCEL)
    let _port = port(sheet);
    let frame = sheet-frame(sheet);
    when (_port & frame
          & port-modifier-state(_port) = $control-key
          & frame-keyboard-interrupt?(frame))
      signal(make(<keyboard-interrupt>))
    end
  end
end method maybe-handle-keyboard-interrupt;
