Module:    win32-duim
Synopsis:  Win32 keyboard mapping implementation
Author:    David Gray, Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// Win32 keysyms

define constant $keysym-table :: <simple-object-vector>
  = make(<simple-object-vector>, size: 256);

define inline function virtual-key->keysym
    (vk :: <integer>) => (key :: false-or(<symbol>))
  (vk >= 0) & (vk <= 256)
  & $keysym-table[vk]
end function virtual-key->keysym;

// This table was initially generated from:
// gema -match -line -p '\#define VK_<I><S><I>\W\n=\t\w/$VK-$1$2\/\* $3 \*\/ \=\> \#"@downcase{$1}"\;\n' winuser.h 
//---*** What about keysyms for ordinary printable characters?
define method initialize-virtual-key->keysym () => ()
  let vk = $keysym-table;
  //	vk[$VK-LBUTTON        /* #x01 */] := #"lbutton";
  //	vk[$VK-RBUTTON        /* #x02 */] := #"rbutton";
	vk[$VK-CANCEL         /* #x03 */] := #"cancel";		// or maybe #"abort" ?
  //	vk[$VK-MBUTTON        /* #x04 */] := #"mbutton";
	vk[$VK-BACK           /* #x08 */] := #"back";		// or #"rubout", #"backspace"
	vk[$VK-TAB            /* #x09 */] := #"tab";
	vk[			 #x0A   ] := #"linefeed";
	vk[$VK-CLEAR          /* #x0C */] := #"clear";		// or #"page" ?
	vk[$VK-RETURN         /* #x0D */] := #"return";
	vk[$VK-SHIFT          /* #x10 */] := #"shift";
	vk[$VK-CONTROL        /* #x11 */] := #"control";
	vk[$VK-MENU           /* #x12 */] := #"menu";
	vk[$VK-PAUSE          /* #x13 */] := #"pause";
	vk[$VK-CAPITAL        /* #x14 */] := #"capital";	// or #"caps-lock" ?
	vk[$VK-ESCAPE         /* #x1B */] := #"escape";
	vk[$VK-SPACE          /* #x20 */] := #"space";
	vk[$VK-PRIOR          /* #x21 */] := #"prior";
	vk[$VK-NEXT           /* #x22 */] := #"next";
	vk[$VK-END            /* #x23 */] := #"end";
	vk[$VK-HOME           /* #x24 */] := #"home";
	vk[$VK-LEFT           /* #x25 */] := #"left";
	vk[$VK-UP             /* #x26 */] := #"up";
	vk[$VK-RIGHT          /* #x27 */] := #"right";
	vk[$VK-DOWN           /* #x28 */] := #"down";
	vk[$VK-SELECT         /* #x29 */] := #"select";
	vk[$VK-PRINT          /* #x2A */] := #"print";
	vk[$VK-EXECUTE        /* #x2B */] := #"execute";
	vk[$VK-SNAPSHOT       /* #x2C */] := #"snapshot";
	vk[$VK-INSERT         /* #x2D */] := #"insert";
	vk[$VK-DELETE         /* #x2E */] := #"delete";
	vk[$VK-HELP           /* #x2F */] := #"help";
	vk[$VK-LWIN           /* #x5B */] := #"lwin";
	vk[$VK-RWIN           /* #x5C */] := #"rwin";
	vk[$VK-APPS           /* #x5D */] := #"apps";
	vk[$VK-NUMPAD0        /* #x60 */] := #"numpad0";	// or #"0" etc.?
	vk[$VK-NUMPAD1        /* #x61 */] := #"numpad1";
	vk[$VK-NUMPAD2        /* #x62 */] := #"numpad2";
	vk[$VK-NUMPAD3        /* #x63 */] := #"numpad3";
	vk[$VK-NUMPAD4        /* #x64 */] := #"numpad4";
	vk[$VK-NUMPAD5        /* #x65 */] := #"numpad5";
	vk[$VK-NUMPAD6        /* #x66 */] := #"numpad6";
	vk[$VK-NUMPAD7        /* #x67 */] := #"numpad7";
	vk[$VK-NUMPAD8        /* #x68 */] := #"numpad8";
	vk[$VK-NUMPAD9        /* #x69 */] := #"numpad9";
	vk[$VK-MULTIPLY       /* #x6A */] := #"multiply";
	vk[$VK-ADD            /* #x6B */] := #"add";
	vk[$VK-SEPARATOR      /* #x6C */] := #"separator";
	vk[$VK-SUBTRACT       /* #x6D */] := #"subtract";
	vk[$VK-DECIMAL        /* #x6E */] := #"decimal";
	vk[$VK-DIVIDE         /* #x6F */] := #"divide";
	vk[$VK-F1             /* #x70 */] := #"f1";
	vk[$VK-F2             /* #x71 */] := #"f2";
	vk[$VK-F3             /* #x72 */] := #"f3";
	vk[$VK-F4             /* #x73 */] := #"f4";
	vk[$VK-F5             /* #x74 */] := #"f5";
	vk[$VK-F6             /* #x75 */] := #"f6";
	vk[$VK-F7             /* #x76 */] := #"f7";
	vk[$VK-F8             /* #x77 */] := #"f8";
	vk[$VK-F9             /* #x78 */] := #"f9";
	vk[$VK-F10            /* #x79 */] := #"f10";
	vk[$VK-F11            /* #x7A */] := #"f11";
	vk[$VK-F12            /* #x7B */] := #"f12";
  //	vk[$VK-F13            /* #x7C */] := #"f13";
  //	vk[$VK-F14            /* #x7D */] := #"f14";
  //	vk[$VK-F15            /* #x7E */] := #"f15";
  //	vk[$VK-F16            /* #x7F */] := #"f16";
  //	vk[$VK-F17            /* #x80 */] := #"f17";
  //	vk[$VK-F18            /* #x81 */] := #"f18";
  //	vk[$VK-F19            /* #x82 */] := #"f19";
  //	vk[$VK-F20            /* #x83 */] := #"f20";
  //	vk[$VK-F21            /* #x84 */] := #"f21";
  //	vk[$VK-F22            /* #x85 */] := #"f22";
  //	vk[$VK-F23            /* #x86 */] := #"f23";
  //	vk[$VK-F24            /* #x87 */] := #"f24";
	vk[$VK-NUMLOCK        /* #x90 */] := #"numlock";
	vk[$VK-SCROLL         /* #x91 */] := #"scroll";
  //	vk[$VK-LSHIFT         /* #xA0 */] := #"lshift";
  //	vk[$VK-RSHIFT         /* #xA1 */] := #"rshift";
  //	vk[$VK-LCONTROL       /* #xA2 */] := #"lcontrol";
  //	vk[$VK-RCONTROL       /* #xA3 */] := #"rcontrol";
  //	vk[$VK-LMENU          /* #xA4 */] := #"lmenu";
  //	vk[$VK-RMENU          /* #xA5 */] := #"rmenu";
  //	vk[$VK-PROCESSKEY     /* #xE5 */] := #"processkey";
  //	vk[$VK-ATTN           /* #xF6 */] := #"attn";
  //	vk[$VK-CRSEL          /* #xF7 */] := #"crsel";
  //	vk[$VK-EXSEL          /* #xF8 */] := #"exsel";
  //	vk[$VK-EREOF          /* #xF9 */] := #"ereof";
  //	vk[$VK-PLAY           /* #xFA */] := #"play";
  //	vk[$VK-ZOOM           /* #xFB */] := #"zoom";
  //	vk[$VK-NONAME         /* #xFC */] := #"noname";
  //	vk[$VK-PA1            /* #xFD */] := #"pa1";
  //	vk[$VK-OEM_CLEAR      /* #xFE */] := #"oem_clear";
end method initialize-virtual-key->keysym;

initialize-virtual-key->keysym();

// If the virtual key code represents a key which has a corresponding
// ASCII character, then return that character.
define method virtual-key->character
    (vk :: <integer>) => (char :: false-or(<character>))
  if (vk < #x30)
    select (vk)
      $VK-BACK, $VK-TAB, #x0A, $VK-CLEAR, $VK-RETURN, $VK-ESCAPE, $VK-SPACE =>
	as(<character>, vk);
      $VK-DELETE =>
	as(<character>, #x7F);
      otherwise => #f;
    end
  //---*** What about the printable symbol characters?
  elseif (vk <= #x5A)		// '0'..'9', 'A'..'Z'
    as(<character>,vk)
  else
    #f
  end
end method virtual-key->character;
