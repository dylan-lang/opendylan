Module:    Win32-Multimedia
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



define C-struct <long-bounds>
  sealed inline-only slot lMinimum-value :: <LONG>;
  sealed inline-only slot lMaximum-value :: <LONG>;
  pack: 1;
end;

define C-struct <dword-bounds>
  sealed inline-only slot dwMinimum-value :: <DWORD>;
  sealed inline-only slot dwMaximum-value :: <DWORD>;
  pack: 1;
end;

define C-union <bounds-union>
  sealed inline-only slot long-bounds-value	 :: <long-bounds>;
  sealed inline-only slot dword-bounds-value	 :: <dword-bounds>;
  sealed inline-only array slot dwReserved-array :: <DWORD>,
	length: 6, address-getter: dwReserved-value;
  pack: 1;
  pointer-type-name: <bounds-union*>;
end;

define sealed inline-only method lMinimum-value ( u :: <bounds-union*> )
 => ( minimum :: <signed-long> )
  u.long-bounds-value.lMinimum-value
end;

define sealed inline-only method lMaximum-value ( u :: <bounds-union*> )
 => ( maximum :: <signed-long> )
  u.long-bounds-value.lMaximum-value
end;

define sealed inline-only method dwMinimum-value ( u :: <bounds-union*> )
 => ( minimum :: <unsigned-long> )
  u.dword-bounds-value.dwMinimum-value
end;

define sealed inline-only method dwMaximum-value ( u :: <bounds-union*> )
 => ( maximum :: <unsigned-long> )
  u.dword-bounds-value.dwMaximum-value
end;




// Hand translations of some complicated macros.

define inline function MCI-MAKE-MSF (m :: <integer>, s :: <integer>,
					  f :: <integer>)
 => ( x :: <integer> )
  logior(LOBYTE(m), ash(LOWORD(s), 8), ash(LOBYTE(f), 16))
end;

define inline constant MCI-MAKE-HMS = MCI-MAKE-MSF;

define inline function MCI-MAKE-TMSF (t :: <integer>, m :: <integer>,
				      s :: <integer>, f :: <integer>)
 => ( x :: <integer> )
  logior(LOBYTE(t), ash(LOWORD(m), 8), 
	 ash(logior(LOBYTE(s), ash(LOWORD(f),8)), 16))
end;

define inline function sndAlias(ch0, ch1) => (value :: <integer>)
  logior($SND-ALIAS-START + LOBYTE(ch0), ash(LOBYTE(ch1), 8))
end;
