Module:    Win32-GDI
Synopsis:  Manual translation of some macros and other special cases.
Author:    David N. Gray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// integer subranges used internally:
/* ---- doesn't yet work in DFMC:  (DNG 10/29/96)  ???
define constant <U8> = limited(<integer>, min: 0, max: #xFF);
define constant <U16> = limited(<integer>, min: 0, max: #xFFFF);
define constant <U24> = limited(<integer>, min: 0, max: #xFFFFFF);
*/
define constant <U8> = <integer>;
define constant <U16> = <integer>;
define constant <U24> = <integer>;

//  defined in "wingdi.h" as macros:

define inline function RGB(red :: <U8>, green :: <U8>, blue :: <U8> )
			 => value :: <U24>;
  logior ( logior(red, ash(green,8)), ash(blue,16) )
end RGB;

define inline function PALETTERGB(red :: <U8>, green :: <U8>, blue :: <U8>)
	=> value :: <integer>;
  logior(#x02000000, RGB(red,green,blue))
end PALETTERGB;


define inline function PALETTEINDEX(i :: <U16>) => value :: <integer>;
  logior(#x01000000, i)
end PALETTEINDEX;


define inline function GetRValue(rgb :: <integer>) => red :: <U8>;
  logand(#xFF,rgb)
end;

define inline function GetGValue(rgb :: <integer>) => green :: <U8>;
  logand(#xFF, ash(rgb,-8))
end;

define inline function GetBValue(rgb :: <integer>) => blue :: <U8>;
  logand(#xFF, ash(rgb,-16))
end;


// support writing Unicode text; these work on WinDows 95 as well as NT:

define C-function TextOutW
  parameter hdc1       :: <HDC>;
  parameter int2       :: <C-int>;
  parameter int3       :: <C-int>;
  parameter lpcwstr4   :: <LPCWSTR>;
  parameter int5       :: <C-int>;
  result value :: <BOOL>;
  c-name: "TextOutW", c-modifiers: "__stdcall";
end;

define C-function GetTextExtentPoint32W
  parameter hdc1       :: <HDC>;
  parameter lpcwstr2   :: <LPCWSTR>;
  parameter int3       :: <C-int>;
  parameter lpsize4    :: <LPSIZE>;
  result value :: <BOOL>;
  c-name: "GetTextExtentPoint32W", c-modifiers: "__stdcall";
end;
