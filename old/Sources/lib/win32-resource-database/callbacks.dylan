Module:    win32-resource-database-internal 
Synopsis:  resource database
Synopsis:  public interface to resource database
Author:    Roman Budzianowski
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// It is not an issue here since we have only one callback of each type
// but in general we need
define macro res-callback-definer 
  { define res-callback ?c-fun:name of ?dylan-fun:name ?signature:* end }
  =>
  { define C-callable-wrapper ?c-fun of ?dylan-fun ?signature end }
end;

// Declare callback functions. 

define res-callback EnumTypesProc of EnumTypesFunc
  parameter hModule :: <HANDLE>;
  parameter lpType :: <LPCTSTR>;
  parameter lParam :: <LONG>;
  result val :: <BOOL>;
  c-name: "_EnumTypesFunc", c-modifiers: "__stdcall";
end;
  
define res-callback EnumNamesProc of EnumNamesFunc
  parameter hModule :: <HANDLE>;
  parameter lpType :: <LPCTSTR>;
  parameter lpName :: <LPTSTR>;
  parameter lParam :: <LONG>;
  result val :: <BOOL>;
  c-name: "_EnumNamesFunc", c-modifiers: "__stdcall";
end;

define res-callback EnumLangsProc of EnumLangsFunc
  parameter hModule :: <HANDLE>;
  parameter lpType :: <LPCTSTR>;
  parameter lpName :: <LPCTSTR>;
  parameter wLang :: <WORD>;
  parameter lParam :: <LONG>;
  result val :: <BOOL>;
  c-name: "_EnumLangsFunc", c-modifiers: "__stdcall";
end;

