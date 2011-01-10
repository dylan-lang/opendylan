Module:       DUIM-Presentations-Internals
Synopsis:     DUIM presentation system
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// 'present' and friends

define generic sheet-present
    (sheet :: <sheet>, object, type :: <type>, view :: <view>,
     #key for-context-type)
 => (record);

define function present
    (object, type :: <type>, sheet :: <sheet>,
     #key view :: false-or(<view>) = #f,
	  for-context-type)
 => (record)
  //---*** Massage arguments...
  sheet-present(sheet, object, type, view,
		for-context-type: for-context-type)
end function present;

define method sheet-present
    (sheet :: <presentation-sheet>, object, type :: <type>, view :: <view>,
     #key for-context-type)
 => (record)
end method sheet-present;
