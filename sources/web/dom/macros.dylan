Module:       dom-internals
Synopsis:     Document Object Model
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Generally useful macros

define macro inc!
  { inc! (?place:expression, ?dx:expression) }
    => { ?place := ?place + ?dx; }
  { inc! (?place:expression) }
    => { ?place := ?place + 1; }
end macro inc!;

define macro dec!
  { dec! (?place:expression, ?dx:expression) }
    => { ?place := ?place - ?dx; }
  { dec! (?place:expression) }
    => { ?place := ?place - 1; }
end macro dec!;


define macro max!
  { max! (?place:expression, ?others:*) }
    => { ?place := max(?place, ?others) }
end macro max!;

define macro min!
  { min! (?place:expression, ?others:*) }
    => { ?place := min(?place, ?others) }
end macro min!;


/// Macros for lists only

// 'push!' and 'pop!' are intended to be called only on lists
define macro push!
  { push! (?list:expression, ?item:expression) }
    => { ?list := add!(?list, ?item) }
end macro push!;

// NB: this returns #() -- not #f -- when the list is empty!
define macro pop!
  { pop! (?list:expression) }
    => { begin
           let _result = head(?list);
           ?list := tail(?list);
           _result
	 end }
end macro pop!;
