Module:       DUIM-Presentations-Internals
Synopsis:     DUIM presentation system
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// 'accept' and friends

// A list of conses
define thread variable *input-context* :: <list> = #();

define inline function input-context-type
    (context-entry) => (type :: <type>)
  head(context-entry)
end function input-context-type;

define inline function input-context-tag
    (context-entry) => (tag :: <function>)
  tail(context-entry)
end function input-context-tag;


define generic sheet-accept
    (sheet :: <sheet>, type :: <type>, view :: <view>,
     #key default, default-type :: false-or(<type>), provide-default,
	  prompt, prompt-mode)
 => (object, type :: false-or(<type>));

define function accept
    (type :: <type>, sheet :: <sheet>,
     #key view :: false-or(<view>) = #f,
	  default = $unsupplied, default-type :: false-or(<type>) = #f, provide-default = #t,
	  prompt = #t, prompt-mode = #"normal")
 => (object, type :: false-or(<type>))
  //---*** Massage arguments...
  sheet-accept(sheet, type, view,
	       default: default, default-type: default-type, provide-default: provide-default,
	       prompt: prompt, prompt-mode: prompt-mode)
end function accept;

define method sheet-accept
    (sheet :: <presentation-sheet>, type :: <type>, view :: <view>,
     #key default = $unsupplied, default-type :: false-or(<type>) = #f, provide-default = #t,
	  prompt = #t, prompt-mode = #"normal")
 => (object, type :: false-or(<type>))
end method sheet-accept;

define function do-with-input-context
    (type, override, body-function, context-function)
end function do-with-input-context;

define function find-applicable-presentation (...)
end function find-applicable-presentation;

define function highlight-applicable-presentation (...)
end function highlight-applicable-presentation;
