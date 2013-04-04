Module:   dfmc-conditions-implementation
Author:   haahr, jonathan, keith, swm
Synopsis: Reporting program conditions.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// A simple, three-tiered approach to the amount of detail a condition
// presents is provided.  Symbols are used to make singleton
// dispatching easy.
//
// TODO: move *detail-level* into the policy object.

define constant <detail-level> = one-of(#"terse", #"normal", #"verbose");

define thread variable *detail-level* :: <detail-level> = #"normal";


// format-condition is just a little gloss over format, but handled
// independent of the format printers, mainly to protect us from that
// interface changing.  It also incorporates the detail level as a
// specializable parameter.
//
// The default method for <program-condition>s takes advantage of the
// fact that all program conditions obey the <format-string-condition>
// (aka <simple-condition>) protocol of having a format string and its
// arguments in the condition itself.  Methods for conditions outside
// the program condition hierarchy just handle the default level by
// using the %s/%= distinction in format.

define generic format-condition
    (stream :: <stream>,
     condition :: <condition>,
     detail-level :: <detail-level>) => ();

/*
define method format-condition
    (stream :: <stream>,
     condition :: <condition>,
     detail-level :: <detail-level>) => ()
  format(stream, "%=", condition);
end method format-condition;

define method format-condition
    (stream :: <stream>,
     condition :: <condition>,
     detail-level == #"terse") => ()
  format(stream, "%s", condition);
end method format-condition;
*/

define method format-condition
    (stream :: <stream>,
     condition :: <program-condition>,
     detail-level :: <detail-level>) => ()
  apply(format /* lisp-format-to-dylan-stream */, stream,
	condition.condition-format-string,
	condition.condition-format-arguments);
  values()
end method format-condition;

define method print-object
    (condition :: <program-condition>, stream :: <stream>) => ()
  format-condition
    (stream, condition, 
     #"terse" /* if (verbose?) *detail-level* else #"terse" end */)
  // The verbose? keyword parameter maps to the default, because that's
  // what %= produces;  %s leads to a false verbose? value, which the
  // condition printing interprets as terse.
end method;
