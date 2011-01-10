Module:       duim-utilities
Synopsis:     DUIM utilities
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Protocols

// Given a name, defines an abstract class (no slots, but may have methods
// that are implemented purely by calling functions in the advertised protocol),
// and the predicates.  For example, 'define protocol class sheet (object) end'
// creates the class <sheet>, the generic function 'sheet?', and two methods for
// 'sheet?', one on <sheet> and one on <object>.
define macro protocol-class-definer
  { define protocol-class ?:name (?supers:*) ?slots:* end }
    => { define open abstract class "<" ## ?name ## ">" (?supers)
           ?slots
	 end class;
         define protocol-predicate ?name; }
 slots:
  { } => { }
  { ?slot:*; ... } => { ?slot; ... }
 slot:
  { virtual slot ?:variable, #rest ?options:expression }
    => { virtual slot ?variable, ?options }
end macro protocol-class-definer;

define macro protocol-predicate-definer
  { define protocol-predicate ?:name }
    => { define open generic ?name ## "?" (x) => (true? :: <boolean>);
         define method ?name ## "?" (x :: "<" ## ?name ## ">") => (true? :: <boolean>) #t end;
         define method ?name ## "?" (x :: <object>) => (true? :: <boolean>) #f end; }
end macro protocol-predicate-definer;

define macro protocol-definer
  //--- We don't use the name or supers yet...
  { define protocol ?:name (?supers:*) ?slots-and-generics:* end }
    => { ?slots-and-generics }
 slots-and-generics:
  { } => { }
  { ?slot-or-generic:*; ... }
    => { ?slot-or-generic; ... }
 slot-or-generic:
  { getter ?getter-name:name ?getter-arglist:* => ?values:* }
    => { define open generic ?getter-name ?getter-arglist => ?values }
  { getter ?getter-name:name ?getter-arglist:* }
    => { define open generic ?getter-name ?getter-arglist }
  { setter ?setter-name:name ?setter-arglist:* => ?values:* }
    => { define open generic ?setter-name ?setter-arglist => ?values }
  { setter ?setter-name:name ?setter-arglist:* }
    => { define open generic ?setter-name ?setter-arglist }
  { function ?function-name:name ?function-arglist:* => ?values:* }
    => { define open generic ?function-name ?function-arglist => ?values }
  { function ?function-name:name ?function-arglist:* }
    => { define open generic ?function-name ?function-arglist }
end macro protocol-definer;
