Module:   dfmc-common
Author:   Jonathan Bachrach, Keith Playford, Paul Haahr
Synopsis: Modelled and compile-time objects with names.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// TODO: Name is used willy-nilly in the compiler for all kinds of junk,
// including variable name fragments and probably strings, so I've had to
// open it up completely. 

define constant <some-kind-of-a-name> = <object>;

define compiler-open generic name 
    (object :: <object>) 
 => (name :: <some-kind-of-a-name>);

define compiler-open generic name-setter 
    (name :: <some-kind-of-a-name>, object :: <object>) 
 => (name :: <some-kind-of-a-name>);

define compiler-open generic named? 
    (object :: <object>) => (well? :: <boolean>);

define compiler-open abstract class <named-object> (<object>)
  slot name :: <some-kind-of-a-name> = #f, init-keyword: name:;
end class <named-object>;

define method named? (o :: <named-object>) => (well? :: <boolean>)
  o.name & #t
end method named?;
