Module:    dfmc-back-end-protocol
Author:    Jonathan Bachrach, Keith Playford
Synopsis:  Compiler-front-end independent back-end
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define abstract open class <back-end> (<object>) 
  constant slot mangler = make(<mangler>);
end class;

define abstract open class <lambda-compiled-data> (<object>) 
end class;

define abstract open class <local-variable> (<object>) 
end class;

define constant $back-end-registry = make(<stretchy-vector>);
define thread variable *cached-back-end* :: false-or(<back-end>) = #f;
define thread variable *cached-back-end-name* :: false-or(<symbol>) = #f;

define class <back-end-registry-entry> (<object>)
  constant slot back-end-class :: <class>,
    required-init-keyword: back-end-class:;
  constant slot back-end-type :: <symbol>,
    required-init-keyword: back-end-type:;
  constant slot back-end-platform-name :: false-or(<symbol>),
    required-init-keyword: back-end-platform-name:;
end;

define function register-back-end (class :: <class>,
                                   type :: <symbol>,
                                   platform-name :: false-or(<symbol>)) => ();
    add!($back-end-registry,
       make(<back-end-registry-entry>,
            back-end-class: class,
            back-end-type: type,
            back-end-platform-name: platform-name));
end;

define function find-back-end
    (type :: <symbol>, platform-name :: <symbol>)
 => (entry)
  choose(method (x)
           x.back-end-type == type 
             & (~ x.back-end-platform-name | x.back-end-platform-name == platform-name)
         end, $back-end-registry)
end;

define function find-back-end-object
    (name :: <symbol>, platform-name :: <symbol>)
 => (back-end)
  if (name ~== *cached-back-end-name*)
    let entries = find-back-end(name, platform-name);
    if (~ empty?(entries))
      *cached-back-end* := make(back-end-class(first(entries)));
      *cached-back-end-name* := name;
    else
      error("Compiler back-end %s is not available for %s", name, platform-name);
    end;
  end;
  *cached-back-end*
end;

define sideways method current-back-end () => (back-end)
  let ld = current-library-description();
  if (ld)
    let name = library-description-compiler-back-end-name(ld);
    let platform-name = library-description-platform-name(ld);
    find-back-end-object(name, platform-name)
  end;
end;
