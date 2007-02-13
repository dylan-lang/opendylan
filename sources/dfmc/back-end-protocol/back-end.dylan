Module:    dfmc-back-end-protocol
Author:    Jonathan Bachrach, Keith Playford
Synopsis:  Compiler-front-end independent back-end
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
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
  constant slot target-architecture :: false-or(<symbol>),
    required-init-keyword: target-architecture:;
  constant slot target-os :: false-or(<symbol>),
    required-init-keyword: target-os:;
end;

define function register-back-end (class :: <class>,
                                   type :: <symbol>,
                                   architecture :: false-or(<symbol>),
                                   os :: false-or(<symbol>)) => ();
    add!($back-end-registry,
       make(<back-end-registry-entry>,
            back-end-class: class,
            back-end-type: type,
            target-architecture: architecture,
            target-os: os));
end;

define function find-back-end (type :: <symbol>,
                               architecture :: <symbol>,
                               os :: <symbol>) => (entry);
  choose(method (x)
           x.back-end-type == type 
             & (~ x.target-architecture | x.target-architecture == architecture)
             & (~ x.target-os | x.target-os == os)
         end, $back-end-registry)
end;

define function find-back-end-object (name :: <symbol>,
                                      architecture :: <symbol>,
                                      os :: <symbol>) => (back-end)
  if (name ~== *cached-back-end-name*)
    let entries = find-back-end(name, architecture, os);
    if (~ empty?(entries))
      *cached-back-end* := make(back-end-class(first(entries)));
      *cached-back-end-name* := name;
    else
      error("Invalid back-end %s", name);
    end;
  end;
  *cached-back-end*
end;

define sideways method current-back-end () => (back-end)
  if (current-library-description())
    let name = current-back-end-name();
    let arch = current-processor-name();
    let os = current-os-name();
    find-back-end-object(name, arch, os)
  end;
end;
