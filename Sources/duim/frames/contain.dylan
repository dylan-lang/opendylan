Module:       duim-frames-internals
Synopsis:     DUIM frames
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Containers

define protocol <<contain-protocol>> ()
  function contain
    (object, #key, #all-keys) => (object, frame :: <frame>);
  function make-container
    (object, #key, #all-keys) => (frame :: <frame>);
end protocol <<contain-protocol>>;

define variable *contain-uses-own-thread?* :: <boolean> = #f;

define method contain
    (pane, #rest initargs, #key own-thread? = *contain-uses-own-thread?*, #all-keys)
 => (pane :: <sheet>, frame :: <frame>) 
  dynamic-extent(initargs);
  with-keywords-removed (initargs = initargs, #[owner:])
    let container = apply(make-container, pane, initargs);
    local method start-container-frame () => ()
	    with-abort-restart ()
	      start-frame(container)
	    end
	  end method;
    if (own-thread?)
      make(<thread>,
	   name: frame-title(container),
	   function: start-container-frame)
    else
      start-container-frame()
    end;
    values(pane, container)
  end
end method contain;

define method contain
    (class :: <class>, #rest initargs, #key)
 => (pane :: <sheet>, frame :: <frame>)
  dynamic-extent(initargs);
  apply(contain, make(class), initargs)
end method contain;

define method make-container
    (frame :: <frame>, #rest initargs, #key) => (frame :: <frame>)
  ignore(initargs);
  frame
end method make-container;


/// Container frames

define sealed class <container-frame> (<simple-frame>)
  sealed constant slot container-uses-own-thread? :: <boolean> = *contain-uses-own-thread?*,
    init-keyword: own-thread?:;
end class <container-frame>;

define sealed domain make (singleton(<container-frame>));
define sealed domain initialize (<container-frame>);

// Wraps an object into a container frame
define method make-container-frame
    (object, #rest initargs,
     #key own-thread? = *contain-uses-own-thread?*, #all-keys) => (frame :: <frame>)
  dynamic-extent(initargs);
  apply(make, <container-frame>, title: "Container", initargs)
end method make-container-frame;


/// General containers

define method make-container
    (layout :: <layout-pane>, #rest initargs, #key) => (frame :: <frame>)
  dynamic-extent(initargs);
  apply(make-container-frame, layout, layout: layout, initargs)
end method make-container;

define method make-container 
    (sheet :: <sheet>, #rest initargs, #key) => (frame :: <frame>)
  dynamic-extent(initargs);
  apply(make-container, 
        make(<column-layout>, children: vector(sheet)),
        initargs)
end method make-container;


/// Menu containers

define method make-container 
    (menu-bar :: <menu-bar>, #rest initargs, #key) => (frame :: <frame>)
  dynamic-extent(initargs);
  apply(make-container-frame, menu-bar, menu-bar: menu-bar, initargs)
end method make-container;

define method make-container
    (menu :: <menu>, #rest initargs, #key) => (frame :: <frame>)
  dynamic-extent(initargs);
  apply(make-container, make(<menu-bar>, children: vector(menu)), initargs)
end method make-container;

define method make-container
    (component :: <menu-box>, #rest initargs, #key) => (frame :: <frame>)
  dynamic-extent(initargs);
  apply(make-container, 
        make(<menu>, label: "Menu", children: vector(component)), initargs)
end method make-container;

define method make-container 
    (button :: <menu-button>, #rest initargs, #key) => (frame :: <frame>)
  dynamic-extent(initargs);
  apply(make-container, 
        make(<menu>, label: "Menu", children: vector(button)), initargs)
end method make-container;


/// Command table containers

define method make-container 
    (command-table :: <command-table>, #rest initargs, #key) => (frame :: <frame>)
  dynamic-extent(initargs);
  apply(make-container-frame, command-table, 
        command-table: command-table,
        initargs)
end method make-container;


/// Status bar and tool bar containers

define method make-container 
    (status-bar :: <status-bar>, #rest initargs, #key) => (frame :: <frame>)
  dynamic-extent(initargs);
  apply(make-container-frame, status-bar, status-bar: status-bar, initargs)
end method make-container;

define method make-container 
    (tool-bar :: <tool-bar>, #rest initargs, #key) => (frame :: <frame>)
  dynamic-extent(initargs);
  apply(make-container-frame, tool-bar, tool-bar: tool-bar, initargs)
end method make-container;
