Module:    environment-tools
Synopsis:  Environment tools
Author:    Andy Armstrong, Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Property page protocols

define open generic frame-property-types
    (frame :: <environment-frame>,
     class :: subclass(<environment-object>))
 => (types :: <sequence>);

define open generic frame-default-property-type
    (frame :: <environment-frame>,
     class :: subclass(<environment-object>))
 => (type :: false-or(<symbol>));

define open generic make-frame-property-page
    (frame :: <environment-frame>, 
     class :: subclass(<environment-object>),
     type :: <symbol>)
 => (page :: <property-page>);

define open generic make-frame-property-page-displayer
    (frame :: <environment-frame>, 
     class :: subclass(<environment-object>),
     type :: <symbol>)
 => (label :: <string>, displayer :: <displayer-mixin>);

define open generic refresh-frame-property-page
    (frame :: <environment-frame>, page :: <abstract-sheet>,
     object :: false-or(<environment-object>), type :: <symbol>,
     #key clean?, new-thread?)
 => ();

define open generic invalidate-frame-property-page
    (frame :: <environment-frame>, page :: <abstract-sheet>,
     object :: false-or(<environment-object>), type :: <symbol>)
 => ();


/// Default methods

define method make-frame-property-page
    (frame :: <environment-frame>, 
     class :: subclass(<environment-object>),
     type :: <symbol>)
 => (page :: <property-page>)
  with-frame-manager (frame-manager(frame))
    let (label, displayer) 
      = make-frame-property-page-displayer(frame, class, type);
    make(<property-page>,
	 label: label,
	 id: type,
	 child: displayer,
	 input-focus: displayer.displayer-default-input-focus)
  end
end method make-frame-property-page;

define method refresh-frame-property-page
    (frame :: <environment-frame>,
     displayer :: <sheet>,
     object :: false-or(<environment-object>),
     type :: <symbol>,
     #key clean? = #f, new-thread? = #t)
 => ()
  // Do nothing
end method refresh-frame-property-page;

define method invalidate-frame-property-page
    (frame :: <environment-frame>,
     displayer :: <sheet>,
     object :: false-or(<environment-object>),
     type :: <symbol>)
 => ()
  // Do nothing
end method invalidate-frame-property-page;

define method property-page-displayer
    (page :: <property-page>) => (displayer :: <displayer-mixin>)
  sheet-child(page)
end method property-page-displayer;

define method refresh-frame-property-page
    (frame :: <environment-frame>, page :: <property-page>,
     object :: false-or(<environment-object>), type :: <symbol>,
     #key clean? = #f, new-thread? = #t)
 => ()
  let displayer = page.property-page-displayer;
  refresh-frame-property-page
    (frame, displayer, object, type, clean?: clean?, new-thread?: new-thread?)
end method refresh-frame-property-page;

define method refresh-frame-property-page
    (frame :: <environment-frame>, 
     displayer :: <displayer-mixin>,
     object :: false-or(<environment-object>), type :: <symbol>,
     #key clean? = #f, new-thread? = #t)
 => ()
  displayer-object
    (displayer, clean?: clean?, new-thread?: new-thread?) := object
end method refresh-frame-property-page;

define method invalidate-frame-property-page
    (frame :: <environment-frame>, page :: <property-page>,
     object :: false-or(<environment-object>), type :: <symbol>)
 => ()
  let displayer = page.property-page-displayer;
  invalidate-frame-property-page(frame, displayer, object, type)
end method invalidate-frame-property-page;

define method invalidate-frame-property-page
    (frame :: <environment-frame>, 
     displayer :: <displayer-mixin>,
     object :: false-or(<environment-object>), type :: <symbol>)
 => ()
  displayer.displayer-valid? := #f;
end method invalidate-frame-property-page;

/*---*** not currently used
define method environment-property-page-type
    (frame :: <environment-frame>,  pane :: <environment-property-mixin>, 
     page :: <property-page>)
 => (type :: <symbol>)
  let pages = environment-property-pages(pane);
  let class = environment-property-pane-class(pane);
  let types = frame-property-types(frame, class);
  let index
    = find-key(pages, curry(\=, page))
        | error("Unexpectedly can't find the page");
  types[index]
end method environment-property-page-type;

define method find-property-sheet
    (pane :: <property-page>) 
 => (property-sheet :: <environment-property-mixin>)
  find-ancestor-of-type(pane, <environment-property-mixin>)
    | begin
	let frame = sheet-frame(pane);
	assert(instance?(frame, <environment-property-mixin>),
	       "Failed to find environment-property-mixin parent for %=",
	       pane);
	frame
      end
end method find-property-sheet;

define method update-displayer-properties
    (displayer :: <displayer-mixin>) => ()
  let property-page = sheet-parent(displayer);
  let property-sheet = find-property-sheet(property-page);
  let frame = sheet-frame(displayer);
  refresh-frame-properties
    (frame-owner(frame) | frame,
     property-sheet,
     current-object(property-sheet))
end method update-displayer-properties;
*/

/// General property pages

define class <environment-property-pane-state> (<object>)
  constant slot state-pages :: <object-table> = make(<object-table>);
end class <environment-property-pane-state>;

define class <environment-property-mixin> (<object>)
  slot current-object = #f,
    init-keyword: object:;
  constant slot property-pages-cache = make(<table>);
end class <environment-property-mixin>;

define open generic pane-sheet-with-selection
    (pane) => (sheet :: false-or(<sheet>));
    
define method pane-sheet-with-selection
    (pane :: <environment-property-mixin>)
 => (sheet :: false-or(<sheet>))
  let page = property-sheet-current-page(pane);
  if (page)
    pane-sheet-with-selection(page)
  end
end method pane-sheet-with-selection;

define method pane-sheet-with-selection
    (page :: <property-page>)
 => (sheet :: false-or(<sheet>))
  let displayer = page.property-page-displayer;
  pane-sheet-with-selection(displayer)
end method pane-sheet-with-selection;

define class <environment-property-frame> 
    (<environment-fixed-project-frame>,
     <environment-property-mixin>, 
     <property-frame>)
end class <environment-property-frame>;

define pane <environment-property-pane> (<environment-property-mixin>)
  slot environment-property-pane-class :: false-or(<class>) = #f,
    init-keyword: class:,
    setter: %class-setter;
  constant slot %frame = #f,
    init-keyword: frame:;
  constant slot environment-property-pane-activate-callback = #f,
    init-keyword: activate-callback:;
  constant slot environment-property-pane-value-changed-callback = #f,
    init-keyword: value-changed-callback:;
  slot environment-property-pane-page :: false-or(<symbol>) = #f,
    init-keyword: page:,
    setter: %page-setter;
  pane environment-tab-control (pane)
    begin
      let frame = environment-property-pane-frame(pane);
      let page = environment-property-pane-page(pane);
      let (pages, page)
	= make-environment-property-pane-pages(pane, page: page);
      make(<tab-control>,
	   pages: pages,
	   current-page: page,
	   client: pane,
	   value-changed-callback: 
	     method (gadget)
	       ignore(gadget);
	       note-environment-property-pane-page-changed(pane)
	     end)
    end;
  layout (pane) pane.environment-tab-control;
end pane <environment-property-pane>;

define method initialize
    (pane :: <environment-property-pane>, #key) => ()
  next-method();
  unless (environment-property-pane-class(pane))
    let object = environment-property-pane-object(pane);
    unless (object)
      error("Neither object: nor class: initialization keywords specified for %=",
            pane)
    end;
    pane.%class := object-class(object)
  end;
end method initialize;

define method environment-property-pane-page-setter
    (page :: <symbol>, pane :: <environment-property-pane>)
 => (page :: <symbol>)
  unless (page == pane.environment-property-pane-page)
    let gadget = environment-tab-control(pane);
    pane.%page := page;
    gadget-value(gadget, do-callback?: #t) := page
  end;
  page
end method environment-property-pane-page-setter;

define method note-environment-property-pane-page-changed
    (pane :: <environment-property-pane>, #key refresh? = #t) => ()
  let frame = environment-property-pane-frame(pane);
  let gadget = pane.environment-tab-control;
  let page = gadget-value(gadget);
  pane.%page := page;
  let callback = pane.environment-property-pane-value-changed-callback;
  callback & callback(pane);
  refresh? & refresh-environment-property-pane(pane)
end method note-environment-property-pane-page-changed;

define method environment-property-pane-frame
    (pane :: <environment-property-pane>)
 => (frame :: false-or(<environment-fixed-project-frame>))
  pane.%frame | sheet-frame(pane)
end method environment-property-pane-frame;

define method environment-property-pane-class-setter
    (class :: <class>, pane :: <environment-property-pane>,
     #key page, state :: false-or(<environment-property-pane-state>))
 => (class :: <class>)
  let frame = pane.environment-property-pane-frame;
  let tab-control = pane.environment-tab-control;
  let old-page = tab-control.tab-control-current-page;
  let page = page | frame-default-property-type(frame, class);
  if (class == environment-property-pane-class(pane))
    // Just switch the pages and leave it at that
    let keyword
      = select (page by instance?)
	  <symbol> => page;
	  <gadget> => gadget-id(page)
	end;
    gadget-value(tab-control) := keyword
  else
    pane.%class := class;
    let (pages, new-page)
      = make-environment-property-pane-pages(pane, page: page);
    if (pages = tab-control-pages(tab-control))
      gadget-value(tab-control) := gadget-id(new-page)
    else
      tab-control-pages(tab-control, page: new-page) := pages
    end;
  end;
  let page-states = state & state.state-pages;
  if (page-states)
    for (page in tab-control.tab-control-pages)
      let displayer = property-page-displayer(page);
      let keyword = gadget-id(page);
      let page-state = element(page-states, keyword, default: #f);
      if (page-state)
	displayer.displayer-state := page-state
      end
    end
  end;
  if (tab-control.tab-control-current-page ~= old-page)
    note-environment-property-pane-page-changed(pane, refresh?: #f)
  end;
  class
end method environment-property-pane-class-setter;

define method environment-property-pane-object
    (pane :: <environment-property-pane>)
 => (object)
  current-object(pane)
end method environment-property-pane-object;

define method environment-property-pane-object-setter
    (object, pane :: <environment-property-pane>, 
     #key page, state :: false-or(<environment-property-pane-state>))
 => (object)
  environment-property-pane-class(pane, page: page, state: state)
    := object-class(object);
  unless (object == environment-property-pane-object(pane))
    current-object(pane) := object;
    refresh-environment-property-pane(pane)
  end;
  object
end method environment-property-pane-object-setter;

define method make-environment-property-pane-page
    (property-sheet :: <environment-property-mixin>,
     frame :: <environment-fixed-project-frame>, class :: <class>, type)
 => (page :: <property-page>)
  let cache = property-pages-cache(property-sheet);
  let cached-element = element(cache, type, default: #f);
  cached-element
    | begin
        let page = make-frame-property-page(frame, class, type);
        element(cache, type) := page;
        page
      end
end method make-environment-property-pane-page;

define method make-environment-property-pane-pages 
    (pane :: <environment-property-pane>, #key page)
 => (pages :: <sequence>, page :: <sheet>)
  let class = environment-property-pane-class(pane);
  let frame = environment-property-pane-frame(pane);
  let types = frame-property-types(frame, class);
  assert(~empty?(types),
	 "No property pages found for class %=",
	 class);
  let default-type
    = page | frame-default-property-type(frame, class) | types[0];
  let index = find-key(types, curry(\=, default-type));
  assert(index,
	 "Unexpectedly can't find the property type %=",
	 default-type);
  let pages
    = map(method (type :: <symbol>) => (page :: <property-page>)
	    make-environment-property-pane-page(pane, frame, class, type)
	  end,
	  types);
  let page :: <property-page> = pages[index];
  values(pages, page)
end method make-environment-property-pane-pages;

define method refresh-environment-property-pane
    (pane :: <environment-property-pane>, 
     #key pages, clean?, new-thread? = #t, refresh-all? = #f)
 => ()
  let frame = environment-property-pane-frame(pane);
  let object = environment-property-pane-object(pane);
  refresh-frame-properties
    (frame, pane, object, pages: pages, 
     clean?: clean?, new-thread?: new-thread?, refresh-all?: refresh-all?)
end method refresh-environment-property-pane;

define method refresh-frame-properties
    (frame :: <environment-frame>,
     pane :: <environment-property-mixin>, 
     object :: false-or(<environment-object>),
     #key pages, clean?, new-thread? = #t, refresh-all? = #f)
 => ()
  current-object(pane) := object;
  let current-page = property-sheet-current-page(pane);
  let current-keyword = gadget-id(current-page);
  local method refresh-named-page (keyword :: <symbol>) => ()
	  let page = environment-property-page(pane, keyword);
	  if (page)
	    case
	      (refresh-all? | keyword == current-keyword) =>
		refresh-frame-property-page
		  (frame, page, object, keyword, 
		   clean?: clean?, new-thread?: new-thread?);
	      clean? =>
		invalidate-frame-property-page(frame, page, object, keyword);
	      otherwise =>
		#f;
	    end
	  end
	end method refresh-named-page;
  with-busy-cursor (frame)
    case
      pages           => do(refresh-named-page, pages);
      current-keyword => refresh-named-page(current-keyword);
      otherwise       => #f;
    end
  end
end method refresh-frame-properties;

define method environment-property-page
    (pane :: <environment-property-mixin>, name :: <symbol>)
 => (page :: false-or(<sheet>))
  let tab-control = pane.environment-tab-control;
  block (return)
    for (page in tab-control-pages(tab-control))
      if (gadget-id(page) = name)
	return(page)
      end
    end;
    #f
  end
end method environment-property-page;

define method environment-property-pane-state
    (pane :: <environment-property-pane>)
 => (state :: <environment-property-pane-state>)
  let state = make(<environment-property-pane-state>);
  let table = state.state-pages;
  let tab-control = pane.environment-tab-control;
  for (page in tab-control.tab-control-pages)
    let displayer = page.property-page-displayer;
    let keyword = gadget-id(page);
    let page-state = keyword & displayer.displayer-current-state;
    if (page-state)
      table[keyword] := page-state
    end
  end;
  state
end method environment-property-pane-state;

define method execute-displayer-activate-callback
    (pane :: <environment-property-pane>, object) => ()
  let activate-callback = environment-property-pane-activate-callback(pane);
  if (activate-callback)
    activate-callback(pane, object)
  end
end method execute-displayer-activate-callback;

define method execute-displayer-activate-callback
    (gadget :: <gadget>, object) => ()
  let client = gadget-client(gadget);
  if (client)
    execute-displayer-activate-callback(client, object)
  end
end method execute-displayer-activate-callback;

define method execute-displayer-activate-callback
    (gadget :: <property-page>, object) => ()
  let tab-control = find-ancestor-of-type(gadget, <tab-control>);
  if (tab-control)
    execute-displayer-activate-callback(tab-control, object)
  end
end method execute-displayer-activate-callback;

define method make-frame-property-frame
    (frame :: <environment-frame>, object :: false-or(<environment-object>),
     #key title, types, page)
 => (pane :: <property-frame>)
  let class = object-class(object);
  let all-types = frame-property-types(frame, class);
  let types
    = if (types)
        let chosen-types = intersection(types, all-types);
        if (empty?(chosen-types)) all-types else chosen-types end
      else
        all-types
      end;
  let pages 
    = map(method (type)
            make-frame-property-page(frame, class, type)
          end,
          types);
  let title 
    = title
      | format-to-string("%s Properties - %s",
                         environment-object-type-name(object),
                         frame-default-object-name(frame, object));
  let dialog 
    = make(<environment-property-frame>, 
           owner: frame,
           title: title,
           project: frame.frame-project,
           object: object,
           pages: pages,
           page: page,
           calling-frame: frame,
           apply-callback: identity,
           width:  500, height: 400);
  for (page in pages,
       type in types)
    refresh-frame-property-page(dialog, page, object, type, clean?: #t)
  end;
  dialog
end method make-frame-property-frame;

// Delegate pop-up menus to the frame containing this property sheet
define method do-display-environment-popup-menu
    (frame :: <environment-property-frame>, sheet :: <sheet>, object :: <environment-object>, 
     #key x, y) => ()
  let owner = frame-owner(frame);
  when (owner)
    do-display-environment-popup-menu(owner, sheet, object, x: x, y: y)
  end
end method do-display-environment-popup-menu;

define variable *show-properties-frame* = #f;

define sideways method show-properties
    (project :: <project-object>, object :: <environment-object>,
     #key page)
 => ()
  //---*** cpage: This function should either take a frame parameter or
  //              environment-primary-frame() should take a project
  //              parameter.
  let frame = environment-primary-frame();
  *show-properties-frame*
    := display-object-properties(frame, object, frame: *show-properties-frame*,
//---*** cpage: 1997.11.15 I don't think types: should be given here. In particular,
//              #"general" is only applicable to <environment-object>, but #f is
//              a permissible object to browse, and has no property types.
                                 types: #[#"general"],
                                 page: page);
  //---*** Hack for now since the caching doesn't work
  *show-properties-frame* := #f;
end method show-properties;

define method display-object-properties
    (owner :: <environment-frame>, object, #key page, title, frame, types)
 => (pane :: <property-frame>)
  let property-frame
    = frame
        | make-frame-property-frame(owner, object, 
                                    page: page, title: title, types: types);
  if (frame)
    refresh-frame-properties
      (frame, frame, object, clean?: #t, refresh-all?: #t)
  end;
  start-frame(property-frame);
  property-frame
end method display-object-properties;

define method property-sheet-current-page
    (frame :: <property-frame>) => (page :: <property-page>)
  let tab-control = frame-layout(frame);
  tab-control-current-page(tab-control)
end method property-sheet-current-page;

define method property-sheet-current-page
    (pane :: <environment-property-pane>) => (page :: <property-page>)
  let tab-control = environment-tab-control(pane);
  tab-control-current-page(tab-control)
end method property-sheet-current-page;


