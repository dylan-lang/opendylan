Module:       duim-gadgets-internals
Synopsis:     DUIM gadgets
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Tab controls

define open abstract class <tab-control>
    (<oriented-gadget-mixin>,
     <key-press-gadget-mixin>,
     <value-gadget-mixin>,
     <basic-gadget>)
  // Are the tabs at the top or the bottom (tab-control or workspace?)
  sealed constant slot tab-control-tabs-position :: <vertical-position> = #"top",
    init-keyword: tabs-position:;
  // The single visible sheet corresponding to the selected "tab"
  sealed slot tab-control-current-page :: false-or(<sheet>) = #f,
    init-keyword: current-page:,
    setter: %current-page-setter;
  // All of sheets that we can select between
  sealed slot tab-control-pages :: <sequence> = #[],
    init-keyword: pages:,
    setter: %pages-setter;
  sealed slot gadget-label-key :: <function> = gadget-label,
    init-keyword: label-key:;
  sealed slot gadget-value-key :: <function> = tab-control-default-value-key,
    init-keyword: value-key:;
end class <tab-control>;

define sealed class <tab-control-page> 
    (<basic-page>, <single-child-wrapping-pane>)
end class <tab-control-page>;

define method initialize (pane :: <tab-control>, #key value) => ()
  next-method();
  unless (tab-control-current-page(pane))
    unless (empty?(tab-control-pages(pane)))
      pane.%current-page := tab-control-pages(pane)[0]
    end
  end
end method initialize;

define sealed domain make (singleton(<tab-control-page>));
define sealed domain initialize (<tab-control-page>);


define method tab-control-current-page-setter
    (child :: false-or(<sheet>), pane :: <tab-control>)
 => (child :: false-or(<sheet>))
  pane.%current-page := child;
  when (child)
    let frame = sheet-frame(pane);
    when (frame & page-initial-focus(child))
      frame-input-focus(frame) := page-initial-focus(child)
    end
  end;
  child
end method tab-control-current-page-setter;

define method tab-control-current-page-setter
    (name :: <string>, pane :: <tab-control>)
 => (name :: <string>)
  let child = tab-control-named-child(pane, name);
  if (child)
    tab-control-current-page(pane) := child
  else
    error("No child named %s in tab control %=", name, pane)
  end
end method tab-control-current-page-setter;

define method tab-control-pages-setter
    (pages :: <sequence>, pane :: <tab-control>, #key page)
 => (pages :: <sequence>)
  let old-pages = tab-control-pages(pane);
  let old-page  = tab-control-current-page(pane);
  let page
    = select (page by instance?)
	<sheet>   => page;
	otherwise => find-tab-control-page(pane, page);
      end
      | old-page;	// Keep old page current if none specified
  let new-page
    = if (member?(page, pages))
	page
      else
	unless (empty?(pages)) pages[0] end
      end;
  case
    old-pages ~= pages =>
      pane.%pages := pages;
      pane.%current-page := new-page;
      note-pages-changed(pane);
    old-page ~= new-page =>
      tab-control-current-page(pane) := new-page;
  end;
  pages
end method tab-control-pages-setter;

define open generic note-pages-changed (pane :: <tab-control>) => ();

define constant $tab-control-default-label :: <string> = "{no-label}";

define method tab-control-labels
    (pane :: <tab-control>) => (labels :: <sequence>)
  let label-key = gadget-label-key(pane);
  map-as(<simple-vector>,
         method (gadget) label-key(gadget) | $tab-control-default-label end,
         tab-control-pages(pane))
end method tab-control-labels;

define method tab-control-named-child
    (pane :: <tab-control>, name :: <string>)
 => (child :: false-or(<sheet>))
  let key = position(tab-control-labels(pane), name, test: \=);
  when (key)
    tab-control-pages(pane)[key];
  end
end method tab-control-named-child;


/// Gadget value handling

define method tab-control-default-value-key
    (page :: <sheet>) => (value)
  #f
end method tab-control-default-value-key;

define method tab-control-default-value-key
    (page :: <labelled-gadget-mixin>) => (value)
  gadget-id(page) | gadget-label(page)
end method tab-control-default-value-key;

define method gadget-value
    (pane :: <tab-control>) => (value)
  let page = tab-control-current-page(pane);
  when (page)
    gadget-value-key(pane)(page)
  end
end method gadget-value;

define method find-tab-control-page
    (pane :: <tab-control>, value) => (page :: false-or(<sheet>))
  let value-key = gadget-value-key(pane);
  block (return)
    for (page in tab-control-pages(pane))
      when (value-key(page) = value)
        return(page)
      end
    end
  end
end method find-tab-control-page;

define method do-gadget-value-setter
    (pane :: <tab-control>, value) => ()
  let page = find-tab-control-page(pane, value);
  tab-control-current-page(pane) := page
end method do-gadget-value-setter;
