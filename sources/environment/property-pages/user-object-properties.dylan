Module:    environment-property-pages
Synopsis:  Environment property pages
Author:    Andy Armstrong, Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// User object property pages

define sideways method frame-property-types
    (frame :: <environment-frame>, class :: subclass(<user-object>))
 => (types :: <list>)
  concatenate(next-method(), #(#"contents"))
end method frame-property-types;

define sideways method frame-default-property-type
    (frame :: <environment-frame>, class :: subclass(<user-object>))
 => (type :: false-or(<symbol>))
  #"contents"
end method frame-default-property-type;


/// Slot wrapper
//
// This object encapsulates a slot name and value

//---*** Can this really be a string?
define constant <slot-getter-type>
  = type-union(<string>, <function-object>);

define sealed class <slot-wrapper> (<object-wrapper>)
  sealed constant slot wrapper-getter :: <slot-getter-type>,
    required-init-keyword: name:;
end class <slot-wrapper>;
  
define sealed domain make (singleton(<slot-wrapper>));
define sealed domain initialize (<slot-wrapper>);

define method frame-object-contents
    (frame :: <environment-frame>, object :: <user-object>)
 => (contents :: <sequence>)
  let project = frame.ensure-frame-project;
  let (names, values) = composite-object-contents(project, object);
  map(method 
	  (name :: <slot-getter-type>, value :: <environment-object>)
       => (wrapper :: <slot-wrapper>)
	make(<slot-wrapper>, name: name, object: value)
      end,
      names, values)
end method frame-object-contents;

define method frame-sort-object-contents
    (frame :: <environment-frame>, contents :: <sequence>,
     order :: <symbol>)
 => (contents :: <sequence>)
  local method contents-label-key (object) => (label :: <string>)
	  frame-print-object-content(frame, object)
	end method contents-label-key;
  select (order)
    #"slot" =>
      frame-sort-items(frame, contents,
		       label-key: contents-label-key);
    #"reverse-slot" =>
      frame-sort-items(frame, contents,
		       label-key: contents-label-key,
		       test: \>);
    #"value" =>
      frame-sort-items(frame, contents,
		       key: wrapper-object,
		       label-key: contents-label-key);
  end
end method frame-sort-object-contents;


/// Property page

define sideways method make-frame-property-page-displayer
    (frame :: <environment-frame>, 
     class :: subclass(<user-object>),
     type == #"contents")
 => (label :: <string>, displayer :: <table-control-displayer>)
  let project = frame.ensure-frame-project;
  let displayer
    = make(<table-control-displayer>,
	   element-label: "slot",
	   information-available?-function: curry(application-tethered?, project),
	   transaction-function: curry(perform-application-transaction, project),
	   children-generator: curry(frame-object-contents, frame),
	   headings: #["Slot", "Value"],
	   widths:   #[200, 1000],
	   generators: vector(identity, wrapper-object),
	   sort-orders: #[#[#"slot", #"reverse-slot"], #"value"],
	   sort-order: #"slot",
	   sort-function: curry(frame-sort-object-contents, frame),
	   label-key: curry(frame-print-object-content, frame));
  values("Contents", displayer)
end method make-frame-property-page-displayer;

define method frame-print-object-content
    (frame :: <environment-fixed-project-frame>, object :: <environment-object>)
 => (string :: <string>)
  frame-print-environment-object(frame, object)
end method frame-print-object-content;

define method frame-print-object-content
    (frame :: <environment-fixed-project-frame>, object :: <slot-wrapper>)
 => (string :: <string>)
  let slot = object.wrapper-getter;
  select (slot by instance?)
    <environment-object> => frame-object-unique-name(frame, slot);
    <string>             => slot;
  end
end method frame-print-object-content;


/// Browsing of slot entries

define method frame-browse-slot-wrapper-getter
    (frame :: <environment-frame>, target :: <command-target>)
 => ()
  let pane = target.target-pane;
  let wrapper = target.target-object;
  let getter = wrapper.wrapper-getter;
  frame-browse-target(frame, target: make-command-target(pane, getter))
end method frame-browse-slot-wrapper-getter;

define function frame-browse-target-getter
    (frame :: <environment-frame>) => ()
  frame-browse-slot-wrapper-getter(frame, frame.frame-command-target)
end function frame-browse-target-getter;

define constant $browse-target-getter-doc
  = "Opens a browser on the slot getter for the selected slot.";

define constant $browse-target-getter-command
  = make-command-decorator("Browse Slot Getter", frame-browse-target-getter,
			   documentation: $browse-target-getter-doc);

define command-table *slot-wrapper-browse-popup-menu-command-table* 
    (*global-command-table*)
  command $describe-target-command;
  command $browse-target-command;
  command $browse-target-type-command;
  command $browse-target-getter-command;
end command-table *slot-wrapper-browse-popup-menu-command-table*;

define command-table *slot-wrapper-popup-menu-command-table* (*global-command-table*)
  include *slot-wrapper-browse-popup-menu-command-table*;
  include *popup-menu-edit-command-table*;
  include *popup-menu-documentation-command-table*;
  include *popup-menu-clipboard-command-table*;
  include *popup-menu-properties-command-table*;
end command-table *slot-wrapper-popup-menu-command-table*;

define method command-table-for-target
    (frame :: <environment-frame>, object :: <slot-wrapper>)
 => (comtab :: <command-table>)
  let slot = object.wrapper-getter;
  if (instance?(slot, <environment-object>))
    *slot-wrapper-popup-menu-command-table*
  else
    next-method()
  end
end method command-table-for-target;
