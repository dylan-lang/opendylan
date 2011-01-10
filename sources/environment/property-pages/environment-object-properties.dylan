Module:    environment-property-pages
Synopsis:  Environment property pages
Author:    Chris Page, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Some useful constants (move into environment-tools?)

define constant $list-separator = ", ";


/// Environment object property pages

define sideways method frame-property-types
    (frame :: <environment-frame>, 
     object :: subclass(<environment-object>))
 => (types :: <list>)
  #(#"general")
end method frame-property-types;

define sideways method frame-default-property-type
    (frame :: <environment-frame>, 
     class :: subclass(<environment-object>))
 => (type :: false-or(<symbol>))
  #"general"
end method frame-default-property-type;


/// environment object general property types

define method general-property-types
    (object :: subclass(<environment-object>))
 => (types :: <list>)
  #(#"name", #"kind", #"source-location", #"contents")
end method general-property-types;


/// environment object general property page

// this is a generic page that shows the object general attributes and
// summary information (name, kind, location, contents, etc.).
//
// the types of information to display are returned by general-property-types(),
// which can be specialized, just like frame-property-types().


// object icon

//---*** cpage: this is possibly a kludge; windows guidelines say that the
//              system font size is 8 point, and observations show that the
//              Explorer property pages use 6 pixels of vertical spacing.
//              It may be that the spacing should be calculated based on
//              the actual system font size (perhaps this is what is meant
//              by "dialog units").
define constant $y-spacing = 6;
//---*** cpage: This value is based upon observing the Explorer property
//              pages.
define constant $x-spacing = 20;

define class <general-properties-state> (<displayer-state>)
end class <general-properties-state>;

define pane <general-properties-displayer> (<displayer-mixin>)
  layout (pane)
    make(<column-layout>, y-spacing: $y-spacing);
end pane <general-properties-displayer>;

define sideways method make-frame-property-page 
    (frame :: <environment-frame>, class :: subclass(<environment-object>),
     type == #"general")
 => (page :: <property-page>)
  with-frame-manager (frame-manager(frame))
    let displayer = make(<general-properties-displayer>);
    make(<property-page>, 
         label: "General", 
         id: type,
         child: displayer)
  end
end method make-frame-property-page;

define sideways method refresh-frame-property-page
    (frame :: <environment-frame>,
     displayer :: <general-properties-displayer>,
     environment-object :: <environment-object>, type == #"general",
     #key clean?, new-thread? = #t)
 => ()
  //---*** We should really update this piecemeal
  ignore(new-thread?);
  let state = displayer.displayer-state;
  let new-state :: <general-properties-state>
    = case
	state & state.displayer-state-object == environment-object =>
	  state;
	otherwise =>
	  displayer.displayer-state
	    := make(<general-properties-state>, object: environment-object);
      end;
  let clean? = ~displayer.displayer-valid? | clean?;
  let new-state? = displayer.displayer-new-state?;
  if (clean? | new-state?)
    let project = frame.ensure-frame-project;
    let module = frame-current-module(frame);
    let property-gadgets = make(<stretchy-vector>);
    let group-items :: <stretchy-vector> = make(<stretchy-vector>);
    let property-types
      = general-property-types(object-class(environment-object));
    local method make-stretchy-label
	      (name :: <string>) => (label :: <label>)
	    make(<label>,
		 label:     name,
		 mnemonic:  #f,
		 min-width: 200, max-width: $fill)
	  end method make-stretchy-label,

          method start-new-section
	      () => ()
	    if (size(property-gadgets) > 0)
	      add!(property-gadgets, make(<separator>));
	    end;
	    group-items.size := 0
	  end method start-new-section,

          method add-label-and-value
	      (label, value :: <string>) => ()
	    let label
	      = if (instance?(label, <sheet>))
		  label
		else
		  make(<label>, label: label, mnemonic: #f)
		end;
	    add!(group-items, 
		 vector(label, make-stretchy-label(value)));
	  end add-label-and-value,

          method finish-section
	      (#key y-alignment = #"top") => ()
	    add!(property-gadgets,
		 make(<table-layout>,
		      contents: copy-sequence(group-items),
		      columns: 2,
		      x-spacing: $x-spacing,
		      y-spacing: $y-spacing,
		      x-alignment: #[#"left", #"left"],
		      y-alignment: y-alignment))
	  end finish-section;

    // Name group
    if (member?(#"name", property-types))
      start-new-section();
      let name = frame-object-unique-name(frame, environment-object);
      let (small-icon, large-icon)
	= environment-object-icon(project, environment-object);
      let image-label
	= if (large-icon)
	    make(<label>,
		 label:  large-icon,
		 width:  32, min-width:  32, max-width:  32,
		 height: 32, min-height: 32, max-height: 32)
	  else
	    with-spacing (thickness: 8)
	      make(<label>,
		   label:  small-icon,
		   width:  16, min-width:  16, max-width:  16,
		   height: 16, min-height: 16, max-height: 16)
	    end
	  end;
      add-label-and-value(image-label, name);
      finish-section(y-alignment: #"center")
    end if;
    // Common properties group
    if (member?(#"kind", property-types)
	| member?(#"source-location", property-types)
	| member?(#"defined-in", property-types)
	| member?(#"exported", property-types)
	| member?(#"contents", property-types)
	| member?(#"address", property-types))
      start-new-section();
      if (member?(#"kind", property-types))
	let kind-label
	  = environment-object-kind-label(frame, environment-object);
	add-label-and-value("Type:", kind-label)
      end if;
      if (member?(#"source-location", property-types))
	let source-location
	  = print-environment-object-location(project, environment-object);
	add-label-and-value("Source:", source-location)
      end if;
      if (member?(#"defined-in", property-types))
	let namespace
	  = print-environment-object-namespace
	      (frame, environment-object, module);
	add-label-and-value("Defined in:", namespace);
      end if;
      if (member?(#"defined-in", property-types))
	let home-name = environment-object-home-name(project, environment-object);
	let exported?
	  = case
	      ~home-name =>
		$n/a;
	      name-exported?(project, home-name) =>
		"yes";
	      otherwise =>
		"no";
	    end;
	add-label-and-value("Exported?:", exported?);
      end if;
      if (member?(#"contents", property-types))
	//---*** Temporarily, if the text is too long, split it into two
	//       labels. Eventually, this should probably use some kind of
	//       multiple line text gadget.
	let contents
	  = environment-object-contents(project, environment-object);
	let (contents-1, contents-2)
	  = if (size(contents) > 50)
	      let separator-pos
		= subsequence-position(contents, $list-separator, count: 4);
	      if (separator-pos)
		let line-break = separator-pos + 2;
		values(copy-sequence(contents, end: line-break),
		       copy-sequence(contents, start: line-break))
	      end
	    else
	      values(contents, #f)
	    end;
	add-label-and-value("Contains:", contents-1);
	contents-2 & add-label-and-value("", contents-2)
      end if;
      finish-section()
    end if;
    // Application objects group
    if (member?(#"address", property-types))
      start-new-section();
      let address
	= print-application-object-address(project, environment-object, module);
      add-label-and-value("Address:", address);
      finish-section()
    end;
    // Integer objects group
    if (member?(#"decimal", property-types))
      start-new-section();
      let decimal
	= number-object-to-string(project, environment-object, 
				  format: #"decimal");
      add-label-and-value("Decimal:", decimal);
      let hex
	= number-object-to-string(project, environment-object, 
				  format: #"hex", prefix?: #t);
      add-label-and-value("Hex:", hex);
      let octal
	= number-object-to-string(project, environment-object, 
				  format: #"octal", prefix?: #t);
      add-label-and-value("Octal:", octal);
      let binary
	= number-object-to-string(project, environment-object, 
				  format: #"binary", prefix?: #t);
      add-label-and-value("Binary:", binary);
      finish-section()
    end;
    // Function parameters, values group
    if (member?(#"parameters", property-types)
	  | member?(#"values", property-types))
      start-new-section();
      if (member?(#"parameters", property-types))
	let parameters 
	  = print-function-parameters(project, environment-object, module);
	add-label-and-value("Parameters:", parameters)
      end if;
      if (member?(#"values", property-types))
	let values
	  = print-function-values(project, environment-object, module);
	add-label-and-value("Values:", values)
      end if;
      finish-section()
    end if;
    let layout = pane-layout(displayer);
    layout.sheet-children := property-gadgets;
    let mapped? = sheet-mapped?(layout);
    //---*** andrewa: what should we really be doing here?
    unless (relayout-parent(layout))
      relayout-children(layout)
    end;
    when (mapped?)
      sheet-mapped?(layout) := #t
    end
  end;
  frame-status-message(frame) := ""
end method refresh-frame-property-page;

define sideways method refresh-frame-property-page
    (frame :: <environment-frame>, 
     displayer :: <general-properties-displayer>,
     environment-object == #f, type == #"general",
     #key clean?, new-thread? = #t)
 => ()
  let layout = pane-layout(displayer);
  displayer.displayer-state := #f;
  sheet-children(layout) := #[];
end method refresh-frame-property-page;

// Home namespace display

define method print-environment-object-namespace
    (frame :: <environment-frame>,
     object :: <environment-object>,
     namespace :: false-or(<namespace-object>))
 => (location :: <string>)
  ignore(frame, object, namespace);
  $not-applicable
end method print-environment-object-namespace;

define method print-environment-object-namespace
    (frame :: <environment-frame>,
     module :: <module-object>,
     namespace :: <namespace-object>)
 => (location :: <string>)
  let project = frame.frame-current-project;
  let library = environment-object-library(project, module);
  if (library)
    frame-object-unique-name(frame, library)
  else
    $not-available
  end
end method print-environment-object-namespace;

define method print-environment-object-namespace
    (frame :: <environment-frame>,
     definition :: <definition-object>,
     namespace :: <namespace-object>)
 => (location :: <string>)
  let project = frame.frame-current-project;
  let definition-home-name = environment-object-home-name(project, definition);
  if (~definition-home-name)
    //---*** cpage: This home name is currently not available for libraries
    //              and modules. I need to figure out how to implement it
    //              in <dfmc-database>. Hopefully, then, this $n/a case
    //              won't be necessary.
    $not-available
  else
    let module = name-namespace(project, definition-home-name);
    //---*** andrewa: is this the right thing to be doing here?
    let module-name
      = environment-object-name(project, module, namespace)
          | environment-object-home-name(project, module);
    if (module-name)
      let library = name-namespace(project, module-name);
      concatenate("module ",
		  environment-object-primitive-name(project, module-name),
		  $list-separator, "library ",
		  environment-object-primitive-name(project, library))
    else
      $unknown
    end
  end if
end method print-environment-object-namespace;


/// Application object property displays

define method print-application-object-address
    (project :: <project-object>,
     application-object :: <application-object>,
     namespace :: false-or(<namespace-object>))
 => (address :: <string>)
  ignore(namespace);
  if (application-tethered?(project))
    let address = application-object-address(project, application-object);
    if (address)
      concatenate("#x", print-environment-object-to-string(project, address))
    end
  end
    | $not-available
end method print-application-object-address;


/// environment-object-kind

define method environment-object-kind-label
    (frame :: <environment-frame>,
     object :: <environment-object>)
 => (label :: <string>)
  environment-object-type-name(object)
end method environment-object-kind-label;

define method environment-object-kind-label
    (frame :: <environment-frame>,
     object :: <variable-object>)
 => (label :: <string>)
  let project = frame.ensure-frame-project;
  let type = environment-object-type(project, object);
  if (type)
    frame-default-object-name(frame, type)
  else
    $n/a
  end
end method environment-object-kind-label;

define method environment-object-kind-label
    (frame :: <environment-frame>,
     object :: <definition-object>)
 => (label :: <string>)
  let project = frame.ensure-frame-project;
  let modifiers = definition-modifiers(project, object);
  let type-name = next-method();
  let kind-name = "";
  for (modifier in modifiers)
    // Convert modifier to string and capitalize
    //--- There's a string-capitalize function in DUIM we should probably use instead
    let modifier-name = as-lowercase(as(<string>, modifier));
    when (size(modifier-name) > 0)
      let char = modifier-name[0];
      when (alpha-char?(char))
        modifier-name[0] := as-uppercase(char);
      end;
    end when;
    kind-name := concatenate-as(<byte-string>, kind-name, modifier-name, " ")
  end for;
  concatenate-as(<byte-string>, kind-name, type-name)
end method environment-object-kind-label;

define method environment-object-kind-label
    (frame :: <environment-frame>,
     object :: <application-object>)
 => (label :: <string>)
  //--- Maybe definition object should override application object
  //--- in the CPL?
  if (instance?(object, <definition-object>))
    next-method()
  else
    let project = frame.ensure-frame-project;
    let class = application-object-class(project, object);
    if (class)
      frame-default-object-name(frame, class)
    else
      next-method()
    end
  end
end method environment-object-kind-label;

