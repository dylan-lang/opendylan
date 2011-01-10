Module:    environment-debugger
Author:    Jason Trenouth, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant my-debug-message :: <function> = always(#t);
// define constant my-debug-message :: <function> = debug-message;

/// $DEBUGGER-STACK-DOC

define constant $debugger-stack-doc :: <string> = "Stack";

/// <DEBUGGER-STACK-PANE> (internal)

define constant $stack-filters
  = #[#["All frames",              #"frames"],
      #["All visible frames",      #"visible"],
      #["All local frames",        #"local"],
      #["Filtered frames",         #"filtered"],
      #["Filtered visible frames", #"filtered-visible"],
      #["Filtered local frames",   #"filtered-local"]];


define sealed pane <debugger-stack-pane> ()
  pane %displayer (pane)
    begin
      // *debugger* is dynamic-bound but we need to close over its value.
      let debugger = *debugger*;
      let project = debugger.ensure-frame-project;
      make(<filtering-tree-control-displayer>,
	   element-label: "stack frame",
	   documentation: $debugger-pane-tooltips? & $debugger-stack-doc,
	   selection-mode: #"single",
	   transaction-function: curry(perform-application-transaction, project),
	   children-generator: curry(get-stack-pane-node-children, debugger),
	   children-predicate: curry(stack-pane-node-children?, debugger),
	   depth: if ($debugger-settings.expand-backtrace-initially)
		    1
		  else
		    0
		  end,
	   filter-types: $stack-filters,
	   filter-type: select ($debugger-settings.stack-modules)
			  #"current" => #"filtered-local";
			  #"used"    => #"filtered-visible";
			  #"all"     => #"filtered";
			end,
	   filter-function: curry(filter-stack-contents, debugger),
	   filter-type-only?: #t,
	   icon-function: curry(stack-frame-node-icon, debugger),
	   information-available?-function: curry(application-tethered?, project),
	   label-key: curry(stack-pane-node-label, debugger),
	   always-show-selection?: #t,
	   popup-menu-callback: display-environment-popup-menu,
	   value-changed-callback: method (displayer, value)
				     ignore(displayer);
				     stack-pane-node-select(debugger, value)
				   end,
	   activate-callback: curry(environment-activate-callback, debugger))
    end;
  layout (pane)
    pane.%displayer
end pane <debugger-stack-pane>;


/// DEBUGGER-STACK-DISPLAYER (internal)

define inline function debugger-stack-displayer
    (debugger :: <debugger>)
 => (displayer :: <filtering-tree-control-displayer>)
  let stack-pane = debugger.debugger-stack-pane;
  stack-pane.%displayer
end function debugger-stack-displayer;


/// DEBUGGER-STACK-GADGET (internal)

define inline function debugger-stack-gadget
    (debugger :: <debugger>) => (stack-gadget :: <tree-control>)
  displayer-collection-gadget(debugger.debugger-stack-displayer)
end function debugger-stack-gadget;


/// DEBUGGER-STACK (internal)

define method debugger-stack
    (debugger :: <debugger>) => (stack :: <sequence>)
  let displayer = debugger.debugger-stack-displayer;
  let thread = debugger.debugger-thread;
  displayer-items(displayer, thread)
end method debugger-stack;


/// DEBUGGER-FILTERED-STACK (internal)

define method debugger-filtered-stack
    (debugger :: <debugger>) => (stack :: <sequence>)
  let displayer = debugger.debugger-stack-displayer;
  let thread = debugger.debugger-thread;
  tree-control-displayer-children(displayer, thread)
end method debugger-filtered-stack;


/// UPDATE-DEBUGGER-STACK-PANE (internal)

define function update-debugger-stack-pane 
    (debugger :: <debugger>, #key refresh? :: <boolean> = #f) => ()
  let project = debugger.ensure-frame-project;
  let application = project.project-application;
  let thread = debugger.debugger-thread;
  let stack-gadget = debugger.debugger-stack-gadget;

  local
    method node-expanded?
	(node :: <tree-node>) => (expanded? :: <boolean>)
      node-state(node) == #"expanded"
    end method node-expanded?,

    method first-call-frame-expanded?
	() => (expanded? :: <boolean>)
      let (first-call-frame-wrapper, call-frame-index)
        = debugger.debugger-first-call-frame-wrapper;
      ignore(first-call-frame-wrapper);
      if (call-frame-index)
	let thread-node = find-node(stack-gadget, thread);
	let first-call-frame-node
	  = node-expanded?(thread-node)
	      & element(node-children(thread-node), call-frame-index,
			default: #f);
	first-call-frame-node & node-expanded?(first-call-frame-node)
      end
    end method first-call-frame-expanded?,

    method refresh-stack-trace (debugger :: <debugger>)
      let stepping? = application & application-just-stepped?(application, thread);
      let expand-first-call-frame?
	= $debugger-settings.expand-first-frame
	    | first-call-frame-expanded?();
      let displayer = debugger.debugger-stack-displayer;
      local method select-first-call-frame
		() => ()
	      //--- Select the first node in the tree if there is no current
	      //--- selection, or if we are stepping.
	      let value = gadget-value(stack-gadget);
	      if (~value
		    | instance?(value, <thread-object>)
		    | stepping?)
		debugger-select-first-call-frame
		  (debugger, expand?: expand-first-call-frame? | stepping?)
	      else
		debug-message("Not selecting first call frame!")
	      end
	    end method select-first-call-frame;
      if (displayer.displayer-object == project)
	refresh-displayer(displayer, after-function: select-first-call-frame)
      else
	displayer-object(displayer, after-function: select-first-call-frame)
	  := project
      end;
    end method refresh-stack-trace;
  
  // Only update the stack gadget if it is showing and the debugger is enabled
  if (sheet-mapped?(stack-gadget) & debugger.debugger-enabled?)
    execute-debugger-function(refresh-stack-trace, debugger,
			      message: "Refreshing stack...")
  end
end function update-debugger-stack-pane;


/// DEBUGGER-SELECT-FIRST-CALL-FRAME (internal)

define function debugger-select-first-call-frame
    (debugger :: <debugger>, #key expand? :: <boolean> = #t) => ()
  let thread = debugger.debugger-thread;
  let stack-gadget = debugger.debugger-stack-gadget;
  if (sheet-mapped?(stack-gadget))
    let thread-node = find-node(stack-gadget, thread);
    if (thread-node)
      expand-node(stack-gadget, thread-node);
      let (first-call-frame-wrapper, first-call-frame-index)
	= debugger.debugger-first-call-frame-wrapper;
      if (expand? & first-call-frame-index)
	let frame-node
	  = element(node-children(thread-node), first-call-frame-index,
		    default: #f);
	if (frame-node)
	  expand-node(stack-gadget, frame-node)
	else
	  debug-message("Failed to find a frame node!")
	end
      else
	if (expand?)
	  debug-message("Failed to find the first call frame index!")
	end
      end;
      unless (first-call-frame-wrapper)
	debug-message("Failed to find first call frame wrapper!")
      end;
      gadget-value(stack-gadget, do-callback?: #t) := first-call-frame-wrapper
    else
      let project = debugger.ensure-frame-project;
      debug-message
	("Failed to find node for thread '%s' -- not selecting first frame",
	 frame-default-object-name(debugger, thread))
    end
  end
end function debugger-select-first-call-frame;


/// DEBUGGER-FIRST-CALL-FRAME-WRAPPER (internal)

define function debugger-first-call-frame-wrapper
    (debugger :: <debugger>, #key stack)
 => (wrapper :: false-or(<stack-frame-wrapper>), index :: false-or(<integer>))
  block (return)
    let frame-wrappers = stack | debugger.debugger-filtered-stack;
    let project :: <project-object> = debugger.ensure-frame-project;
    for (wrapper :: <stack-frame-wrapper> in frame-wrappers,
	 index :: <integer> from 0)
      let type = wrapper.wrapper-type;
      when (member?(type, #[#"dylan-call", #"internal-call", #"foreign-call"]))
	return(wrapper, index)
      end
    end;
    #f
  end
end function debugger-first-call-frame-wrapper;


/// DEBUGGER-STACK-FRAME-TYPES (internal)

define constant $debugger-stack-frame-types :: <vector>
  = #[#"dylan-call", #"internal-call", #"foreign-call", #"cleanup",
      #"unknown"];


/// STACK-FRAME-FUNCTION-NAME-VISIBILITY (internal)

define function stack-frame-function-name-visibility
    (debugger :: <debugger>, function :: <application-code-object>)
 => (visibility :: <symbol>)
  let project :: <project-object> = debugger.ensure-frame-project;
  let generic-function
    = if (instance?(function, <method-object>))
	let generic-function = method-generic-function(project, function);
	my-debug-message("  Method has %s GF",
			 if (generic-function) "a" else "no" end);
	generic-function
      end;
  let function = generic-function | function;
  let current-module :: false-or(<module-object>)
    = frame-current-module(debugger);
  // 'name' will be non-#f iff 'function' is visible in 'current-module'.
  let name :: false-or(<name-object>)
    = current-module
        & environment-object-name(project, function, current-module);
  // 'home-name' will be non-#f iff 'function' has a name in some module.
  let home-name :: false-or(<name-object>)
    = environment-object-home-name(project, function);
  my-debug-message
    ("  Home-name: %s",
     if (home-name) environment-object-primitive-name(project, home-name)
     else "none" end);
  my-debug-message
    ("  Name: %s",
     if (name) environment-object-primitive-name(project, name)
     else "none" end);
  let local? = name & current-module == name-namespace(project, name);
  case
    local?    => #"local";
    name      => #"visible";
    home-name => #"not-visible";
    otherwise => #"no-name";
  end
end function stack-frame-function-name-visibility;


/// SHOW-STACK-FRAME-TYPE? (internal)

define function show-stack-frame-type? 
    (debugger :: <debugger>, wrapper :: <stack-frame-wrapper>)
 => (show? :: <boolean>)
  let type = wrapper.wrapper-type;
  let show-types = $debugger-settings.stack-show-frame-types;
  my-debug-message("  Type: %= [show types %=]",
		   type, show-types);
  member?(type, show-types)
    | type == #"initialization-call"
end function show-stack-frame-type?;


/// SHOW-STACK-FRAME-TEXT? (internal)

define function show-stack-frame-text? 
    (debugger :: <debugger>, wrapper :: <stack-frame-wrapper>)
 => (show? :: <boolean>)
  let frame-name = stack-pane-node-label(debugger, wrapper);
  let include = $debugger-settings.stack-include;
  let exclude = $debugger-settings.stack-exclude;
  (empty?(include) | subsequence-position(frame-name, include))
    & (empty?(exclude) | ~subsequence-position(frame-name, exclude))
end function show-stack-frame-text?;


/// STACK-FRAME-WRAPPER
//
// This object wraps up a stack frame, recording extra information
// useful to the debugger stack pane.

define sealed class <stack-frame-wrapper> (<object-wrapper>)
  constant sealed slot wrapper-type :: <symbol>,
    required-init-keyword: type:;
  constant sealed slot wrapper-visibility :: <symbol>,
    required-init-keyword: visibility:;
end class <stack-frame-wrapper>;
  
define sealed domain make (singleton(<stack-frame-wrapper>));
define sealed domain initialize (<stack-frame-wrapper>);


/// STACK-PANE-NODE-CHILDREN? (internal)

define sealed method stack-pane-node-children?
    (debugger :: <debugger>, project :: <project-object>)
 => (contents? == #t)
  #t
end method stack-pane-node-children?;

define sealed method stack-pane-node-children?
    (debugger :: <debugger>, thread :: <thread-object>)
 => (contents? == #t)
  #t
end method stack-pane-node-children?;

define sealed method stack-pane-node-children? 
    (debugger :: <debugger>, wrapper :: <stack-frame-wrapper>)
 => (contents? :: <boolean>)
  select (wrapper.wrapper-type)
    #"dylan-call", #"internal-call", #"initialization-call", #"foreign-call" =>
      let frame :: <stack-frame-object> = wrapper.wrapper-object;
      let project = debugger.ensure-frame-project;
      stack-frame-local-variable-count(project, frame) > 0;
    #"cleanup", #"unknown" =>
      #f;
  end select
end method stack-pane-node-children?;

define sealed method stack-pane-node-children? 
    (debugger :: <debugger>, local-variable :: <local-variable-object>)
 => (contents? == #f)
  #f
end method stack-pane-node-children?;


/// GET-STACK-PANE-NODE-CHILDREN (internal)

define sealed method get-stack-pane-node-children
    (debugger :: <debugger>, project :: <project-object>)
 => (contents :: <sequence>)
  vector(debugger.debugger-thread)
end method get-stack-pane-node-children;

define sealed method get-stack-pane-node-children 
    (debugger :: <debugger>, wrapper :: <stack-frame-wrapper>)
 => (contents :: <sequence>)
  select (wrapper.wrapper-type)
    #"dylan-call", #"internal-call", #"initialization-call", #"foreign-call" =>
      let project = debugger.ensure-frame-project;
      let frame :: <stack-frame-object> = wrapper.wrapper-object;
      stack-frame-local-variables(project, frame);
    #"cleanup", #"unknown" =>
      #[];
  end select
end method get-stack-pane-node-children;

define sealed method get-stack-pane-node-children
    (debugger :: <debugger>, thread :: <thread-object>)
 => (contents :: <sequence>)
  let project = debugger.ensure-frame-project;

  map(method (frame :: <stack-frame-object>)
	let type = stack-frame-derived-type(project, frame);
	let visibility 
	  = if (type == #"dylan-call")
	      let function = stack-frame-function(project, frame);
	      stack-frame-function-name-visibility(debugger, function)
	    else
	      #"local"
	    end;
	make(<stack-frame-wrapper>,
	     object: frame,
	     type: type,
	     visibility: visibility)
      end,
      thread-complete-stack-trace(project, thread))
end method get-stack-pane-node-children;


/// FILTER-STACK-CONTENTS (internal)

define sealed method filter-stack-contents
    (debugger :: <debugger>, contents :: <sequence>,
     type-filter :: <symbol>, substring-filter :: <string>)
 => (contents :: <sequence>)
  let no-filter? = empty?(substring-filter);
  local 
    method visible?
	(wrapper :: <stack-frame-wrapper>,
	 #key filtered? :: <boolean> = #f,
	      visible?  :: <boolean> = #f,
	      local?    :: <boolean> = #f)
     => (visible? :: <boolean>)
      let visibility = wrapper.wrapper-visibility;
      (~filtered? 
	 | (show-stack-frame-type?(debugger, wrapper)
	      & show-stack-frame-text?(debugger, wrapper)))
	& (~visible? | visibility == #"local" | visibility == #"visible")
	& (~local?   | visibility == #"local")
    end method visible?,

    method object-matches-type-filter? 
	(wrapper :: <stack-frame-wrapper>) => (matches? :: <boolean>)
      select (type-filter)
	#"frames"           => #t;
	#"visible"          => visible?(wrapper, visible?: #t);
	#"local"            => visible?(wrapper, local?: #t);
	#"filtered"         => visible?(wrapper, filtered?: #t);
	#"filtered-visible" => visible?(wrapper, filtered?: #t, visible?: #t);
	#"filtered-local"   => visible?(wrapper, filtered?: #t, local?: #t);
      end
    end method object-matches-type-filter?,

    method object-matches-substring-filter?
	(wrapper :: <stack-frame-wrapper>) => (matches? :: <boolean>)
      no-filter?
        | begin
	    let label = stack-pane-node-label(debugger, wrapper);
	    subsequence-position(label, substring-filter) ~= #f
	  end
    end method object-matches-substring-filter?,
    
    method show-object?
	(object :: <object>) => (show? :: <boolean>)
      ~instance?(object, <stack-frame-wrapper>)
	| object == debugger-first-call-frame-wrapper(debugger, stack: contents)
	| (object-matches-type-filter?(object)
	     & object-matches-substring-filter?(object))
    end method show-object?;

  let results = make(<stretchy-vector>);

  for (object in contents)
    if (show-object?(object))
      add!(results, object)
    end
  end;

  results
end method filter-stack-contents;


/// STACK-FRAME-DERIVED-TYPE (internal)
///
///---*** This probably should be in dfmc-environment-application

define function stack-frame-derived-type
    (project :: <project-object>, frame :: <stack-frame-object>)
 => (type :: <symbol>)
  let type = stack-frame-type(project, frame);
  if (type == #"dylan-call")
    let function = stack-frame-function(project, frame);
    case
      function =>
	if (instance?(function, <internal-method-object>))
	  #"internal-call"
	else
	  #"dylan-call"
	end;
      stack-frame-source-location(project, frame) =>
	#"initialization-call";
      otherwise =>
	#"internal-call";
    end
  else
    type
  end
end function stack-frame-derived-type;


/// STACK-FRAME-OVERRIDE-NAME (internal)

define sealed method stack-frame-override-name
    (project :: <project-object>, frame :: <stack-frame-object>, #key type)
 => (name :: false-or(<byte-string>))
  let function = stack-frame-function(project, frame);
  let anonymous? = function & ~environment-object-home-name(project, function);
  let type = type | stack-frame-derived-type(project, frame);
  select (type)
    #"initialization-call" => "Initialization frame";
    anonymous?             => "Anonymous method";
    otherwise              => #f;
  end
end method stack-frame-override-name;


/// STACK-PANE-NODE-LABEL (internal)
///
/// ---*** ENVIRONMENT: need to add knowledge of args/other-locals to env printing code

define sealed method stack-pane-node-label
    (debugger :: <debugger>, thread :: <thread-object>)
 => (name :: <byte-string>)
  frame-default-object-name(debugger, thread)
end method stack-pane-node-label;

define sealed method stack-pane-node-label 
    (debugger :: <debugger>, wrapper :: <stack-frame-wrapper>)
 => (name :: <byte-string>)
  let project  = debugger.ensure-frame-project;
  let frame :: <stack-frame-object> = wrapper.wrapper-object;
  let function = stack-frame-function(project, frame);
  let type = wrapper.wrapper-type;
  stack-frame-override-name(project, frame, type: type)
    | frame-default-object-name(debugger, function | frame)
end method stack-pane-node-label;

define sealed method stack-pane-node-label
    (debugger :: <debugger>, variable :: <local-variable-object>)
 => (name :: <byte-string>)
  let project = debugger.ensure-frame-project;
  let value   = variable-value(project, variable);
  let module  = debugger.frame-current-module;
  format-to-string
    ("%s = %s",
     frame-default-object-name(debugger, variable),
     //--- Don't use the cached version here, since the contents
     //--- can change making the object's representation change.
     //--- e.g. collections print with their elements.
     print-environment-object-to-string(project, value, namespace: module))
end method stack-pane-node-label;


/// STACK-PANE-NODE-SELECT (internal)

define function stack-pane-node-select
    (debugger :: <debugger>, object :: <object>) => ()
  let frame :: false-or(<stack-frame-object>)
    = select (object by instance?)
	<thread-object> =>
	  #f;
	<local-variable-object> =>
	  debugger-local-variable-stack-frame(debugger, object);
	<stack-frame-wrapper> =>
	  object.wrapper-object
      end;
  execute-debugger-function
    (method (debugger :: <debugger>)
       let displayer = debugger.debugger-source-pane.%source-displayer;
       refresh-frame-property-page(debugger, displayer, frame, #"source");
       update-debugger-register-window(debugger)
     end,
     debugger,
     message: "Refreshing source ...")
end function stack-pane-node-select;


/// DEBUGGER-LOCAL-VARIABLE-STACK-FRAME (internal)

define function debugger-local-variable-stack-frame
    (debugger :: <debugger>, object :: <local-variable-object>)
 => (stack-frame :: false-or(<stack-frame-object>))
  // Use the stack frame that contains the local variable
  let stack-gadget = debugger.debugger-stack-gadget;
  let nodes = node-parents(find-node(stack-gadget, object));
  ~empty?(nodes) & nodes[0].node-object.wrapper-object
end function debugger-local-variable-stack-frame;


/// DEBUGGER-FIRST-FRAME (internal)

define function debugger-first-frame
    (debugger :: <debugger>)
 => (stack-frame :: false-or(<stack-frame-object>))
  let stack = debugger.debugger-filtered-stack;
  let wrapper = element(stack, 0, default: #f);
  wrapper & wrapper.wrapper-object
end function debugger-first-frame;


/// STACK-PANE-CHANGE-FRAME (internal)
///
/// As if user selected on element in stack pane

define function stack-pane-change-frame
    (debugger :: <debugger>, wrapper :: <stack-frame-wrapper>)
  let stack-gadget = debugger.debugger-stack-gadget;
  gadget-value(stack-gadget) := wrapper;
  stack-pane-node-select(debugger, wrapper);
end function stack-pane-change-frame;
  

/// DEBUGGER-CURRENT-STACK-FRAME (internal)
///
/// ---*** DEBUGGER: could remember current frame by noting gestures
 
define function debugger-current-stack-frame 
    (debugger :: <debugger>)
 => (frame :: false-or(<stack-frame-object>))
  let stack-gadget = debugger.debugger-stack-gadget;
  let wrapper = stack-gadget.gadget-value;
  let object
    = if (instance?(wrapper, <stack-frame-wrapper>))
	wrapper.wrapper-object
      else
	wrapper
      end;
  select (object by instance?)
    <stack-frame-object> =>
      object;
    <thread-object> =>
      debugger.debugger-first-frame;
    <local-variable-object> =>
      debugger-local-variable-stack-frame(debugger, object);
    otherwise =>
      #f;
  end
end function debugger-current-stack-frame;


/// DEBUGGER-STEPPING-STACK-FRAME (internal)
 
define function debugger-stepping-stack-frame 
    (debugger :: <debugger>) => (frame :: <stack-frame-object>)
  debugger.debugger-current-stack-frame
    | debugger.debugger-first-frame
    | begin
	let wrapper = debugger.debugger-stack[0];
	wrapper.wrapper-object
      end
end function debugger-stepping-stack-frame;


/// DEBUGGER-STEPPING-OUT-STACK-FRAME (internal)

// This function differs from debugger-stepping-stack-frame by trying
// to make sure that the function you step out to is actually one that
// will be visible with the current filtering.

define function debugger-stepping-out-stack-frame 
    (debugger :: <debugger>) => (frame :: <stack-frame-object>)
  let stack = debugger.debugger-stack;
  let filtered-stack = debugger.debugger-filtered-stack;
  let start-frame = debugger.debugger-stepping-stack-frame;
  let filtered-key = position(filtered-stack, start-frame);
  if (filtered-key & filtered-key < size(filtered-stack) - 1)
    let step-out-wrapper = filtered-stack[filtered-key + 1];
    let unfiltered-key = position(stack, step-out-wrapper);
    if (unfiltered-key & ~zero?(unfiltered-key))
      let wrapper = stack[unfiltered-key - 1];
      let function = wrapper.wrapper-object;
      my-debug-message("Stepping out to %s",
		       stack-pane-node-label(debugger, wrapper));
      function
    end
  end
    // Just give up and use the selected frame
    | start-frame
end function debugger-stepping-out-stack-frame;


/// STACK-FRAME-TYPE-LABEL (internal) 

define function stack-frame-type-label (type :: <symbol>)
 => (label :: <string>)
  select (type)
    #"dylan-call"          => "Dylan function calls";
    #"initialization-call" => "Dylan initialization calls";
    #"internal-call"       => "Internal Dylan function calls";
    #"foreign-call"        => "Foreign function calls";
    #"cleanup"             => "Cleanup frames";
    #"unknown"             => "Unknown stack frame types";
  end select;
end function stack-frame-type-label;


/// $DEBUGGER-STACK-FRAME-MODULES (internal)

define constant $debugger-stack-frame-modules :: <vector>
  = #[#"current", #"used", #"all"];


/// STACK-FRAME-MODULES-LABEL (internal)

define function stack-frame-modules-label (modules :: <symbol>)
  select (modules)
    #"current" => "Current module";
    #"used"    => "Current module and imported from used modules";
    #"all"     => "All modules";
  end select;
end function stack-frame-modules-label;


/// <OPTIONS-STACK-PAGE> (internal)

define sealed pane <options-stack-page> ()
  pane stack-filter-dialog-types-box (dialog)
    make(<check-box>,
	 orientation: #"vertical",
	 items: $debugger-stack-frame-types,
	 label-key: stack-frame-type-label);
  pane stack-filter-dialog-modules-box (dialog)
    make(<radio-box>,
	 orientation: #"vertical",
	 items: $debugger-stack-frame-modules,
	 label-key: stack-frame-modules-label);
  pane stack-filter-dialog-include-field (dialog)
    make(<text-field>);
  pane stack-filter-dialog-exclude-field (dialog)
    make(<text-field>);
  layout (dialog)
    vertically (spacing: $vertical-spacing)
      grouping ("Show stack frames of types", max-width: $fill)
	dialog.stack-filter-dialog-types-box
      end;
      grouping ("Show stack frames from modules", max-width: $fill)
	dialog.stack-filter-dialog-modules-box
      end;
      grouping ("Show stack frames matching", max-width: $fill)
	make(<table-layout>,
	     x-spacing: $vertical-spacing,
	     y-spacing: $vertical-spacing,
	     contents:
	       vector(vector(make(<label>, label: "Include"),
			     dialog.stack-filter-dialog-include-field),
		      vector(make(<label>, label: "Exclude"),
			     dialog.stack-filter-dialog-exclude-field)))
      end;
    end;
end pane <options-stack-page>;

define sealed domain make (singleton(<options-stack-page>));
define sealed domain initialize (<options-stack-page>);


/// INITIALIZE-PAGE (internal)

define sealed method initialize-page 
    (debugger :: <debugger>, dialog :: <options-stack-page>)
 => ()
  ignore(debugger);
  dialog.stack-filter-dialog-include-field.gadget-value
    := $debugger-settings.stack-include;
  dialog.stack-filter-dialog-exclude-field.gadget-value
    := $debugger-settings.stack-exclude;
  dialog.stack-filter-dialog-modules-box.gadget-value
    := $debugger-settings.stack-modules;
  dialog.stack-filter-dialog-types-box.gadget-value
    := $debugger-settings.stack-show-frame-types;
end method initialize-page;


/// OPTIONS-PAGE-NAME (internal)

define sealed method options-page-name 
    (page :: <options-stack-page>) => (name :: <string>)
  "Stack"
end method options-page-name;


/// UPDATE-FROM-PAGE (internal)

define sealed method update-from-page 
    (debugger :: <debugger>, page :: <options-stack-page>)
  let types = page.stack-filter-dialog-stack-frame-types;
  let dylan-calls?    = member?(#"dylan-call",    types);
  let internal-calls? = member?(#"internal-call", types);
  let foreign-calls?  = member?(#"foreign-call",  types);
  let cleanup-frames? = member?(#"cleanup",       types);
  let unknown-frames? = member?(#"unknown",       types);
  $debugger-settings.stack-show-dylan-calls    := dylan-calls?;
  $debugger-settings.stack-show-internal-calls := internal-calls?;
  $debugger-settings.stack-show-foreign-calls  := foreign-calls?;
  $debugger-settings.stack-show-cleanup-frames := cleanup-frames?;
  $debugger-settings.stack-show-unknown-frames := unknown-frames?;
  $debugger-settings.stack-include := page.stack-filter-dialog-include | "";
  $debugger-settings.stack-exclude := page.stack-filter-dialog-exclude | "";
  $debugger-settings.stack-modules := page.stack-filter-dialog-modules;
  refresh-frame(debugger)
end method update-from-page;


/// STACK-FILTER-DIALOG-STACK-FRAME-TYPES (internal)

define function stack-filter-dialog-stack-frame-types
    (dialog :: <options-stack-page>)
 => (types :: <sequence>)
  dialog.stack-filter-dialog-types-box.gadget-value;
end function stack-filter-dialog-stack-frame-types;


/// STACK-FILTER-DIALOG-INCLUDE (internal) 

define function stack-filter-dialog-include 
    (dialog :: <options-stack-page>)
 => (include :: false-or(<string>))
  non-empty-string(dialog.stack-filter-dialog-include-field.gadget-value)
end function stack-filter-dialog-include;


/// STACK-FILTER-DIALOG-EXCLUDE (internal)

define function stack-filter-dialog-exclude 
    (dialog :: <options-stack-page>)
 => (exclude :: false-or(<string>))
  non-empty-string(dialog.stack-filter-dialog-exclude-field.gadget-value)
end function stack-filter-dialog-exclude;


/// STACK-FILTER-DIALOG-MODULES (internal)

define function stack-filter-dialog-modules 
    (dialog :: <options-stack-page>)
 => (modules :: <symbol>)
  dialog.stack-filter-dialog-modules-box.gadget-value;
end function stack-filter-dialog-modules;


/// STACK-FRAME-NODE-ICON (internal)
///
/// ---*** force return #f as second arg to avoid compiler corruption

define method stack-frame-node-icon 
    (debugger :: <debugger>, object)
 => (small-icon, large-icon)
  environment-object-icon(debugger.ensure-frame-project, object)
end method stack-frame-node-icon;

define method stack-frame-node-icon 
    (debugger :: <debugger>, wrapper :: <stack-frame-wrapper>)
 => (small-icon, large-icon)
  let frame = wrapper.wrapper-object;
  let first-frame = debugger-first-frame(debugger);
  select (frame)
    first-frame => values($current-location-image, #f);
    otherwise   => next-method();
  end
end method stack-frame-node-icon;
