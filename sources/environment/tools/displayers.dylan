Module:    environment-tools
Synopsis:  Environment tools
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Displayers

define open abstract class <displayer-state> (<object>)
  constant slot displayer-state-object :: <environment-object>,
    required-init-keyword: object:;
end class <displayer-state>;

define open abstract class <displayer-mixin> (<sheet>)
  constant slot displayer-extra-pane :: false-or(<sheet>) = #f,
    init-keyword: extra-pane:;
  slot displayer-enabled? :: <boolean> = #t,
    setter: %enabled?-setter,
    init-keyword: enabled?:;
  slot displayer-valid? :: <boolean> = #f;
  slot displayer-new-state? :: <boolean> = #f;
  slot displayer-state :: false-or(<displayer-state>) = #f,
    setter: %state-setter,
    init-keyword: state:;
  constant slot %lock :: <lock> = make(<lock>);
  slot displayer-operation :: false-or(<displayer-operation>) = #f;
  slot displayer-ratios :: false-or(<sequence>) = #f,
    init-keyword: ratios:;
  constant slot displayer-ratios-changed-callback :: false-or(<function>) = #f,
    init-keyword: ratios-changed-callback:;
  constant slot displayer-transaction-function :: false-or(<function>) = #f,
    init-keyword: transaction-function:;
end class <displayer-mixin>;

define open generic displayer-default-input-focus
    (displayer :: <displayer-mixin>)
 => (sheet :: false-or(<abstract-sheet>));

define open generic note-displayer-enabled-state-changed
    (displayer :: <displayer-mixin>) => ();

define open generic update-displayer-state
    (displayer :: <displayer-mixin>) => ();

define method displayer-default-input-focus
    (displayer :: <displayer-mixin>)
 => (sheet == #f)
  #f
end method displayer-default-input-focus;

define method displayer-object
    (displayer :: <displayer-mixin>)
 => (object :: false-or(<environment-object>))
  let state = displayer.displayer-state;
  state & state.displayer-state-object
end method displayer-object;

define method update-displayer-state
    (displayer :: <displayer-mixin>) => ()
  #f
end method update-displayer-state;

define method displayer-current-state
    (displayer :: <displayer-mixin>)
 => (state :: false-or(<displayer-state>))
  update-displayer-state(displayer);
  displayer.displayer-state
end method displayer-current-state;

define method displayer-state-setter
    (state :: false-or(<displayer-state>), displayer :: <displayer-mixin>)
 => (state :: false-or(<displayer-state>))
  unless (state == displayer.displayer-state)
    displayer.displayer-new-state? := #t;
    displayer.%state := state
  end;
  state
end method displayer-state-setter;

define method displayer-enabled?-setter
    (enabled? :: <boolean>, displayer :: <displayer-mixin>)
 => (enabled? :: <boolean>)
  displayer.%enabled? := enabled?;
  note-displayer-enabled-state-changed(displayer);
  enabled?
end method displayer-enabled?-setter;

define method maybe-add-extra-pane
    (displayer :: <displayer-mixin>, pane :: <sheet>)
 => (layout :: <sheet>)
  let extra-pane = displayer.displayer-extra-pane;
  if (extra-pane)
    make(<column-splitter>,
	 ratios: displayer.displayer-ratios,
	 children: vector(pane, extra-pane),
	 split-bar-moved-callback:
	   method (splitter :: <splitter>, #rest arguments)
	     ignore(arguments);
	     let callback = displayer.displayer-ratios-changed-callback;
	     displayer.displayer-ratios := splitter.gadget-ratios;
	     callback & callback(displayer)
	   end)
  else
    pane
  end
end method maybe-add-extra-pane;

define open generic make-displayer-state
    (displayer :: <displayer-mixin>, object) => (state :: <displayer-state>);

define open generic note-displayer-state-updated
    (displayer :: <displayer-mixin>, 
     #key after-function, new-thread?,
          state :: false-or(<displayer-state>))
 => ();

define open generic refresh-displayer-state
    (state :: <displayer-state>) => ();

define method note-displayer-state-updated
    (displayer :: <displayer-mixin>, 
     #key after-function, new-thread? = #f,
          state :: false-or(<displayer-state>) = #f)
 => ()
  after-function & after-function()
end method note-displayer-state-updated;

define method note-displayer-enabled-state-changed
    (displayer :: <displayer-mixin>) => ()
  #f
end method note-displayer-enabled-state-changed;

define method refresh-displayer-state
    (state :: <displayer-state>) => ()
  #f
end method refresh-displayer-state;

define method execute-displayer-gadget-activate-callback
    (gadget :: <collection-gadget-mixin>) => ()
  let displayer = gadget-client(gadget);
  if (displayer)
    let values = gadget-value(gadget);
    let value
      = case
	  ~instance?(values, <sequence>) => values;
	  ~empty?(values)                => values[0];
	  otherwise                      => #f;
	end;
    execute-displayer-activate-callback(displayer, value)
  end
end method execute-displayer-gadget-activate-callback;

define method execute-displayer-activate-callback
    (pane :: <displayer-mixin>, object) => ()
  let page = sheet-parent(pane);
  execute-displayer-activate-callback(page, object)
end method execute-displayer-activate-callback;

define method pane-sheet-with-selection
    (pane :: <displayer-mixin>)
 => (sheet :: false-or(<sheet>))
  #f
end method pane-sheet-with-selection;


/// Displayer operations

define constant <operation-state> 
  = one-of(#"querying", #"cancelled", #"finished");

define class <displayer-operation> (<object>)
  sealed slot operation-state :: <operation-state> = #"querying";
end class <displayer-operation>;

define macro with-displayer-lock
  { with-displayer-lock (?displayer:expression)
      ?body:body
    end }
 => { with-lock (?displayer.%lock)
	?body
      end }
end macro with-displayer-lock;

define macro with-displayer-operation
  { with-displayer-operation (?operation:name = ?displayer:expression)
      ?body:body
    end }
 => { with-displayer-lock (?displayer)
	let ?operation = ?displayer.displayer-operation;
	?body
      end }
end macro with-displayer-operation;


/// Items caching

define abstract class <items-cache> (<object>)
end class <items-cache>;

define generic object-cached-items
    (object, cache :: <items-cache>) => (items :: false-or(<sequence>));

define generic object-cached-items-setter
    (items :: <sequence>, object, cache :: <items-cache>)
 => (items :: <sequence>);

define generic clear-items-cache
    (cache :: false-or(<items-cache>)) => ();

define method clear-items-cache (no-cache == #f) => ()
  #f
end method clear-items-cache;


define class <sequence-items-cache> (<items-cache>)
  slot cache-items :: false-or(<sequence>) = #f;
end class <sequence-items-cache>;

define method object-cached-items
    (object, cache :: <sequence-items-cache>)
 => (items :: false-or(<sequence>))
  cache.cache-items
end method object-cached-items;

define method object-cached-items-setter
    (items :: <sequence>, object, cache :: <sequence-items-cache>)
 => (items :: <sequence>)
  cache.cache-items := items
end method object-cached-items-setter;

define method clear-items-cache
    (cache :: <sequence-items-cache>) => ()
  cache.cache-items := #f
end method clear-items-cache;


define class <tree-items-cache> (<items-cache>)
  constant slot cache-children-table :: <table> = make(<table>);
end class <tree-items-cache>;

define method object-cached-items
    (object, cache :: <tree-items-cache>)
 => (items :: false-or(<sequence>))
  element(cache.cache-children-table, object, default: #f)
end method object-cached-items;

define method object-cached-items-setter
    (items :: <sequence>, object, cache :: <tree-items-cache>)
 => (items :: <sequence>)
  element(cache.cache-children-table, object) := items
end method object-cached-items-setter;

define method clear-items-cache
    (cache :: <tree-items-cache>) => ()
  remove-all-keys!(cache.cache-children-table)
end method clear-items-cache;


/// Some standard collection displayers

define class <collection-gadget-displayer-state> (<displayer-state>)
  slot displayer-state-items-cache :: false-or(<items-cache>) = #f,
    init-keyword: items-cache:;
  constant slot displayer-state-labels-cache :: <object-table> = make(<object-table>);
  slot displayer-state-sorted-items-cache :: false-or(<items-cache>) = #f,
    init-keyword: sorted-items-cache:;
  slot displayer-state-sort-order :: false-or(<symbol>) = #f,
    init-keyword: sort-order:;
  slot displayer-state-names-cached?-cache :: false-or(<items-cache>) = #f;
  slot displayer-state-value :: <object> = #f;
  slot displayer-state-expanded-objects :: false-or(<sequence>) = #f;
end class <collection-gadget-displayer-state>;

define macro with-displayer-items-cache
  { with-displayer-items-cache 
        (?displayer:expression, ?cache:name, ?object:expression)
      ?body:body
    end }
    =>
    { begin
	let state = ?displayer.displayer-state;
	let cache 
	  = state.?cache
	      | begin
		  ?cache ## "-setter"
		    (make-displayer-items-cache(?displayer),
		     state)
		end;
	object-cached-items(?object, cache)
	  | begin
	      object-cached-items(?object, cache) := ?body
	    end
      end }
end macro with-displayer-items-cache;

define method refresh-displayer-state
    (state :: <collection-gadget-displayer-state>)
 => ()
  next-method();
  clear-items-cache(state.displayer-state-items-cache);
  clear-items-cache(state.displayer-state-sorted-items-cache);
  clear-items-cache(state.displayer-state-names-cached?-cache);
  remove-all-keys!(state.displayer-state-labels-cache)
end method refresh-displayer-state;

define class <collection-gadget-displayer-mixin> (<displayer-mixin>)
  constant slot displayer-element-label :: <string> = $unknown,
    init-keyword: element-label:;
  constant slot displayer-value-changed-callback :: false-or(<function>) = #f,
    init-keyword: value-changed-callback:;
  constant slot displayer-activate-callback :: false-or(<function>) = #f,
    init-keyword: activate-callback:;
  constant slot displayer-items-changed-callback :: false-or(<function>) = #f,
    init-keyword: items-changed-callback:;
  constant slot displayer-information-available?-function :: false-or(<function>) = #f,
    init-keyword: information-available?-function:;
  constant slot displayer-children-generator :: <function>,
    required-init-keyword: children-generator:;
  constant slot displayer-selection-mode :: <symbol> = #"multiple",
    init-keyword: selection-mode:;
  constant slot displayer-test-function :: <function> = \=,
    init-keyword: test:;
  constant slot displayer-label-key :: <function> = identity,
    init-keyword: label-key:;
  constant slot displayer-value-key :: <function> = identity,
    init-keyword: value-key:;
  constant slot displayer-icon-function :: false-or(<function>) = #f,
    init-keyword: icon-function:;
  constant slot displayer-sort-function :: <function> = identity,
    init-keyword: sort-function:;
  constant slot displayer-sort-order :: false-or(<symbol>) = #f,
    init-keyword: sort-order:;
  constant slot displayer-always-show-selection? :: <boolean> = #t,
    init-keyword: always-show-selection?:;
end class <collection-gadget-displayer-mixin>;

define open generic displayer-collection-gadget
    (displayer :: <collection-gadget-displayer-mixin>);
//---*** andrewa: 'define frame' doesn't specify a return type
// => (gadget :: <collection-gadget>);

define open generic displayer-item-count
    (displayer :: <collection-gadget-displayer-mixin>)
 => (display-count :: false-or(<integer>), total-count :: false-or(<integer>));

define open generic compute-displayer-items
    (displayer :: <collection-gadget-displayer-mixin>, object)
 => (items :: <sequence>);

define open generic displayer-items
    (displayer :: <collection-gadget-displayer-mixin>, object)
 => (items :: <sequence>);

define open generic displayer-display-items
    (displayer :: <collection-gadget-displayer-mixin>, object)
 => (items :: <sequence>);

define open generic displayer-sorted-items
    (displayer :: <collection-gadget-displayer-mixin>, object)
 => (items :: <sequence>);

define method displayer-default-input-focus
    (displayer :: <collection-gadget-displayer-mixin>)
 => (sheet :: <collection-gadget>)
  displayer.displayer-collection-gadget
end method displayer-default-input-focus;

define method note-displayer-enabled-state-changed
    (displayer :: <collection-gadget-displayer-mixin>) => ()
  next-method();
  let gadget = displayer.displayer-collection-gadget;
  gadget.gadget-enabled? := displayer.displayer-enabled?
end method note-displayer-enabled-state-changed;

define method make-displayer-items-cache
    (displayer :: <collection-gadget-displayer-mixin>)
 => (cache :: <sequence-items-cache>)
  make(<sequence-items-cache>)
end method make-displayer-items-cache;

define method displayer-project
    (displayer :: <collection-gadget-displayer-mixin>)
 => (project :: <project-object>)
  //---*** Should displayers really know about projects?
  let frame = displayer.sheet-frame;
  frame.frame-project
end method displayer-project;

define method displayer-information-available?
    (displayer :: <collection-gadget-displayer-mixin>)
 => (available? :: <boolean>)
  let test-value
    = displayer.displayer-state
        & begin
	    let function = displayer.displayer-information-available?-function;
	    if (function)
	      function()
	    else
	      project-compiler-database(displayer.displayer-project)
	    end
	  end;
  test-value ~= #f
end method displayer-information-available?;

define method displayer-sorted-items
    (displayer :: <collection-gadget-displayer-mixin>, object)
 => (items :: <sequence>)
  let sort-function = displayer.displayer-sort-function;
  let items
    = if (sort-function == identity)
	displayer-display-items(displayer, object)
      else
	with-displayer-items-cache 
	    (displayer, displayer-state-sorted-items-cache, object)
	  let state = displayer.displayer-state;
	  let items = displayer-display-items(displayer, object);
	  let sort-function = displayer.displayer-sort-function;
	  let sort-order = state.displayer-state-sort-order;
	  if (sort-order)
	    sort-function(items, sort-order)
	  else
	    items
	  end
	end
      end;
  //--- We do this so that the names are computed and then cached during
  //--- this slow operation, rather than when the items are about to be
  //--- displayed which needs to be fast. This makes the tree control update
  //--- instantly once the items are all computed.
  with-displayer-items-cache
      (displayer, displayer-state-names-cached?-cache, object)
    ensure-displayer-item-labels-cached(displayer, items);
    //--- Return empty list to fill the cache, since it has to be a sequence
    #()    
  end;
  items
end method displayer-sorted-items;

define method displayer-display-items
    (displayer :: <collection-gadget-displayer-mixin>, object)
 => (items :: <sequence>)
  displayer-items(displayer, object)
end method displayer-display-items;

define method displayer-items
    (displayer :: <collection-gadget-displayer-mixin>, object)
 => (items :: <sequence>)
  with-displayer-items-cache
      (displayer, displayer-state-items-cache, object)
    compute-displayer-items(displayer, object);
  end
end method displayer-items;

define method compute-displayer-items
    (displayer :: <collection-gadget-displayer-mixin>, object)
 => (items :: <sequence>)
  displayer.displayer-children-generator(object)
end method compute-displayer-items;

define method displayer-item-label
    (displayer :: <collection-gadget-displayer-mixin>, item :: <object>)
 => (label :: <string>)
  let state = displayer.displayer-state;
  let cache = state.displayer-state-labels-cache;
  element(cache, item, default: #f)
    | begin
	let label = displayer.displayer-label-key(item);
	element(cache, item) := label
      end
end method displayer-item-label;

define method ensure-displayer-item-labels-cached
    (displayer :: <collection-gadget-displayer-mixin>, items :: <sequence>)
 => ()
  let label-key = displayer.displayer-label-key;
  let state     = displayer.displayer-state;
  let cache     = state.displayer-state-labels-cache;
  for (item in items)
    unless (element(cache, item, default: #f))
      element(cache, item) := label-key(item)
    end
  end;
  items
end method ensure-displayer-item-labels-cached;

define method displayer-item-count
    (displayer :: <collection-gadget-displayer-mixin>)
 => (count :: false-or(<integer>), total :: false-or(<integer>))
  size(displayer-items(displayer, displayer.displayer-object))
end method displayer-item-count;

define method pane-sheet-with-selection
    (pane :: <collection-gadget-displayer-mixin>)
 => (sheet :: <sheet>)
  pane.displayer-collection-gadget
end method pane-sheet-with-selection;

define method execute-displayer-activate-callback
    (displayer :: <collection-gadget-displayer-mixin>, object) => ()
  let activate-callback = displayer.displayer-activate-callback;
  if (activate-callback)
    let gadget = displayer.displayer-collection-gadget;
    activate-callback(object)
  else
    next-method()
  end
end method execute-displayer-activate-callback;

define method displayer-object-icon
    (displayer :: <collection-gadget-displayer-mixin>, object)
 => (icon)
  let icon-function = displayer.displayer-icon-function;
  if (icon-function)
    icon-function(object)
  else
    environment-object-icon(displayer.displayer-project, object)
  end
end method displayer-object-icon;

define method make-displayer-state
    (pane :: <collection-gadget-displayer-mixin>, object)
 => (class :: <collection-gadget-displayer-state>)
  make(<collection-gadget-displayer-state>, 
       object: object,
       sort-order: pane.displayer-sort-order)
end method make-displayer-state;

define method update-displayer-state
    (displayer :: <collection-gadget-displayer-mixin>) => ()
  next-method();
  let state = displayer.displayer-state;
  if (state)
    let gadget = displayer.displayer-collection-gadget;
    state.displayer-state-value := gadget.gadget-value;
  end
end method update-displayer-state;

define method displayer-object-setter
    (object :: false-or(<environment-object>),
     displayer :: <collection-gadget-displayer-mixin>,
     #key clean? = #f, new-thread? = #t, after-function)
 => ()
  let frame = sheet-frame(displayer);
  with-busy-cursor (frame)
    with-displayer-lock (displayer)
      let old-state = displayer.displayer-state;
      let new-state
	= case
	    old-state & (old-state.displayer-state-object == object) =>
	      old-state;
	    otherwise =>
	      let state = object & make-displayer-state(displayer, object);
	      displayer.displayer-state := state
	  end;
      let new-state? = displayer.displayer-new-state?;
      let clean? = ~displayer.displayer-valid? | clean?;
      if (clean? | new-state?)
	refresh-displayer
	  (displayer, clean?: clean?, new-thread?: new-thread?,
	   after-function: after-function)
      else
	//--- We need to do this to get the status bar to update,
	//--- but it doesn't feel quite right somehow...
	note-displayer-items-changed(displayer);
	after-function & after-function()
      end
    end
  end
end method displayer-object-setter;


// Some displayers want to establish a debugger transaction
// around entire display operations to avoid the overhead
// involved with establishing several debugger transactions
// in a running application later on during the display

define macro with-displayer-transaction
  { with-displayer-transaction (?displayer:expression)
      ?:body
    end }
    =>
    {
     let transaction-function = ?displayer.displayer-transaction-function;
     if (transaction-function)
       transaction-function(method () ?body end)
     else
       ?body
     end
     }
end macro;

define macro with-displayer-transaction-method
  { with-displayer-transaction-method (?displayer:expression)
      ?body:body
    end }
    =>
    { 
     let transaction-function = ?displayer.displayer-transaction-function;
     let display-method = method () ?body end;
     if (transaction-function)
       method () transaction-function(display-method) end
     else
       display-method
     end
     }
end macro;


define method refresh-displayer
    (displayer :: <collection-gadget-displayer-mixin>,
     #key clean? = #t, new-thread? = #t, after-function)
 => ()
  let frame = sheet-frame(displayer);
  let state = displayer.displayer-state;
  let gadget = displayer.displayer-collection-gadget;
  update-displayer-state(displayer);
  if (clean? & state)
    refresh-displayer-state(state)
  end;
  if (new-thread?)
    set-displayer-contents(displayer, #[]);
    with-frame-background-operation (frame, "Refreshing...")
      with-abort-restart ()
	with-environment-handlers (frame)
	  with-displayer-transaction (displayer)
	    note-displayer-state-updated
	      (displayer, 
	       after-function: after-function,
	       new-thread?: #t,
	       state: state)
	  end
	end
      end
    end
  else
    with-busy-cursor (frame)
      with-displayer-transaction (displayer)
	note-displayer-state-updated
	  (displayer,
	   after-function: after-function,
	   state: state)
      end
    end
  end
end method refresh-displayer;

define thread variable *handle-item-callbacks?* :: <boolean> = #t;

define method note-displayer-items-changed
    (displayer :: <collection-gadget-displayer-mixin>) => ()
  if (*handle-item-callbacks?*)
    let callback = displayer.displayer-items-changed-callback;
    if (callback)
      callback(displayer)
    else
      note-displayer-items-updated
	(displayer, displayer.displayer-element-label)
    end
  end
end method note-displayer-items-changed;

define method compute-displayer-contents
    (displayer :: <collection-gadget-displayer-mixin>)
 => (sorted-items :: <sequence>)
  let state = displayer.displayer-state;
  if (displayer-information-available?(displayer))
    let object = state & state.displayer-state-object;
    displayer-sorted-items(displayer, object)
  else
    #[]
  end
end method compute-displayer-contents;

define method set-displayer-contents
    (displayer :: <collection-gadget-displayer-mixin>,
     sorted-items :: <sequence>,
     #key state :: false-or(<collection-gadget-displayer-state>) = #f)
 => ()
  let gadget = displayer.displayer-collection-gadget;
  with-atomic-redisplay (gadget)
    if (gadget-items(gadget) ~= sorted-items)
      gadget-items(gadget) := sorted-items
    else
      update-gadget(gadget)
    end;
    let value = if (state) state.displayer-state-value end;
    gadget-value(gadget)
      := case
	   value =>
	     value;
	   gadget-selection-mode(gadget) == #"multiple" =>
	     #[];
	   otherwise =>
	     #f;
	 end
  end
end method set-displayer-contents;

define method note-displayer-state-updated
    (displayer :: <collection-gadget-displayer-mixin>,
     #key after-function, new-thread? = #f,
          state :: false-or(<collection-gadget-displayer-state>) = #f)
 => ()
  let frame = sheet-frame(displayer);
  if (new-thread?)
    let current-operation
      = with-displayer-operation (operation = displayer)
	  if (operation)
	    operation.operation-state := #"cancelled"
	  end;
	  displayer.displayer-operation := make(<displayer-operation>)
	end;
    let sorted-items = compute-displayer-contents(displayer);
    call-in-frame(frame,
		  with-displayer-transaction-method (displayer)
		    with-displayer-operation (operation = displayer)
		      if (operation
			    & operation == current-operation
			    & operation.operation-state ~== #"cancelled")
			displayer.displayer-operation := #f;
			set-displayer-contents
			  (displayer, sorted-items, state: state);
			displayer.displayer-valid? := #t;
			displayer.displayer-new-state? := #f;
			note-displayer-items-changed(displayer);
			operation.operation-state := #"finished";
		      end
		    end;
		    after-function & after-function()
		  end)
  else
    let sorted-items = compute-displayer-contents(displayer);
    call-in-frame
      (frame,
       with-displayer-transaction-method (displayer)
	 set-displayer-contents(displayer, sorted-items, state: state);
	 displayer.displayer-valid? := #t;
	 displayer.displayer-new-state? := #f;
	 note-displayer-items-changed(displayer);
	 after-function & after-function();
       end)
  end

end method note-displayer-state-updated;

define function displayer-toggle-sort-order
    (displayer :: <collection-gadget-displayer-mixin>, order)
 => ()
  let state = displayer.displayer-state;
  if (state)
    with-busy-cursor (sheet-frame(displayer))
      with-displayer-lock (displayer)
	let old-order = state.displayer-state-sort-order;
	state.displayer-state-sort-order
	  := select (order by instance?)
	       <sequence> =>
		 let order-0 = order[0];
		 let order-1 = order[1];
		 if (old-order = order-0) order-1 else order-0 end;
	       otherwise =>
		 if (old-order = order) #f else order end;
	     end;
	clear-items-cache(state.displayer-state-sorted-items-cache);
	note-displayer-state-updated(displayer)
      end
    end
  end
end function displayer-toggle-sort-order;

define function project-not-built-message
    (project :: <project-object>, #key message = $no-information-available)
 => (message :: <string>)
  concatenate(message,
	      if (~project.project-compiler-database
		    & project.project-can-be-built?)
		concatenate(" ", $project-not-built)
	      else
		""
	      end)
end function project-not-built-message;

define method note-displayer-items-updated
    (displayer :: <collection-gadget-displayer-mixin>, name :: <string>) => ()
  let frame = sheet-frame(displayer);
  let project = displayer.displayer-project;
  let (count, total) 
    = displayer-information-available?(displayer)
        & displayer-item-count(displayer);
  let message
    = case
	total & total ~= count =>
	  format-to-string("%d %s (%d total)",
			   count,
			   string-pluralize(name, count: count),
			   total);
	count =>
	  format-to-string("%d %s",
			   count,
			   string-pluralize(name, count: count));
	otherwise =>
	  project-not-built-message(project);
      end;
  frame-status-message(frame) := message
end method note-displayer-items-updated;


/// Filtering displayers

define pane <filtering-collection-gadget-displayer-mixin> 
    (<collection-gadget-displayer-mixin>)
  constant slot displayer-filter-function :: <function>,
    required-init-keyword: filter-function:;
  constant slot displayer-filter-types :: <sequence>,
    required-init-keyword: filter-types:;
  constant slot displayer-filter-type :: <symbol>,
    init-keyword: filter-type:;
  constant slot displayer-filter-extra-types :: <sequence> = #[],
    init-keyword: filter-extra-types:;
  constant slot displayer-filter-extra-type :: false-or(<symbol>) = #f,
    init-keyword: filter-extra-type:;
  constant slot displayer-filter-extra-text-label :: false-or(<string>) = #f,
    init-keyword: filter-extra-text-label:;
  constant slot displayer-filter-text-label :: <string> = "with names containing",
    init-keyword: filter-text-label:;
  constant slot displayer-filter-type-only? :: <boolean> = #f,
    init-keyword: filter-type-only?:;
  pane displayer-type-filter-pane (pane)
    make(<option-box>,
         items: pane.displayer-filter-types,
	 value: pane.displayer-filter-type,
	 label-key: first,
	 value-key: second,
         value-changed-callback: method (sheet)
                                   update-displayer-filter(pane)
                                 end,
	 documentation: "Chooses the subset of objects to show.",
         fixed-width?: ~pane.displayer-filter-type-only?);
  pane displayer-extra-type-filter-pane (pane)
    make(<option-box>,
         items: pane.displayer-filter-extra-types,
	 value: pane.displayer-filter-extra-type,
	 label-key: first,
	 value-key: second,
         value-changed-callback: method (sheet)
                                   update-displayer-filter(pane)
                                 end,
	 documentation: "Chooses the subset of objects to show.",
         fixed-width?: #t);
  pane displayer-substring-filter-pane (pane)
    make(<text-field>,
         value-changed-callback: method (sheet)
                                   update-displayer-filter(pane)
                                 end,
	 activate-callback: method (sheet)
			      update-displayer-filter(pane)
			    end,
	 documentation: "Filters the subset of objects by substring.",
	 //---*** What should the width really be?
         width: 100, fixed-width?: #t);
  pane displayer-filters-layout (pane)
    begin
      let extra-types? = ~empty?(pane.displayer-filter-extra-types);
      let extra-label = pane.displayer-filter-extra-text-label;
      let filter-type-only? = pane.displayer-filter-type-only?;
      let children
	= if (filter-type-only?)
	    vector(pane.displayer-type-filter-pane)
	  else
	    vector(pane.displayer-type-filter-pane,
		   extra-label
		     & make(<label>, label: extra-label),
		   extra-types?
		     & pane.displayer-extra-type-filter-pane,
		   make(<label>, label: pane.displayer-filter-text-label),
		   pane.displayer-substring-filter-pane)
	  end;
      make(<row-layout>,
	   spacing: 2, y-alignment: #"center",
	   children: remove(children, #f))
    end;
  pane displayer-filtered-collection-gadget (pane)
    vertically (spacing: 2)
      pane.displayer-filters-layout;
      pane.displayer-collection-gadget;
    end;
  layout (pane)
    maybe-add-extra-pane(pane, pane.displayer-filtered-collection-gadget);
end pane <filtering-collection-gadget-displayer-mixin>;

define constant $default-substring-filter = "";

define class <filtering-collection-gadget-displayer-state>
    (<collection-gadget-displayer-state>)
  slot displayer-state-filtered-items-cache :: false-or(<items-cache>) = #f,
    init-keyword: filtered-items-cache:;
  slot displayer-state-type-filter :: <symbol>,
    required-init-keyword: type-filter:;
  slot displayer-state-extra-type-filter :: false-or(<symbol>) = #f,
    init-keyword: extra-type-filter:;
  slot displayer-state-substring-filter :: <string>,
    required-init-keyword: substring-filter:;
end class <filtering-collection-gadget-displayer-state>;

define method make-displayer-state
    (displayer :: <filtering-collection-gadget-displayer-mixin>, object)
 => (state :: <filtering-collection-gadget-displayer-state>)
  make(<filtering-collection-gadget-displayer-state>, 
       object: object,
       sort-order: displayer.displayer-sort-order,
       type-filter: displayer.displayer-filter-type,
       extra-type-filter: displayer.displayer-filter-extra-type,
       substring-filter: $default-substring-filter)
end method make-displayer-state;

define method refresh-displayer-state
    (state :: <filtering-collection-gadget-displayer-state>)
 => ()
  next-method();
  clear-items-cache(state.displayer-state-filtered-items-cache)
end method refresh-displayer-state;

define method note-displayer-enabled-state-changed
    (displayer :: <filtering-collection-gadget-displayer-mixin>) => ()
  next-method();
  let enabled? = displayer.displayer-enabled?;
  let gadget = displayer.displayer-collection-gadget;
  displayer.displayer-type-filter-pane.gadget-enabled? := enabled?;
  displayer.displayer-extra-type-filter-pane.gadget-enabled? := enabled?;
  displayer.displayer-substring-filter-pane.gadget-enabled? := enabled?
end method note-displayer-enabled-state-changed;

define method displayer-filter-options
    (displayer :: <filtering-collection-gadget-displayer-mixin>)
 => (type-filter :: <symbol>, extra-type-filter :: false-or(<symbol>),
     substring-filter :: <string>)
  let type-filter-pane = displayer.displayer-type-filter-pane;
  let extra-type-filter-pane = displayer.displayer-extra-type-filter-pane;
  let substring-filter-pane = displayer.displayer-substring-filter-pane;
  let type-filter = gadget-value(type-filter-pane);
  let extra-type-filter = gadget-value(extra-type-filter-pane);
  let substring-filter = gadget-value(substring-filter-pane);
  values(type-filter, extra-type-filter, substring-filter)
end method displayer-filter-options;

define method set-displayer-contents
    (displayer :: <filtering-collection-gadget-displayer-mixin>,
     sorted-items :: <sequence>,
     #key state :: false-or(<collection-gadget-displayer-state>) = #f)
 => ()
  next-method();
  let state = displayer.displayer-state;
  let type-filter-pane = displayer.displayer-type-filter-pane;
  let extra-type-filter-pane = displayer.displayer-extra-type-filter-pane;
  let substring-filter-pane = displayer.displayer-substring-filter-pane;
  let type-filter = state & state.displayer-state-type-filter;
  let extra-type-filter = state & state.displayer-state-extra-type-filter;
  let substring-filter = state & state.displayer-state-substring-filter;
  gadget-value(type-filter-pane)
    := type-filter | displayer.displayer-filter-type;
  gadget-value(extra-type-filter-pane)
    := extra-type-filter | displayer.displayer-filter-extra-type;
  gadget-value(substring-filter-pane) 
    := substring-filter | $default-substring-filter;
end method set-displayer-contents;

define method update-displayer-filter
    (displayer :: <filtering-collection-gadget-displayer-mixin>)
 => (updated? :: <boolean>)
  let state = displayer.displayer-state;
  if (state)
    with-displayer-lock (displayer)
      let (type-filter, extra-type-filter, substring-filter)
	= displayer.displayer-filter-options;
      unless (type-filter = state.displayer-state-type-filter
		& extra-type-filter = state.displayer-state-extra-type-filter
		& substring-filter = state.displayer-state-substring-filter)
	state.displayer-state-type-filter := type-filter;
	state.displayer-state-extra-type-filter := extra-type-filter;
	state.displayer-state-substring-filter := substring-filter;
	clear-items-cache(state.displayer-state-filtered-items-cache);
	clear-items-cache(state.displayer-state-sorted-items-cache);
	note-displayer-state-updated(displayer);
	#t
      end
    end
  end
end method update-displayer-filter;

define method displayer-display-items
    (displayer :: <filtering-collection-gadget-displayer-mixin>, object)
 => (items :: <sequence>)
  let filter-function = displayer.displayer-filter-function;
  if (filter-function == identity)
    displayer-items(displayer, object)
  else
    with-displayer-items-cache
	(displayer, displayer-state-filtered-items-cache, object)
      let state = displayer.displayer-state;
      let items = displayer-items(displayer, object);
      let type-filter = state.displayer-state-type-filter;
      let extra-type-filter = state.displayer-state-extra-type-filter;
      let substring-filter = state.displayer-state-substring-filter;
      if (extra-type-filter)
	filter-function
	  (items, type-filter, extra-type-filter, substring-filter)
      else
	filter-function(items, type-filter, substring-filter)
      end
    end
  end
end method displayer-display-items;

define method displayer-item-count
    (displayer :: <filtering-collection-gadget-displayer-mixin>)
 => (count :: false-or(<integer>), total :: false-or(<integer>))
  //--- This only works for non-trees
  displayer-object-items-count(displayer, displayer.displayer-object)
end method displayer-item-count;

define method displayer-object-items-count
    (displayer :: <filtering-collection-gadget-displayer-mixin>, object)
 => (count :: <integer>, total :: <integer>)
  let all-children = displayer-items(displayer, object);
  let children = displayer-display-items(displayer, object);
  values(size(children), size(all-children))
end method displayer-object-items-count;


/// List control displayer

define pane <list-control-displayer> 
    (<collection-gadget-displayer-mixin>)
  pane displayer-collection-gadget (pane)
    make(<list-control>,
         selection-mode: pane.displayer-selection-mode,
         client: pane,
         label-key: curry(displayer-item-label, pane),
	 test: displayer-test-function(pane),
         value-key: displayer-value-key(pane),
         icon-function: curry(displayer-object-icon, pane),
         always-show-selection?: displayer-always-show-selection?(pane),
	 value-changed-callback: method (gadget)
                                   let frame = sheet-frame(gadget);
                                   note-frame-selection-updated(frame);
				   let callback
				     = pane.displayer-value-changed-callback;
				   callback & callback(pane, gadget-value(gadget))
                                 end,
         activate-callback: execute-displayer-gadget-activate-callback,
         popup-menu-callback: display-environment-popup-menu);
  layout (pane)
    maybe-add-extra-pane(pane, pane.displayer-collection-gadget);
end pane <list-control-displayer>;

define pane <filtering-list-control-displayer>
    (<filtering-collection-gadget-displayer-mixin>,
     <list-control-displayer>)
end pane <filtering-list-control-displayer>;


/// Table control displayer

define pane <table-control-displayer>
    (<collection-gadget-displayer-mixin>)
  constant slot table-control-displayer-headings :: <sequence>,
    required-init-keyword: headings:;
  constant slot table-control-displayer-generators :: <sequence>,
    required-init-keyword: generators:;
  constant slot table-control-displayer-widths :: false-or(<sequence>) = #f,
    init-keyword: widths:;
  constant slot table-control-displayer-alignments :: false-or(<sequence>) = #f,
    init-keyword: alignments:;
  constant slot table-control-displayer-sort-orders :: false-or(<sequence>) = #f,
    init-keyword: sort-orders:;
  pane displayer-collection-gadget (pane)
    make(<table-control>,
         selection-mode: pane.displayer-selection-mode,
         client: pane,
         headings: pane.table-control-displayer-headings,
         generators: pane.table-control-displayer-generators,
         widths: pane.table-control-displayer-widths,
         alignments: pane.table-control-displayer-alignments,
	 callbacks: pane.table-control-displayer-sort-callbacks,
	 test: pane.displayer-test-function,
         label-key: curry(displayer-item-label, pane),
         value-key: pane.displayer-value-key,
         icon-function: curry(displayer-object-icon, pane),
         always-show-selection?: displayer-always-show-selection?(pane),
         value-changed-callback: method (gadget)
                                   let frame = sheet-frame(gadget);
                                   note-frame-selection-updated(frame);
				   let callback
				     = pane.displayer-value-changed-callback;
				   callback & callback(pane, gadget-value(gadget))
                                 end,
         activate-callback: execute-displayer-gadget-activate-callback,
         popup-menu-callback: display-environment-popup-menu);
  layout (pane)
    maybe-add-extra-pane(pane, pane.displayer-collection-gadget);
end pane <table-control-displayer>;

define pane <filtering-table-control-displayer> 
    (<filtering-collection-gadget-displayer-mixin>,
     <table-control-displayer>)
end pane <filtering-table-control-displayer>;

define function table-control-displayer-sort-callbacks
    (displayer :: <table-control-displayer>)
 => (callbacks :: false-or(<sequence>))
  let sort-orders = displayer.table-control-displayer-sort-orders;
  if (sort-orders)
    map(method (order) => (callback :: <function>)
	  method (gadget)
	    ignore(gadget);
	    displayer-toggle-sort-order(displayer, order)
	  end
	end,
	sort-orders)
  end
end function table-control-displayer-sort-callbacks;

define method ensure-displayer-item-labels-cached
    (displayer :: <table-control-displayer>, items :: <sequence>)
 => ()
  let label-key  = displayer.displayer-label-key;
  let generators = displayer.table-control-displayer-generators;
  let state      = displayer.displayer-state;
  let cache      = state.displayer-state-labels-cache;
  for (item in items)
    for (generator in generators)
      let column-item = generator(item);
      unless (element(cache, column-item, default: #f))
	element(cache, column-item) := label-key(column-item)
      end
    end
  end;
  items
end method ensure-displayer-item-labels-cached;


/// Tree control displayer

define pane <tree-control-displayer> (<collection-gadget-displayer-mixin>)
  constant slot displayer-children-predicate :: false-or(<function>) = #f,
    init-keyword: children-predicate:;
  constant slot displayer-depth :: <integer> = 0,
    init-keyword: depth:;
  pane displayer-collection-gadget (pane)
    make(<tree-control>,
         selection-mode: pane.displayer-selection-mode,
         client: pane,
         depth: pane.displayer-depth,
         children-generator: curry(tree-control-displayer-children, pane),
         children-predicate: curry(tree-control-displayer-children?, pane),
	 //---*** Unfortunately, this hides the expand boxes, too
	 // show-root-edges?: #f,
	 test: pane.displayer-test-function,
         label-key: curry(displayer-item-label, pane),
         value-key: pane.displayer-value-key,
         icon-function: curry(displayer-object-icon, pane),
         always-show-selection?: pane.displayer-always-show-selection?,
         //---*** Unfortunately, this scrolls badly in the X direction, too
	 keep-selection-visible?: #f,
	 value-changed-callback: method (gadget)
                                   let frame = sheet-frame(gadget);
                                   note-frame-selection-updated(frame);
				   let callback
				     = pane.displayer-value-changed-callback;
				   callback & callback(pane, gadget-value(gadget))
                                 end,
         activate-callback: execute-displayer-gadget-activate-callback,
         popup-menu-callback: display-environment-popup-menu);
  layout (pane)
    maybe-add-extra-pane(pane, pane.displayer-collection-gadget);
end pane <tree-control-displayer>;

define pane <filtering-tree-control-displayer> 
    (<filtering-collection-gadget-displayer-mixin>,
     <tree-control-displayer>)
end pane <filtering-tree-control-displayer>;

define method make-displayer-items-cache
    (displayer :: <tree-control-displayer>)
 => (cache :: <tree-items-cache>)
  make(<tree-items-cache>)
end method make-displayer-items-cache;

define method update-displayer-state
    (displayer :: <tree-control-displayer>) => ()
  next-method();
  let state = displayer.displayer-state;
  if (state)
    let gadget = displayer.displayer-collection-gadget;
    state.displayer-state-expanded-objects
      := gadget.tree-control-expanded-objects
  end
end method update-displayer-state;

define method compute-displayer-contents
    (displayer :: <tree-control-displayer>)
 => (new-roots :: <sequence>)
  dynamic-bind (*handle-item-callbacks?* = #f)
    let depth = displayer.displayer-depth;
    local method object-children
	      (object :: <object>, current-depth :: <integer>)
	   => (children :: <sequence>)
	    let children = tree-control-displayer-children(displayer, object);
	    // Fill in the caches for the children
	    map(if (depth == current-depth)
		  curry(tree-control-displayer-children?, displayer)
		else
		  rcurry(object-children, current-depth + 1)
		end,
		children);
	    children
	  end method object-children;
    if (displayer-information-available?(displayer))
      let object = displayer.displayer-object;
      object-children(object, 0)
    else
      #[]
    end
  end
end method compute-displayer-contents;

define method set-displayer-contents
    (displayer :: <tree-control-displayer>,
     new-roots :: <sequence>,
     #key state :: false-or(<collection-gadget-displayer-state>) = #f)
 => ()
  dynamic-bind (*handle-item-callbacks?* = #f)
    let tree-control = displayer.displayer-collection-gadget;
    with-atomic-redisplay (tree-control)
      if (tree-control-roots(tree-control) ~= new-roots)
	tree-control-roots(tree-control) := new-roots
      else
	update-gadget(tree-control)
      end;
      if (state)
	tree-control-expanded-objects(tree-control)
	  := state.displayer-state-expanded-objects;
	gadget-value(tree-control) := state.displayer-state-value
      end
    end
  end
end method set-displayer-contents;

define method compute-displayer-items
    (displayer :: <tree-control-displayer>, object)
 => (children :: <sequence>)
  let predicate-function = displayer-children-predicate(displayer);
  let children-generator = displayer-children-generator(displayer);
  let has-children? = ~predicate-function | predicate-function(object);
  if (has-children?)
    children-generator(object)
  else
    #[]
  end
end method compute-displayer-items;

define method displayer-item-count
    (displayer :: <tree-control-displayer>)
 => (count :: false-or(<integer>), total :: false-or(<integer>))
  let items = gadget-items(displayer.displayer-collection-gadget);
  size(items)
end method displayer-item-count;

define method tree-control-displayer-children
    (displayer :: <tree-control-displayer>, object)
 => (children :: <sequence>)
  displayer-sorted-items(displayer, object)
end method tree-control-displayer-children;

define method tree-control-displayer-children?
    (displayer :: <tree-control-displayer>, object)
 => (children? :: <boolean>)
  let predicate-function = displayer-children-predicate(displayer);
  if (predicate-function)
    // Do we want to cache predicate function results too?
    predicate-function(object)
  else
    let children = displayer-display-items(displayer, object);
    ~empty?(children)
  end
end method tree-control-displayer-children?;
