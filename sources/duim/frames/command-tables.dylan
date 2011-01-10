Module:       duim-frames-internals
Synopsis:     DUIM frames
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Command tables

// Command table registry, maps command table names to command tables
define variable *command-tables* :: <object-table> = make(<table>);

// Command tables are a way of grouping a set of related command objects
// or "command functions" and providing user interfaces to the commands.
// Interaction styles include via pull-down menus, keystroke accelerators,
// direct manipulations (such as presentation translations and drag&drop),
// and command lines.
define sealed class <standard-command-table> (<command-table>)
  // A symbol that names the command table
  sealed constant slot command-table-name,
    required-init-keyword: name:;
  // A sequence of command tables from which this one inherits
  sealed slot command-table-inherit-from,
    required-init-keyword: inherit-from:,
    setter: %inherit-from-setter;
  // A table of all the commands in this command table; the key is a
  // <command-oid> object, the value is the command line name string
  // or a boolean
  sealed slot command-table-commands :: <object-table> = make(<table>);
  // This is a sequence of decorators used to build menu and tool bars, etc.
  sealed slot command-table-menu :: <stretchy-object-vector> = make(<stretchy-vector>);
  // A cache of all the keystroke accelerators for this command table
  sealed slot %accelerators = #f;
  /*
  //--- This belongs in a command-line layer
  // A sorted sequence that maps from command-line names to <command-oid>
  // objects for this command table; entries are a pair consisting of the
  // command line name string and the <command-oid>
  sealed slot %command-line-names :: false-or(<stretchy-object-vector>) = #f;
  // A completion alist of command line names for this command table and
  // all the command tables it inherits from
  sealed slot %completion-alist = #f;
  sealed slot %completion-alist-tick = 0; 
  */
  /*
  //--- This belongs in a presentation layer
  // The set of presentation translators for this command table
  sealed slot command-table-translators :: false-or(<stretchy-object-vector>) = #f;
  sealed slot %translators-cache :: false-or(<object-table>) = #f;
  */
  // A resource id for creating menus from this command table
  sealed constant slot command-table-resource-id = #f,
    init-keyword: resource-id:;
end class <standard-command-table>;

define sealed domain make (singleton(<standard-command-table>));
define sealed domain initialize (<standard-command-table>);

define method command-table-inherit-from-setter
    (inherit-from, command-table :: <standard-command-table>)
 => (command-tables :: <sequence>)
  unless (inherit-from == command-table-inherit-from(command-table))
    command-table.%inherit-from := inherit-from;
    /* command-table.%completion-alist := #f; */
    /* when (command-table.%translators-cache)
	 remove-all-keys!(command-table.%translators-cache)
       end */
  end;
  inherit-from
end method command-table-inherit-from-setter;


/// Making command tables

// define command-table *compiler* (*file*, *edit*)
//   command-table {a command table decorator}
//   menu      {a menu decorator}
//   command   {a command or function decorator}
//   menu-item "Compile" [of <command>] = compile-file
//   menu-item "Load"    [of <command>] = make(<command, function: load-file);
//   menu-item "Help"    [of <command>] = <help-command>;
//   menu-item "About"   [of <command>] = (<help-command>, about?: #t);
// end
define macro command-table-definer
  { define command-table ?:name (?supers:*) ?options:* end }
    => { define command-table-variable ?name (?supers) ?options end;
         define command-table-menu ?name (?supers) ?options end }
end macro command-table-definer;

define macro command-table-variable-definer
  { define command-table-variable ?:name (?supers:*) ?options:* end }
    => { define variable ?name :: <standard-command-table>
           = apply(make, <command-table>,
		   name: ?#"name",
		   inherit-from: vector(?supers), 
                   ?options) }
 supers:
  { } => { }
  { ?super:*, ... } => { ?super, ... }
 options:
  { } => { #[] }
  { ?option:*; ... } => { concatenate(?option, ...) }
 option:
  { inherit-menu  ?:expression } => { vector(inherit-menu:, ?expression) }
  { resource-id   ?:expression } => { vector(resource-id:,  ?expression) }
  { command-table ?:expression } => { #[] }
  { command   ?:expression } => { #[] }
  { menu      ?:expression } => { #[] }
  { menu-item ?item:* } => { #[] }
  { include   ?item:* } => { #[] }
  { separator ?item:* } => { #[] }
  { separator } => { #[] }
end macro command-table-variable-definer;

/*
//--- Used to be this...
define macro command-table-variable-definer
  { define command-table-variable ?:name (?supers:*) ?options:* end }
    => { define variable ?name :: <standard-command-table>
           = make(<command-table>,
		  name: ?#"name",
		  inherit-from: vector(?supers), ?options) }
 supers:
  { } => { }
  { ?super:*, ... } => { ?super, ... }
 options:
  { } => { }
  { ?option:*; ... } => { ?option, ... }
 option:
  { inherit-menu ?:expression } => { inherit-menu: ?expression }
  { resource-id  ?:expression } => { resource-id:  ?expression }
  { command-table ?:expression } => { }
  { command   ?:expression } => { }
  { menu      ?:expression } => { }
  { menu-item ?item:* } => { }
  { include   ?item:* } => { }
  { separator ?item:* } => { }
  { separator } => { }
end macro command-table-variable-definer;
*/

define macro command-table-menu-definer
  { define command-table-menu ?:name (?supers:*) end }
    => { }
  // The decorator cases
  { define command-table-menu ?:name (?supers:*)
      command-table ?decorator:expression;
      ?more-items:*
    end }
    => { add-command(?name, ?decorator);
	 define command-table-menu ?name (?supers) ?more-items end }
  { define command-table-menu ?:name (?supers:*)
      menu ?decorator:expression;
      ?more-items:*
    end }
    => { add-command(?name, ?decorator);
	 define command-table-menu ?name (?supers) ?more-items end }
  { define command-table-menu ?:name (?supers:*)
      command ?decorator:expression;
      ?more-items:*
    end }
    => { add-command(?name, ?decorator);
	 define command-table-menu ?name (?supers) ?more-items end }
  // The 'menu-item' cases
  { define command-table-menu ?:name (?supers:*)
      menu-item ?label of ?type = ?command, #rest ?options:expression;
      ?more-items:*
    end }
    => { add-command-table-menu-item
	   (?name, ?label, ?type, ?command, ?options);
	 define command-table-menu ?name (?supers) ?more-items end }
  { define command-table-menu ?:name (?supers:*)
      menu-item ?label of ?type = ?command;
      ?more-items:*
    end }
    => { add-command-table-menu-item
	   (?name, ?label, ?type, ?command);
	 define command-table-menu ?name (?supers) ?more-items end }
  { define command-table-menu ?:name (?supers:*)
      menu-item ?label = ?command, #rest ?options:expression;
      ?more-items:*
    end }
    => { add-command-table-menu-item
	   (?name, ?label, #f, ?command, ?options);
	 define command-table-menu ?name (?supers) ?more-items end }
  { define command-table-menu ?:name (?supers:*)
      menu-item ?label = ?command;
      ?more-items:*
    end }
    => { add-command-table-menu-item
	   (?name, ?label, #f, ?command);
	 define command-table-menu ?name (?supers) ?more-items end }
  // The 'separator' cases
  { define command-table-menu ?:name (?supers:*)
      separator;
      ?more-items:*
    end }
    => { add-command-table-menu-item
	   (?name, #f, <separator>, #f);
	 define command-table-menu ?name (?supers) ?more-items end }
  { define command-table-menu ?:name (?supers:*)
      separator ?label;
      ?more-items:*
    end }
    => { add-command-table-menu-item
	   (?name, ?label, <separator>, #f);
	 define command-table-menu ?name (?supers) ?more-items end }
  // The 'include' cases
  { define command-table-menu ?:name (?supers:*)
      include ?command-table;
      ?more-items:*
    end }
    => { add-command-table-menu-item
	   (?name, #f, <separator>, ?command-table);
	 define command-table-menu ?name (?supers) ?more-items end }
  { define command-table-menu ?:name (?supers:*)
      include ?label = ?command-table;
      ?more-items:*
    end }
    => { add-command-table-menu-item
	   (?name, ?label, <separator>, ?command-table);
	 define command-table-menu ?name (?supers) ?more-items end }
  // Fall-through...
  { define command-table-menu ?:name (?supers:*)
      ?non-menu-item:*; ?more-items:*
    end }
    => { define command-table-menu ?name (?supers) ?more-items end }
 // We use these secondary patterns instead of '?label:expression' in order
 // to prevent the whole '"label" of <type> = command' input from matching
 label:
  { ?:expression }
    => { ?expression }
 type:
  { ?:expression }
    => { ?expression }
 command:
  { (?class:expression, #rest ?options:expression) }
    => { list(?class, ?options) }
  { ?:expression }
    => { ?expression }
 command-table:
  { ?:expression }
    => { ?expression }
end macro command-table-menu-definer;

define sealed method make
    (class == <command-table>,
     #key name, inherit-from = #[], inherit-menu = #f, resource-id = #f)
 => (command-table :: <standard-command-table>)
  let inherit-from = as(<simple-vector>, inherit-from);
  let command-table
    = make(<standard-command-table>,
	   name: name,
	   inherit-from: inherit-from,
	   resource-id:  resource-id);
  when (inherit-menu)
    install-inherited-menus(command-table, inherit-menu)
  end;
  gethash(*command-tables*, name) := command-table;
  command-table
end method make;

define method install-inherited-menus
    (command-table :: <standard-command-table>, inherit-menu) => ()
  check-type(inherit-menu, type-union(one-of(#f, #t, #"menu", #"accelerators"), <list>));
  for (comtab in command-table-inherit-from(command-table))
    when (~instance?(inherit-menu, <sequence>)
	  | member?(comtab, inherit-menu))
      let menu = command-table-menu(comtab);
      when (menu)
        for (decorator :: <command-decorator> in menu)
	  let object  = decorator-object(decorator);
	  let type    = decorator-type(decorator);
	  let label   = decorator-label(decorator);
	  let image   = decorator-image(decorator);
	  let options = decorator-options(decorator);
	  let accelerator   = decorator-accelerator(decorator);
	  let mnemonic      = decorator-mnemonic(decorator);
	  let documentation = decorator-documentation(decorator);
	  let resource-id   = decorator-resource-id(decorator);
	  select (inherit-menu)
	    #"menu" => 
	      accelerator := $unsupplied;
	      mnemonic    := $unsupplied;
	    #"accelerators" =>
	      label := #f;
	    otherwise => #f
	  end;
	  apply(add-command-table-menu-item,
		command-table, label, type, object,
		image: image,
		accelerator: accelerator, mnemonic: mnemonic,
		documentation: documentation, resource-id: resource-id,
		error?: #f, options)
	end
      end
    end
  end
end method install-inherited-menus;

define method remove-command-table
    (command-table :: <standard-command-table>) => ()
  // This could hack inheritance, too...
  remhash(*command-tables*, command-table)
end method remove-command-table;

// The global command table
// Things like the Mouse-Right (menu) translator live here.
define variable *global-command-table* :: <standard-command-table>
    = make(<command-table>,
	   name: #"global-command-table",
	   inherit-from: #[]);

// CLIM's general "user" command table
define variable *user-command-table* :: <standard-command-table>
    = make(<command-table>,
	   name: #"user-command-table",
	   inherit-from: vector(*global-command-table*));


/// Inheritance

//--- This should be more careful to hit each command table only once
//--- (see the new version in CLIM for how to do this)
define method do-command-table-inheritance
    (function :: <function>, command-table :: <standard-command-table>,
     #key do-inherited? = #t) => ()
  function(command-table);
  when (do-inherited?)
    for (comtab in command-table-inherit-from(command-table))
      do-command-table-inheritance(function, comtab, do-inherited?: #t)
    end
  end
end method do-command-table-inheritance;

define method command-present?
    (command-table :: <standard-command-table>, command :: <command-oid>)
 => (present?)
  gethash(command-table-commands(command-table), command)
end method command-present?;

define method command-accessible?
    (command-table :: <standard-command-table>, command :: <command-oid>)
 => (command-table :: false-or(<standard-command-table>))
  block (return)
    do-command-table-inheritance
      (method (comtab)
	 when (command-present?(comtab, command))
	   return(comtab)
	 end
       end method,
       command-table);
    #f
  end
end method command-accessible?;


/// Adding and removing commands

define method add-command
    (command-table :: <standard-command-table>, command :: <command-oid>,
     #key label, name, menu, image,
	  accelerator = $unsupplied, mnemonic = $unsupplied, resource-id,
	  error? = #t) => ()
  let label = label | name;		// for compatibility
  check-type(label, false-or(<string>));
  when (command-present?(command-table, command))
    when (error?)
      cerror("Remove the command and proceed",
	     "The command %= is already present in %=", command, command-table)
    end;
    remove-command(command-table, command)
  end;
  let menu-label   = #f;
  let menu-options = #();
  when (menu)
    menu-label   := if (instance?(menu, <list>)) head(menu) else menu end;
    menu-options := if (instance?(menu, <list>)) tail(menu) else #()  end
  end;
  check-type(menu-label, false-or(<string>));
  let commands = command-table-commands(command-table);
  if (label)
    add-command-line-name(command-table, command, label)
  else
    gethash(commands, command) := #t
  end;
  case
    menu =>
      let type = if (instance?(command, <function>)) <function> else <command> end;
      apply(add-command-table-menu-item,
	    command-table, menu-label, type, command,
	    image: image,
	    accelerator: accelerator, mnemonic: mnemonic, resource-id: resource-id,
	    error?: error?, menu-options);
    (supplied?(accelerator) & accelerator)
    | (supplied?(mnemonic) & mnemonic) =>
      let type = if (instance?(command, <function>)) <function> else <command> end;
      add-command-table-menu-item
	(command-table, #f, type, command,
	 accelerator: accelerator, mnemonic: mnemonic, resource-id: resource-id,
	 error?: error?);
  end
end method add-command;

define method add-command
    (command-table :: <standard-command-table>, decorator :: <command-decorator>,
     #key label, name, menu, image,
	  accelerator = $unsupplied, mnemonic = $unsupplied, resource-id,
	  error? = #t) => ()
  ignore(menu);
  let object = decorator-object(decorator);
  let type   = decorator-type(decorator);
  let label  = label | name | decorator-label(decorator);
  let image  = image        | decorator-image(decorator);
  let accelerator
    = if (supplied?(accelerator)) accelerator else decorator-accelerator(decorator) end;
  let mnemonic
    = if (supplied?(mnemonic)) mnemonic else decorator-mnemonic(decorator) end;
  let resource-id = resource-id | decorator-resource-id(decorator);
  add-command-table-menu-item
    (command-table, label, type, object,
     image: image,
     accelerator: accelerator, mnemonic: mnemonic, resource-id: resource-id,
     error?: error?)
end method add-command;


define method remove-command
    (command-table :: <standard-command-table>, command :: <command-oid>) => ()
  // Remove old command names
  /* let names = command-table.%command-line-names;
     block (break)
       while (names)
	 let index = find-key(names, method (entry) second(entry) = command end);
	 case
	   index =>
	     names := remove-at!(names, index);
	     inc!(*completion-cache-tick*);
	   otherwise =>
	     break()
	 end
       end
     end; */
  // Remove old menu items
  let menu       = command-table-menu(command-table);
  let decorators = #();
  when (menu)
    for (decorator :: <command-decorator> in menu)
      when ((  decorator-type(decorator) == <command>
	     | decorator-type(decorator) == <function>)
	    & decorator-object(decorator) = command)
	push!(decorators, decorator)
      end
    end;
    for (decorator in decorators)
      remove!(menu, decorator)
    end
  end;
  remhash(command-table-commands(command-table), command)
end method remove-command;

define method remove-command
    (command-table :: <standard-command-table>, decorator :: <command-decorator>) => ()
  remove-command(command-table, decorator-object(decorator))
end method remove-command;

/*
define method remove-command-entirely (command :: <command-oid>) => ()
  for (command-table in *command-tables*)
    remove-command(command-table, command)
  end
end method remove-command-entirely;
*/

define method do-command-table-commands
    (function :: <function>, command-table :: <standard-command-table>,
     #key do-inherited? = #f) => ()
  dynamic-extent(function);
  do-command-table-inheritance
    (method (comtab)
       for (command in key-sequence(command-table-commands(comtab)))
	 function(command, comtab)
       end
     end method,
     command-table,
     do-inherited?: do-inherited?)
end method do-command-table-commands;


/// Command table menus

// The radio box and check box types are a bit odd:
//  add-command-table-menu-item
//    (command-table, symbol, <radio-box>, #t,	;or <check-box>
//     items: #[#["True", #t], #["False", #f]],
//     label-key: first, value-key: second,
//     callback: method (g) format-out("Value changed to %=", gadget-value(g)) end)
define method add-command-table-menu-item
    (command-table :: <standard-command-table>, label, type, object,
     #rest keys,
     #key documentation, after = #"end",
	  accelerator = $unsupplied, mnemonic = $unsupplied, resource-id,
          image, text-style, error? = #t,
          items, label-key, value-key, test, callback, update-callback)
 => (decorator :: <command-decorator>)
  dynamic-extent(keys);
  ignore(items, text-style, label-key, value-key, test, callback, update-callback);
  let type
    = type
      | select (object by instance?)
	  <function> =>		 // menu-item "New"  = new-file
	    <function>;
	  <command> =>		 // menu-item "New"  = make(<command>, function: new-file)
	    <command>;
	  <command-table> =>	 // menu-item "File" = *file-command-table*
	    <command-table>;
	  subclass(<command>) => // menu-item "New"  = <new-file-command>
	    <command>;
	  <list> =>		 // menu-item "New"  = (<file-command>, new?: #t)
	    <command>;
	end;
  select (type)
    <command>, <function>, <command-table> =>
      check-type(label, false-or(<string>));
    <menu>, <separator>, <push-box>, <radio-box>, <check-box> =>
      check-type(label, false-or(type-union(<string>, <symbol>)));
  end;
  when (supplied?(accelerator) & accelerator)
    assert(instance?(accelerator, <accelerator>),
	   "%= is not a character or keyboard gesture", accelerator)
  end;
  when (supplied?(mnemonic) & mnemonic)
    assert(instance?(mnemonic, <mnemonic>),
	   "%= is not a character or keyboard gesture", mnemonic)
  end;
  check-type(documentation, false-or(<string>));
  let menu = command-table-menu(command-table);
  let old-item
    = label
      & find-value(menu, method (decorator :: <command-decorator>)
			   label-equal?(decorator-label(decorator), label)
			 end method);
  when (old-item)
    when (error?)
      cerror("Remove the menu item and proceed",
	     "The menu item %= is already present in %=", label, command-table)
    end;
    remove-command-table-menu-item(command-table, label)
  end;
  command-table.%accelerators := #f;		// decache
  with-keywords-removed (options = keys, #[after:, accelerator:, mnemonic:, error?:])
    let menu      = command-table-menu(command-table);
    let decorator = make(<command-decorator>,
			 object:  object,
			 type:    type,
			 label:   label,
			 image:   image,
			 options: options,
			 accelerator:   accelerator,
			 mnemonic:      mnemonic,
			 documentation: documentation,
			 resource-id:   resource-id);
    select (after)
      #"start" =>
	menu := insert-at!(menu, decorator, #"start");
      #"end", #f =>
	menu := insert-at!(menu, decorator, #"end");
      #"sort" =>
	add!(menu, decorator);
        local method label-less? (x, y) => (less? :: <boolean>)
		case
		  ~x => #t;
		  ~y => #f;
		  otherwise => string-less?(x, y);
		end
	      end method;
	command-table-menu(command-table)
	  := sort!(menu,
		   test: method (x :: <command-decorator>, y :: <command-decorator>)
			   label-less?(decorator-label(x), decorator-label(y))
			 end);
      otherwise =>
	if (instance?(after, <string>))
	  let index
	    = find-key(menu,
		       method (decorator :: <command-decorator>)
			 label-equal?(decorator-label(decorator), after)
		       end method);
	  if (index)
	    menu := insert-at!(menu, decorator, index)
	  else
	    error("There is no menu item named %= for 'after:'", after)
	  end
	else
          error("The value for 'after:' is not a string, #\"start\", #\"end\", or #\"sort\"")
	end
    end;
    // Might have done something destructive above, so be careful
    command-table-menu(command-table) := menu;
    // Now that the command is accessible via a menu (or accelerator),
    // make sure that we've really imported it
    when (type == <command> | type == <function>)
      let commands = command-table-commands(command-table);
      let old-name = gethash(commands, object);
      gethash(commands, object) := old-name | #t
    end;
    decorator
  end
end method add-command-table-menu-item;

define method remove-command-table-menu-item
    (command-table :: <standard-command-table>, label) => ()
  check-type(label, false-or(type-union(<string>, <symbol>)));
  let menu = command-table-menu(command-table);
  let index = find-key(menu,
		       method (decorator :: <command-decorator>)
			 label-equal?(decorator-label(decorator), label)
		       end method);
  when (index)
    //--- Is it right for this to remove the whole item even
    //--- if there is still a keystroke accelerator?
    menu := remove-at!(menu, index);
    command-table.%accelerators := #f	// decache
  end
end method remove-command-table-menu-item;

define method do-command-table-menu-items
    (function :: <function>, command-table :: <standard-command-table>,
     #key label, name, do-inherited? = #f) => ()
  dynamic-extent(function);
  let label = label | name;		// for compatibility
  do-command-table-inheritance
    (method (comtab)
       for (decorator :: <command-decorator> in command-table-menu(comtab))
	 when (~label
	       | label-equal?(decorator-label(decorator), label))
	   function(decorator, comtab)
	 end
       end
     end method,
     command-table,
     do-inherited?: do-inherited?)
end method do-command-table-menu-items;

// Starting from the given command table, calls 'function' on all
// of the commands in the command table's menu and all its sub-menus
define method do-command-table-menu-commands
    (function :: <function>, command-table :: <standard-command-table>)
  dynamic-extent(function);
  local method do-one (decorator :: <command-decorator>, comtab)
	  let object = decorator-object(decorator);
	  let type   = decorator-type(decorator);
	  select (type)
	    <function>      => function(object, comtab);
	    <command>       => function(object, comtab);
	    <command-table> => do-command-table-menu-commands(function, object);
	    otherwise       => #f;
	  end
	end method;
  do-command-table-menu-items(do-one, command-table)
end method do-command-table-menu-commands;

// Menu labels can be #f, but we don't want that to match the string "#f"
define function label-equal? (s1, s2) => (true? :: <boolean>)
  s1 & s2
  & if (instance?(s1, <symbol>) | instance?(s2, <symbol>))
      s1 == s2
    else
      string-equal?(s1, s2)
    end
end function label-equal?;


/// Command menu bars and tool bars

define method make-command-menu-bar
    (framem :: <frame-manager>, frame :: <simple-frame>,
     #key command-table = frame-command-table(frame))
 => (menu-bar :: false-or(<menu-bar>))
  when (command-table)
    with-frame-manager (framem)
      let menus = make-menus-from-command-table(command-table, frame, framem);
      when (menus)
	make(<menu-bar>, children: menus)
      end
    end
  end
end method make-command-menu-bar;

//---*** If the command table has a resource ID, do we skip all this?
define method make-menus-from-command-table
    (command-table :: <standard-command-table>,
     frame :: false-or(<frame>), framem :: <frame-manager>,
     #key label = "Misc") => (menus :: <sequence>)
  let misc-items :: <stretchy-object-vector> = make(<stretchy-vector>);
  let menus      :: <stretchy-object-vector> = make(<stretchy-vector>);
  for (decorator :: <command-decorator> in command-table-menu(command-table))
    let type = decorator-type(decorator);
    select (type)
      <command-table> =>
	let comtab = decorator-object(decorator);
	let menu   = make-menu-from-command-table-menu
	               (command-table-menu(comtab), frame, framem,
			label:         as(<string>, decorator-label(decorator)),
			mnemonic:      decorator-mnemonic(decorator),
			documentation: decorator-documentation(decorator),
			resource-id:   decorator-resource-id(decorator),
			command-table: comtab,
			use-accelerators?: #t);
	add!(menus, menu);
      <menu> =>
	// Call the menu creation function to create the menu contents
	let menu = make(<menu>,
			children:      decorator-object(decorator)(frame),
			label:         as(<string>, decorator-label(decorator)),
			mnemonic:      decorator-mnemonic(decorator),
			documentation: decorator-documentation(decorator),
			resource-id:   decorator-resource-id(decorator));
	add!(menus, menu);
      otherwise =>
	add!(misc-items, decorator);
    end
  end;
  unless (empty?(misc-items))
    let misc-menu = make-menu-from-command-table-menu(misc-items, frame, framem,
						      label: label,
						      use-accelerators?: #t);
    add!(menus, misc-menu)
  end;
  menus
end method make-menus-from-command-table;

define method make-menu-from-command-table-menu
    (decorators :: <sequence>,
     frame :: false-or(<frame>), framem :: <frame-manager>,
     #key owner, command-table, label, documentation, resource-id,
	  mnemonic = $unsupplied, item-callback = $unsupplied,
	  use-accelerators? = #f)
 => (menu :: <menu>)
  let menu-items :: <stretchy-object-vector> = make(<stretchy-vector>);
  with-frame-manager (framem)
    local method button-for-command
	      (decorator :: <command-decorator>) => (button :: <menu-button>)
	    let command = decorator-object(decorator);
	    let (callback, command) = callback-for-command(command);
            make(<push-menu-button>,
		 command:       command,
		 label:         as(<string>, decorator-label(decorator)),
		 value:         command,
		 accelerator:   use-accelerators? & decorator-accelerator(decorator),
		 mnemonic:      decorator-mnemonic(decorator),
		 documentation: decorator-documentation(decorator),
		 resource-id:   decorator-resource-id(decorator),
		 enabled?: ~frame | command-enabled?(command, frame),
		 activate-callback:
		   if (supplied?(item-callback)) item-callback
		   else callback end)
          end method,
	  method menu-for-command-table
	      (decorator :: <command-decorator>) => (menu :: <menu>)
	    let comtab = decorator-object(decorator);
            make-menu-from-command-table-menu
              (command-table-menu(comtab), frame, framem,
               label:         as(<string>, decorator-label(decorator)),
	       mnemonic:      decorator-mnemonic(decorator),
	       documentation: decorator-documentation(decorator),
               resource-id:   decorator-resource-id(decorator),
	       command-table: comtab,
	       use-accelerators?: use-accelerators?)
	  end method,
	  // Example:
          //  add-command-table-menu-item
          //    (command-table, symbol, <radio-box>, #f,
          //     items: #[#["Direct methods", #f], #["All methods", #t]],
          //     label-key: first, value-key: second,
          //     callback: method (g) sheet-frame(g).all-methods? := gadget-value(g) end)
	  method component-for-box
	      (decorator :: <command-decorator>, selection-mode)
	   => (menu-box :: <menu-box>)
	    let options  = decorator-options(decorator);
	    let items    = get-property(options, items:, default: #());
	    let callback = get-property(options, callback:);
	    let label-key
	      = get-property(options, label-key:, default: collection-gadget-default-label-key);
	    let value-key
	      = get-property(options, value-key:, default: collection-gadget-default-value-key);
	    let test = get-property(options, test:, default: \==);
	    let update-callback = get-property(options, update-callback:);
	    let callback-args
	      = if (selection-mode == #"none")
		  vector(activate-callback: callback)
		else
		  vector(value-changed-callback: callback)
		end;
	    apply(make, <menu-box>,
		  command:         callback,
		  items:           items,
		  selection-mode:  selection-mode,
		  resource-id:     decorator-resource-id(decorator),
		  value:           decorator-object(decorator),
		  label-key:       label-key,
		  value-key:       value-key, test: test,
		  update-callback: update-callback,
		  callback-args)
	  end method,
          method collect-buttons
	      (buttons :: <stretchy-object-vector>) => ()
            unless (empty?(buttons))
              add!(menu-items,
                   make(<push-menu-box>,
			children: as(<simple-vector>, buttons)));
              buttons.size := 0
            end
          end method;
    let buttons :: <stretchy-object-vector> = make(<stretchy-vector>);
    local method do-decorators (decorators :: <sequence>) => ()
	    for (decorator :: <command-decorator> in decorators)
	      let type = decorator-type(decorator);
	      select (type)
		<function>, <command> =>
		  add!(buttons, button-for-command(decorator));
		<command-table> =>
		  add!(buttons, menu-for-command-table(decorator));
		<menu> =>
		  // Call the menu creation function to create the menu contents
		  let menu = make(<menu>,
				  children:      decorator-object(decorator)(frame),
				  label:         as(<string>, decorator-label(decorator)),
				  mnemonic:      decorator-mnemonic(decorator),
				  documentation: decorator-documentation(decorator),
				  resource-id:   decorator-resource-id(decorator));
		  add!(buttons, menu);
		<separator> =>
		  collect-buttons(buttons);
                  // If the separator specifies a command table, that means we
                  // should "inline" it in the current menu right after the separator
                  when (instance?(decorator-object(decorator), <command-table>))
                    do-decorators(command-table-menu(decorator-object(decorator)))
                  end;
		<radio-box> =>
		  collect-buttons(buttons);
		  add!(menu-items, component-for-box(decorator, #"single"));
		<check-box> =>
		  collect-buttons(buttons);
		  add!(menu-items, component-for-box(decorator, #"multiple"));
		<push-box> =>
		  collect-buttons(buttons);
		  add!(menu-items, component-for-box(decorator, #"none"));
	      end
	    end
          end method;
    do-decorators(decorators);
    make(<menu>,
         owner: owner,
	 label: label,
	 command: command-table,
	 mnemonic: mnemonic,
	 documentation: documentation,
	 resource-id: resource-id,
	 // No need to group the last buttons...
	 children: concatenate(menu-items, buttons))
  end
end method make-menu-from-command-table-menu;


define method make-command-tool-bar
    (framem :: <frame-manager>, frame :: <simple-frame>,
     #key command-table = frame-command-table(frame))
 => (tool-bar :: false-or(<tool-bar>))
  when (command-table)
    // An alist of #[command-table buttons]
    // We do this in order to flatten out any command table inheritance,
    // since tool bars are not hierarchical
    let comtab-buttons :: <stretchy-object-vector> = make(<stretchy-vector>);
    with-frame-manager (framem)
      local method button-for-command
		(decorator :: <command-decorator>) => (button :: <button>)
	      let command = decorator-object(decorator);
	      let (callback, command) = callback-for-command(command);
	      make(<push-button>,
		   command:     command,
		   value:       command,
		   label:       decorator-image(decorator),
		   accelerator: decorator-accelerator(decorator),
		   mnemonic:    decorator-mnemonic(decorator),
		   // The documentation is typically used as a "tool tip",
		   // so we use the item's label instead of the item's
		   // documentation since it looks so much better
		   documentation:     as(<string>, decorator-label(decorator)),
		   enabled?:          ~frame | command-enabled?(command, frame),
		   activate-callback: callback)
	    end method,
	    method component-for-box
		(decorator :: <command-decorator>, selection-mode)
	     => (menu-box :: <menu-box>)
	      let options  = decorator-options(decorator);
	      let items    = get-property(options, items:, default: #());
	      let callback = get-property(options, callback:);
	      let label-key
		= get-property(options, label-key:,
			       default: collection-gadget-default-label-key);
	      let value-key
		= get-property(options, value-key:, 
			       default: collection-gadget-default-value-key);
	      let test = get-property(options, test:, default: \==);
	      make(<menu-box>,
		   command:     callback,
		   items:       items,
		   selection-mode: selection-mode,
		   resource-id: decorator-resource-id(decorator),
		   value:       decorator-object(decorator),
		   label-key:   label-key,
		   value-key:   value-key,
		   test:        test,
		   value-changed-callback: callback)
	    end method,
	    method collect-buttons (command-table :: <standard-command-table>) => ()
	      let entry = find-pair(comtab-buttons, command-table);
	      unless (entry)		// do each command table only once
		let buttons = make(<stretchy-vector>);
		add!(comtab-buttons, vector(command-table, buttons));
		for (decorator :: <command-decorator> in command-table-menu(command-table))
		  let type  = decorator-type(decorator);
		  select (type)
		    <function>, <command> =>
		      when (decorator-image(decorator))
			add!(buttons, button-for-command(decorator))
		      end;
		    <command-table> =>
		      collect-buttons(decorator-object(decorator));
		    <menu> =>
		      error("Menus not supported in command-table tool bars");
		    <separator> =>
		      when (instance?(decorator-object(decorator), <command-table>))
			collect-buttons(decorator-object(decorator))
		      end;
		    <radio-box> =>
		      when (decorator-image(decorator))
			add!(buttons, component-for-box(decorator, #"single"))
		      end;
		    <check-box> =>
		      when (decorator-image(decorator))
			add!(buttons, component-for-box(decorator, #"multiple"))
		      end;
		    <push-box> =>
		      when (decorator-image(decorator))
			add!(buttons, component-for-box(decorator, #"none"))
		      end;
		  end
	        end
	      end
	    end method;
      collect-buttons(command-table);
      // Each command table gets its own closely spaced vbox
      let boxes = make(<stretchy-vector>);
      for (entry in comtab-buttons)
        let buttons = entry[1];
        unless (empty?(buttons))
          add!(boxes, make(<row-layout>,
			   children: buttons,
      			   x-spacing: 0))
        end
      end;
      // Now make the tool bar with a little space between vboxes
      make(<tool-bar>,
	   child: make(<row-layout>,
		       children: boxes,
		       x-spacing: 2))
    end
  end
end method make-command-tool-bar;


define method do-frame-commands
    (function :: <function>, frame :: <frame>,
     #key test :: <function> = identity,
          menu-bar? = #t, tool-bar? = #t, owned-menus? = #t)
 => ()
  local method maybe-do-sheet-command (sheet) => ()
	  when (gadget?(sheet))
	    let command = gadget-command(sheet);
	    if (command & test(command))
	      function(sheet, command)
	    end
	  end
	end method maybe-do-sheet-command;
  when (menu-bar?)
    let menu-bar = frame-menu-bar(frame);
    when (menu-bar)
      do-sheet-tree(maybe-do-sheet-command, menu-bar)
    end
  end;
  when (tool-bar?)
    let tool-bar = frame-tool-bar(frame);
    when (tool-bar)
      do-sheet-tree(maybe-do-sheet-command, tool-bar)
    end
  end;
  when (owned-menus?)
    for (menu in frame-owned-menus(frame))
      do-sheet-tree(maybe-do-sheet-command, menu)
    end
  end
end method do-frame-commands;

// Map over all gadgets corresponding to a command.  Command might be a
// command, a command function, or even a command table.  Note that this
// won't find sub-menus that were added with a type of <menu>.
define method do-command-menu-gadgets
    (function :: <function>, frame :: <frame>,
     command :: type-union(<command>, <function>, <command-table>),
     #key menu-bar? = #t, tool-bar? = #t, owned-menus? = #t)
 => ()
  do-frame-commands
    (method (gadget :: <gadget>, gcommand)
       if (gcommand = command
	     | (instance?(gcommand, <functional-command>)
		  & command-function(gcommand) = command))
	 function(gadget)
       end
     end, frame,
     menu-bar?: menu-bar?, tool-bar?: tool-bar?, owned-menus?: owned-menus?)
end method do-command-menu-gadgets;

define method do-command-menu-gadgets
    (function :: <function>, frame :: <frame>,
     command :: subclass(<command>),
     #key menu-bar? = #t, tool-bar? = #t, owned-menus? = #t)
 => ()
  do-frame-commands
    (method (gadget :: <gadget>, gcommand)
       if (gcommand = command
	     | (instance?(gcommand, <class>)
		  & subtype?(gcommand, command))
	     | (instance?(gcommand, <list>)
		  & subtype?(head(gcommand), command)))
	 function(gadget)
       end
     end, frame,
     menu-bar?: menu-bar?, tool-bar?: tool-bar?, owned-menus?: owned-menus?)
end method do-command-menu-gadgets;


/// Keystroke accelerators

define method do-command-table-accelerators
    (function :: <function>, command-table :: <standard-command-table>,
     #key accelerator, do-inherited? = #f) => ()
  dynamic-extent(function);
  do-command-table-inheritance
    (method (comtab)
       for (decorator :: <command-decorator> in command-table-menu(comtab))
	 when (decorator-accelerator(decorator)
	       & (~accelerator
		  | decorator-accelerator(decorator) = accelerator))
	   function(decorator, comtab)
	 end
       end
     end method,
     command-table,
     do-inherited?: do-inherited?)
end method do-command-table-accelerators;

define method command-table-accelerators
    (command-table :: <standard-command-table>)
 => (accelerators :: <vector>)
  command-table.%accelerators
  | begin
      let accelerators :: <stretchy-object-vector> = make(<stretchy-vector>);
      for (decorator :: <command-decorator> in command-table-menu(command-table))
	let accelerator = decorator-accelerator(decorator);
	when (supplied?(accelerator) & accelerator)
	  add!(accelerators, accelerator)
	end
      end;
      command-table.%accelerators := accelerators;
      accelerators
    end
end method command-table-accelerators;


/// Stubs for command line names

define open generic add-command-line-name
    (command-table :: <command-table>, command, command-name) => ();

define open generic remove-command-line-name
    (command-table :: <command-table>, command-name) => ();

define open generic do-command-line-names
    (function :: <function>, command-table :: <command-table>, #key do-inherited?) => ();

/*
//--- All of this belongs in a command-line layer

define variable *completion-cache-tick*  :: <integer> = 0;

define method add-command-line-name
    (command-table :: <standard-command-table>, command :: <command-oid>, command-name) => ()
  let commands = command-table-commands(command-table);
  // Make the command directly accessible in the command table, but
  // don't change the primary name if there already is one
  unless (instance?(gethash(commands, command), <string>))
    gethash(commands, command) := command-name
  end;
  //--- Use binary insertion on command line names (completion aarrays)
  remove-command-line-name(command-table, command-name);
  let names
    = command-table.%command-line-names
      | (command-table.%command-line-names := make(<stretchy-vector>));
  add!(names, list(command-name, command));
  command-table.%command-line-names
    := sort!(names,
             test: method (x, y)
                     string-less?(first(x), first(y))
                   end);
  inc!(*completion-cache-tick*)
end method add-command-line-name;

define method remove-command-line-name
    (command-table :: <standard-command-table>, command-name) => ()
  //--- Use binary deletion on command line names (completion aarrays)
  let names = command-table.%command-line-names;
  let index = names
	      & find-key(names,
			 method (entry) string-equal?(first(entry), command-name) end);
  when (index)
    let command = second(command-table.names[index]);
    let commands = command-table-commands(command-table);
    when (string-equal?(command-name, gethash(commands, command)))
      // Remove the primary command-line name for this command,
      // but don't claim that the command does not exist
      gethash(commands, command) := #t
    end;
    remove-at!(names, index);
    inc!(*completion-cache-tick*)
  end
end method remove-command-line-name;

define method do-command-line-names
    (function :: <function>, command-table :: <standard-command-table>,
     #key label, name, do-inherited? = #f) => ()
  dynamic-extent(function);
  let label = label | name;		// for compatibility
  do-command-table-inheritance
    (method (comtab)
       let names = command-table.%command-line-names;
       when (names)
	 for (entry in names)
	   when (~label
		 | method (entry) string-equal?(first(entry), label))
	     function(first(entry), second(entry), comtab)
	   end
	 end
       end
     end method,
     command-table,
     do-inherited?: do-inherited?)
end method do-command-line-names;

define method command-line-name-for-command
    (command, command-table :: <standard-command-table>, #key error? = #t)
 => (command-name)
  block (return)
    do-command-table-inheritance
      (method (comtab)
	 let command-name = gethash(command-table-commands(comtab), command);
	 when (instance?(command-name, <string>))
	   return(command-name)
	 end
       end method,
       command-table);
    select (error?)
      #f => #f;
      #"create" => command-name-from-command(command);	//---*** this can't work
      otherwise =>
	error("The command %= has no command line name in %=", command, command-table)
    end
  end
end method command-line-name-for-command;

define method command-table-completion-alist
    (command-table :: <standard-command-table>) => (alist :: <vector>)
  when (~command-table.%completion-alist
	| (*completion-cache-tick* > command-table.%completion-alist-tick))
    command-table.%completion-alist := make(<stretchy-vector>);
    do-command-table-inheritance
      (method (comtab)
	 let names = command-table.%command-line-names;
	 when (names)
	   for (entry in names)
	     add!(command-table.%completion-alist, entry)
	   end
	 end
       end method,
       command-table);
    command-table.%completion-alist
      := remove-duplicates!(command-table.%completion-alist,
			    test: method (x, y) second(x) == second(y) end);
    command-table.%completion-alist
      := sort!(command-table.%completion-alist,
	       test: method (x, y) string-less?(first(x), first(y)) end);
    command-table.%completion-alist-tick := *completion-cache-tick*
  end;
  command-table.%completion-alist
end command-table-completion-alist;
*/


/// Stubs for presentation translators

define open generic add-presentation-translator
    (command-table :: <command-table>, translator, #key error?) => ();

define open generic remove-presentation-translator
    (command-table :: <command-table>, translator-name) => ();

define open generic do-presentation-translators
    (function :: <function>, command-table :: <command-table>, #key do-inherited?) => ();

/*
//--- All of this belongs in a presentation layer

define variable *translators-cache-tick* :: <integer> = 0;

define method add-presentation-translator
    (command-table :: <standard-command-table>, translator,
     #key error? = #t) => ()
  let translator-name = presentation-translator-name(translator);
  let translators
    = command-table-translators(command-table)
      | (command-table-translators(command-table) := make(<stretchy-vector>));
  let place 
    = find-key(translators,
	       method (entry) presentation-translator-name(entry) == translator-name end);
  case
    place =>
      when (error?)
	cerror("Remove the translator and proceed",
	       "The translator %= is already present in %=", 
	       presentation-translator-name(translator), command-table)
      end;
      translators[place] := translator;
    otherwise =>
      add!(translators, translator)
  end;
  // Maybe one day we'll do incremental updating of the cache.  That day
  // is not today.
  inc!(*translators-cache-tick*)
end method add-presentation-translator;

define method remove-presentation-translator
    (command-table :: <standard-command-table>, translator-name) => ()
  let translators = command-table-translators(command-table);
  when (translators)
    let translator
      = if (instance?(translator-name, <symbol>))
	  find-value(command-table-translators(comtab),
		     method (entry) 
		       presentation-translator-name(entry) == translator-name
		     end)
	else
	  translator-name
	end;
    remove!(translators, translator);
    inc!(*translators-cache-tick*)
  end
end method remove-presentation-translator;

define method do-presentation-translators
    (function :: <function>, command-table :: <standard-command-table>,
     #key name, do-inherited? = #f) => ()
  dynamic-extent(function);
  do-command-table-inheritance
    (method (comtab)
       let translators = command-table-translators(comtab);
       when (translators)
	 for (translator in translators)
	   when (~name
		 | translator-name(translator) == name)
	     function(translator, comtab)
	   end
	 end
       end
     end method,
     command-table,
     do-inherited?: do-inherited?)
end method do-presentation-translators;

define method remove-presentation-translator-entirely (name) => ()
  for (command-table in *command-tables*)
    remove-presentation-translator(command-table, name)
  end
end method remove-presentation-translator-entirely;

define method clear-presentation-translator-caches () => ()
  for (command-table in *command-tables*)
    when (command-table.%translators-cache)
      remove-all-keys!(command-table.%translators-cache)
    end
  end;
  inc!(*translators-cache-tick*)
end method clear-presentation-translator-caches;
*/


/// Simple command-defining macrology

/*
//---*** Not yet...

define sealed class <command-argument> (<object>)
  sealed slot command-argument-name, required-init-keyword: name:;
  sealed slot command-argument-type, required-init-keyword: type:;
  sealed slot command-argument-default = #f, init-keyword: default:;
  sealed slot command-argument-provide-default? = #t, init-keyword: provide-default?:;
  sealed slot command-argument-prompt = #f, init-keyword: prompt:;
end class <command-argument>;

define sealed domain make (singleton(<command-argument>));
define sealed domain initialize (<command-argument>);

// For example,
//   define command hardcopy-file
//       (file :: <locator>;
//        printer :: <printer> = $unsupplied, prompt: "printer";
//        copies :: <integer> = 1, prompt: "copies")
//       (command-table: *print-comtab*)
//     do-hardcopy-file(file, printer: printer, copies: copies)
//   end command hardcopy-file;
define macro command-definer
  { define command ?:name (?arguments:*) (#rest ?options:expression) ?:body end }
    => { define command-method ?name (?arguments) ?body end;
         define command-parser ?name (?arguments) (?options) end; }
end macro command-definer;

// Define the method for the command, discarding argument options
define macro command-method-definer
  { define command-method ?:name (?arguments:*) ?:body end }
    => { define method ?name (?arguments) ?body end }
 arguments:
  { } => { }
  // Map over semicolon-separated arguments, reducing each to a
  // parameter list entry.  When we get to the first argument that
  // has a default, insert '#key' into the parameter list
  { ?:variable = ?default:expression, #rest ?options:expression; ?key-arguments }
    => { #key ?variable = ?default, ?key-arguments }
  { ?argument; ... } => { ?argument, ... }
 key-arguments:
  { } => { }
  { ?key-argument; ... } => { ?key-argument, ... }
 argument:
  { ?:variable, #rest ?options:expression }
    => { ?variable }
 key-argument:
  { ?:variable = ?default:expression, #rest ?options:expression }
    => { ?variable = ?default }
end macro command-method-definer;

// Define the method for the command, discarding argument options
define macro command-parser-definer
  { define command-parser ?:name (?arguments:*) (#rest ?options:expression) end }
    => { install-command(?name, vector(?arguments), ?options) }
 arguments:
  { } => { }
  { ?argument; ... } => { ?argument, ... }
 argument:
  { ?:name :: ?type:expression = ?default:expression, #rest ?options:expression }
    => { make(<command-argument>,
	      name: ?"name", type: ?type,
	      default: ?default, ?options) }
  { ?:name :: ?type:expression, #rest ?options:expression }
    => { make(<command-argument>,
	      name: ?"name", type: ?type,
	      ?options) }
end macro command-parser-definer;

define method install-command
    (command, argument-info,
     #key command-table, name, menu, image,
	  accelerator = $unsupplied, mnemonic = $unsupplied) => ()
  when (command-table)
    add-command(command-table, command,
		name: name, menu: menu,
		image: image,
		accelerator: accelerator, mnemonic: mnemonic,
		error?: #f)
  end
end method install-command;
*/
