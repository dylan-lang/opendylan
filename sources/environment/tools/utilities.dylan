Module:    environment-tools
Synopsis:  Environment Tools Utilities
Author:    Andy Armstrong, Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Message boxes

// Each of these message functions can be made to produce alert dialogs
// that look like any of the others by supplying the appropriate keyword
// parameters. The main difference between each of them is the default
// icon and set of buttons.

// DUIM doesn't specify the specializer of exit-type, so we'll do it
// ourselves to prevent errors in environment code.
define constant <exit-type> = one-of(#"ok", #"yes", #"no", #"cancel");

// A little convenience function we use in each of the message functions.
define function assert-owner-supplied (owner) => ()
  assert(~unsupplied?(owner), "Required keyword parameter owner: is unsupplied");
end function assert-owner-supplied;

define function environment-message
    (message :: <string>,
     #rest keys,
     #key owner = $unsupplied,
          title :: false-or(<string>),
          exit-style :: false-or(<symbol>))
 => ()
  assert-owner-supplied(owner);
  apply(notify-user, message,
	title: title | release-product-name(),
	style: #"information",
	keys);
end function environment-message;

define function environment-warning-message
    (message :: <string>,
     #rest keys,
     #key owner = $unsupplied,
          title :: false-or(<string>),
          exit-style :: false-or(<symbol>))
 => ()
  assert-owner-supplied(owner);
  apply(notify-user, message,
	title: title | release-product-name(),
	style: #"warning",
	keys);
end function environment-warning-message;

define function environment-error-message
    (message :: <string>,
     #rest keys,
     #key owner = $unsupplied,
          title :: false-or(<string>),
          exit-style :: false-or(<symbol>))
 => ()
  assert-owner-supplied(owner);
  apply(notify-user, message,
	title: title | release-product-name(),
	style: #"error",
	keys);
end function environment-error-message;

define function environment-question
    (message :: <string>,
     #rest keys,
     #key owner = $unsupplied,
          title :: false-or(<string>),
          style :: <symbol> = #"information",
          exit-style :: false-or(<symbol>),
          cancel :: <boolean>)
 => (ok? :: <boolean>, exit-type :: <exit-type>)
  assert-owner-supplied(owner);
  apply(notify-user, message,
	title: title | release-product-name(),
	exit-style: case
		      exit-style => exit-style;
		      cancel     => #"yes-no-cancel";
		      otherwise  => #"yes-no";
		    end,
	keys)
end function environment-question;

define function environment-action-unavailable
    (frame :: <frame>, message :: <string>)
 => ()
  if (frame-status-bar(frame))
    frame-status-message(frame) := message;
    beep(frame)
  else
    environment-error-message(message, owner: frame)
  end
end function environment-action-unavailable;

define function not-yet-implemented
    (#key message :: false-or(<string>), 
          owner = $unsupplied)
 => ()
  assert-owner-supplied(owner);
  environment-error-message(message | "Not yet implemented!", owner: owner)
end function not-yet-implemented;


/// File filters

define function environment-choose-file
    (#key title :: false-or(<string>),
          owner = $unsupplied,
          directory :: false-or(<directory-locator>) = #f,
          default :: false-or(<file-system-locator>) = directory,
          direction :: one-of(#"input", #"output") = #"input",
          filters :: false-or(<sequence>) = #f,
          filter :: false-or(<symbol>))
 => (filename :: false-or(<file-locator>),
     filter :: false-or(<symbol>))
  assert-owner-supplied(owner);
  let actual-filters = filters & apply(filters-for-file-types, filters);
  let (filename, filter-index)
    = choose-file(title:          title,
		  frame:          owner,
		  default:        default & as(<string>, default),
		  direction:      direction,
		  filters:        actual-filters,
		  default-filter: if (filters)
				    (filter & position(filters, filter)) | 0
				  end);
  values(filename & as(<file-locator>, filename),
	 filters & filter-index & filters[filter-index])
end function environment-choose-file;

define function make-filter
    (extension :: <string>)
  concatenate("*.", extension)
end function make-filter;

define class <lazy-table-value> (<object>)
  constant slot %thunk :: <function>,
    required-init-keyword: thunk:;
end class <lazy-table-value>;

define function lazy-element
    (table :: <table>, key, #rest keys, #key, #all-keys) => (element)
  let value = apply(element, table, key, keys);
  select (value by instance?)
    <lazy-table-value> =>
      let element = value.%thunk();
      table[key] := element;
    otherwise =>
      value
  end
end function lazy-element;

//---*** It would be cool to generalize this to work in functional-extensions
define macro lazy-table-definer
  { define lazy-table ?table-name:name = { ?entries } }
    => { define constant ?table-name :: <table> = make(<table>);
         begin let the-table = ?table-name; ?entries end; }
  { define lazy-table ?table-name:name :: ?table-type:name = { ?entries } }
    => { define constant ?table-name :: ?table-type = make(?table-type);
         begin let the-table = ?table-name; ?entries end; }
 entries:
  { } => { }
  { ?key:expression => ?value:expression, ... }
    => { the-table[ ?key ]
	   := make(<lazy-table-value>,
		   thunk: method () ?value end);
	 ... }
end macro lazy-table-definer;

// cpage: 1997.02.04 Lazily initialize some filter values, in the same
//        way that lazy-table-definer initializes a table of filters.
//        We need this for filters that have the same file type
//        extension so we can get a handle on them to compare for
//        them later.

define variable %filter-for-lid-as-hdp :: <vector> = #[];

define function filter-for-lid-as-hdp () => (filter :: <vector>)
  if (%filter-for-lid-as-text ~= #[])
    %filter-for-lid-as-hdp
  else
    %filter-for-lid-as-hdp
      := vector("Dylan Library Interchange Descriptions (import)",
		make-filter(lid-file-extension()))
  end if
end function filter-for-lid-as-hdp;

define variable %filter-for-lid-as-text :: <vector> = #[];

define function filter-for-lid-as-text () => (filter :: <vector>)
  if (%filter-for-lid-as-text ~= #[])
    %filter-for-lid-as-text
  else
    %filter-for-lid-as-text
      := vector("Dylan Library Interchange Descriptions (as text)",
		make-filter(lid-file-extension()))
  end if
end function filter-for-lid-as-text;

// This filter is for an internal hack to allow internal developers
// to open LID files directly without importing them to project files.
// Hopefully someday we'll all use project files and dispense with this.
define variable %filter-for-lid-without-importing :: <vector> = #[];

define function filter-for-lid-without-importing () => (filter :: <vector>)
  if (%filter-for-lid-without-importing ~= #[])
    %filter-for-lid-without-importing
  else
    %filter-for-lid-without-importing
      := vector("Dylan LIDs (without importing)",
		make-filter(lid-file-extension()))
  end if
end function filter-for-lid-without-importing;

define lazy-table $file-type-filters
  = { #"common"        => /* The common file types for opening files. */
        vector("Common Files",
	       make-filter(project-file-extension()),
	       make-filter(dylan-file-extension()), "*.dyl",
	       "*.spec",
	       "*.rc",
	       "*.txt", "*.text",
	       "*.c",   "*.cpp", "*.cxx",
	       "*.h",   "*.hpp", "*.hxx", "*.inl"),
      #"common-insert" => /* The common file types for inserting source files. */
        vector("Common Files",
	       make-filter(project-file-extension()),
	       make-filter(dylan-file-extension()), "*.dyl",
	       "*.rc",
	       "*.spec",
	       "*.txt", "*.text",
	       "*.c",   "*.cpp", "*.cxx",
	       "*.h",   "*.hpp", "*.hxx", "*.inl",
	       "*.lib"),
      #"common-locate-project" => /* The common file types for locating project/LID files. */
         vector("Common Files",
	       make-filter(project-file-extension()),
	       make-filter(lid-file-extension())),
      #"project"       =>
        vector("Open Dylan Projects",
               make-filter(project-file-extension())),
      #"lid"           => 
        vector("Dylan Library Interchange Descriptions",
               make-filter(lid-file-extension())),
      #"lid-as-hdp"    => filter-for-lid-as-hdp(),
      #"lid-without-importing"
                       => filter-for-lid-without-importing(),
      #"lid-as-text"   => filter-for-lid-as-text(),
      #"dylan"         =>
        vector("Dylan Source Files",
	       make-filter(dylan-file-extension()),
	       "*.dyl"),
      #"tool-spec"     => #["Open Dylan Tool Specifications", "*.spec"],
      #"executable"    => #["Programs",        "*.exe"],
      #"resource"      => #["Resource Files",  "*.rc"],
      #"build-script"  => #["Jam build scripts", "*.jam"],
      #"text"          => #["Text Files",      "*.txt",
                                               "*.text"],
      #"c"             => #["C Source Files",  "*.c",
                                               "*.cpp",
                                               "*.cxx"],
      #"c-include"     => #["C Includes",      "*.h",
                                               "*.hpp",
                                               "*.hxx",
                                               "*.inl"],
      #"library"       => #["C Libraries",     "*.lib"],
      #"all"           => #["All Files (*.*)", "*.*"] };

define function filter-for-file-type
    (type :: <symbol>) => (filter :: <sequence>)
  lazy-element($file-type-filters, type, default: #f)
    | error("Can't find filter for unrecognised file type %s", type)
end function filter-for-file-type;

define function filters-for-file-types
    (#rest types :: <sequence>) => (filters :: <sequence>)
  map(filter-for-file-type, types)
end function filters-for-file-types;


/// Percentages
///---*** should be somewhere more standard?

define class <percentage> (<object>)
  sealed constant slot percentage-value :: <single-float>,
    init-keyword: value:;
end class <percentage>;

define method percentage
    (amount :: <real>, total :: <real>)
 => (percentage :: <percentage>)
  let t = as(<single-float>, total);
  make(<percentage>,
       value: if (t = 0.0) 100.0 else (as(<single-float>, amount) * 100) / t end)
end method percentage;

define method \=
    (object1 :: <percentage>, object2 :: <percentage>)
 => (equal? :: <boolean>)
  object1.percentage-value = object2.percentage-value
end method \=;

define method \<
    (object1 :: <percentage>, object2 :: <percentage>)
 => (less-than? :: <boolean>)
  object1.percentage-value < object2.percentage-value
end method \<;

define constant $percentage-suffix = "%";

define method percentage-label
    (percentage :: <percentage>,
     #key decimal-points :: <integer> = 0)
 => (label :: <string>)
  let value = percentage.percentage-value;
  concatenate(float-to-string(value, decimal-points: decimal-points),
	      $percentage-suffix)
end method percentage-label;


/// Some useful functions

define method frame-undefined-callback
    (frame :: <frame>) => ()
  not-yet-implemented(owner: frame)
end method frame-undefined-callback;

define method frame-sort-items
    (frame :: <frame>, items :: <sequence>,
     #key key = identity, label-key, test = \<)
 => (sorted-items :: <sequence>)
  let sorted-items
    = keyed-sort(items,
	     key: if (key == identity)
		    label-key | curry(frame-default-object-name, frame)
		  else
		    method (item)
		      let object = item.key;
		      if (label-key)
			label-key(object)
		      else
			frame-default-object-name(frame, object)
		      end
		    end
		  end,
	     test: test);
  assert(sorted-items,
	 "This has to be a sequence!");
  sorted-items
end method frame-sort-items;

/*---*** This version messes up in the compiler!
define method frame-sort-items
    (frame :: <frame>, items :: <sequence>,
     #key key = identity, label-key, test = \<)
 => (sorted-items :: <sequence>)
  keyed-sort(items,
	     key: if (key == identity)
		    label-key | curry(frame-default-object-name, frame)
		  else
		    method (item)
		      let object = item.key;
		      if (label-key)
			label-key(object)
		      else
			frame-default-object-name(frame, object)
		      end
		    end
		  end,
	     test: test)
end method frame-sort-items;
*/

/// Labelling

define function make-labels-layout
    (descriptions :: <sequence>,
     #key prefix, y-spacing = 0, border = 0,
          foreground, background, text-style)
 => (layout :: <layout>)
  let labels = make(<vector>, size: descriptions.size);
  for (description in descriptions,
       index from 0)
    let label
      = if (prefix)
	  concatenate-as(<byte-string>, prefix, description)
	else
	  description
	end;
    labels[index] 
      := make(<label>, 
	      label: label, 
	      foreground: foreground,
	      background: background,
	      text-style: text-style)
  end;
  make(<column-layout>, children: labels, y-spacing: y-spacing, border: border)
end function make-labels-layout;


/// "Save" toolbar button title
/// --- hughg, 1997/01/23: Currently only exported for use by env-deuce.
///     This should go elsewhere if/when any other tool has a Save button.

define constant $save-file-title = "Save";


/// Tool cloning

define open generic make-clone
    (frame :: <frame>, #rest initargs) => (new-frame :: <frame>);

define method make-clone
    (frame :: <frame>, #rest initargs)
 => (new-frame :: <frame>)
  apply(make, object-class(frame), initargs)
end method make-clone;

define method make-clone
    (frame :: <frame-primary-object-mixin>, #rest initargs)
 => (new-frame :: <frame-primary-object-mixin>)
  apply(next-method, frame, object: frame.frame-primary-object, initargs)
end method make-clone;


/// Environment persistent state

define variable $default-environment-frame-width  = 800;
define variable $default-environment-frame-height = 600;

define open class <frame-window-settings-mixin> (<frame>)
end class <frame-window-settings-mixin>;


define open generic frame-default-position
    (frame :: <frame-window-settings-mixin>)
 => (x :: false-or(<integer>), y :: false-or(<integer>));

define open generic set-frame-default-position
    (frame :: <frame-window-settings-mixin>, x :: <integer>, y :: <integer>)
 => ();

define open generic frame-default-size
    (frame :: <frame-window-settings-mixin>)
 => (width :: false-or(<integer>), height :: false-or(<integer>));

define open generic set-frame-default-size
    (frame :: <frame-window-settings-mixin>,
     width :: <integer>, height :: <integer>)
 => ();

define open generic frame-default-state
    (frame :: <frame-window-settings-mixin>) => (state :: <symbol>);

define open generic frame-default-state-setter
    (state :: <symbol>, frame :: <frame-window-settings-mixin>)
 => (state :: <symbol>);


define open generic frame-cascades?
    (frame :: <frame-window-settings-mixin>)
 => (save-settings? :: <boolean>);

define open generic save-window-settings
    (frame :: <frame-window-settings-mixin>)
 => ();

define open generic restore-window-settings
    (frame :: <frame-window-settings-mixin>)
 => ();

define method frame-cascades?
    (frame :: <frame-window-settings-mixin>) => (save-settings? :: <boolean>)
  #f
end method frame-cascades?;

define method save-window-settings 
    (frame :: <frame-window-settings-mixin>) => ()
  let (x, y) = frame-position(frame);
  let (width, height) = frame-size(frame);
  set-frame-default-position(frame, x, y);
  set-frame-default-size(frame, width, height);
  frame-default-state(frame)
    := case
	 //--- Get these cases to work...
	 // frame-minimized?(frame) => #"minimized";
	 // frame-maximized?(frame) => #"maximized";
	 otherwise               => #"normal";
       end;
  frame-status-message(frame) := "Window settings saved"
end method save-window-settings;

define method restore-window-settings
    (frame :: <frame-window-settings-mixin>) => ()
  let (width, height) = frame-default-size(frame);
  if (width & height) set-frame-size(frame, width, height) end;
  unless (frame-cascades?(frame))
    let (x, y) = frame-default-position(frame);
    if (x & y) set-frame-position(frame, x, y) end;
  end
end method restore-window-settings;

define method initialize
    (frame :: <frame-window-settings-mixin>, #key) => ()
  next-method();
  restore-window-settings(frame)
end method initialize;

define method handle-event
    (frame :: <frame-window-settings-mixin>,
     event :: <window-configuration-event>)
 => ()
  next-method();
  if (~frame-cascades?(frame))
    save-window-settings(frame)
  end;
end method handle-event;

define method handle-event
    (frame :: <frame-window-settings-mixin>, event :: <frame-exit-event>)
 => ()
  if (~frame-cascades?(frame))
    save-window-settings(frame)
  end;
  next-method()
end method handle-event;

define command-table *window-settings-command-table* (*global-command-table*)
  menu-item "Save Window Settings" = save-window-settings,
    documentation: "Saves the windows default size and position.";
end command-table *window-settings-command-table*;

define settings <windows-settings> (<open-dylan-user-settings>)
  key-name "Windows"
end settings <windows-settings>;

define macro window-settings-definer
  { define window-settings ?name:name :: ?type:expression
      = ?title:expression }
    => 
    { define settings "<" ## ?name ## "-settings>"
          (<windows-settings>)
        key-name ?title;
        slot ?=default-x :: <integer>;
        slot ?=default-y :: <integer>;
        slot ?=default-width  :: <integer>;
        slot ?=default-height :: <integer>;
        slot ?=default-state :: <symbol>;
      end settings "<" ## ?name ## "-settings>";

      define constant "$" ## ?name ## "-settings"
        = make("<" ## ?name ## "-settings>");

      define method frame-default-position
	  (frame :: ?type)
       => (x :: false-or(<integer>), y :: false-or(<integer>))
	let settings = "$" ## ?name ## "-settings";
	values(settings.?=default-x, settings.?=default-y)
      end method frame-default-position;

      define method set-frame-default-position
	  (frame :: ?type, x :: <integer>, y :: <integer>) => ()
	let settings = "$" ## ?name ## "-settings";
	settings.?=default-x := x;
	settings.?=default-y := y;
      end method set-frame-default-position;

      define method frame-default-size
	  (frame :: ?type)
       => (width :: false-or(<integer>), height :: false-or(<integer>))
	let settings = "$" ## ?name ## "-settings";
	values(settings.?=default-width, settings.?=default-height)
      end method frame-default-size;

      define method set-frame-default-size
	  (frame :: ?type, width :: <integer>, height :: <integer>) => ()
	let settings = "$" ## ?name ## "-settings";
	settings.?=default-width  := width;
	settings.?=default-height := height;
      end method set-frame-default-size;

      define method frame-default-state
	  (frame :: ?type) => (state :: <symbol>)
	let settings = "$" ## ?name ## "-settings";
	settings.?=default-state
      end method frame-default-state;

      define method frame-default-state-setter
	  (state :: <symbol>, frame :: ?type) => (state :: <symbol>)
	let settings = "$" ## ?name ## "-settings";
	settings.?=default-state := state
      end method frame-default-state-setter;
    }
end macro window-settings-definer;


/// Frame cascading

define open class <frame-cascading-window-mixin>
    (<frame-window-settings-mixin>)
end class <frame-cascading-window-mixin>;

define method frame-cascades?
    (frame :: <frame-cascading-window-mixin>) => (save-settings? :: <boolean>)
  #t
end method frame-cascades?;

define open generic frame-cascade-position
    (frame :: <frame-cascading-window-mixin>)
 => (x :: false-or(<integer>), y :: false-or(<integer>));

define open generic set-frame-cascade-position
    (frame :: <frame-cascading-window-mixin>, x :: <integer>, y :: <integer>)
 => (); 

define method handle-event
    (frame :: <frame-cascading-window-mixin>, event :: <frame-layed-out-event>)
 => ()
  next-method();
  cascade-frame(frame)
end method handle-event;

define method handle-event
    (frame :: <frame-cascading-window-mixin>, event :: <frame-exit-event>)
 => ()
  // This is a simple scheme so that if you close the last frame then
  // the cascading will start again from that position instead. The 
  // logic is if the cascaded position from the frame we are closing
  // is exactly that of the next cascaded position, then instead we
  // make the cascaded position be that of this old frame.
  let framem = frame-manager(frame);
  let (x, y) = frame-position(frame);
  let (next-x, next-y) = frame-cascade-position(frame);
  let (x-offset, y-offset) = frame-cascade-offset(framem, frame);
  if (x + x-offset = next-x & y + y-offset = next-y)
    set-frame-cascade-position(frame, x, y);
    duim-debug-message("Reverted cascade position to %=, %=", x, y);
  end;
  next-method();
end method handle-event;

//---*** This needs more work...
define method frame-default-cascaded-position
    (frame :: <frame>) => (x :: <integer>, y :: <integer>)
  let primary-frame = environment-primary-frame();
  let (x, y) = frame-position(primary-frame);
  let (width, height) = frame-size(primary-frame);
  let top-y = y + height;
  values(0, if (top-y < 100) top-y else 0 end)
end method frame-default-cascaded-position;

define method frame-position-fully-on-screen?
    (frame :: <frame>, x :: <integer>, y :: <integer>)
 => (fully-on-screen? :: <boolean>)
  let (screen-width, screen-height) = sheet-size(display(frame));
  let (width, height) = frame-size(frame);
  let width-on-screen?  = x + width < screen-width;
  let height-on-screen? = y + height < screen-height;
  duim-debug-message("On screen?: %= & %=: [%=,%=,%=,%=, screen: %=,%=]",
		width-on-screen?, height-on-screen?,
		x, y, width, height,
		screen-width, screen-height);
  width-on-screen? & height-on-screen?
end method frame-position-fully-on-screen?;

define method cascade-frame
    (frame :: <frame-cascading-window-mixin>) => ()
  let (x, y) = frame-cascade-position(frame);
  let fully-on-screen? = x & y & frame-position-fully-on-screen?(frame, x, y);
  let (x :: <integer>, y :: <integer>)
    = if (fully-on-screen?)
	values(x, y)
      else
	let (default-x, default-y) = frame-default-cascaded-position(frame);
	let (x, y) = frame-default-position(frame);
	values(x | default-x, y | default-y)
      end;
  let framem = frame-manager(frame);
  let (x-offset, y-offset) = frame-cascade-offset(framem, frame);
  duim-debug-message("Putting new frame %= at %=, %=", frame, x, y);
  set-frame-position(frame, x, y);
  set-frame-cascade-position(frame, x + x-offset, y + y-offset)
end method cascade-frame;

//---*** These should be computed appropriately for the platform...
define constant $cascade-x-offset = 15;
define constant $cascade-y-offset = 15;

define open generic frame-cascade-offset
    (framem :: <frame-manager>, frame :: <frame-cascading-window-mixin>)
 => (x :: <integer>, y :: <integer>);
    
define method frame-cascade-offset
    (framem :: <frame-manager>, frame :: <frame-cascading-window-mixin>)
 => (x :: <integer>, y :: <integer>)
  values($cascade-x-offset, $cascade-y-offset)
end method frame-cascade-offset;

define method save-window-settings
    (frame :: <frame-cascading-window-mixin>) => ()
  next-method();
  let (x, y) = frame-default-position(frame);
  let framem = frame-manager(frame);
  let (x-offset, y-offset) = frame-cascade-offset(framem, frame);
  set-frame-cascade-position(frame, x + x-offset, y + y-offset)
end method save-window-settings;

define macro cascading-window-settings-definer
  { define cascading-window-settings ?name:name :: ?type:expression
      = ?title:expression }
    => 
    { define window-settings ?name :: ?type = ?title;

      define variable "*" ## ?name ## "-default-x*" :: false-or(<integer>)
        = #f;
      define variable "*" ## ?name ## "-default-y*" :: false-or(<integer>)
        = #f;

      define method frame-cascade-position
          (frame :: ?type)
       => (x :: false-or(<integer>), y :: false-or(<integer>))
        values("*" ## ?name ## "-default-x*",
	       "*" ## ?name ## "-default-y*")
      end method frame-cascade-position;

      define method set-frame-cascade-position
          (frame :: ?type, x :: <integer>, y :: <integer>) => ()
        "*" ## ?name ## "-default-x*" := x;
        "*" ## ?name ## "-default-y*" := y;
      end method set-frame-cascade-position;
    }
end macro cascading-window-settings-definer;


/// Windows command table

define function apply-to-environment-frames
    (frame :: <frame>, function :: <function>,
     #key reverse? :: <boolean> = #f) => ()
  ignore(frame);
  //--- cpage: 1998.07.15 Experimenting with calling function in each
  //           frame's thread, instead of this one. Alternatively, we
  //           may need to create a new, independent thread in which
  //           to execute apply-to-environment-frames.
  local method call-function (_frame :: <frame>) => ()
	  call-in-frame(_frame, function, _frame)
	end method;
  do-frames(method (frame :: <frame>)
	      when (frame-state(frame) ~== #"destroyed")
		function(frame)
	      end
	    end method,
	    z-order: if (reverse?) #"bottom-up" else #"top-down" end)
end function apply-to-environment-frames;

// The set of frames that were minimized with 'Minimize All';
// used to restore them with 'Undo Minimize All'

define variable *minimized-frames* :: <list> = #();

// Set command enabling in all frames
define function command-enabled-in-all-frames?-setter
    (enabled? :: <boolean>,
     command  :: <object>)
 => (enabled? :: <boolean>)
  do-frames(method (frame :: <frame>) => ()
	      when (frame-state(frame) ~== #"destroyed")
		call-in-frame(frame,
			      command-enabled?-setter, enabled?, command, frame)
	      end
	    end method);
  enabled?
end function command-enabled-in-all-frames?-setter;

// Return whether a frame or any of its owned frames is modal
define function frame-modal? 
    (frame :: <frame>) => (modal? :: <boolean>)
  if (frame.frame-mode ~== #"modeless")
    #t
  else
    any?(method (_frame :: <frame>) => (modal? :: <boolean>)
	   _frame.frame-mode ~== #"modeless"
	 end,
	 frame.frame-owned-frames)
  end if
end function frame-modal?;

define method minimize-all-frames
    (frame :: <frame>) => ()
  *minimized-frames* := #();
  apply-to-environment-frames
    (frame,
     method (_frame :: <frame>) => ()
       when (frame-mapped?(_frame)
	       & frame-enabled?(_frame)
	       & ~frame-iconified?(_frame)
	       & ~frame-modal?(frame))
	 iconify-frame(_frame);
	 *minimized-frames* := add!(*minimized-frames*, _frame);
       end
     end method,
     reverse?: #t);
//--- cpage: 1998.07.14 I'm not sure whether we should be doing this
//           front to back or back to front. Whichever seems to preserve
//           the Z ordering best. Windows may be changing the Z order as
//           each window is minimized/restored. I suppose the best method
//           would be to use Windows' Begin/Defer/EndWindowPos to restore
//           and set the Z order simultaneously to restore the Z order
//           (or perhaps to retain relative Z order, but move them in
//           front of all other windows).
  // We minimize back to front, but restore front to back
  *minimized-frames* := reverse!(*minimized-frames*);
  // Update command enabling
  command-enabled-in-all-frames?(undo-minimize-all-frames) := #t;
end method minimize-all-frames;

define method undo-minimize-all-frames (frame :: <frame>) => ()
  //--- cpage: 1998.07.14 restore-frame uses Windows' $SW-SHOWNOACTIVATE to
  //           avoid activating the window. This is less flashy than
  //           deiconify frame, but seems to introduce editor activation
  //           anomalies (all the editors will get flashing insertion points
  //           despite their being inactive). I'm not yet sure which to use.
  do(method (_frame :: <frame>) => ()
       //call-in-frame(_frame, deiconify-frame, _frame);
       restore-frame(_frame);
     end,
     *minimized-frames*);
  *minimized-frames* := #();
  // Make sure the original frame is front and active
  raise-frame(frame);
  // Raise all the frames to make sure they're in front
  //---*** cpage: 1998.09.30 We should probably change this to use
  //              Begin/Defer/EndWindowPos to restore and change Z
  //              order simultaneously.
  raise-all-frames(frame);
  // Update command enabling
  command-enabled-in-all-frames?(undo-minimize-all-frames) := #f;
end method undo-minimize-all-frames;

define method restore-all-frames (frame :: <frame>) => ()
  *minimized-frames* := #();
  local method restore-a-frame (_frame :: <frame>)
	  //call-in-frame(_frame, deiconify-frame, _frame);
	  restore-frame(_frame);
	end method restore-a-frame;
  apply-to-environment-frames(frame, restore-a-frame);
  // Make sure the original frame is front and active
  raise-frame(frame);
  // Raise all the frames to make sure they're in front
  //---*** cpage: 1998.09.30 We should probably change this to use
  //              Begin/Defer/EndWindowPos to restore and change Z
  //              order simultaneously.
  raise-all-frames(frame);
  // Update command enabling
  command-enabled-in-all-frames?(undo-minimize-all-frames) := #f;
end method restore-all-frames;

// Move all the application frames in front of all other frames while
// maintaining relative Z order.
define method raise-all-frames (frame :: <frame>) => ()
  let frames = #();
  local method add-to-list (_frame :: <frame>) => ()
	  frames := add!(frames, _frame);
	end method add-to-list;
  apply-to-environment-frames(frame, add-to-list, reverse?: #t);
  order-frames(frames);
end method raise-all-frames;

define function all-open-frames () => (frames :: <sequence>)
  let frames = make(<stretchy-vector>);
  do-frames(method (frame :: <frame>) => ()
	      unless (frame-state(frame) == #"destroyed"
			| frame-owner(frame)
			| frame-mode(frame) == #"modal")
		add!(frames, frame)
	      end
	    end method);
  sort!(frames,
	test: method (frame1 :: <frame>, frame2 :: <frame>)
	       => (less-than? :: <boolean>)
		if (object-class(frame1) ~= object-class(frame2))
		  case
		    instance?(frame1, <environment-primary-frame>) =>
		      #t;
		    instance?(frame2, <environment-primary-frame>) =>
		      #f;
		    instance?(frame1, <project-browser>) =>
		      #t;
		    instance?(frame2, <project-browser>) =>
		      #f;
		    otherwise =>
		      frame1.frame-title < frame2.frame-title
		  end
		else
		  frame1.frame-title < frame2.frame-title
		end
	      end method)
end function all-open-frames;

define variable $frame-title-optional-suffix :: false-or(<string>) = #f;

//---*** andrewa: it would be better to compute this directly, rather
//---*** than stripping off the extra string all of the time.
define method frame-short-title
    (frame :: <frame>) => (title :: <string>)
  let suffix
    = $frame-title-optional-suffix
        | begin
	    let suffix = concatenate(" - ", release-product-name());
	    $frame-title-optional-suffix := suffix
	  end;
  let title = frame-title(frame);
  let title-size = size(title);
  let suffix-size = size($frame-title-optional-suffix);
  let title-includes-suffix?
    = (title-size > suffix-size)
        & ($frame-title-optional-suffix 
	     = copy-sequence(title, start: title-size - suffix-size));
  if (title-includes-suffix?)
    copy-sequence(title, end: title-size - suffix-size)
  else
    title
  end
end method frame-short-title;

// Generate menu items for a set of windows, numbering them from start:.
define function window-menu-items-from-frames
    (frames :: <sequence>, #key start :: <integer> = 1)
 => (items :: <vector>)
  let items = make(<vector>, size: size(frames));
  for (frame in frames,
       i from 0,
       n from start)
    let name = frame-short-title(frame);
    // Only number the first ten windows, since higher numbers don't
    // provide numeric access keys.
    let label
      = if (n <= 9)
	  format-to-string("&%d %s", n, name)
	else
	  name
	end if;
    items[i] := vector(label, frame);
  end for;
  items
end function window-menu-items-from-frames;

define command-table *open-windows-command-table* (*global-command-table*)
end command-table *open-windows-command-table*;

add-command-table-menu-item
  (*open-windows-command-table*,
   "", <push-box>, #f,
   documentation: "Activates this window.",
   update-callback: method (menu-box :: <menu-box>)
		      let windows = all-open-frames();
		      gadget-items(menu-box) := window-menu-items-from-frames(windows)
		    end,
   label-key: first,
   value-key: second,
   callback: method (menu-box :: <menu-box>)
	       let frame = gadget-value(menu-box);
	       when (frame-iconified?(frame))
		 deiconify-frame(frame)
	       end;
	       when (frame-mapped?)
		 raise-frame(frame)
	       end;
             end);

define command-table *window-actions-command-table* (*global-command-table*)
  menu-item "Minimize All" = minimize-all-frames,
    documentation: "Collapses all of the program's windows to icons.";
  menu-item "Undo Minimize All" = undo-minimize-all-frames,
    documentation: "Undoes the previous 'Minimize All' command.";
  menu-item "Restore All"  = restore-all-frames,
    documentation: "Restores all of the program's windows to normal size.";
  menu-item "Bring All To Front"  = raise-all-frames,
    documentation: "Brings all of the program's windows to the front, leaving"
                   " minimized windows minimized.";
end command-table *window-actions-command-table*;

define command-table *single-window-command-table* (*global-command-table*)
  include *window-actions-command-table*;
  include *open-windows-command-table*;
end command-table *single-window-command-table*;

define command-table *window-show-command-table* (*global-command-table*)
  include *window-actions-command-table*;
  include *window-settings-command-table*;
  include *open-windows-command-table*;
end command-table *window-show-command-table*;


/// Coerce objects to a project

define generic coerce-project
    (project :: <object>)
 => (project :: false-or(<project-object>));

define method coerce-project
    (project :: <project-object>)
 => (project :: <project-object>)
  project
end method coerce-project;

define method coerce-project
    (locator :: <file-locator>)
 => (project :: false-or(<project-object>))
  let extension = locator.locator-extension;
  let directory = locator.locator-directory;
  case
    extension =>
      open-project(locator);
    ~directory =>
      find-project(locator.locator-base);
    otherwise =>
      error("Cannot coerce '%s' to a project", locator);
  end
end method coerce-project;

define method coerce-project
    (object :: <string>)
 => (project :: false-or(<project-object>))
  coerce-project(as(<file-locator>, object))
end method coerce-project;


/// Error handling

define macro with-environment-handlers
  { with-environment-handlers (?frame:name) ?body:body end }
    => { begin
	   let handler <invalid-object-error>
	     = method 
		   (error :: <invalid-object-error>, 
		    next-handler :: <function>)
		 ignore(next-handler);
		 handle-invalid-object-error(?frame, error)
	       end;
	   let handler <timeout-expired>
	     = method
		   (error :: <timeout-expired>, 
		    next-handler :: <function>)
		 ignore(next-handler);
		 handle-expired-timeout(?frame, error)
	       end;
	   ?body
	 end }
end macro with-environment-handlers;

define last-handler (<serious-condition>, test: always(#t)) = environment-handler;
