Module:       duim-frames-internals
Synopsis:     DUIM frames
Author:       Jason Trenouth, Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Help systems

define open abstract class <help-system> (<object>)
  sealed constant slot help-system-name :: <string>,
    required-init-keyword: name:;
end class <help-system>;

define variable *default-help-system* :: false-or(<help-system>) = #f;


// NB: This is for testing the installation of a Help System (viewer) and
// not any particular form of Help content
define open generic help-system-installed?
    (system :: <help-system>) => (installed? :: <boolean>);

define method help-system-installed?
    (system :: <help-system>) => (installed? :: <boolean>)
  #t
end method help-system-installed?;


define open generic frame-manager-help-system
    (framem :: <abstract-frame-manager>)
 => (system :: false-or(<help-system>));

define open generic frame-manager-help-system-setter
    (system :: false-or(<help-system>), framem :: <abstract-frame-manager>)
 => (system :: false-or(<help-system>));

define method frame-manager-help-system
    (framem :: <frame-manager>)
 => (system :: false-or(<help-system>))
  *default-help-system*
end method frame-manager-help-system;

define method frame-manager-help-system-setter
    (system :: false-or(<help-system>), framem :: <frame-manager>)
 => (system :: false-or(<help-system>))
  *default-help-system* := system
end method frame-manager-help-system-setter;


/// Help systems errors

define class <help-system-error> (<simple-error>)
end class <help-system-error>;

define class <help-system-not-installed> (<help-system-error>)
  keyword format-string: = "%s is not installed";
end class <help-system-not-installed>;

define method make
    (class == <help-system-not-installed>, #key system)
 => (condition :: <help-system-not-installed>)
  next-method(class, format-arguments: vector(help-system-name(system)))
end method make;

define class <no-help-system> (<help-system-error>)
  keyword format-string: = "There is no help system defined.";
end class <no-help-system>;


/// Help

define constant <help-pane-type>
    = false-or(type-union(<abstract-sheet>, one-of(#"by-gesture", #"by-focus")));

define constant <named-help-source> = type-union(<symbol>, <help-source>);

define protocol <<help-protocol>> ()
  getter help-source (command :: <help-command>)
 => (source :: false-or(<named-help-source>));
  getter help-secondary-window (command :: <help-command>)
 => (window-name :: false-or(<string>));
  getter help-pane (command :: <help-command>)
 => (pane :: <help-pane-type>);
  getter help-context (command :: <help-command>)
 => (context :: false-or(<symbol>));
  getter help-topic-id (command :: <help-command>)
 => (topic-id);
  getter help-popup? (command :: <help-command>)
 => (pop-up? :: <boolean>);
  getter help-keyword (command :: <help-command>)
 => (keyword :: false-or(<string>));
  getter help-macro (command :: <help-command>)
 => (_macro :: <string>);
  getter help-window-region (command :: <help-command>)
 => (region :: <region>);
  // Initialization
  function initialize-help-pane
    (command :: <help-command>, frame :: <abstract-frame>) => ();
  function initialize-help
    (command :: <help-command>, frame :: <abstract-frame>) => ();
  // Glue to commands
  function display-help
    (framem :: <abstract-frame-manager>, frame :: <abstract-frame>,
     command :: <help-command>) => ();
  // Glue to frames
  function frame-help-source-locator
    (frame :: <abstract-frame>, source :: <help-source>)
 => (locator);
  function frame-help-source
    (frame :: <abstract-frame>, command :: <help-command>)
 => (source :: false-or(<named-help-source>));
  function frame-help-context
    (frame :: <abstract-frame>, command :: <help-command>)
 => (context :: false-or(<symbol>));
  function frame-help-topic-id
    (frame :: <abstract-frame>, command :: <help-command>)
 => (topic-id);
  function frame-help-keyword
    (frame :: <abstract-frame>, command :: <help-command>)
 => (keyword :: false-or(<string>));
end protocol <<help-protocol>>;


/// Help Sources

// Reify entity for refering to a help source file and its mapping
// from symbolic help contexts to whatever identifiers are required
// by the help system: eg integers, strings, urls, or whatever.
define constant $help-sources :: <object-table> = make(<table>);

define sealed class <help-source> (<object>)
  sealed constant slot help-source-name :: <symbol>,
    required-init-keyword: name:;
  sealed slot help-source-locator :: false-or(<string>) = #f,
    init-keyword: locator:;
  sealed constant slot help-source-context-map :: false-or(<object-table>) = #f,
    init-keyword: contexts:;
end class <help-source>;

define sealed method initialize
    (source :: <help-source>, #key name :: <symbol>)
  next-method();
  $help-sources[name] := source
end method initialize;

define macro help-source-definer
  { define help-source ?name:name ?locator:expression ?entries:* end }
    => { make(<help-source>,
	      name: ?#"name",
	      locator: ?locator,
	      contexts: initialize-table (make(<table>)) ?entries end) }
end macro help-source-definer;

define macro initialize-table
  { initialize-table (?table-name:expression) ?entries:* end }
    => { let the-table = ?table-name; ?entries; the-table }
 entries:
  { } => { }
  { ?key:expression => ?value:expression; ... }
    => { the-table[?key] := ?value; ... }
end macro initialize-table;

define method as
    (class == <help-source>, source :: <help-source>) => (source :: <help-source>)
  source
end method as;

define method as
    (class == <help-source>, name :: <symbol>) => (source :: <help-source>)
  $help-sources[name]
end method as;


/// Help Commands

// <help-command> and its subclasses model the standard kinds of help that
// users are normally able to invoke.  Most classes have a 'help-source'
// associated with them.  Some have additional state, such as a 'help-context'.
define open abstract primary class <help-command> (<basic-command>)
end class <help-command>;

// Help on a specific subject
define open abstract primary class <help-on-subject> (<help-command>)
end class <help-on-subject>;

// Help about using the help system itself
define sealed class <help-on-help> (<help-on-subject>)
end class <help-on-help>;

// "About" box help
define sealed class <help-on-version> (<help-on-subject>)
end class <help-on-version>;

// Help on something with an explicit source
define open abstract primary class <help-from-source> (<help-on-subject>)
  sealed slot help-source :: false-or(<named-help-source>) = #f,
    init-keyword: source:;
  sealed slot help-secondary-window :: false-or(<string>) = #f,
    init-keyword: secondary-window:;
end class <help-from-source>;

// Help topics page (supersedes index and contents in Win95)
define sealed class <help-on-topics> (<help-from-source>)
end class <help-on-topics>;

// Help index page
define sealed class <help-on-index> (<help-from-source>)
end class <help-on-index>;

// Help contents page
define sealed class <help-on-contents> (<help-from-source>)
end class <help-on-contents>;

// Help on some UI element
define open abstract primary class <help-on-pane> (<help-from-source>)
  sealed slot help-pane :: false-or(<help-pane-type>) = #f,
    init-keyword: pane:;
end class <help-on-pane>;

define method find-pane-by-gesture
    (frame :: <basic-frame>) => (pane :: false-or(<sheet>))
  // Go into special mode so user can select pane they are interested in
  //---*** Do this
  error("Not implemented yet!")
end method find-pane-by-gesture;

define method find-pane-by-focus
    (frame :: <basic-frame>) => (pane :: false-or(<sheet>))
  frame-mapped?(frame) & frame-input-focus(frame)
end method find-pane-by-focus;

define method default-from-sheet
    (sheet :: false-or(<sheet>), getter :: <function>) => (default)
  block (return)
    for (sh = sheet then sheet-parent(sh),
	 until: ~sh)
      let value = getter(sh);
      when (value)
	return(value)
      end
    end;
    #f
  end
end method default-from-sheet;

// Help on predefined topic
// Sometimes called "What is this?"
define sealed class <help-on-context> (<help-on-pane>)
  sealed slot help-context :: false-or(<symbol>) = #f,
    init-keyword: context:;
  sealed slot help-topic-id = #f,
    init-keyword: topic-id:;
  sealed slot help-popup? :: <boolean> = #f,
    init-keyword: popup?:;
end class <help-on-context>;

// Help on arbitrary string
define sealed class <help-on-keyword> (<help-on-pane>)
  sealed slot help-keyword :: false-or(<string>) = #f,
    init-keyword: keyword:;
end class <help-on-keyword>;

// Help system macro
define sealed class <help-run-macro> (<help-from-source>)
  sealed slot help-macro :: <string>,
    required-init-keyword: macro:;
end class <help-run-macro>;

// Help window can be repositioned
define sealed class <help-reposition> (<help-from-source>)
  sealed slot help-window-region :: <bounding-box>,
    required-init-keyword: region:;
end class <help-reposition>;

// Help system is no longer needed
//---*** Needs to register a callback with DUIM exiting mechanism
define sealed class <help-quit> (<help-command>)
end class <help-quit>;


// Initializes command pane
define method initialize-help-pane
    (command :: <help-command>, frame :: <basic-frame>) => ()
  #f
end method initialize-help-pane;

define method initialize-help-pane
    (command :: <help-on-pane>, frame :: <basic-frame>) => ()
  select (help-pane(command))
    #"by-gesture" =>
      help-pane(command) := find-pane-by-gesture(frame);
    #"by-focus" =>
      help-pane(command) := find-pane-by-focus(frame);
    otherwise => #f;
  end
end method initialize-help-pane;

// Initializes command from frame
define method initialize-help
    (command :: <help-command>, frame :: <basic-frame>) => ()
  #f
end method initialize-help;

define method initialize-help
    (command :: <help-from-source>, frame :: <basic-frame>) => ()
  next-method();
  help-source(command)
    := as(<help-source>,
	  help-source(command)
	  | frame-help-source(frame, command));
  help-source-locator(help-source(command))
    := help-source-locator(help-source(command))
       | frame-help-source-locator(frame, help-source(command));
end method initialize-help;

define method initialize-help
    (command :: <help-on-context>, frame :: <basic-frame>) => ()
  next-method();
  help-context(command)
    := help-context(command)
       | frame-help-context(frame, command);
  help-topic-id(command)
    := help-topic-id(command)
       | frame-help-topic-id(frame, command);
end method initialize-help;

define method initialize-help
    (command :: <help-on-keyword>, frame :: <basic-frame>) => ()
  next-method();
  help-keyword(command)
    := help-keyword(command)
       | frame-help-keyword(frame, command);
end method initialize-help;


/// Glue to Commands

define method do-execute-command
    (frame :: <basic-frame>, command :: <help-command>) => ()
  initialize-help-pane(command, frame);
  initialize-help(command, frame);
  display-help(frame-manager(frame), frame, command)
end method do-execute-command;

define method do-execute-command
    (frame :: <basic-frame>, command :: <help-on-version>) => ()
  // The idea here is that hackers write their own method for this...
  #f
end method do-execute-command;


/// Glue to Frames

define method frame-help-source-locator
    (frame :: <basic-frame>, source :: <help-source>)
 => (locator :: singleton(#f))
  #f
end method frame-help-source-locator;

define method frame-help-source
    (frame :: <basic-frame>, command :: <help-from-source>)
 => (source :: singleton(#f))
  #f
end method frame-help-source;

define method frame-help-source
    (frame :: <basic-frame>, command :: <help-on-pane>)
 => (source :: false-or(<named-help-source>))
  default-from-sheet(help-pane(command), sheet-help-source)
end method frame-help-source;


define method frame-help-context
    (frame :: <basic-frame>, command :: <help-on-context>)
 => (context :: false-or(<symbol>))
  default-from-sheet(help-pane(command), sheet-help-context)
end method frame-help-context;


define method frame-help-keyword
    (frame :: <basic-frame>, command :: <help-on-keyword>)
 => (keyword :: false-or(<string>))
  default-from-sheet(help-pane(command), selected-text)
end method frame-help-keyword;

define method frame-help-topic-id
    (frame :: <basic-frame>, command :: <help-on-context>)
 => (object)
  help-source-context-map(help-source(command))[help-context(command)]
end method frame-help-topic-id;
