Module:       gtk-duim
Synopsis:     GTK help implementation
Author:       Andy Armstrong, Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// GTK help management

/*---*** No help for now...
///---*** WHAT ON EARTH DO WE DO ABOUT THIS?

define abstract open class <gtk-help-system> (<help-system>)
end class <gtk-help-system>;

define sealed class <gtk-help> (<help-system>)
  keyword name: = "GTK Help";
end class <gtk-help>;


/// Frame Manager Help Systems

define variable *default-help-system* :: false-or(<help-system>) = #f;

define open generic frame-manager-help-system
    (framem :: <abstract-frame-manager>)
 => (system :: false-or(<help-system>));

define open generic frame-manager-help-system-setter
    (value :: false-or(<help-system>), framem :: <abstract-frame-manager>)
 => (system :: false-or(<help-system>));

define sealed method frame-manager-help-system
    (framem :: <abstract-frame-manager>)
 => (system :: false-or(<help-system>))
  *default-help-system*
end method frame-manager-help-system;

define sealed method frame-manager-help-system-setter
    (value :: false-or(<help-system>), framem :: <abstract-frame-manager>)
 => (system :: false-or(<help-system>))
  *default-help-system* := value
end method frame-manager-help-system-setter;

*default-help-system* := make(<gtk-help>);


/// Errors

define class <help-system-error> (<simple-error>)
end class;

define class <help-system-not-installed> (<help-system-error>)
  keyword format-string: = "%s is not installed.";
end class <help-system-not-installed>;

define method make (class == <help-system-not-installed>, #key system)
 => (condition :: <help-system-not-installed>)
  next-method(class, format-arguments: vector(help-system-name(system)));
end method make;


/// Display Help

define sealed method display-help
    (framem :: <gtk-frame-manager>, frame :: <basic-frame>,
     command :: <help-command>) => ()
  block ()
    let system = frame-manager-help-system(framem);
    let top-sheet = top-level-sheet(frame);
    when (top-sheet)
      do-display-help(system,
                      window-handle(top-sheet),
                      help-path(system, command),
                      help-id(system, command),
                      help-data(system, command));
    end;
  exception (condition :: <help-system-error>)
    notify-user(format-to-string("%s", condition), owner: frame);
  end block;
end method display-help;


/// Do Display Help

define sealed method do-display-help
    (system :: <gtk-help>, handle, path, id, data)
 => ()
  //---*** What should this do?
end method do-display-help;


/// Help Path

define sealed method help-path
    (system :: <gtk-help-system>, command :: <help-command>)
 => (path :: <string>)
  ""
end method help-path;

define sealed method help-path
    (system :: <gtk-help-system>, command :: <help-from-source>)
 => (path :: <string>)
  let path = as(<string>, help-source-locator(help-source(command)));
  let path-window = unless (any?(curry(\=, '>'), path))
                      help-path-window(system, command, path)
                    end unless;
  if (path-window)
    concatenate(path, ">", path-window)
  else
    path
  end
end method help-path;


/// Help Path Window

define sealed method help-path-window
    (system :: <gtk-help>, command :: <help-from-source>, path :: <string>)
 => (window :: <string>)
  help-secondary-window(command)
  | path
end method help-path-window;


/// Help ID (HtmlHelp)

define sealed method help-id
    (system :: <gtk-help>, command :: <help-on-index>)
 => (id :: <integer>)
  $HH-DISPLAY-TOPIC
end method help-id;

define sealed method help-id
    (system :: <gtk-help>, command :: <help-on-contents>)
 => (id :: <integer>)
  $HH-DISPLAY-TOPIC
end method help-id;

define sealed method help-id
    (system :: <gtk-help>, command :: <help-on-topics>)
 => (id :: <integer>)
  $HH-DISPLAY-TOPIC
end method help-id;

define sealed method help-id
    (system :: <gtk-help>, command :: <help-on-context>)
 => (id :: <integer>)
  $HH-HELP-CONTEXT
end method help-id;

define sealed method help-id
    (system :: <gtk-help>, command :: <help-on-keyword>)
 => (id :: <integer>)
  $HH-KEYWORD-LOOKUP
end method help-id;


/// Help Data

define sealed method help-data
    (system :: <gtk-help-system>, command :: <help-command>)
 => (data :: <integer>)
  0
end method help-data;

define sealed method help-data
    (system :: <gtk-help-system>, command :: <help-on-context>)
 => (data :: <integer>)
  help-topic-id(command)
end method help-data;

define sealed method help-data
    (system :: <gtk-help-system>, command :: <help-on-keyword>)
 => (data :: <machine-word>)
  pointer-address(as(<C-string>, help-keyword(command)))
end method help-data;

define sealed method help-data
    (system :: <gtk-help-system>, command :: <help-run-macro>)
 => (data :: <machine-word>)
  pointer-address(as(<C-string>, help-macro(command)))
end method help-data;

/*---*** This won't work yet!
define sealed method help-data
    (system :: <gtk-help-system>, command :: <help-reposition>)
 => (data :: <integer>)
  let region = help-window-region(command);
  let (left, top, right, bottom) = box-edges(region);
  let info = make(<LPHELPWININFO>,
                  x:  left, y:  top,
                  dx: right - left, dy: bottom - top,
                  wMax: $SW-SHOWNA);
  info
end method help-data;
*/
*/
