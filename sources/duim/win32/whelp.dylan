Module:    win32-duim
Synopsis:  Win32 help implementation
Author:    Jason Trenouth, Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Win32 Help management

// We could potentially have several FFI declarations for WinHelp
// One for each type of data (4th argument). This way the FFI would
// automatically take care of translating things like Dylan strings 
// into Char*'s (addresses of C strings). However, the code in this
// file was already divided up to use a single call to WinHelp so I 
// just added explicit conversions (eg calls 'pointer-address', etc)
// into the various 'help-data' methods. (Jason -- 26th March 1997)

/// Help Systems

define abstract class <win32-help-system> (<help-system>)
end class <win32-help-system>;

define sealed class <winhelp> (<win32-help-system>)
  keyword name: = "WinHelp";
end class <winhelp>;

define sealed class <htmlhelp> (<win32-help-system>)
  keyword name: = "HTMLHelp";
end class <htmlhelp>;

// Initialize the default help system on Windows
*default-help-system* := make(<htmlhelp>);


/// Windows help display

define constant <c-string-help-command> = <help-run-macro>;
define constant <alink-help-command> = <help-on-keyword>;

define macro with-help-data
  { with-help-data (?data:name = ?system:expression, ?command:expression)
      ?:body
    end }
    => { do-with-help-data(?system, ?command, method (?data) ?body end) }
end macro with-help-data;

define sealed method do-with-help-data
    (system :: <win32-help-system>,
     command :: <help-command>, continuation :: <function>)
 => (#rest values)
  continuation(help-data(system, command))
end method do-with-help-data;

define sealed method do-with-help-data
    (system :: <win32-help-system>,
     command :: <c-string-help-command>, continuation :: <function>)
 => (#rest values)
  with-c-string (data = help-data(system, command))
    continuation(pointer-address(data))
  end
end method do-with-help-data;

define sealed method do-with-help-data
    (system :: <win32-help-system>,
     command :: <alink-help-command>, continuation :: <function>)
 => (#rest values)
  with-c-string (data = help-data(system, command))
    with-stack-structure (alink :: <LPHH-AKLINK>)
      init-alink(alink, data);
      continuation(pointer-address(alink));
    end with-stack-structure;
  end with-c-string;
end method do-with-help-data;

define inline method init-alink (alink :: <LPHH-AKLINK>, data :: <c-string>) => ()
  alink.cbstruct-value := safe-size-of(<HH-AKLINK>);
  alink.freserved := #f;
  alink.pszkeywords-value := data;
  alink.pszurl-value := null-pointer(<LPCTSTR>);
  alink.pszmsgtext-value := null-pointer(<LPCTSTR>);
  alink.pszmsgtitle-value := null-pointer(<LPCTSTR>);
  alink.pszwindow-value := null-pointer(<LPCTSTR>);
  alink.findexonfail-value := #t;
end method init-alink;

define sealed method display-help
    (framem :: <win32-frame-manager>, frame :: <basic-frame>,
     command :: <help-command>) => ()
  block ()
    let system = frame-manager-help-system(framem);
    check-help-system-installed(system);
    let top-sheet = top-level-sheet(frame);
    when (top-sheet)
      with-help-data (data = system, command)
        do-display-help(system,
                        window-handle(top-sheet),
                        help-path(system, command),
                        help-id(system, command),
                        data)
      end
    end;
  exception (condition :: <help-system-error>)
    notify-user(format-to-string("%s", condition), owner: frame);
  end
end method display-help;


/// Registry utilities

define class <registry-entry-lookup-error> (<simple-error>)
  keyword format-string: = "Registry entry lookup error \"%s\" with code: %d";
end class <registry-entry-lookup-error>;

define method make
    (class :: subclass(<registry-entry-lookup-error>), #key name, result)
 => (condition :: <registry-entry-lookup-error>)
  next-method(class, format-arguments: vector(name, result))
end method make;

define macro with-open-registry-subkey
  { with-open-registry-subkey (?subkey:name = ?key:expression, ?name:expression) ?body:body end }
    => { do-with-open-registry-subkey(?key, ?name, method (?subkey) ?body end) };
end macro with-open-registry-subkey;

define method do-with-open-registry-subkey
    (key, subkeyname, body :: <function>) => (#rest values)
  let (result, subkey)
    = RegOpenKeyEx(key, subkeyname, 0,
                   %logior($KEY-ENUMERATE-SUB-KEYS, $KEY-QUERY-VALUE));
  if (result = $ERROR-SUCCESS)
    block ()
      body(subkey)
    cleanup
      RegCloseKey(subkey);
    end
  else
    error(make(<registry-entry-lookup-error>, name: subkeyname, result: result))
  end
end method do-with-open-registry-subkey;

define macro with-open-registry-path
  { with-open-registry-path (?subkey:name = ?key:expression, ?path:*) ?body:body end }
    => { do-with-open-registry-path(?key, list(?path), method (?subkey) ?body end) }
end macro with-open-registry-path;

define method do-with-open-registry-path
    (key, path, body :: <function>) => (#rest values)
  if (empty?(path))
    body(key)
  else
    with-open-registry-subkey (subkey = key, first(path))
      do-with-open-registry-path(subkey, rest(path), body)
    end
  end
end method do-with-open-registry-path;

/*---*** No longer used ...
define method read-registry-string
    (key, name) => (value)
  let buffer-size :: <integer> = 2048;
  with-stack-structure (buffer :: <C-string>, size: buffer-size)
    with-stack-structure (count :: <LPDWORD>)
      pointer-value(count) := buffer-size;
      let (result, type)
        = RegQueryValueEx(key, name, null-pointer(<LPDWORD>), buffer, count);
      if (result ~= $ERROR-SUCCESS | type ~= $REG-SZ)
        error(make(<registry-entry-lookup-error>, name: name, result: result))
      else
        as(<byte-string>, buffer)
      end
    end
  end
end method read-registry-string;
*/


/// Installation checking

define generic check-help-system-installed
    (system :: false-or(<help-system>)) => ();

define sealed method check-help-system-installed
    (system :: <help-system>) => ()
  unless (help-system-installed?(system))
    error(make(<help-system-not-installed>, system: system))
  end
end method check-help-system-installed;

define sealed method check-help-system-installed
    (system == #f) => ()
  error(make(<no-help-system>))
end method check-help-system-installed;


/// See <http://msdn.microsoft.com/workshop/author/htmlhelp/faq.asp#17> which
/// describes how to check for the presence of HTML Help.
define constant $HHCTRL-OCX = "{ADB880A6-D8FF-11CF-9377-00AA003B7A11}";

define sealed method help-system-installed?
    (system :: <htmlhelp>)
 => (installed? :: <boolean>)
  block ()
    with-open-registry-path (hh-key = $HKEY-CLASSES-ROOT, "CLSID", $HHCTRL-OCX)
      // If the key exists, HTMLHelp is installed...
      #t
    end;
  exception (condition :: <registry-entry-lookup-error>)
    #f
  end
end method help-system-installed?;


/// Help display


/// 'do-display-help' methods

define sealed method do-display-help
    (system :: <winhelp>, handle, path, id, data) => ()
  WinHelp(handle, path, id, data)
end method do-display-help;

define sealed method do-display-help
    (system :: <htmlhelp>, handle, path, id, data) => ()
  HtmlHelp(handle, path, id, data)
end method do-display-help;


/// Help path

define sealed method help-path
    (system :: <win32-help-system>, command :: <help-command>)
 => (path :: <string>)
  ""
end method help-path;

define sealed method help-path
    (system :: <win32-help-system>, command :: <help-from-source>)
 => (path :: <string>)
  let path = as(<string>, help-source-locator(help-source(command)));
  if (path-has-window?(path))
    path
  else
    let window = help-path-window(system, command, path);
    if (window)
      concatenate(path, ">", window)
    else
      path
    end
  end
end method help-path;

define method path-has-window? (path :: <string>) => (window? :: <boolean>)
  member?('>', path);
end method path-has-window?;

/// Help path window

define sealed method help-path-window
    (system :: <win32-help-system>, command :: <help-from-source>, path :: <string>)
 => (window :: false-or(<string>))
  help-secondary-window(command)
end method help-path-window;

/// Help ID (WinHelp)

define sealed method help-id
    (system :: <winhelp>, command :: <help-on-help>)
 => (id :: <integer>)
  $HELP-HELPONHELP
end method help-id;

define sealed method help-id
    (system :: <winhelp>, command :: <help-on-topics>)
 => (id :: <integer>)
  $HELP-FINDER
end method help-id;

define sealed method help-id
    (system :: <winhelp>, command :: <help-on-index>)
 => (id :: <integer>)
  $HELP-INDEX
end method help-id;

define sealed method help-id
    (system :: <winhelp>, command :: <help-on-contents>)
 => (id :: <integer>)
  $HELP-CONTENTS
end method help-id;

define sealed method help-id
    (system :: <winhelp>, command :: <help-on-context>)
 => (id :: <integer>)
  if (help-popup?(command))
    $HELP-CONTEXTPOPUP
  else
    $HELP-CONTEXT
  end
end method help-id;

define sealed method help-id
    (system :: <winhelp>, command :: <help-on-keyword>)
 => (id :: <integer>)
  $HELP-KEY
end method help-id;

define sealed method help-id
    (system :: <winhelp>, command :: <help-run-macro>)
 => (id :: <integer>)
  $HELP-COMMAND
end method help-id;

define sealed method help-id
    (system :: <winhelp>, command :: <help-reposition>)
 => (id :: <integer>)
  $HELP-SETWINPOS
end method help-id;

define sealed method help-id
    (system :: <winhelp>, command :: <help-quit>)
 => (id :: <integer>)
  $HELP-QUIT
end method help-id;


/// Help ID (HtmlHelp)

define sealed method help-id
    (system :: <htmlhelp>, command :: <help-on-index>)
 => (id :: <integer>)
  $HH-DISPLAY-TOPIC
end method help-id;

define sealed method help-id
    (system :: <htmlhelp>, command :: <help-on-contents>)
 => (id :: <integer>)
  $HH-DISPLAY-TOPIC
end method help-id;

define sealed method help-id
    (system :: <htmlhelp>, command :: <help-on-topics>)
 => (id :: <integer>)
  $HH-DISPLAY-TOPIC
end method help-id;

define sealed method help-id
    (system :: <htmlhelp>, command :: <help-on-context>)
 => (id :: <integer>)
  $HH-HELP-CONTEXT
end method help-id;

define sealed method help-id
    (system :: <htmlhelp>, command :: <help-on-keyword>)
 => (id :: <integer>)
  $HH-ALINK-LOOKUP
end method help-id;


/// Help data

define sealed method help-data
    (system :: <win32-help-system>, command :: <help-command>)
 => (data :: <integer>)
  0
end method help-data;

define sealed method help-data
    (system :: <win32-help-system>, command :: <help-on-context>)
 => (data :: <integer>)
  help-topic-id(command)
end method help-data;

define sealed method help-data
    (system :: <win32-help-system>, command :: <help-on-keyword>)
 => (data :: <string>)
  help-keyword(command)
end method help-data;

define sealed method help-data
    (system :: <win32-help-system>, command :: <help-run-macro>)
 => (data :: <string>)
  help-macro(command)
end method help-data;

/*---*** This won't work yet!
define sealed method help-data
    (system :: <win32-help-system>, command :: <help-reposition>)
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
