Module:    source-control-hope-backend
Synopsis:  Environment-Hope Interface
Author:    Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// HOPE back-end

define sealed class <hope-source-control-system> (<source-control-system>)
  // The name of the image to be used as the server
  sealed constant slot %image-name :: <string>  = "hope";
  // When #t, this starts a server session and uses it persistently
  // When #f, this just uses 'os/run-application' (yech!)
  sealed constant slot %use-session? :: <boolean> = #f;
  keyword name:  = #"HOPE";
  keyword label: = "HOPE";
  keyword title: = "Harlequin's Ongoing Project Environment (HOPE)";
end class <hope-source-control-system>;

define sealed domain make (singleton(<hope-source-control-system>));
define sealed domain initialize (<hope-source-control-system>);

define method note-source-control-system-selected
    (sccs :: <hope-source-control-system>) => ()
  *claim-command-string*     := *hope-claim-command-string*;
  *check-out-command-string* := *hope-check-out-command-string*;
  *check-in-command-string*  := *hope-check-in-command-string*;
  *abandon-command-string*   := *hope-abandon-command-string*;
  *merge-command-string*     := *hope-merge-command-string*;
  *diff-command-string*      := *hope-diff-command-string*;
  *report-command-string*    := *hope-report-command-string*;
  *add-command-string*       := *hope-add-command-string*;
  *remove-command-string*    := *hope-remove-command-string*;
end method note-source-control-system-selected;



/// Argument parsing

define class <hope-command-options> (<source-control-command-options>)
  sealed slot command-compound :: <string>,
    required-init-keyword: compound:;
  sealed slot command-unit :: <string>,
    required-init-keyword: unit:;
  sealed slot command-branch :: <string>,
    required-init-keyword: branch:;
  sealed slot command-reason :: false-or(<string>) = #f,
    init-keyword: reason:;
end class <hope-command-options>;

define constant *reason-allowed-commands*
  = vector(<sccs-claim-command>, <sccs-check-in-command>,
           <sccs-add-command>, <sccs-remove-command>);

define method source-control-command-info
    (sccs :: <hope-source-control-system>,
     command-class :: subclass(<source-code-control-command>),
     #key defaults :: false-or(<hope-command-options>),
          pathname :: false-or(<file-locator>) = #f)
 => (info :: <source-control-command-info>)
  let (compound, unit, branch)
    = if (pathname)
        get-compound-and-unit(pathname)
      else
        values(#f, #f, #f)
      end;
  let reason? :: <boolean> = member?(command-class, *reason-allowed-commands*);
  let options
    = concatenate
        (vector(make(<source-control-option>,
                     label: "Compound",
                     keyword: compound:,
                     type:  <string>,
                     getter: command-compound,
                     documentation: #f,
                     default: compound | (defaults & defaults.command-compound),
                     required?: #t),
                make(<source-control-option>,
                     label: "Unit",
                     keyword: unit:,
                     type:  <string>,
                     getter: command-unit,
                     documentation: #f,
                     default: unit | (defaults & defaults.command-unit),
                     required?: #f),
                make(<source-control-option>,
                     label: "Branch",
                     keyword: branch:,
                     type:  <string>,
                     getter: command-branch,
                     documentation: #f,
                     default: branch | (defaults & defaults.command-branch),
                     required?: #f)),
         if (reason?)
           vector(make(<source-control-option>,
                       label: "Reason",
                       keyword: reason:,
                       type:  <string>,
                       getter: command-reason,
                       documentation: #f,
                       default: (defaults & defaults.command-reason),
                       required?: #f))
         else
           #[]
         end);
  make(<source-control-command-info>,
       title: "Select Compound and Unit",
       class: <hope-command-options>,
       options: options)
end method source-control-command-info;


/// Commands

define abstract class <hope-command>
    (<source-code-control-command>, <basic-string-command>)
  slot sccs-command-pathname :: false-or(<string>) = #f;
  slot sccs-command-compound :: false-or(<string>) = #f;
  slot sccs-command-unit :: false-or(<string>) = #f;
  slot sccs-command-branch :: false-or(<string>) = #f;
  slot sccs-command-reason :: false-or(<string>) = #f;
end class <hope-command>;

define method initialize
    (command :: <hope-command>, #key options :: <hope-command-options>) => ()
  next-method();
  let pathname = options.command-pathname;
  sccs-command-pathname(command) := pathname & as(<string>, pathname);
  sccs-command-compound(command) := options.command-compound;
  sccs-command-unit(command)     := options.command-unit;
  sccs-command-branch(command)   := options.command-branch;
  sccs-command-reason(command)   := options.command-reason;
end method initialize;

define method execute-command
    (command :: <hope-command>) => (#rest values)
  let sccs = command-server(command);
  ensure-server-started(sccs);
  let pathname  = sccs-command-pathname(command);
  let locator   = pathname & as(<file-locator>, pathname);
  let directory = locator  & locator-directory(locator);
  if (directory)
    let old-directory = working-directory();
    block ()
      working-directory() := directory;
      do-source-control-commands(sccs, string-for-command(command));
    cleanup
      working-directory() := old-directory
    end
  else
    do-source-control-commands(sccs, string-for-command(command))
  end;
  values(#t, pathname, #f)
end method execute-command;


define method ensure-server-started
    (sccs :: <hope-source-control-system>)
  when (sccs.%use-session?)
    #f                //--- implement the "session" code here
  end
end method ensure-server-started;

define method do-source-control-commands
    (sccs :: <hope-source-control-system>, #rest strings)
  if (sccs.%use-session?)
    // Execute the commands one at a time
    for (string :: <string> in strings)
      do-source-control-command(sccs, string)
    end
  else
    // Concatenate the commands, then execute them as one
    let strings = as(<list>, strings);
    let string  = head(strings);
    let strings = tail(strings);
    while (~empty?(strings))
      string  := concatenate-as(<string>, string, " \";\" ", head(strings));
      strings := tail(strings)
    end;
    do-source-control-command(sccs, string)
  end
end method do-source-control-commands;

define method do-source-control-command
    (sccs :: <hope-source-control-system>, string :: <string>)
  if (sccs.%use-session?)
    #f                //--- implement the "session" code here
  else
    //--- This already waits, but don't we want to return a status code?
    run-application(concatenate-as(<string>, sccs.%image-name, " ", string),
                    under-shell?: #t)
  end
end method do-source-control-command;


define method string-for-argument
    (server :: <hope-source-control-system>, command :: <hope-command>,
     name == #"compound", value :: false-or(<string>))
 => (string :: <string>)
  maybe-format-to-string("-compound %s ", value)
end method string-for-argument;

define method string-for-argument
    (server :: <hope-source-control-system>, command :: <hope-command>,
     name == #"unit", value :: false-or(<string>))
 => (string :: <string>)
  maybe-format-to-string("-unit %s ", value)
end method string-for-argument;

define method string-for-argument
    (server :: <hope-source-control-system>, command :: <hope-command>,
     name == #"branch", value :: false-or(<string>))
 => (string :: <string>)
  maybe-format-to-string("-branch %s ", value)
end method string-for-argument;

define method string-for-argument
    (server :: <hope-source-control-system>, command :: <hope-command>,
     name == #"reason", value :: false-or(<string>))
 => (string :: <string>)
  maybe-format-to-string("-reason \"%s\" ", value)
end method string-for-argument;

define inline function maybe-format-to-string
    (format-string :: <string>, value :: false-or(<string>))
 => (string :: <string>)
  if (value & ~empty?(value))
    format-to-string(format-string, value)
  else
    ""
  end
end function maybe-format-to-string;


define macro hope-command-definer
  { define hope-command ?:name ?slots:* end }
    => { define sealed string-command "<hope-" ## ?name ## "-command>"
             ("<sccs-" ## ?name ## "-command>", <hope-command>)
           inherited named-argument compound is sccs-command-compound;
           inherited named-argument unit     is sccs-command-unit;
           inherited named-argument branch   is sccs-command-branch;
           inherited named-argument reason   is sccs-command-reason;
           ?slots
         end;
         define sealed domain make (singleton("<hope-" ## ?name ## "-command>"));
         define sealed domain initialize ("<hope-" ## ?name ## "-command>");
         define sealed method class-for-sccs-command
             (sccs :: <hope-source-control-system>, class == "<sccs-" ## ?name ## "-command>")
          => (class == "<hope-" ## ?name ## "-command>")
           "<hope-" ## ?name ## "-command>"
         end method class-for-sccs-command; }
end macro hope-command-definer;


/// The HOPE commands
/// NB: no spaces after arguments because 'string-for-argument' does this

// 'note-source-control-system-selected' copies the Hope command strings
// into the current command strings, but the command string chooser
// updates and saves the back-end commands strings.

define variable *hope-claim-command-string*
  = "checkout -claim soft $(compound)$(unit)$(branch)$(reason)";

define hope-command claim
  keyword pattern-string: = *claim-command-string*;
end;


define variable *hope-check-out-command-string*
  = "checkout $(compound)$(unit)$(branch)";

define hope-command check-out
  keyword pattern-string: = *check-out-command-string*;
end;


define variable *hope-check-in-command-string*
  = "checkin $(compound)$(unit)$(branch)$(reason)";

define hope-command check-in
  keyword pattern-string: = *check-in-command-string*;
end;


define variable *hope-abandon-command-string*
  = "abandon $(compound)$(unit)$(branch)";

define hope-command abandon
  keyword pattern-string: = *abandon-command-string*;
end;


define variable *hope-merge-command-string*
  = "merge $(compound)$(unit)$(branch)$(reason)";

define hope-command merge
  keyword pattern-string: = *merge-command-string*;
end;


define variable *hope-diff-command-string*
  = "diff $(compound)$(unit)$(branch)";

define hope-command diff
  keyword pattern-string: = *diff-command-string*
end;


define variable *hope-report-command-string*
  = "report $(compound)$(unit)$(branch)";

define hope-command report
  keyword pattern-string: = *report-command-string*;
end;


define variable *hope-add-command-string*
  = "add $(compound)$(unit)$(branch)$(reason)";

define hope-command add
  keyword pattern-string: = *add-command-string*;
end;


define variable *hope-remove-command-string*
  = "remove $(compound)$(unit)$(branch)$(reason)";

define hope-command remove
  keyword pattern-string: = *remove-command-string*;
end;


/// HOPE compound and unit heuristication

define variable *hope-source-directories* :: <vector>
  = #[#[#["dylan", "sources"],        "D",        "trunk"],
      #[#["dylan"],                "D",        "trunk"],
      #[#["lispsrc", "clc"],        "LISP",        "trunk"],
      #[#["clc"],                "LISP",        "trunk"],
      #[#["lucid", "mods"],        "LCL",        "trunk"],
      #[#["clim2"],                "CLIM",        "trunk"],
      #[#["clim2", "clim-top"],        "CLIM",        "trunk"]];

define method get-compound-and-unit
    (pathname :: <pathname>)
 => (compound :: false-or(<string>), unit :: false-or(<string>), branch :: false-or(<string>))
  block (return)
    let locator = as(<file-locator>, pathname);
    let (compound, unit, branch) = read-version-file(locator);
    when (compound & unit)
      return(compound, unit, branch)
    end;
    let name = locator-name(locator);
    let directory = begin
                      let directory-locator = locator-directory(locator);
                      if (directory-locator) locator-path(directory-locator)
                      else #[] end
                    end;
    unless (empty?(directory))
      // The idea here is to locate one of the "sub-paths" given in
      // *hope-source-directories* in the supplied directory.  When
      // we find it, we use its associated compute, and then just
      // append the rest of the stuff in the directory.  It's crude,
      // but it seems to work pretty well.
      for (entry in *hope-source-directories*)
        let dir = entry[0];
        let pos = subsequence-position(directory, dir, test: \=);
        when (pos)
          let suffix = copy-sequence(directory, start: pos + size(dir));
          let compound :: <stretchy-object-vector> = as(<stretchy-vector>, entry[1]);
          let branch = entry[2];
          // Now append the rest of the compound name
          for (component in suffix)
            add!(compound, '-');
            for (char in component)
              add!(compound, char)
            end
          end;
          return(as(<string>, compound), name, branch)
        end
      end
    end;
    values(#f, name, #f)
  end
end method get-compound-and-unit;

define method read-version-file
    (locator :: <locator>)
 => (compound :: false-or(<string>), unit :: false-or(<string>), branch :: false-or(<string>))
  block (return)
    let version = merge-locators(as(<file-locator>, ".version"), locator);
    when (file-exists?(version))
      with-open-file (stream = version, direction: #"input")
        let name = locator-name(locator);
        local method position
                  (string :: <byte-string>, char :: <byte-character>,
                   #key start: _start :: <integer> = 0,
                        end:   _end   :: <integer> = size(string))
               => (index :: false-or(<integer>))
                block (return)
                  for (i :: <integer> = _start then i + 1,
                       until: i = _end)
                    when (string[i] = char)
                      return(i)
                    end
                  end;
                  #f
                end
              end method;
        while (~stream-at-end?(stream))
          let line = read-line(stream);
          let c1   =      position(line, ',');
          let c2   = c1 & position(line, ',', start: c1 + 1);
          let c3   = c2 & position(line, ',', start: c2 + 1);
          when (c3)
            let compound = copy-sequence(line, start: 0,      end: c1);
            let branch   = copy-sequence(line, start: c1 + 1, end: c2);
            let unit     = copy-sequence(line, start: c2 + 1, end: c3);
            when (branch = ".")
              branch := "trunk";
            end;
            when (unit = name)
              return(compound, unit, branch)
            end
          end
        end
      end
    end;
    values(#f, #f, #f)
  end
end method read-version-file;


/// Initialization

define settings <Harlequin_HIS-user-settings> (<current-user-software-settings>)
  key-name "Harlequin_HIS";
end settings <Harlequin_HIS-user-settings>;

define settings <HOPE-user-settings> (<Harlequin_HIS-user-settings>)
  key-name "HOPE";
  slot hope-version :: <string> = "None!", key: #f;
end settings <HOPE-user-settings>;

define constant $hope-settings = make(<HOPE-user-settings>);

when ($hope-settings.hope-version ~= "None!")
  register-source-control-class(<hope-source-control-system>)
end;
