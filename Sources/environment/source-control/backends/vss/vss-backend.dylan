Module:    source-control-vss-backend
Synopsis:  Visual SourceSafe backend for Dylan environment
Author:    Gary Palter
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Visual SourceSafe

define sealed class <vss-source-control-system> (<source-control-system>)
  slot %database :: false-or(<IVSSDatabase>) = #f;
  // slot %project-cache :: <table> = make(<table>);
  keyword name:  = #"SourceSafe";
  keyword label: = "SourceSafe";
  keyword title: = "Microsoft Visual SourceSafe";
end class <vss-source-control-system>;

define sealed domain make (singleton(<vss-source-control-system>));
define sealed domain initialize (<vss-source-control-system>);

define method note-source-control-system-selected
    (sccs :: <vss-source-control-system>) => ()
  #f
end method note-source-control-system-selected;

define settings <microsoft-local-settings> (<local-software-settings>)
  key-name "Microsoft";
end settings <microsoft-local-settings>;

define settings <sourcesafe-local-settings> (<microsoft-local-settings>)
  key-name "SourceSafe";
  slot current-database :: <string> = "None!", key: "Current Database";
  slot api-current-database :: <string> = "None!", key: "API Current Database";
end settings <sourcesafe-local-settings>;

define constant $sourcesafe-settings = make(<sourcesafe-local-settings>);

define settings <sourcesafe-databases> (<sourcesafe-local-settings>)
  key-name "Databases";
end settings <sourcesafe-databases>;

define constant $sourcesafe-databases = make(<sourcesafe-databases>);


/// Logging into a SourceSafe database

define class <vss-login-options> (<source-control-login-options>)
  sealed slot login-user :: <string>,
    required-init-keyword: user:;
  sealed slot login-password :: <string>,
    required-init-keyword: password:;
  sealed slot login-database :: <string>,
    required-init-keyword: database:;
end class <vss-login-options>;

define method source-control-login-info
    (sccs :: <vss-source-control-system>, 
     command-class :: subclass(<source-code-control-command>),
     #key defaults :: false-or(<vss-login-options>),
          pathname :: false-or(<file-locator>) = #f)
 => (info :: false-or(<source-control-login-info>))
  unless (sccs.%database)
    let options
      = vector(make(<source-control-option>,
		    label: "User",
		    keyword: user:,
		    type:  <string>,
		    getter: login-user,
		    documentation: #f,
		    default: login-name(),
		    required?: #t),
	       make(<source-control-option>,
		    label: "Password",
		    keyword: password:,
		    type:  #"password",
		    getter: login-password,
		    documentation: #f,
		    default: #f,
		    required?: #t),
	       make(<source-control-option>,
		    label: "Database",
		    keyword: database:,
		    type:  <string>,
		    getter: login-database,
		    documentation: #f,
		    default: default-database-name(),
		    required?: #t));
    make(<source-control-login-info>,
	 title: "Select Database",
	 class: <vss-login-options>,
	 options: options)
  end
end method source-control-login-info;

define method source-control-login
    (sccs :: <vss-source-control-system>, options :: <vss-login-options>)
 => (logged-in? :: <boolean>, message :: false-or(<string>));
  let database-pathname = database-pathname-from-name(options.login-database);
  if (database-pathname)
    block ()
      OLE-initialize();
      let db = make-VSSDatabase();
      IVSSDatabase/Open(db, as(<string>, database-pathname),
			options.login-user, options.login-password);
      sccs.%database := db;
      values(#t, #f)
    exception (condition :: <ole-error>)
      values(#f, format-to-string("Login failed: %s", condition-to-string(condition)))
    end
  else
    values(#f, format-to-string("Unknown database: %s", options.login-database))
  end
end method source-control-login;

define function default-database-name () => (name :: false-or(<string>))
  local method try (path :: <string>) => (name :: false-or(<string>))
	  path & locator-name(as(<directory-locator>, path))
	end method try;
  try($sourcesafe-settings.current-database)
    | try($sourcesafe-settings.api-current-database)
end function default-database-name;

define function database-pathname-from-name
    (name :: <string>) => (database-pathname :: false-or(<file-locator>))
  initialize-settings($sourcesafe-databases, #f);
  let handle = settings-handle($sourcesafe-databases);
  when (handle)
    let handle = as(<HKEY>, handle);
    with-stack-structure (count :: <LPDWORD>)
      pointer-value(count) := 0;
      let (status, type)
	= RegQueryValueEx(handle, name, null-pointer(<LPDWORD>), null-pointer(<LPBYTE>), count);
      when (zero?(status) & (type = $REG-SZ))
	with-stack-structure (buffer :: <C-string>, size: pointer-value(count))
	  let status
	    = RegQueryValueEx(handle, name, null-pointer(<LPDWORD>), buffer, count);
	  when (zero?(status))
	    merge-locators(as(<file-locator>, "SRCSAFE.INI"),
			   as(<directory-locator>, buffer))
	  end
	end
      end
    end
  end
end function database-pathname-from-name;


/// Argument parsing

define class <vss-command-options> (<source-control-command-options>)
  sealed slot command-project :: <string>,
    required-init-keyword: project:;
  sealed slot command-file :: <string>,
    required-init-keyword: file:;
  sealed slot command-reason :: false-or(<string>) = #f,
    init-keyword: reason:;
end class <vss-command-options>;

define constant *file-required-commands* = vector(<sccs-add-command>, <sccs-remove-command>);

define constant *reason-allowed-commands*
  = vector(<sccs-claim-command>, <sccs-check-in-command>, <sccs-add-command>);

define method source-control-command-info
    (sccs :: <vss-source-control-system>, 
     command-class :: subclass(<source-code-control-command>),
     #key defaults :: false-or(<vss-command-options>),
          pathname :: false-or(<file-locator>) = #f)
 => (info :: <source-control-command-info>)
  let (project, file)
    = if (pathname) 
	get-project-and-file(pathname)
      else
	values(#f, #f)
      end;
  let file-required? :: <boolean> = member?(command-class, *file-required-commands*);
  let reason? :: <boolean> = member?(command-class, *reason-allowed-commands*);
  let options
    = concatenate
        (vector(make(<source-control-option>,
		     label: "Project",
		     keyword: project:,
		     type:  <string>,
		     getter: command-project,
		     documentation: #f,
		     default: (defaults & defaults.command-project) | project,
		     required?: #t),
		make(<source-control-option>,
		     label: "File",
		     keyword: file:,
		     type:  <string>,
		     getter: command-file,
		     documentation: #f,
		     default: file | (defaults & defaults.command-file),
		     required?: file-required?)),
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
       title: "Select Project and File",
       class: <vss-command-options>,
       options: options)
end method source-control-command-info;

/// SourceSafe project and file heuristication -- HACK!
define method get-project-and-file
    (pathname :: <file-locator>)
 => (project :: false-or(<string>), file :: false-or(<string>))
  let file = locator-name(pathname);
  let directory = locator-directory(pathname);
  if (directory & locator-name(directory))
    let names = make(<deque>);
    while (directory)
      push(names, locator-name(directory));
      directory := locator-directory(directory);
    end;
    let project = "$";
    pop(names);
    while (~empty?(names))
      let name = pop(names);
      project := concatenate(project, "/", name)
    end;
    values(project, file)
  else
    values(#f, file)
  end
end method get-project-and-file;


/// Commands

define abstract class <vss-source-control-command>
    (<source-code-control-command>, <basic-command>)
end class <vss-source-control-command>;

define macro with-vss-ole-error-interpretation
  { with-vss-ole-error-interpretation (?sccs:name, ?command:name) ?:body end }
    => { interpreting-vss-ole-errors (?sccs, ?command, method () ?body end) }
end macro with-vss-ole-error-interpretation;

define function interpreting-vss-ole-errors
    (sccs :: <vss-source-control-system>, command :: <vss-source-control-command>, f :: <function>)
  block ()
    f()
  exception (condition :: <ole-error>)
    let status = condition.ole-error-status;
    when (FAILED?(status))
      //---*** TODO: Better error handling!
      //---***  (Unfortunately, it looks like the VSS automation only returns BAD-PARAMETER!)
      error(make(<source-control-command-failed>, source-control: sccs, command: command));
    end
  end
end function interpreting-vss-ole-errors;

define method execute-command
    (command :: <vss-source-control-command>) => (#rest values)
  let sccs = command-server(command);
  do-source-control-command(command)
end method execute-command;

define function find-vss-item
    (sccs :: <vss-source-control-system>, command :: <vss-source-control-command>,
     #key ignore-file? :: <boolean> = #f)
 => (vss-item :: <IVSSItem>, pathname :: <string>)
  let options  = command.sccs-command-options;
  let pathname = command-pathname(options);
  let project  = command-project(options);
  let file     = command-file(options);
  let vss-spec = if (file & ~ignore-file?)
                   concatenate(project, "/", file)
                 else
                   project
                 end;
  values(IVSSDatabase/VSSItem(sccs.%database, vss-spec, #f),
	 if (file & ~ignore-file?)
	   let filename = as(<file-locator>, file);
	   as(<string>, merge-locators(filename, locator-directory(pathname)))
	 else
	   as(<string>, locator-directory(pathname))
	 end)
end function find-vss-item;

///---*** TO DO: Add other arguments?
define macro vss-command-definer
  { define vss-command ?:name (?sccs:name, ?command:name, ?options:name, ?pathname:name)
      ?:body
    end }
    => { define sealed class "<vss-" ## ?name ## "-command>"
	     ("<sccs-" ## ?name ## "-command>", <vss-source-control-command>)
	 end;
	 define sealed domain make (singleton("<vss-" ## ?name ## "-command>"));
	 define sealed domain initialize ("<vss-" ## ?name ## "-command>");
	 define sealed method class-for-sccs-command
	     (sccs :: <vss-source-control-system>, class == "<sccs-" ## ?name ## "-command>")
	  => (class == "<vss-" ## ?name ## "-command>")
	   "<vss-" ## ?name ## "-command>"
	 end method class-for-sccs-command; 
         define sealed method do-source-control-command
	     (?command :: "<sccs-" ## ?name ## "-command>")
          => (#rest values)
           let ?sccs :: <vss-source-control-system> = command-server(?command);
	   let ?options :: <vss-command-options> = sccs-command-options(?command);
	   let ?pathname :: <string> = as(<string>, command-pathname(?options));
           with-vss-ole-error-interpretation (?sccs, ?command)
             ?body
           end
         end method do-source-control-command; }
end macro vss-command-definer;

///---*** TO DO: Error handling!

define vss-command claim (sccs, command, options, pathname)
  let (target, pathname) = find-vss-item(sccs, command);
  IVSSItem/CheckOut(target,
                    command-reason(options) | "",
                    pathname,
                    //---*** TODO: Are these the right settings?
                    %logior($VSSFLAG-CHKEXCLUSIVENO, $VSSFLAG-REPREPLACE));
  values(#t, pathname, #f)
end vss-command claim;

define vss-command check-out (sccs, command, options, pathname)
  let (target, pathname) = find-vss-item(sccs, command);
  let pathname-byref = inout-ref(as(<BSTR>, pathname));
  IVSSItem/Get(target,
               pathname-byref,
               //---*** TODO: Are these the right settings?
               $VSSFLAG-REPREPLACE);
  values(#t, pathname, #f)
end vss-command check-out;

define vss-command check-in (sccs, command, options, pathname)
  let (target, pathname) = find-vss-item(sccs, command);
  if (command-file(options) & ~IVSSItem/IsDifferent(target, pathname))
    values(#f, pathname, 
	   format-to-string("%s has not been changed since it was checked out", 
			    command-file(options)))
  else
    IVSSItem/CheckIn(target,
		     command-reason(options) | "",
		     pathname,
		     //---*** TODO: Are these the right settings?
		     %logior($VSSFLAG-REPREPLACE, $VSSFLAG-UPDUNCH));
    values(#t, pathname, #f)
  end
end vss-command check-in;

define vss-command abandon (sccs, command, options, pathname)
  let (target, pathname) = find-vss-item(sccs, command);
  IVSSItem/UndoCheckOut(target,
                        pathname,
                        //---*** TODO: Are these the right settings?
                        $VSSFLAG-REPREPLACE);
  values(#t, pathname, #f)
end vss-command abandon;

define vss-command merge (sccs, command, options, pathname)
  let (target, pathname) = find-vss-item(sccs, command);
  let pathname-byref = inout-ref(as(<BSTR>, pathname));
  IVSSItem/Get(target,
               pathname-byref,
               //---*** TODO: Are these the right settings?
               $VSSFLAG-REPMERGE);
  values(#t, pathname, #f)
end vss-command merge;

/*
define vss-command diff (sccs, command, options, pathname)
  //---*** HOW?  (Automation doesn't provide anything more than a boolean!)
  error(make(<source-control-unsupported-command>, source-control: sccs, command: command))
end vss-command diff;
*/

/*
define vss-command report (sccs, command, options, pathname)
  //---*** TODO: Get the Versions list and format it nicely but how do I display it?
  error(make(<source-control-unsupported-command>, source-control: sccs, command: command))
end vss-command report;
*/

define vss-command add (sccs, command, options, pathname)
  let target = find-vss-item(sccs, command, ignore-file?: #t);
  IVSSItem/Add(target,
	       pathname,
	       command-reason(options) | "",
               //---*** TODO: Are these the right settings?
	       0);
  values(#t, 
	 pathname,
	 format-to-string("%s has been added to %s",
			  command-file(options), command-project(options)))
end vss-command add;

define vss-command remove (sccs, command, options, pathname)
  let (target, pathname) = find-vss-item(sccs, command);
  if (IVSSItem/Deleted(target))
    values(#t,
	   pathname, 
	   format-to-string("%s already deleted; use the SourceSafe explorer to purge/restore it",
			    command-file(options)))
  else
    IVSSItem/Deleted(target) := #t;
    values(#t,
	   pathname,
	   format-to-string("%s has been marked as deleted", command-file(options)))
  end
end vss-command remove;


/// Initialization

when ($sourcesafe-settings.current-database ~= "None!"
	| $sourcesafe-settings.api-current-database ~= "None!")
  register-source-control-class(<vss-source-control-system>)
end;

