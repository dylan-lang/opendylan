Module:    environment-tools
Synopsis:  Project Commands
Author:    Andy Armstrong, Chris Page, Jason Trenouth, Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Project persistence

define settings <project-history> (<open-dylan-user-settings>)
  key-name "Projects";
  slot project1 :: <string> = "";
  slot project2 :: <string> = "";
  slot project3 :: <string> = "";
  slot project4 :: <string> = "";
  slot project5 :: <string> = "";
  slot project6 :: <string> = "";
  slot project7 :: <string> = "";
  slot project8 :: <string> = "";
  slot project9 :: <string> = "";
end settings <project-history>;

define open generic register-opened-file (filename :: <file-locator>) => ();

define constant $project-history = make(<project-history>);

define function most-recent-project
    () => (project :: false-or(<file-locator>))
  let project = $project-history.project1;
  unless (project = "") as(<file-locator>, project) end
end function most-recent-project;

define function most-recent-project-setter
    (project :: <file-locator>) => (project :: <file-locator>)
  register-opened-file(project);
  unless (project = most-recent-project())
    let projects = most-recent-projects(n: 8);
    let new-projects
      = concatenate(vector(project),
		    remove(projects, project, test: \=));
    most-recent-projects() := new-projects
  end;
  project
end function most-recent-project-setter;

define function most-recent-projects 
    (#key n :: <integer> = 9) => (projects :: <sequence>)
  let projects
    = remove(vector($project-history.project1,
		    $project-history.project2,
		    $project-history.project3,
		    $project-history.project4,
		    $project-history.project5,
		    $project-history.project6,
		    $project-history.project7,
		    $project-history.project8,
		    $project-history.project9),
	     "", test: \=);
  map(curry(as, <file-locator>),
      copy-sequence(projects, end: min(n, size(projects))))
end function most-recent-projects;

define function most-recent-projects-setter
    (projects :: <sequence>) => (projects :: <sequence>)
  let project-names = map(curry(as, <string>), projects);
  let blanks = 9 - size(project-names);
  let padded-project-names
    = if (blanks > 0)
	concatenate(project-names, make(<vector>, fill: "", size: blanks))
      else
	project-names
      end;
  $project-history.project1 := padded-project-names[0];
  $project-history.project2 := padded-project-names[1];
  $project-history.project3 := padded-project-names[2];
  $project-history.project4 := padded-project-names[3];
  $project-history.project5 := padded-project-names[4];
  $project-history.project6 := padded-project-names[5];
  $project-history.project7 := padded-project-names[6];
  $project-history.project8 := padded-project-names[7];
  $project-history.project9 := padded-project-names[8];
  projects
end function most-recent-projects-setter;

tune-in($project-channel,
	method (message :: <project-opened-message>)
	  let project = message.message-project;
	  let filename = project.project-filename;
	  most-recent-project() := filename | project.project-debug-filename;
	  if (project-can-be-debugged?(project)
		& environment-active-on-opening?())
	    //--- We switch the active project in a new thread so that
	    //--- 'active-project-setter' doesn't block doing a re-entrant
	    //--- broadcast when it signals <project-now-active-message>.
	    make(<thread>,
		 name: "Updating the active project",
		 function: method ()
			     with-abort-restart ()
			       active-project() := project
			     end
			   end)
	  end
	end,
	message-type: <project-opened-message>);

/// File persistence

define settings <file-history> (<open-dylan-user-settings>)
  key-name "Files";
  slot file1 :: <string> = "";
  slot file2 :: <string> = "";
  slot file3 :: <string> = "";
  slot file4 :: <string> = "";
  slot file5 :: <string> = "";
  slot file6 :: <string> = "";
  slot file7 :: <string> = "";
  slot file8 :: <string> = "";
  slot file9 :: <string> = "";
end settings <file-history>;

define constant $file-history = make(<file-history>);

define function most-recent-file
    () => (file :: false-or(<file-locator>))
  let file = $file-history.file1;
  unless (file = "") as(<file-locator>, file) end
end function most-recent-file;

define function most-recent-file-setter
    (locator :: <file-locator>) => (file :: <file-locator>)
  register-opened-file(locator);
  unless (locator = most-recent-file())
    let files = most-recent-files(n: 8);
    let new-files
      = concatenate(vector(locator),
		    remove(files, locator, test: \=));
    most-recent-files() := new-files
  end;
  locator
end function most-recent-file-setter;

define function most-recent-files 
    (#key n :: <integer> = 9) => (files :: <sequence>)
  let files
    = remove(vector($file-history.file1,
		    $file-history.file2,
		    $file-history.file3,
		    $file-history.file4,
		    $file-history.file5,
		    $file-history.file6,
		    $file-history.file7,
		    $file-history.file8,
		    $file-history.file9),
	     "", test: \=);
  map(curry(as, <file-locator>),
      copy-sequence(files, end: min(n, size(files))))
end function most-recent-files;

define function most-recent-files-setter
    (files :: <sequence>) => (files :: <sequence>)
  let file-names = map(curry(as, <string>), files);
  let blanks = 9 - size(file-names);
  let padded-file-names
    = if (blanks > 0)
	concatenate(file-names, make(<vector>, fill: "", size: blanks))
      else
	file-names
      end;
  $file-history.file1 := padded-file-names[0];
  $file-history.file2 := padded-file-names[1];
  $file-history.file3 := padded-file-names[2];
  $file-history.file4 := padded-file-names[3];
  $file-history.file5 := padded-file-names[4];
  $file-history.file6 := padded-file-names[5];
  $file-history.file7 := padded-file-names[6];
  $file-history.file8 := padded-file-names[7];
  $file-history.file9 := padded-file-names[8];
  files
end function most-recent-files-setter;


/// Open a project

define method frame-open-project
    (frame :: <environment-frame>, object :: <object>) => ()
  ignore(frame);
  let project = coerce-project(object);
  if (project)
    ensure-project-browser-showing-project(project)
  else
    environment-error-message
      (format-to-string("Project %s does not exist", object),
       owner: frame)
  end
end method frame-open-project;


/// Create a new text or project file

define function open-project-file
    (location :: <file-locator>, #key frame) => ()
  if (frame)
    frame-open-project(frame, location)
  else
    find-project-browser(location)
  end
end function open-project-file;

define function create-new-project
    (#rest args,
     #key frame :: false-or(<environment-frame>),
	  open-new-project? :: <boolean> = #t)
 => (projects :: <sequence> /* of: <project-object> */)
  let project-locations = apply(new-project-wizard, args);
  let projects = make(<stretchy-vector>);
  // Try to open all the projects.  If any fail (in COERCE-PROJECT), we
  // don't add them to the result sequence or OPEN-PROJECT-FILE them.
  for (location in project-locations)
    let project = coerce-project(location);
    if (project)
      add!(projects, project);
      if (open-new-project?)
	open-project-file(location, frame: frame);
      end;
    end;
  end;
  projects
end function create-new-project;

define frame <new-file-dialog> (<dialog-frame>)
  constant slot frame-project :: false-or(<project-object>) = #f,
    init-keyword: project:;
  pane %dialog-new-project-button (frame)
    make(<radio-button>,
         label: "Project",
         id: #"project",
         activate-callback: exit-dialog);
  pane %dialog-new-dylan-button (frame)
    make(<radio-button>,
         label: "Dylan File",
         id: #"dylan",
         activate-callback: exit-dialog);
  pane %dialog-new-text-button (frame)
    make(<radio-button>,
         label: "Text File",
         id: #"text",
         activate-callback: exit-dialog);
  pane %new-project-icon (frame)
    make(<label>,
	 label: $project-file-large-bitmap,
	 width:  32, min-width:  32, max-width:  32,
	 height: 32, min-height: 32, max-height: 32);
  pane %new-dylan-file-icon (frame)
    make(<label>,
	 label: $dylan-file-large-bitmap,
	 width:  32, min-width:  32, max-width:  32,
	 height: 32, min-height: 32, max-height: 32);
  pane %new-text-file-icon (frame)
    make(<label>,
	 label: $text-file-large-bitmap,
	 width:  32, min-width:  32, max-width:  32,
	 height: 32, min-height: 32, max-height: 32);
  pane %add-to-project-button (frame)
    make(<check-button>,
	 label: "Insert file into project?",
	 value: frame.frame-project ~= #f);
  pane %buttons (frame)
    make(<radio-box>,
	 value: #"dylan",
	 child: make(<table-layout>,
		     columns: 2,
		     y-alignment: #"center",
		     y-spacing: 2,
		     children:
		       vector(frame.%new-project-icon,
			      frame.%dialog-new-project-button,
			      frame.%new-dylan-file-icon,
			      frame.%dialog-new-dylan-button,
			      frame.%new-text-file-icon,
			      frame.%dialog-new-text-button)));
  pane %button-layout (frame)
    grouping ("Create a new file of type")
      vertically (spacing: 2, max-width: $fill)
	frame.%buttons;
      end
    end;
  layout (frame)
    if (frame.frame-project)
      vertically (spacing: 6)
        frame.%button-layout;
        frame.%add-to-project-button;
      end
    else
      frame.%button-layout
    end;
  keyword title: = "New";
end frame <new-file-dialog>;

// This method is used by the editor as well.
define method frame-new-file (frame :: <environment-frame>) => ()
  let project = frame.frame-current-project;
  let dialog = make(<new-file-dialog>, owner: frame, project: project);
  when (start-dialog(dialog))
    let add-to-project? = gadget-value(dialog.%add-to-project-button);
    let type = gadget-value(dialog.%buttons);
    let new-files
      = select (type)
	  #"project" =>
	    let projects = create-new-project(frame: frame);
	    map(project-filename, projects);
	  otherwise =>
	    let filename
	      = environment-choose-file
	          (title:     "New",
		   owner:     frame,
		   direction: #"output",
		   directory: project & project.project-directory,
		   filters:   vector(type));
	    if (filename
		  & (file-exists?(filename)
		       | create-empty-file(filename, type: type)))
	      editor-open-file(filename);
	      vector(filename)
	    else
	      #[]
	    end;
	end;
    if (add-to-project?)
      do(method (new-file)
	   do-frame-insert-source-file
	     (frame, filename: new-file, project: project)
	 end,
	 new-files)
    end
  end;
end method frame-new-file;

define function create-empty-file
    (filename :: <file-locator>, #key type) => (success? :: <boolean>)
  with-open-file (stream = filename, 
		  direction: #"output",
		  if-exists: #"signal",
		  if-does-not-exist: #"create")
    when (type == #"dylan")
      //---*** Add a Dylan file header here
      #f
    end;
    write-line(stream, "")
  end;
  #t
end function create-empty-file;


/// Open Selection command

//---*** cpage: 1997.08.22 I've temporarily created this new command local to
//              the project browser. This should be moved some place more global,
//              like dylanworks-frame.dylan or perhaps environment-framework.

define open generic frame-open-selection (frame :: <frame>) => ();

// Default method
define method frame-open-selection (frame :: <frame>) => ()
  // Do nothing
  #f
end method frame-open-selection;


/// Open file/project

define variable *open-lid-projects?* :: <boolean> = #t;

// This function decides whether or not the environment will open
// .lid files directly, or instead will try to import them first.
//----*** cpage: 1998.03.01 This function is a bit of a hack. It is used to allow
//               directly opening LID files in project windows without having to
//               import them to project files. This is currently intended only
//               for private use by Functional Developer developers. Moreover, this
//               function is brittle in that it checks whether undocumented (DOS)
//               environment variables are set.
define function open-lid-projects?
    () => (open? :: <boolean>)
  //--- Only support this if the user is using user registries
  let using-registries?
    = environment-variable("OPEN_DYLAN_USER_REGISTRIES");
  using-registries?
    & *open-lid-projects?*
end function open-lid-projects?;


/// Open a file

define variable %default-open-file-filters :: <sequence> = #[];

// Delayed initialization of default filters
define function default-open-file-filters () => (filters :: <sequence>)
  if (%default-open-file-filters ~= #[])
    %default-open-file-filters
  else
    %default-open-file-filters
      := if (open-lid-projects?()) // HACK ALERT!
	   filters-for-file-types
	     (#"common", #"dylan", #"tool-spec", #"project", #"lid",
	      #"lid-as-hdp", #"lid-as-text", #"executable", #"all")
	 else
	   filters-for-file-types
	     (#"common", #"dylan", #"tool-spec", #"project", #"lid",
	      #"lid-as-text", #"executable", #"all")
	 end
  end if
end function default-open-file-filters;

define variable *default-open-file-filter-index* = 0;

define function open-file
    (#key owner :: false-or(<frame>) = #f,
          filename,
          deuce-frame,
          direction = #"input",
          default,
          default-type,
          filters,
          default-filter)
 => (status-code :: false-or(<integer>))
  let filters-supplied? = filters & #t;
  let filters = filters | default-open-file-filters();
  let (filename, filter-index)
    = if (filename ~= #f)
        values(filename, #f)
      else
        let default-filter = default-filter
                               | if (filters-supplied?)
                                   0
                                 else
                                   *default-open-file-filter-index*
                                 end;
	//---*** andrewa: Convert to environment-choose-file?
	choose-file(title: "Open",
		    owner: owner,
		    direction: direction,
		    default: default, default-type: default-type,
		    filters: filters, default-filter: default-filter)
      end if;
  when (~filters-supplied? & filter-index ~= #f)
    *default-open-file-filter-index* := filter-index;
  end;
  when (filename)
    let filename = as(<file-locator>, filename);
    // hughg, 1997/10/17:  We probably eventually want to do something
    // nicer than nested 'if's or a select to decide how to open
    // different file types.
    let type = filename.environment-locator-type;
    select (type)
      #"lid" =>
	let import-by-default? = ~open-lid-projects?();
	let (import?, as-text?)
	  = if (filter-index)
	      let filter = filters[filter-index];
	      values(import-by-default?
		       | filter == filter-for-lid-as-hdp(),
		     filter == filter-for-lid-as-text())
	    else
	      values(import-by-default?, #f)
	    end;
	case
	  as-text? =>
	    editor-open-file(filename, deuce-frame: deuce-frame);
	    0;
	  ~import? =>
	    open-project-file(filename, frame: owner); // HACK ALERT!
	    0;
	  otherwise =>
	    import-lid-file(frame: owner, filename: filename);
	end;
      #"hdp", #"ddb" =>
	open-project-file(filename, frame: owner);
        0;
      #"exe" =>
	let project = create-exe-project-from-file(filename);
	ensure-project-browser-showing-project(project);
	0;
      otherwise =>
        editor-open-file(filename, deuce-frame: deuce-frame);
        0;
    end
  end
end function open-file;


/// Open/Close file commands

// No caller should need to supply a "deuce-frame:" argument: a more
// specialized method for the editor will fill it in then call next-method.
define method frame-open-file
    (frame :: <environment-frame>,
     #rest keys, #key filename, deuce-frame, #all-keys)
 => ()
  apply(open-file, owner: frame, keys);
end method frame-open-file;

define method frame-close-file
    (frame :: <environment-frame>, #key filename)
 => ()
  debug-assert(filename = #f,
               "frame-close-file doesn't support the filename argument yet!");
  //--- cpage: 1997.08.26 Although this function is named frame-close-file, it
  //           is being used here to mean "close the window", which will only
  //           close a file if the exit handler for the frame chooses to.
  //           Alternatively, we could change the appropriate command tables to
  //           call frame-exit instead of frame-close-file.
  exit-frame(frame);
end method frame-close-file;


/// ENV-OPEN-FILE (ENVIRONMENT-MANAGER)

define sideways method environment-open-file 
    (filename :: <file-locator>)
 => (success? :: <boolean>)
  open-file(owner: environment-primary-frame(), 
	    filename: as(<string>, filename)) = #f
end method environment-open-file;



/// Project error handling

define class <project-error> (<format-string-condition>, <error>)
end class <project-error>;

define class <link-error> (<project-error>)
end class <link-error>;

define macro with-frame-project-operation
  { with-frame-project-operation (?frame:name, ?project:expression)
      ?body:body
    end }
    => { do-frame-project-operation(method () ?body end, ?frame, ?project) }
end macro with-frame-project-operation;

define method display-project-error
    (frame :: <environment-frame>, project :: <project-object>,
     error :: <project-error>)
 => ()
  let message = format-to-string("Operation failed\n\n%s", error);
  environment-error-message(message, owner: frame);
end method display-project-error;

define method display-project-error
    (frame :: <environment-frame>, project :: <project-object>,
     error :: <link-error>)
 => ()
  let message
    = format-to-string("Link failed\n\n"
		       "%s\n\n"
		       "Would you like to view the log file?",
		       error);
  if (environment-question(message, owner: frame, style: #"error"))
    let build-directory = project-build-directory(project);
    //---*** andrewa: need a way to find the real log file
    let build-log-file
      = format-to-string("%s\\build.log", build-directory);
    frame-open-file(frame, filename: build-log-file)
  end
end method display-project-error;

define function do-frame-project-operation
    (function :: <function>, frame :: <environment-frame>, 
     project :: <project-object>)
 => (#rest results)
  block ()
    function()
  exception (error :: <project-error>)
    display-project-error(frame, project, error)
  end
end function do-frame-project-operation;

define method compiler-condition-handler
    (frame :: <environment-frame>,
     handler-type == #"project-not-found", 
     library :: <string>)
 => (filename :: false-or(<file-locator>))
  ignore(handler-type);
  choose-missing-project(frame, library: library)
end method compiler-condition-handler;

define method compiler-condition-handler
    (frame :: <environment-frame>,
     handler-type == #"project-file-not-found", 
     filename :: <string>)
 => (filename :: false-or(<file-locator>))
  ignore(handler-type);
  choose-missing-project(frame, filename: as(<file-locator>, filename))
end method compiler-condition-handler;

define function choose-missing-project
    (frame :: <environment-frame>,
     #key filename :: false-or(<file-locator>),
          library :: false-or(<string>))
 => (filename :: false-or(<file-locator>))
  let message
    = format-to-string("Missing project '%s'", 
		       filename | library | $unknown);
  let filename
    = environment-choose-file
        (title:   message,
	 owner:   frame,
	 default: filename,
	 filters: #[#"common-locate-project", #"project", #"lid"]);
  if (filename)
    do-frame-insert-source-file(frame, filename: filename);
    filename
  end
end function choose-missing-project;

define method compiler-condition-handler
    (frame :: <environment-frame>,
     handler-type == #"link-error", message :: <string>)
 => (filename :: singleton(#f))
  error(make(<link-error>,
	     format-string: "%s",
	     format-arguments: vector(message)))
end method compiler-condition-handler;

define method compiler-condition-handler
    (frame :: <environment-frame>,
     handler-type == #"link-warning", message :: <string>)
 => (filename :: singleton(#f))
  environment-warning-message(message, owner: frame)
end method compiler-condition-handler;

define method compiler-condition-handler
    (frame :: <environment-frame>,
     handler-type == #"fatal-error", message :: <string>)
 => (filename :: singleton(#f))
  error(make(<project-error>,
	     format-string: "%s",
	     format-arguments: vector(message)))
end method compiler-condition-handler;

define method compiler-condition-handler
    (frame :: <environment-frame>,
     handler-type == #"yes-no", 
     message :: <string>)
 => (yes? :: <boolean>)
  environment-question(message, owner: frame)
end method compiler-condition-handler;

define method compiler-condition-handler
    (frame :: <environment-frame>,
     handler-type == #"warning", 
     message :: <string>)
 => ()
  environment-warning-message(message, owner: frame)
end method compiler-condition-handler;


/// Build command table

define open generic frame-parse-project
    (frame :: <environment-frame>, #key process-subprojects?) => ();
define open generic frame-compile-project
    (frame :: <environment-frame>, #key process-subprojects?) => ();
define open generic frame-clean-compile-project
    (frame :: <environment-frame>, #key process-subprojects?) => ();
define open generic frame-link-project
    (frame :: <environment-frame>, #key process-subprojects?) => ();

define open generic frame-build-project
    (frame :: <environment-frame>, #key process-subprojects?) => ();
define open generic frame-clean-build-project
    (frame :: <environment-frame>, #key process-subprojects?) => ();
define open generic frame-remove-build-products
    (frame :: <environment-frame>, #key process-subprojects?) => ();

define open generic frame-build-release
    (frame :: <environment-frame>) => ();


define constant $no-database-message
  = "The project '%s' does not have a compilation database. "
    "Compile the project first?";

define constant $project-not-built-message
  = "The target '%s' does not exist. Build it first?";

define macro with-built-project
  { with-built-project (?frame:name, ?options:*)
      ?body:body
    end }
    => { do-with-built-project(method (?frame) ?body end, ?frame, ?options) }
end macro with-built-project;

define method do-with-built-project
    (function :: <function>, frame :: <environment-frame>, #key force?) => ()
  let project = frame.ensure-frame-project;
  let filename = project.project-full-build-filename;
  let rebuild?
    = project.project-can-be-built?
        & (force?
	     | (~file-exists?(filename)
		  & environment-question
		      (format-to-string($project-not-built-message, filename),
		       owner: frame,
		       style: #"warning",
		       exit-style: #"ok-cancel")));
  case
    rebuild? =>
      let compile? = ~project-compiled?(project);
      frame-do-build-project(frame,
			     parse?: #f, compile?: compile?, link?: #t,
			     clean?: #f,
			     finish-function: function);
    file-exists?(filename) =>
      function(frame);
    ~project.project-can-be-built? =>
      environment-error-message
	(format-to-string
	   ("The target '%s' does not exist and cannot be built.",
	    filename),
	 owner: frame);
    otherwise =>
      #f;
  end
end method do-with-built-project;


define macro with-project-database
  { with-project-database (?frame:name, ?options:*)
      ?body:body
    end }
    => { do-with-project-database(method (?frame) ?body end, ?frame, ?options) }
end macro with-project-database;

define method do-with-project-database
    (function :: <function>, frame :: <environment-frame>,
     #key force?, link?)
 => ()
  let project = frame.ensure-frame-project;
  let database = project-compiler-database(project);
  case
    database =>
      function(frame);
    project.project-can-be-built?
      & (force?
	   | environment-question
	       (format-to-string
		  (if (~link? | file-exists?(project.project-full-build-filename))
		     $no-database-message
		   else
		     $project-not-built-message
		   end,
		   frame-default-object-name(frame, project)),
		owner: frame,
		style: #"warning",
		exit-style: #"ok-cancel")) =>
      frame-do-build-project(frame,
			     parse?: #f, compile?: #t, link?: link?,
			     clean?: #f,
			     finish-function: function);
  end
end method do-with-project-database;


define method frame-ensure-project-database
    (frame :: <environment-frame>, #key force?)
 => (exists? :: <boolean>)
  //---*** This doesn't really work, because do-with-project-database
  //---*** does all of its work in another thread...
  do-with-project-database(always(#f), frame, force?: force?);
  project-compiler-database(frame.ensure-frame-project) ~= #f
end method frame-ensure-project-database;


define method frame-advanced-build-dialog
    (frame :: <environment-frame>) => ()
  let project = frame.ensure-frame-project;
  let project-name = environment-object-primitive-name(project, project);
  with-compiler-progress
      ((progress-window, note-compiler-progress) = frame,
       title: format-to-string("Building %s", project-name),
       lightweight?: #f)
    ignore(progress-window, note-compiler-progress);
    #f
  end
end method frame-advanced-build-dialog;
  

define method frame-parse-project
    (frame :: <environment-frame>, #key process-subprojects? = #t) => ()
  frame-do-build-project(frame,
			 parse?: #t, compile?: #f, link?: #f,
			 clean?: #f,
			 process-subprojects?: process-subprojects?)
end method frame-parse-project;

define method frame-compile-project
    (frame :: <environment-frame>, #key process-subprojects? = #t) => ()
  frame-do-build-project(frame,
			 parse?: #f, compile?: #t, link?: #f,
			 clean?: #f,
			 process-subprojects?: process-subprojects?)
end method frame-compile-project;

define method frame-clean-compile-project
    (frame :: <environment-frame>, #key process-subprojects? = #t) => ()
  frame-do-build-project(frame,
			 parse?: #f, compile?: #t, link?: #f,
			 clean?: #t,
			 process-subprojects?: process-subprojects?)
end method frame-clean-compile-project;

define method frame-link-project
    (frame :: <environment-frame>, #key process-subprojects? = #t) => ()
  frame-do-build-project(frame,
			 parse?: #f, compile?: #f, link?: #t,
			 clean?: #f,
			 process-subprojects?: process-subprojects?)
end method frame-link-project;

define method frame-build-release
    (frame :: <environment-frame>) => ()
  let project = frame.ensure-frame-project;
  let compile? = ~project-compiled?(project);
  frame-do-build-project(frame,
			 parse?: #f, compile?: compile?, link?: #t,
			 clean?: #f, release?: #t,
			 process-subprojects?: #t)
end method frame-build-release;

define method frame-build-project
    (frame :: <environment-frame>, #key process-subprojects? = #t) => ()
  let project = frame.ensure-frame-project;
  let compile? = ~project-compiled?(project);
  frame-do-build-project(frame,
			 parse?: #f, compile?: compile?, link?: #t,
			 clean?: #f,
			 process-subprojects?: process-subprojects?)
end method frame-build-project;

define method frame-clean-build-project
    (frame :: <environment-frame>, #key process-subprojects? = #t) => ()
  let project = frame.ensure-frame-project;
  let compile? = ~project-compiled?(project);
  frame-do-build-project(frame,
			 parse?: #f, compile?: compile?, link?: #t,
			 clean?: #t,
			 process-subprojects?: process-subprojects?)
end method frame-clean-build-project;

define method frame-confirm-remove-build-products
    (frame :: <environment-frame>, project :: <project-object>)
 => (confirm? :: <boolean>)
  environment-question
    (format-to-string
       ("Removing the build products for '%s' also removes all browsing information.\n"
        "Are you sure?",
	frame-default-object-name(frame, project)),
     owner: frame,
     style: #"warning",
     exit-style: #"ok-cancel")
end method frame-confirm-remove-build-products;

define method frame-remove-build-products
    (frame :: <environment-frame>, #key process-subprojects? = #t) => ()
  let project = frame.ensure-frame-project;
  if (frame-confirm-remove-build-products(frame, project))
    let project-window = find-project-browser-showing-project(project);
    if (project-window)
      call-in-frame
	(project-window,
	 method ()
	   frame-remove-build-products
	     (project-window, process-subprojects?: process-subprojects?)
	 end)
    else
      remove-project-build-products
	(project, process-subprojects?: process-subprojects?);
    end
  end
end method frame-remove-build-products;


define method frame-do-build-project
    (frame :: <environment-frame>,
     #key parse?, compile?, link?, clean? = #f, release? = #f,
	  save-databases? = $unsupplied, copy-sources? = $unsupplied,
	  process-subprojects? = #t,
	  start? = #f, startup-option = #"start",
          finish-function = #f,
          own-thread? = #t)
 => ()
  let project = frame.ensure-frame-project;
  when (unsupplied?(save-databases?))
    save-databases? := environment-default-save-databases()
  end;
  when (unsupplied?(copy-sources?))
    copy-sources? := environment-default-copy-sources()
  end;
  let timeout = $project-compilation-timeout;
  let project-name = environment-object-primitive-name(project, project);
  unless (link? & frame-warn-if-application-tethered(frame, project))
    let link-mode = environment-default-link-mode();
    let upgrade?  = environment-default-upgrade-warnings();
    let built? = #f;
    with-compiler-progress 
	((progress-window, note-compiler-progress) = frame,
	 title: format-to-string("Building %s", project-name),
	 lightweight?: #t,
	 own-thread?: own-thread?)
      with-compiler-locked (progress-window, timeout: timeout)
	compilation-commands-enabled?(frame) := #f;
	built?
	  := frame-do-build-project-1
	       (frame, project,
		parse?:   parse?,
		compile?: compile?,
		link?:    link?,
		release?: release?,
		clean?:   clean?,
		process-subprojects?: process-subprojects?,
		save-databases?:      save-databases?,
		copy-sources?:        copy-sources?,
		link-mode:            link-mode,
		upgrade-warnings?:    upgrade?,
		start?:                 start?,
		startup-option:         startup-option,
		finish-function:        finish-function,
		note-compiler-progress: note-compiler-progress,
		progress-window:        progress-window);
      end;
    cleanup
      compilation-commands-enabled?(frame) := #t;
      let (message, warnings?)
	= compilation-status
	    (frame, 
	     parse?: parse?, compile?: compile?, link?: link?,
	     aborted?: ~built?);
      frame-status-message(frame) := message;
    end
  end
end method frame-do-build-project;

define constant $build-related-commands
  = vector(// Project menu
           frame-insert-source-file,
           frame-remove-selected-sources,
           frame-move-source-file-prev,
           frame-move-source-file-next,
	   frame-edit-project-settings,
	   frame-build-project,
	   frame-clean-build-project,
	   frame-advanced-build-dialog,
	   frame-remove-build-products,
	   frame-build-release,

	   // Application menu
	   frame-start-application,
	   frame-attach-application,
	   frame-start-or-resume-application,
	   frame-debug-application,
	   frame-interact,
           frame-create-thread);

define method compilation-commands-enabled?-setter
    (enabled? :: <boolean>, frame :: <environment-frame>)
 => (enabled? :: <boolean>)
  for (command in $build-related-commands)
    command-enabled?(command, frame) := enabled?
  end;
  enabled?
end method compilation-commands-enabled?-setter;

// This must be called inside of 'with-compiler-locked'
define method frame-do-build-project-1
    (frame :: <environment-frame>, project :: <project-object>,
     #key parse?, compile?, link?, clean? = #f, release?,
	  save-databases? = #t, copy-sources? = #f,
	  process-subprojects? = #t,
	  link-mode = #"ask", upgrade-warnings? = #f, 
	  start? = #f, startup-option = #"start",
          finish-function = #f,
          note-compiler-progress :: <function> = ignore,
          progress-window = #f)
 => (built? :: <boolean>)
  let owner :: <frame> = progress-window | frame;
  frame-status-message(frame) := "";
  local method condition-handler
	    (type :: <symbol>, message :: <string>)
	  compiler-condition-handler(frame, type, message)
	end method condition-handler;
  with-frame-project-operation (frame, project)
    let compiler-build? = parse? | compile?;
    // First, see if any files need to be saved.
    when (compiler-build?)
      let build? = editor-save-project-files(project, owner);
      // In case the editor obscured our window, pop back to the top.
      raise-frame(owner);
      unless (build?)
	abort();
      end;
    end;
    when (release?)
      let directory = project.project-release-directory;
      if (~directory-empty?(directory)
	    & environment-question
	        (format-to-string
		   ("Release directory '%s' already exists\n"
		    "Remove old contents first?",
		    directory),
		 owner: owner))
	do-directory
	  (method 
	       (full-directory :: <pathname>, filename :: <string>,
		type :: <file-type>)
	     if (type == #"file")
	       let full-filename
		 = make(<file-locator>,
			directory: directory,
			name:      filename);
	       delete-file(full-filename)
	     end
	   end,
	   directory)
      end
    end;
    //---*** We need to be smarter about the progress, making the link
    //---*** an integral part of the progress.
    let status
      = case
	  parse? =>
	    note-compiler-progress(0, 1, heading-label: "Parsing...", item-label: "");
	    parse-project-source(project,
				 progress-callback:    note-compiler-progress,
				 error-handler:        condition-handler,
				 process-subprojects?: process-subprojects?);
	  compile? =>
	    note-compiler-progress(0, 1, heading-label: "Compiling...", item-label: "");
	    build-project(project,
			  clean?:               clean?,
			  link?:                #f,
			  progress-callback:    note-compiler-progress,
			  error-handler:        condition-handler,
			  save-databases?:      save-databases?,
			  copy-sources?:        copy-sources?,
			  process-subprojects?: process-subprojects?);
	end;
    record-breakpoint-source-locations(project);
    let serious-warnings?
      = block (break)
	  do-compiler-warnings
	    (method (warning)
	       select (warning by instance?)
		 <compiler-error-object>           => break(#t);
		 <serious-compiler-warning-object> => break(#t);
		 <warning-object>                  => upgrade-warnings? & break(#t);
	       end
	     end method, project, project);
	  #f
	end;
    unless(status)
      abort()
    end;
    if (link?)
      if (~compiler-build?
	  | ~serious-warnings?
	  | link-mode == #"always"	//--- obsolete, but it might be in the registry
	  | link-mode == #"force"
	  | ((link-mode == #"warnings"	//--- obsolete, but it might be in the registry
	      | link-mode == #"ask")
	     & environment-question
	         ("There were serious warnings during compilation; link anyway?",
		  owner: owner,
	          style: #"warning")))
	note-compiler-progress(0, 1, heading-label: "Linking...", item-label: "");
	unless (frame-warn-if-application-tethered(frame, project))
	  link-project(project,
		       progress-callback:    note-compiler-progress, 
		       error-handler:        condition-handler,
		       process-subprojects?: process-subprojects?,
		       release?:             release?);
	  let label = if (compile?) "Project built" else "Project linked" end;
	  note-compiler-progress(1, 1, heading-label: label, item-label: "");
	  let filename = project-full-build-filename(project);
	  if (file-exists?(filename))
            finish-function & finish-function(frame);
	    when (start?)
	      frame-do-run-application(frame, project, startup-option: startup-option)
	    end;
	    #t
	  else
	    let message = format-to-string("The target '%s' does not exist after build.",
					   filename);
	    environment-error-message(message, owner: owner)
	  end
	end
      else
	let label
	  = build-progress-message(parse?:   parse?,
				   compile?: compile?,
				   link?:    link?,
				   release?: release?,
				   success?: ~serious-warnings?);
	note-compiler-progress(1, 1, heading-label: label, item-label: "")
      end
    else
      let label
	= build-progress-message(parse?:   parse?,
				 compile?: compile?,
				 link?:    link?,
				 release?: release?,
				 success?: ~serious-warnings?);
      note-compiler-progress(1, 1, heading-label: label, item-label: "");
      finish-function & finish-function(frame);
      ~serious-warnings?
    end
  end
end method frame-do-build-project-1;

define method build-progress-message
    (#key parse?, compile?, link?, release?, success?) => (message :: <string>)
  if (success?)
    case
      compile? & link? & release? => "Project released";
      compile? & link?            => "Project built";
      compile?                    => "Project compiled";
      link?                       => "Project linked";
      parse?                      => "Project parsed";
      otherwise                   => error("Strange build state!");
    end
  else
    case
      compile? & link? & release? => "Project release failed";
      compile? & link?            => "Project build failed";
      compile?                    => "Project compile failed";
      link?                       => "Project link failed";
      parse?                      => "Project parse failed";
      otherwise                   => error("Strange build state!");
    end
  end
end method build-progress-message;

define method frame-do-run-application
    (frame :: <environment-frame>, project :: <project-object>,
     #key startup-option = #"start")
 => ()
  let machine = project.project-debug-machine | environment-host-machine();
  if (environment-active-on-starting?())
    active-project() := project
  end;
  frame-clear-obsolete-breakpoints(frame, project);
  run-application(project, machine: machine, startup-option: startup-option)
end method frame-do-run-application;

define function project-full-debug-pathname
    (project :: <project-object>) => (pathname :: <file-locator>)
  project.project-debug-filename
    | project.project-full-build-filename
end function project-full-debug-pathname;

define method frame-application-tethered?
    (frame :: <frame>, project :: <project-object>)
 => (tethered? :: <boolean>, state :: false-or(<application-state>))
  let application = project-application(project);
  let state = application & application-state(application);
  //---*** hughg, 1998/11/06: It's not clear what "tethered?" should be
  // if the state is #"uninitialized" or #"closed".
  values(select (state)
	   #"running", #"stopped" => #t;
	   otherwise		  => #f;
	 end,
	 state)
end method frame-application-tethered?;

define method frame-warn-if-application-tethered
    (frame :: <frame>, project :: <project-object>,
     #key message)
 => (tethered? :: <boolean>)
  let (tethered?, state) = frame-application-tethered?(frame, project);
  if (tethered?)
    let state-word :: <string>
      = select (state)
	  #"running" => "running";
	  #"stopped" => "paused";
	  otherwise  => "";
	end;
    let message
      = message
          | format-to-string("%s is connected to %s application '%s'.\n\n"
			     "Stop connected application and continue?",
			     release-product-name(),
			     state-word,
			     project-full-debug-pathname(project));
    if (environment-question
	  (message,
	   owner: frame,
	   style: #"warning",
	   exit-style: #"ok-cancel"))
      close-application(project, wait-for-termination?: #t);
      #f
    else
      #t
    end
  else
    #f
  end
end method frame-warn-if-application-tethered;

define function compilation-warning-count-message
    (project :: <project-object>, #key warnings)
 => (message :: false-or(<byte-string>))
  let ordinary = 0;
  let serious  = 0;
  let errors   = 0;
  local method count-warning
	    (warning :: <warning-object>)
	  select (warning by instance?)
	    <compiler-error-object>           => errors   := errors   + 1;
	    <serious-compiler-warning-object> => serious  := serious  + 1;
	    <warning-object>                  => ordinary := ordinary + 1;
	  end
	end;
  if (warnings)
    do(count-warning, warnings)
  else
    do-compiler-warnings(count-warning, project, project)
  end;
  let message
    = if (ordinary + serious + errors ~= 0)
	if (errors = 0)
	  format-to-string("%d %s, %d %s",
			   serious,
			   string-pluralize("serious warning", count: serious),
			   ordinary,
			   string-pluralize("warning",         count: ordinary))
	else
	  format-to-string("%d %s, %d %s, %d %s",
			   errors,
			   string-pluralize("error",           count: errors),
			   serious,
			   string-pluralize("serious warning", count: serious),
			   ordinary,
			   string-pluralize("warning",         count: ordinary))
	end
      end;
  message
end function compilation-warning-count-message;

define constant $build-aborted-string   = "stopped";
define constant $build-completed-string = "completed";

define function compilation-status
    (frame :: <environment-frame>, 
     #key parse?, compile?, link?, aborted?)
 => (message :: <string>, warnings? :: <boolean>)
  let operation
    = case
	parse?           => "Parse";
	compile? & link? => "Build";
	compile?         => "Compile";
	link?            => "Link";
	otherwise        => "Build";
      end;
  let action
    = if (aborted?) $build-aborted-string else $build-completed-string end;
  let project = frame.ensure-frame-project;
  let warning-count-message = compilation-warning-count-message(project);
  let message
    = if (warning-count-message)
	format-to-string("%s %s. %s.",
			 operation, action, warning-count-message)
      else
	format-to-string("%s %s with no warnings.",
			 operation, action)
      end;
  values(message, warning-count-message ~= #f)
end function compilation-status;

define variable $build-bitmap    :: <label-type> = "Build";
define variable $compile-bitmap  :: <label-type> = "Compile";
define variable $load-bitmap     :: <label-type> = "Load";
define variable $run-bitmap      :: <label-type> = "Start/Resume";
define variable $pause-bitmap    :: <label-type> = "Pause";
define variable $profile-bitmap  :: <label-type> = #f;
define variable $stop-bitmap     :: <label-type> = "Stop";
define variable $interact-bitmap :: <label-type> = "Interact";
define variable $debug-bitmap    :: <label-type> = "Debug";

define constant $build-doc
  = "Compiles changed source files and links them into an executable.";
define constant $clean-build-doc
  = "Compiles all source files and links them into an executable.";
define constant $advanced-build-doc
  = "Display the advanced build dialog.";
define constant $build-release-doc
  = "Build a standalone version of the project.";

define constant $attach-doc
  = "Attaches the project to a running process.";
define constant $debug-doc
  = "Finds debugger window, starting or pausing executable as necessary.";
define constant $interact-doc
  = "Finds interactor window, starting or pausing executable as necessary.";

define constant $remove-build-products-doc
  = "Removes all files produced by earlier builds.";
/*---*** andrewa: not used at the moment
define constant $run-doc
  = "Starts or resumes execution of the target application.";
define constant $stop-doc
  = "Pauses execution of the target application.";
*/


define constant $build-title       = "Build";
// define constant $clean-build-title = "Clean Build";
define constant $interact-title    = "Interact";
define constant $debug-title       = "Debug";
define constant $run-title         = "Start/Resume";
define constant $pause-title       = "Pause";
define constant $stop-title        = "Stop";


define command-table *build-command-table* (*global-command-table*)
  menu-item "Build"                 = frame-build-project,
    accelerator:   make-keyboard-gesture(#"f7"),
    documentation: $build-doc;
  menu-item "Clean Build"           = frame-clean-build-project,
    accelerator:   make-keyboard-gesture(#"f7", #"control"),
    documentation: $clean-build-doc;
  menu-item "Advanced Build..."     = frame-advanced-build-dialog,
    accelerator:   make-keyboard-gesture(#"f7", #"shift"),
    documentation: $advanced-build-doc;
  menu-item "Remove Build Products" = frame-remove-build-products,
    documentation: $remove-build-products-doc;
end command-table *build-command-table*;

define command-table *build-release-command-table* (*global-command-table*)
  menu-item "Build Release"                 = frame-build-release,
    documentation: $build-release-doc;
end command-table *build-release-command-table*;


/// Updating after a build

define open generic frame-note-project-rebuilt
    (frame :: <frame>) => ();

define open generic frame-note-project-warnings-updated
    (frame :: <frame>) => ();

define method frame-note-project-rebuilt
    (frame :: <frame>) => ()
  #f
end method frame-note-project-rebuilt;

define method frame-note-project-warnings-updated
    (frame :: <frame>) => ()
  #f
end method frame-note-project-warnings-updated;

tune-in($project-channel,
	method (message :: <project-message>)
	  let project = message.message-project;
	  do-project-frames(method (frame)
			      call-in-frame(frame, frame-note-project-rebuilt, frame)
			    end,
			    project)
	end,
	message-type: <project-database-updated-message>);

tune-in($project-channel,
	method (message :: <project-message>)
	  let project = message.message-project;
	  do-project-frames
	    (method (frame)
	       do-project-frames(frame-note-project-warnings-updated, project)
	     end,
	     project)
	end,
	message-type: <project-warnings-updated-message>);


/// Support for build-related Editor commands

define table $compilation-functions = 
  { #"project-parse"         => frame-parse-project,
    #"project-compile"       => frame-compile-project,
    #"project-clean-compile" => frame-clean-compile-project,
    #"project-link"          => frame-link-project,
    #"project-build"         => frame-build-project,
    #"project-clean-build"   => frame-clean-build-project };

define sideways method compile-project-location
    (project :: <project-object>, location :: <source-location>, 
     scope :: <symbol>)
 => ()
  ignore(location); // ---*** maybe remove location altogether?
  with-environment-frame (project-browser = default-port(),
                          <project-browser>,
                          project: project)
    let compile-function = element($compilation-functions, scope, default: #f);
    if (compile-function)
      compile-function(project-browser);
    else
      environment-error-message
	(format-to-string("Unknown compilation scope '%s'.",
			  as(<string>, scope)),
	 owner: project-browser);
    end;
  end;
end method compile-project-location;


/// Project command table

define open generic frame-insert-source-file (frame :: <environment-frame>) => ();
define open generic frame-edit-project-settings
    (frame :: <environment-frame>, #key project) => ();

define method frame-insert-source-file
    (frame :: <environment-frame>) => ()
  do-frame-insert-source-file(frame)
end method frame-insert-source-file;

define method do-frame-insert-source-file
    (frame :: <environment-frame>, 
     #key filename :: false-or(<file-locator>),
          after,
          project = frame.ensure-frame-project)
 => ()
  let project-window = find-project-browser-showing-project(project);
  if (project-window)
    call-in-frame
      (project-window,
       method ()
	 environment-property-pane-page(project-window.tab-layout) := #"sources";
	 do-frame-insert-source-file
	   (project-window, filename: filename, after: after, project: project)
       end)
  else
    environment-error-message
      (format-to-string("Project %s not open, so cannot insert a file into it.",
			environment-object-primitive-name(project, project)),
       owner: frame)
  end
end method do-frame-insert-source-file;


/// Project command tables

define command-table *project-reorder-files-command-table* (*global-command-table*)
  menu-item "Move File &Up"    = frame-move-source-file-prev,
    accelerator:   make-keyboard-gesture(#"up", #"alt"),
    documentation: "Moves a file up in the list of project sources.";
  menu-item "Move File &Down"  = frame-move-source-file-next,
    accelerator:   make-keyboard-gesture(#"down", #"alt"),
    documentation: "Moves a file down in the list of project sources.";
end command-table *project-reorder-files-command-table*;

define command-table *project-edit-command-table* (*global-command-table*)
  menu-item "Insert File..."  = frame-insert-source-file,
    documentation: "Inserts a file into the project.";
  menu-item "Remove File"     = frame-remove-selected-sources,
    documentation: "Removes the selected source file from the project.";
end command-table *project-edit-command-table*;

define command-table *project-settings-command-table* (*global-command-table*)
  menu-item "Settings..." = frame-edit-project-settings,
    documentation: "Enables you to change the project settings.";
end command-table *project-settings-command-table*;

define command-table *project-command-table* (*global-command-table*)
  include *build-command-table*;
  include *build-release-command-table*;
  include *project-settings-command-table*;
end command-table *project-command-table*;


/// Export commands

define open generic frame-export-data
    (frame :: <environment-frame>) => ();

define method frame-export-data
    (frame :: <environment-frame>) => ()
  let (class, format, location)
    = choose-report(owner: frame);
  if (class)
    let project = frame.ensure-frame-project;
    let report
      = make(class,
	     project: project,
	     format: format);
    select (location)
      #"clipboard" =>
	with-frame-background-operation (frame, "Exporting report...")
	  let string = create-report-to-string(report);
	  with-clipboard (clipboard = top-level-sheet(frame))
	    add-clipboard-data(clipboard, string)
	  end;
	  call-in-frame(frame,
			method ()
			  frame-status-message(frame)
			    := "Report copied to clipboard"
			end)
	end;
      #"file" =>
        let file
	  = environment-choose-file
	      (title: "New Report",
	       owner: frame,
	       direction: #"output");
	if (file)
	  with-frame-background-operation (frame, "Exporting report...")
	    with-open-file (stream = file, direction: #"output")
	      write-report(stream, report)
	    end;
	    call-in-frame(frame, frame-open-report, frame, file)
	  end
	end;
      #"directory" =>
	let directory
	  = choose-directory(owner: frame,
			     title: "Report Directory");
	if (directory)
          with-frame-background-operation (frame, "Exporting report...")
	    let directory = as(<directory-locator>, directory);
	    let file = create-multi-file-report(report, directory);
	    if (file-exists?(file))
	      call-in-frame(frame, frame-open-report, frame, file)
	    end
	  end
	end;
    end
  end
end method frame-export-data;

define method frame-open-report
    (frame :: <environment-frame>, locator :: <file-locator>) => ()
  case
    frame-open-by-default?(frame, locator) =>
      frame-open-object(frame, locator);
    locator-has-source?(locator) =>
      frame-edit-object(frame, locator);
    otherwise =>
      #f;
  end
end method frame-open-report;

define frame <report-dialog> (<dialog-frame>)
  slot report-info :: <report-info> = default-report(),
    init-keyword: info:;
  pane report-class-pane (frame)
    make(<list-box>,
	 items: sorted-available-reports(),
	 value: frame.report-info,
	 label-key: report-info-title,
	 lines: 10,
	 scroll-bars: #"vertical",
	 value-changed-callback:
	   method (gadget)
	     let info = gadget-value(gadget);
	     frame.report-info := info;
	     gadget-items(frame.report-format-pane)
	       := info.report-info-formats;
	     gadget-items(frame.report-location-pane)
	       := info.report-info-locations;
	   end);
  pane report-format-pane (frame)
    make(<option-box>,
	 items: frame.report-info.report-info-formats,
	 label-key: method (format)
		      report-info-format-name(frame.report-info, format)
		    end);
  pane report-location-pane (frame)
    make(<option-box>,
	 items: frame.report-info.report-info-locations,
	 label-key: method (location)
		      select (location)
			#"clipboard" => "Clipboard";
			#"file"      => "File";
			#"directory" => "Directory";
		      end
		    end);
  layout (frame)
    grouping ("Export information:")
      vertically (spacing: 2)
        make(<label>, label: "Report:");
        frame.report-class-pane;
	make(<table-layout>,
	     columns: 2,
	     spacing: 4,
	     x-alignment: #[#"right", #"left"],
	     children: vector(make(<label>, label: "Format:"),
			      frame.report-format-pane,
			      make(<label>, label: "Location:"),
			      frame.report-location-pane))
      end
    end grouping;
  keyword title: = "Export...";
  keyword width: = 300;
end frame <report-dialog>;

define function sorted-available-reports
    () => (reports :: <sequence>)
  sort(as(<vector>, available-reports()),
       test: method (info1 :: <report-info>, info2 :: <report-info>)
	       let title1 = info1.report-info-title;
	       let title2 = info2.report-info-title;
	       title1 < title2
	     end)
end function sorted-available-reports;

define function default-report
    () => (report :: <report-info>)
  sorted-available-reports()[0]
end function default-report;

define function report-info-locations
    (info :: <report-info>) => (locations :: <sequence>)
  if (info.report-info-multi-file?)
    #[#"clipboard", #"file", #"directory"]
  else
    #[#"clipboard", #"file"]
  end
end function report-info-locations;

define function choose-report
    (#key owner :: <frame>)
 => (class :: false-or(subclass(<report>)),
     format :: false-or(<report-format>),
     location :: false-or(<symbol>))
  let dialog = make(<report-dialog>, owner: owner);
  if (start-dialog(dialog))
    values(dialog.report-class-pane.gadget-value.report-info-class,
	   dialog.report-format-pane.gadget-value,
	   dialog.report-location-pane.gadget-value)
  end
end function choose-report;

define command-table *export-command-table* (*global-command-table*)
  menu-item "Export..." = frame-export-data,
    documentation: "Exports data to make it available to other applications.";
end command-table *export-command-table*;


/// Saving Project Databases

define frame <save-databases-dialog> (<dialog-frame>)
  sealed slot %save-databases-dialog-projects :: <sequence>,
    required-init-keyword: projects:;
  pane %progress-item-label (frame)
    make(<label>, min-width: 1, max-width: $fill);
  pane %progress-bar (frame)
    make(<progress-bar>, min-width: 1, max-width: $fill);
  layout (frame)
    vertically (spacing: 8)
      frame.%progress-item-label;
      frame.%progress-bar;
    end;
  keyword title: = "Saving...";
  keyword width: = 300;
  keyword exit-callback:   = #f;
  keyword cancel-callback: = #f;
end frame <save-databases-dialog>;

define method handle-event
    (dialog :: <save-databases-dialog>, event :: <frame-mapped-event>)
 => ()
  let projects = dialog.%save-databases-dialog-projects;
  let count = size(projects);
  with-busy-cursor (dialog)
    dialog.%progress-bar.gadget-value-range := range(from: 0, to: count);
    for (i from 0 below count)
      let project = projects[i];
      dialog.%progress-bar.gadget-value := i;
      dialog.%progress-item-label.gadget-label
        := concatenate("Database: ",
                       environment-object-display-name(project, project, #f));
      save-project-database(project)
    end;
    dialog.%progress-bar.gadget-value := count;
  end;
  exit-dialog(dialog);
end method handle-event;

define function save-all-project-databases (#key owner :: <frame>) => ()
  let dialog = make(<save-databases-dialog>,
                    owner: owner,
                    projects: open-projects());
  start-dialog(dialog);
end function save-all-project-databases;


/// Go menu browse commands

define variable $home-bitmap :: <label-type> = "Home";

// define constant $home-doc = "Browses the project's library.";

define open generic frame-browse-project-library (frame :: <frame>) => ();

define method frame-browse-project-library
    (frame :: <environment-frame>) => ()
  with-project-database (frame)
    let project = frame.ensure-frame-project;
    let library = project.project-library;
    frame-browse-object(frame, library);
  end;
end method frame-browse-project-library;

define variable $browse-object-dialog-width :: <integer> = 250;

define frame <browse-object-dialog> (<dialog-frame>)
  constant slot %frame :: <environment-frame>,
    required-init-keyword: environment-frame:;
  pane object-name-pane (dialog)
    make(<text-field>,
	 documentation: "The name of an object to browse.",
	 value-changing-callback:
	   method (gadget)
	     let dialog = sheet-frame(gadget);
	     let object = find-named-object(dialog.%frame, gadget-value(gadget) | "");
	     dialog-exit-enabled?(dialog) := object & #t;
	   end method,
	 activate-callback: exit-dialog);
  layout (dialog)
    horizontally (spacing: 4)
      make(<label>, label: "Object:");
      dialog.object-name-pane;
    end;
  input-focus (dialog)
    dialog.object-name-pane;
  keyword title: = "Object...";
end frame <browse-object-dialog>;

define method frame-browse-named-object
    (frame :: <environment-frame>) => ()
  with-project-database (frame)
    let project = frame.ensure-frame-project;
    let dialog = make(<browse-object-dialog>,
		      owner: frame,
		      width: max($browse-object-dialog-width, 250),
		      environment-frame: frame);
    dialog-exit-enabled?(dialog) := #f;
    when (start-dialog(dialog))
      let (width, height) = frame-size(dialog);
      $browse-object-dialog-width := width;
      let name   = dialog.object-name-pane.gadget-value;
      let object = find-named-object(frame, name);
      object & frame-browse-object(frame, object)
    end
  end;
end method frame-browse-named-object;

// Label for when no project database is available for browsing libraries and modules
define constant $database-unavailable = "(No Compiler Database)";

define command-table *browse-libraries-command-table* (*global-command-table*)
end command-table *browse-libraries-command-table*;

add-command-table-menu-item
  (*browse-libraries-command-table*, "", <push-box>, #f,
   documentation: "Browses this library.",
   update-callback:
     method (menu-box :: <menu-box>)
       let frame     = sheet-frame(menu-box);
       let project   = frame-current-project(frame);
       let library   = project & project-library(project);
       let libraries = library & vector(library);
       //--- cpage: 1998.06.24 Currently, this just shows the library defined
       //           by the project. Conceivably, we may allow projects to
       //           define more than one. Alternatively, we may want to make
       //           this display used libraries.
       let items
	 = libraries
	     & map(method (library :: <library-object>) => (item :: <vector>)
		     let name
		       = /*frame-default-object-name(frame, library)
		           |*/ environment-object-display-name
		               (project, library, #f, qualify-names?: #f);
		     vector(name, library)
		   end method,
		   libraries);
       if (items)
	 gadget-items(menu-box) := items;
       else
	 // Insert a single place-holder item and disable it
	 gadget-items(menu-box) := vector(vector($database-unavailable, #f));
//---*** cpage: 1998.08.19 Actually, it seems we can't disable the sheet at
//              this time (or this is the incorrect way to do it).
//	 let button = sheet-children(menu-box)[0];
//	 button.gadget-enabled? := #f;
       end if;
     end method,
   label-key: first,
   value-key: second,
   callback: method (menu-box :: <menu-box>)
	       let frame  = sheet-frame(menu-box);
	       let object = gadget-value(menu-box);
	       object & frame-browse-object(frame, object)
             end);

define command-table *browse-modules-command-table* (*global-command-table*)
end command-table *browse-modules-command-table*;

add-command-table-menu-item
  (*browse-modules-command-table*, "", <push-box>, #f,
   documentation: "Browses this module.",
   update-callback:
     method (menu-box :: <menu-box>)
       let frame   = sheet-frame(menu-box);
       let project = frame-current-project(frame);
       let library = project & project-library(project);
       let modules = library & library-modules(project, library, imported?: #f);
       let items
	 = modules
	     & map(method (module :: <module-object>) => (item :: <vector>)
		     let home-library = environment-object-library(project, module);
		     let name = //if (library == home-library)
		                //  frame-default-object-name(frame, module)
		                //else
				  environment-object-display-name
				    (project, module, #f, qualify-names?: #f)
		       /*end*/;
		     vector(name, module)
		   end method,
		   modules);
       if (items)
	 gadget-items(menu-box) := items;
       else
	 // Insert a single place-holder item and disable it
	 gadget-items(menu-box) := vector(vector($database-unavailable, #f));
//---*** cpage: 1998.08.19 Actually, it seems we can't disable the sheet at
//              this time (or this is the incorrect way to do it).
//	 let button = sheet-children(menu-box)[0];
//	 button.gadget-enabled? := #f;
       end if;
     end method,
   label-key: first,
   value-key: second,
   callback: method (menu-box :: <menu-box>)
	       let frame  = sheet-frame(menu-box);
	       let object = gadget-value(menu-box);
	       object & frame-browse-object(frame, object)
             end);

define command-table *browse-locations-command-table* (*global-command-table*)
  menu-item "Library"     = *browse-libraries-command-table*;
  menu-item "Module"      = *browse-modules-command-table*;
  menu-item "Threads"     = frame-browse-threads,
    documentation: "Opens browser window showing current application threads.";
  menu-item "Breakpoints" = frame-browse-all-breakpoints,
    documentation: "Shows all current breakpoints in project window.";
  menu-item "Object..."   = frame-browse-named-object,
    documentation: "Opens browser window showing a named object.";
  separator;
  menu-item "Profiler"    = frame-find-profiler,
    documentation: "Opens the profiler window.";
end command-table *browse-locations-command-table*;


/// Project Browser command table

define constant $recent-projects-menu-items = 4;
define constant $recent-files-menu-items   = 5;

define constant $maximum-pathname-length = 30;

//---*** andrewa: this is rather Windows specific...
define function abbreviate-pathname-in-menu
    (locator :: <file-locator>) => (short-pathname :: <string>)
  let pathname = as(<string>, locator);
  let directory = locator.locator-directory;
  let directory-elements = directory.locator-path;
  let directory-element-count = size(directory-elements);
  if (pathname.size > $maximum-pathname-length & directory-element-count > 2)
    let server = directory.locator-server;
    let name   = locator.locator-name;
    let first-directory = directory-elements[0];
    let last-directory  = directory-elements[directory-element-count - 1];
    with-output-to-string (stream)
      when (server) format(stream, "%s", as(<string>, server)) end;
      format(stream, "\\%s\\...\\%s", first-directory, last-directory);
      when (name)   format(stream, "\\%s",   name) end;
    end
  else
    pathname
  end if
end function abbreviate-pathname-in-menu;

// Generate menu items for a set of file pathnames, numbering them from start:.
define function menu-items-from-pathnames
    (pathnames :: <sequence>, #key start :: <integer> = 1)
 => (items :: <vector>)
  let names = map(locator-name, pathnames);
  let items = make(<vector>, size: size(pathnames));
  for (pathname :: <file-locator> in pathnames,
       i :: <integer> from 0,
       n :: <integer> from start)
    // If there are any other items with the same name, attempt to
    // disambiguate the name by displaying more of the pathname.
    let file-name = pathname.locator-name;
    let label
      = format-to-string
          ("&%d %s",
	   n,
	   if (find-key(names, curry(\=, file-name), skip: 1) ~= #f)
	     abbreviate-pathname-in-menu(pathname)
	   else
	     file-name
	   end);
    items[i] := vector(label, pathname);
  end;
  items
end function menu-items-from-pathnames;

define command-table *recent-projects-command-table* (*global-command-table*)
end command-table *recent-projects-command-table*;

add-command-table-menu-item
  (*recent-projects-command-table*, "", <push-box>, #f,
   documentation: "Opens this project.",
   update-callback: method (menu-box :: <menu-box>)
		      let projects
			= most-recent-projects(n: $recent-projects-menu-items);
		      gadget-items(menu-box) := menu-items-from-pathnames(projects)
		    end,
   label-key: first,
   value-key: second,
   callback: method (menu-box :: <menu-box>)
	       let frame = sheet-frame(menu-box);
	       let project-filename = gadget-value(menu-box);
	       frame-open-project(frame, project-filename)
             end);

define command-table *recent-files-command-table* (*global-command-table*)
end command-table *recent-files-command-table*;

add-command-table-menu-item
  (*recent-files-command-table*, "", <push-box>, #f,
   documentation: "Opens this file.",
   update-callback: method (menu-box :: <menu-box>)
		      let files
			= most-recent-files(n: $recent-files-menu-items);
		      gadget-items(menu-box)
			:= menu-items-from-pathnames
			     (files, start: $recent-projects-menu-items + 1)
		    end method,
   label-key: first,
   value-key: second,
   callback: method (menu-box :: <menu-box>)
	       let frame = sheet-frame(menu-box);
	       let file-filename = gadget-value(menu-box);
	       frame-open-file(frame, filename: file-filename)
             end);
