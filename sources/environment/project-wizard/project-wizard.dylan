Module:    environment-project-wizard
Author:    Hugh Greene
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// ---*** ISSUES
// - Win32-specific code will have to be factored out if/when we port.
// - We could probably really do with an "Add thing to project ..." Wizard,
//   which could cover adding CORBA clients, new modules, new DUIM frame/
//   sheet/gadget classes, new collection classes, etc.  The set of things
//   which could be added could be dynamically extensible.
//   (After writing the next note, I realised that each "thing" might add
//   different constraints, and some might be incompatible, so you'd have
//   to check when adding a new "thing".  You might also want to warn the
//   user if they later change project settings and break these constraints.)
// - This Wizard is really doing about five different things, which could
//   probably do with being split up or restructured.
//     1) Choose a name and location for the project.
//     2) Choose (some of) the project settings.
//     3) Choose arbitrary libraries and modules (from the repository).
//     4) "Mix in" *one* kind of "project type" functionality, which may
//        require additional libraries and place constraints on some
//        settings.  (E.g., OLE Control, OLE Type Library Interface, etc.)
//     5) Choose template code to include, based on all the above.
//  - Please, someone, come up with a unified convention for slot/pane names
//    in this file and project-description.dylan!

/// ---*** TO DO
// - Finish off the <multi-line-text-pane> class and friends, so that I
//   can have nicer help text layout.
// - Test template projects, and un-comment-out the UI for them.  (They
//   are implemented but completely untested and I'm out of time.)
// - Use environment{-,-warning,-error}-message instead of notify-user ...
//   except they're defined in environment-tools, which we don't want to
//   have to "use" here.


/// ----------------------------------------------------------------------
/// MISCELLANEOUS DEFINITIONS

define constant <target-type> :: <type> = one-of(#"exe", #"dll");


/// ----------------------------------------------------------------------
/// PROJECT-WIZARD IMAGE HANDLING

define variable $wizard-image :: false-or(<image>) = #f;
define variable $wizard-frame-icon :: false-or(<image>) = #f;
define variable *wizard-images-initialized?* = #f;

define function initialize-wizard-images ()
  unless (*wizard-images-initialized?*)
    let _id = as(<byte-string>, "WIZARD");
    //---*** Hack the size for now, since DUIM doesn't calculate it correctly.
    let _bitmap
      = read-image-as(<win32-bitmap>, _id, #"bitmap", width: 165, height: 250);
    when (_bitmap)
      $wizard-image := _bitmap;
    end;
    let _id = as(<byte-string>, "AAC"); // Project window icon.
    let _image = read-image-as(<win32-icon>, _id, #"small-icon");
    when (_image)
      $wizard-frame-icon := _image;
    end;
    *wizard-images-initialized?* := #t
  end;
end function;


/// ----------------------------------------------------------------------
/// PROJECT-WIZARD PERSISTENT OPTIONS

define settings <project-wizard-settings> (<open-dylan-user-settings>)
  key-name "Wizard";
  slot default-location :: <string> = "";
  slot default-start-function :: <string> = "main";
  slot default-author :: <string>
    = "The name(s) of the author(s) of the project.";
  slot default-copyright :: <string>
    = "(C) YYYY, YOUR-COMPANY.  All rights reserved.";
  slot default-version :: <string>
    = "A string used by your version control system.";
  slot default-compilation-mode :: <symbol> = #"loose";
  slot use-templates :: <boolean> = #f;
  slot show-synopsis :: <boolean> = #t;
  slot show-author :: <boolean> = #t;
  slot show-copyright :: <boolean> = #t;
  slot show-version :: <boolean> = #f;
end settings <project-wizard-settings>;

define constant $project-wizard-settings = make(<project-wizard-settings>);

define function project-wizard-default-location
    () => (location :: <string>)
  let location = $project-wizard-settings.default-location;
  if (location.empty?)
    as(<string>,
       element(user-projects-path() | #(), 0, default: working-directory()))
  else
    location
  end
end function project-wizard-default-location;


/// ----------------------------------------------------------------------
/// PROJECT-WIZARD STRINGS

/*
define table $project-wizard-strings
  = { #"custom-project-description" =>
	#["A custom project is one created from scratch.",
	  "You must provide a name and location for the project and you can",
	  "change the default version information.",
	  "You can choose which other libraries and modules to use in your project",
	  "You can also provide information for the project and source file headers."],
      #"template-project-description" =>
	#["A template project is one copied from another project.",
	  "You must provide a new name and location for the copy and you can",
	  "change the default version information."],
      #"wizard-intro" =>
	vector(pair("This Wizard helps you to create a new %s project.",
		    method () vector(release-product-name()) end)),
      #"chooser-description" =>
	vector("This page lets you choose libraries your project will use,",
	       pair("from groups of libraries provided with %s,",
		    method () vector(release-product-name()) end),
	       "and which modules to use from those libraries.",
	       "Double-click on an item in one of the lists above to change",
	       "whether it is used.  Use the check box below each list",
	       "to change multiple selected items at once."),
      #"headers-page-description" =>
	#["This page lets you include some standard information",
	  "in the project and source files this Wizard will create.",
	  "To omit any of these keywords, leave their values blank."]
    };
*/

define table $project-type-descriptions
  = { #"custom" =>
	"A custom project is one created from scratch."
	"  You must provide a name and location for the project and you can"
	"change the default version information."
	"  You can choose which other libraries and modules to use in your project"
	"  You can also provide information for the project and source file headers.",
      #"template" =>
	"A template project is one copied from another project."
	"  You must provide a new name and location for the copy and you can"
	"  change the default version information."
    };

define table $strings-mentioning-environment-name
  = { #"wizard-intro" =>
		"This Wizard helps you to create a new %s project.",
      #"chooser-help" =>
		"This page lets you choose libraries your project will use,"
		" from groups of libraries provided with %s, and which"
		" modules to use from those libraries."
		"  Double-click on an item in one of the lists to change"
		" whether it is used.  You can select multiple items and"
		" then use the buttons below each list to change several"
		" at once."
    };

define variable *wizard-strings-initialized?* = #f;

define function initialize-wizard-strings () => ()
  unless (*wizard-strings-initialized?*)
    for (string keyed-by key in $strings-mentioning-environment-name)
      $strings-mentioning-environment-name[key]
        := format-to-string($strings-mentioning-environment-name[key],
			    release-product-name());
    end;
/* For multi-line text:
    for (value keyed-by key in $project-wizard-strings)
      for (i from 0 to size(value))
	when (instance?(entry[i], <pair>))
	  let args-generator = tail(maybe-pair);
	  entry[i]
	    := apply(format-to-string(head(maybe-pair), args-generator()))
	end;
      end;
    end;
*/
    *wizard-strings-initialized?* := #t;
  end;
end function;


/// ----------------------------------------------------------------------
/// PROJECT-WIZARD SUB-DIALOGS

define function make-require-value-callback (value) => (callback :: <function>)
  method (gadget :: <sheet>)
    let val = gadget-value(gadget);
    // Require that there be a value and, if it's a string, it's non-empty.
    unless (val & (~instance?(val, <string>) | ~empty?(val)))
      beep(gadget);
      gadget-value(gadget) := value;
    end;
  end
end function make-require-value-callback;

/// Advanced Project Settings

define sealed frame <advanced-settings-frame> (<dialog-frame>)
  // The name fields mustn't be empty.  We reset them to the initial name.
  pane project-library-name-pane (frame)
    make(<text-field>);
  pane project-module-name-pane (frame)
    make(<text-field>);
  // The start function mustn't be empty.  We reset it to the initial name.
  pane project-start-function-name-pane (frame)
    make(<text-field>);
  // The version fields mustn't be empty.  We reset them to the initial vals.
  pane project-major-version-pane (frame)
    make(<text-field>, type: <integer>, max-width: 8);
  pane project-minor-version-pane (frame)
    make(<text-field>, type: <integer>, max-width: 8);
  pane project-compilation-mode-pane (frame)
    make(<radio-box>,
         enabled?: ~(frame.project-compilation-mode-required?),
         items: #[#["&Interactive development mode", #"loose"],
                  #["&Production mode",              #"tight"]],
         value: #"loose",
         label-key: first, value-key: second);
  slot project-compilation-mode-required? :: <boolean>,
    required-init-keyword: compilation-mode-required?:;
  pane project-win32-subsystem-pane (frame)
    make(<option-box>,
         enabled?: ~(frame.project-win32-subsystem-required?) & 
		   frame.project-target-type ~= #"dll",
         items: #[#["Windows GUI", #"gui"], #["Windows Console", #"console"]],
         label-key: first, value-key: second);
  slot project-win32-subsystem-required? :: <boolean>,
    required-init-keyword: win32-subsystem-required?:;
  constant slot project-target-type :: <symbol>,
    required-init-keyword: target-type:;
  layout (frame)
    vertically (y-spacing: 4)
      grouping ("Library and module names", max-width: $fill)
	make(<table-layout>,
	     y-spacing: 4,
	     contents:
	       vector(vector(make(<label>, label: "&Library name:"),
			     frame.project-library-name-pane),
		      vector(make(<label>, label: "&Module name:"),
			     frame.project-module-name-pane)))
      end grouping;
      grouping ("Start function", max-width: $fill)
	make(<table-layout>,
	     y-spacing: 4,
	     contents:
	       vector(vector(make(<label>, label: "&Function:"),
			     frame.project-start-function-name-pane)))
      end grouping;
      grouping ("Version information", max-width: $fill)
	make(<table-layout>,
	     y-spacing: 4,
	     contents:
	       vector(vector(make(<label>, label: "Ma&jor (interface version):"),
			     frame.project-major-version-pane),
		      vector(make(<label>, label: "Mi&nor (implementation version):"),
			     frame.project-minor-version-pane)))
      end grouping;
      grouping ("Compilation mode", max-width: $fill)
	frame.project-compilation-mode-pane
      end grouping;
      grouping ("Windows subsystem", max-width: $fill)
        horizontally ()
          make(<label>, label: "&Subsystem:");
          frame.project-win32-subsystem-pane;
        end horizontally
      end grouping;
    end;
  keyword modal: = #t;
  keyword title: = "Advanced Project Settings";
  required keyword library-name:;
  required keyword module-name:;
  required keyword start-function-name:;
  required keyword major-version:;
  required keyword minor-version:;
  required keyword compilation-mode:;
  required keyword win32-subsystem:;
end frame <advanced-settings-frame>;

define sealed method initialize
  (frame :: <advanced-settings-frame>,
   #key	library-name :: <string>, module-name :: <string>,
        start-function-name :: <string>,
        major-version :: <string>, minor-version :: <string>,
        compilation-mode :: <symbol>, win32-subsystem :: <symbol>,
   #all-keys)
  next-method();

  gadget-value(frame.project-library-name-pane) := library-name;
  gadget-value-changed-callback(frame.project-library-name-pane)
    := make-require-value-callback(library-name);

  gadget-value(frame.project-module-name-pane) := module-name;
  gadget-value-changed-callback(frame.project-module-name-pane)
    := make-require-value-callback(module-name);

  gadget-value(frame.project-start-function-name-pane) := start-function-name;
  gadget-value-changed-callback(frame.project-start-function-name-pane)
    := make-require-value-callback(start-function-name);

  gadget-value(frame.project-major-version-pane) := major-version;
  gadget-value-changed-callback(frame.project-major-version-pane)
    := make-require-value-callback(major-version);

  gadget-value(frame.project-minor-version-pane) := minor-version;
  gadget-value-changed-callback(frame.project-minor-version-pane)
    := make-require-value-callback(minor-version);

  gadget-value(frame.project-compilation-mode-pane) := compilation-mode;

  gadget-value(frame.project-win32-subsystem-pane) := win32-subsystem;
end method initialize;

/// Advanced Scepter Settings

// A callback which forces the gadget value to be between min-value
// and max-value (both INclusive), defaulting to default-value.
define function make-require-range-callback
    (name :: <string>, min-value, max-value, default-value)
 => (callback :: <function>)
  method (gadget :: <sheet>)
    let val = gadget-value(gadget);
    // Require that there be a value and, if it's a string, it's non-empty.
    unless (val & val >= min-value & val <= max-value)
      beep(gadget);
      notify-user(format-to-string("%s must be between %= and %=",
				   name, min-value, max-value),
		  style: #"error", exit-style: #"ok",
		  owner: sheet-frame(gadget));
      gadget-value(gadget) := default-value;
    end;
  end
end function make-require-range-callback;

define constant $scepter-default-port :: <integer> = 49152;

define constant <scepter-use-type> :: <type>
  = one-of(#"protocol", #"stubs", #"skeletons");
define constant <scepter-using-type> :: <type>
  = one-of(#"client", #"server");

define sealed frame <scepter-advanced-settings-frame> (<dialog-frame>)
  pane scepter-advanced-trace?-pane (frame)
    make(<check-button>, label: "Trace ORB operation");
  pane scepter-advanced-no-co-location?-pane (frame)
    make(<check-button>, label: "Suppress co-location optimization");
  pane scepter-advanced-debug?-pane (frame)
    make(<check-button>, label: "Debug server errors");
  pane scepter-advanced-alternative-port?-pane (frame)
    make(<check-button>, label: "Use alternative port number:",
	 value-changed-callback:
	   method (cb :: <check-button>)
	     gadget-enabled?(frame.scepter-advanced-port-pane)
	       := gadget-value(cb);
	   end);
  pane scepter-advanced-port-pane (frame)
    make(<text-field>,
	 enabled?:
	   gadget-value(frame.scepter-advanced-alternative-port?-pane),
	 value: $scepter-default-port,
	 value-type: <integer>,
	 value-changed-callback:
	   make-require-range-callback
             ("The port number", 0, 65535, $scepter-default-port));
  layout (frame)
    vertically (y-spacing: 8)
      grouping ("General ORB options", max-width: $fill)
	vertically (y-spacing: 4)
	  frame.scepter-advanced-trace?-pane;
	  frame.scepter-advanced-no-co-location?-pane;
	end;
      end;
      grouping ("Server options", max-width: $fill)
	vertically (y-spacing: 4)
	  frame.scepter-advanced-debug?-pane;
	  horizontally (x-spacing: 4)
	    frame.scepter-advanced-alternative-port?-pane;
	    frame.scepter-advanced-port-pane;
	  end;
	end;
      end;
    end;
  keyword modal: = #t;
  keyword title: = "Advanced ORB Settings";
end frame <scepter-advanced-settings-frame>;

define sealed method initialize
  (frame :: <scepter-advanced-settings-frame>,
   #key	trace? :: <boolean>, no-co-location? :: <boolean>,
	debug? :: <boolean>, port? :: <boolean>,
	port :: <integer>, skeletons? :: <boolean>,
   #all-keys)
  next-method();

  gadget-value(frame.scepter-advanced-trace?-pane) := trace?;
  gadget-value(frame.scepter-advanced-no-co-location?-pane) := no-co-location?;
  gadget-value(frame.scepter-advanced-debug?-pane) := debug?;
  gadget-value(frame.scepter-advanced-alternative-port?-pane)
    := port?;
  gadget-value(frame.scepter-advanced-port-pane) := port;
  gadget-enabled?(frame.scepter-advanced-debug?-pane) := skeletons?;
  gadget-enabled?(frame.scepter-advanced-alternative-port?-pane)
    := skeletons?;
end method initialize;

define class <scepter-advanced-settings> (<object>)
  slot scepter-advanced-trace? :: <boolean>,
    init-value: #f,
    init-keyword: trace?:;
  slot scepter-advanced-no-co-location? :: <boolean>,
    init-value: #f,
    init-keyword: no-co-location?:;
  slot scepter-advanced-debug? :: <boolean>,
    init-value: #f,
    init-keyword: debug?:;
  slot scepter-advanced-port :: false-or(<integer>),
    init-value: #f,
    init-keyword: port:
end class <scepter-advanced-settings>;

define method scepter-advanced-settings-callback (pb :: <push-button>)
  let frame = sheet-frame(pb);
  let (get-pane, settings) = apply(values, pb.gadget-id);
  let advanced-dialog
    = make(<scepter-advanced-settings-frame>, owner: frame,
	   trace?: settings.scepter-advanced-trace?,
	   no-co-location?: settings.scepter-advanced-no-co-location?,
	   debug?: settings.scepter-advanced-debug?,
	   port?:
	     settings.scepter-advanced-port & #t,
	   port:
	     settings.scepter-advanced-port
	     | $scepter-default-port,
	   skeletons?: 
	     (#"skeletons" = gadget-value(get-pane(frame))));
  let exit-status = start-dialog(advanced-dialog);
  when (exit-status)
    // Copy the info into this frame.
    settings.scepter-advanced-trace?
      := gadget-value
	   (advanced-dialog.scepter-advanced-trace?-pane);
    settings.scepter-advanced-no-co-location?
      := gadget-value
	   (advanced-dialog.scepter-advanced-no-co-location?-pane);
    settings.scepter-advanced-debug?
      := gadget-value
	   (advanced-dialog.scepter-advanced-debug?-pane);
    settings.scepter-advanced-port
      := gadget-value
	   (advanced-dialog.scepter-advanced-alternative-port?-pane)
	 & gadget-value
	     (advanced-dialog.scepter-advanced-port-pane);
  end;
end method scepter-advanced-settings-callback;


/// ----------------------------------------------------------------------
/// PROJECT-WIZARD FRAME CLASS

define constant $default-indent :: <integer> = 40;
define constant $radio-indent   :: <integer> = 18;

define macro indenting
  { indenting () ?stuff:* end }
 => { horizontally () make(<label>, min-width: $default-indent); ?stuff end }
  { indenting (?amount:expression) ?stuff:* end }
 => { horizontally () make(<label>, min-width: ?amount); ?stuff end }
end macro indenting;

define macro labels
  { labels ?labels end }
 => { vertically () ?labels end }
labels:
  { } => { }
  { ?label; ... } => { ?label; ... }
label:
  { ?string:expression } => { make(<label>, label: ?string) }
end macro labels;

define constant format-interface-name-example =
    curry(format-to-string, 
	  "The %s class for an interface 'IFoo' will be named '<IFoo%s>'.");

define sealed frame <project-wizard-frame> (<wizard-frame>)
  keyword icon:  = $wizard-frame-icon;
  keyword image: = $wizard-image;
  keyword center?: = #t;
  keyword page-changed-callback: = project-wizard-page-changed-callback;
  keyword next-callback: = project-wizard-move-to-next-page;

  //////////////////////////////////////////////////
  // Page: Project Type
  //
  pane project-type-pane (frame)
    make(<list-box>, // could be a <list-control> later, for icons like VC++
	 selection-mode: #"single", scroll-bars: #"vertical",
	 items: sort!(choose(method (o) o.repository-object-minimum-edition.
			       release-contains-edition? end,
			     as(<vector>, $project-types)),
                      test: method (o1, o2)
                              o1.project-type-order < o2.project-type-order
                            end),
	 label-key: repository-object-label, value-key: repository-object-id,
	 selection: #[0],
         value-changed-callback: compose(update-wizard-from-type, sheet-frame),
	 activate-callback: 
		compose(project-wizard-move-to-next-page, sheet-frame));
  pane project-type-description-pane (frame)
    make(<text-editor>,
	 border-type: #"sunken",
	 scroll-bars: #"vertical",
	 read-only?: #t, tab-stop?: #f,
	 value: repository-object-documentation
                  ($project-types[gadget-value(frame.project-type-pane)]));
  pane type-page (frame)
    make(<wizard-page>,
	 id: #"type-page",
	 child:
	   begin
	     vertically (y-spacing: 16)
               vertically ()
	         make(<label>, label: $strings-mentioning-environment-name[#"wizard-intro"]);
                 labels
"First you must supply basic information about what type of project to create.";
                 end;
               end;
	       vertically (y-spacing: 8)
	         vertically (y-spacing: 2)
                   make(<label>, label: "Project type:");
	           frame.project-type-pane;
	         end;
	         vertically (y-spacing: 2)
                   make(<label>, label: "Description:");
	           frame.project-type-description-pane;
		 end;
	       end;
             end;
	   end);

  //////////////////////////////////////////////////
  // Pages: Pre-project-name info for Motley (project from OLE type info)
  //
  // Page to choose the type library to work from.
  pane project-motley-type-libraries-pane (frame)
    //---*** Do we need some "Refresh" button in case the user forgot to
    //---*** install the library and does so while the Wizard's running?
    // Yes, this list-box intentionally starts with no selection.
    make(<list-box>,
	 selection-mode: #"single",
	 scroll-bars: #"dynamic",
	 items: get-registry-type-libraries(),
         label-key: registry-type-library-name, value-key: identity,
         value-changed-callback:
           method (lb)
             let frame = sheet-frame(lb);
             let location = wizard-motley-type-library-location(frame);
             let project-location-text-pane
               = frame.project-motley-location-pane.file-browse-text-pane;
             gadget-value(project-location-text-pane, do-callback?: #t)
                 := location;
           end,
	 activate-callback: 
		compose(project-wizard-move-to-next-page, sheet-frame));
  pane project-motley-location-pane (frame)
    begin
      let pane = make(<file-browse-pane>);
      pane.file-browse-text-pane.gadget-value-changing-callback
	:= note-required-field-changed-callback;
      pane.file-browse-text-pane.gadget-value-changed-callback
	:= note-required-field-changed-callback;
      pane.file-browse-function
	:= method (#rest args, #key, #all-keys)
	     ignore(args);
	     let default = gadget-value(pane.file-browse-text-pane);
	     choose-file
               (title: "Choose file containing OLE type library information",
		default: default,
		frame: frame)
	   end;
      //--- Should we provide file filters for chooser dialog?
      pane
    end;
  pane motley-source-page (frame)
    make(<wizard-page>,
	 id: #"motley-source-page",
	 child:
	   begin
	     vertically (y-spacing: 16)
               labels
"On this page you must choose a source of OLE type library information.";
"You can choose an installed library from the list or select a file from";
"which to extract a type library."
               end;
	       grouping ("Type library source", max-width: $fill)
                 vertically (y-spacing: 16)
	           vertically (y-spacing: 2)
                     make(<label>, label: "&Installed type libraries:");
	             frame.project-motley-type-libraries-pane;
	           end;
	           horizontally (y-alignment: #"bottom")
                     make(<label>, label: "&Location:");
	             frame.project-motley-location-pane;
                   end;
                 end
	       end grouping;
	     end;
	   end);

  // Page to choose the kind of code to generate.
  pane project-motley-stubs-pane (frame)
    make(<radio-box>,
	 child:
           vertically (y-spacing: 8)
	     vertically ()
	       make(<radio-button>, label: "Dispatch &client interfaces",
		    id: #"dispatch-clients");
	       indenting ($radio-indent) labels
"Interfaces to use COM objects via OLE Automation's IDispatch."
	       end; end;
	     end;
	     vertically ()
	       make(<radio-button>, label: "Dispatch &server skeletons",
		    id: #"dispatch-servers");
	       indenting ($radio-indent) labels
"Skeletons of dispatch servers for you to implement."
	       end; end;
	     end;
             make(<separator>);
	     vertically ()
	       make(<radio-button>, label: "Custom (&vtable) interfaces",
		    id: #"vtable-interfaces");
	       indenting ($radio-indent) labels
"Server and client interfaces to use COM objects via COM custom interfaces."
	       end; end;
	     end;
	     vertically ()
	       make(<radio-button>, label: "&Dual interfaces",
		    id: #"dual-interfaces");
	       indenting ($radio-indent) labels
"Dual (dispatch and custom) server interfaces for you to implement, and";
"custom client interfaces."
	       end; end;
	     end;
           end,
           value: #"dispatch-clients", //---*** should be remembered?
	   value-changed-callback:
	     compose(note-dialog-page-neighbors-changed, sheet-frame));
  //---*** This page needs to be changed to have a description pane which
  //---*** gives more details on each option.  Then the text for each radio
  //---*** button can be kept to one line.
  pane motley-stubs-page (frame)
    make(<wizard-page>,
	 id: #"motley-stubs-page",
	 child:
	   begin
	     vertically (y-spacing: 16)
               labels
"On this page you must decide which kind of stubs you want to generate from the";
"OLE type library information.";
"";
"If the type information source changes, the stubs will be regenerated the next";
"time you build the project.";
               end;
	       grouping ("Generate stubs", max-width: $fill)
                 frame.project-motley-stubs-pane
	       end grouping;
	     end;
	   end);

  // This page only comes up if vtable or dual interfaces are selected.
  // It allows input of server and client suffixes.
  pane motley-suffix-page (frame)
    make(<wizard-page>,
      id: #"motley-suffix-page",
      child:
	begin
	  vertically (y-spacing: 16)
	    labels
"You may choose suffixes to distinguish client and server classes, or choose";
"not to generate client classes at all.";
	    end;
	    grouping ("Class suffixes", max-width: $fill)
	      vertically (y-spacing: 8)
		horizontally (y-alignment: #"bottom")
		  make(<label>, label: "&Server class suffix:");
		  frame.motley-server-suffix
		end;
		frame.motley-server-suffix-example;
		make(<separator>);
		horizontally (y-alignment: #"bottom")
		  make(<text-field-option>, 
		    label: "&Generate client classes with suffix:",
		    enabled?: #t, text-field: frame.motley-client-suffix,
		    value-changed-callback: note-client-suffix-changed-method);
		  frame.motley-client-suffix;
		end;
		frame.motley-client-suffix-example;
	      end;
	    end grouping;
	 end;
       end);
  pane motley-server-suffix (frame)
    make(<text-field>,
      value-changing-callback: note-server-suffix-changed-method,
      value-changed-callback: note-server-suffix-changed-method);
  pane motley-server-suffix-example (frame)
    make(<label>, label: format-interface-name-example("server", ""),
	 max-width: $fill);
  pane motley-client-suffix-example (frame)
    make(<label>, label: format-interface-name-example("client", "-client"),
	 max-width: $fill);
  pane motley-client-suffix (frame)
    make(<text-field>, value: "-client",
      value-changing-callback: note-client-suffix-changed-method,
      value-changed-callback: note-client-suffix-changed-method);

  // Page to choose the interfaces (from the type library) to handle.
  pane motley-interfaces-page (frame)
    make(<wizard-page>,
	 id: #"motley-interfaces-page",
	 child:
	   begin
	     vertically (y-spacing: 16)
               labels
"On this page you can choose which interfaces and COM classes to translate.";
               end;
	       grouping ("Interfaces and COM classes to translate",
			 max-width: $fill)
                 frame.project-motley-interfaces?-pane
	       end grouping;
	     end;
	   end);
  pane project-motley-interfaces?-pane (frame)
    make(<radio-box>,
      child:
	vertically (y-spacing: 0)
	  make(<radio-button>, 
	       label: "Translate &all.", id: #"all");
	  make(<radio-button>, 
	       label: "Translate &selected:", id: #"selected");
	  frame.project-motley-interfaces-pane;
	end);
  pane project-motley-interfaces-pane (frame)
    make(<list-box>,
      selection-mode: #"multiple",
      scroll-bars: #"dynamic",
      label-key: identity, value-key: identity,
      value-changed-callback: 
        method (p)
          frame.project-motley-interfaces?-pane.gadget-value := #"selected";
        end
    );


  //////////////////////////////////////////////////
  // Pages: Pre-project-location information for Scepter/CORBA projects.
  pane scepter-use-idl-file?-pane (frame)
    make(<check-button>,
	 label: "Use existing IDL file",
	 value: #t,
	 value-changed-callback:
	   method (cb :: <check-button>)
	     let using-idl-file? = gadget-value(cb);
	     file-browse-pane-enabled?(frame.scepter-idl-file-pane)
	       := using-idl-file?;
	     gadget-enabled?(frame.scepter-copy-idl-file?-pane)
	       := using-idl-file?;
	     note-required-field-changed-callback(cb);
	   end);
  pane scepter-copy-idl-file?-pane (frame)
    make(<check-button>,
	 label: "Copy IDL file into Project",
	 value: #t);
  pane scepter-idl-file-pane (frame)
    begin
      let pane = make(<file-browse-pane>);
      pane.file-browse-text-pane.gadget-value-changing-callback
	:= note-required-field-changed-callback;
      pane.file-browse-text-pane.gadget-value-changed-callback
	:= note-required-field-changed-callback;
      pane.file-browse-function
	:= method (#rest args, #key, #all-keys)
	     ignore(args);
	     let default = gadget-value(pane.file-browse-text-pane);
	     choose-file
               (title: "Choose CORBA IDL file",
		default: default,
		filters: #[#["CORBA IDL file", "*.idl"]], default-filter: 0,
		frame: frame)
	   end;
      pane
    end;
  pane scepter-client-use-pane (frame)
    make(<option-box>,
	 enabled?: #f,
	 value: #"stubs",
	 items: #[#["protocol",				#"protocol"],
		  #["protocol and stubs",		#"stubs"],
		  #["protocol, stubs and skeletons",	#"skeletons"]],
	 label-key: first, value-key: second);
  pane scepter-server-use-pane (frame)
    make(<option-box>,
	 enabled?: #f,
	 value: #"skeletons",
	 items: #[#["protocol",				#"protocol"],
		  #["protocol, stubs and skeletons",	#"skeletons"]],
	 label-key: first, value-key: second);
  pane scepter-client-advanced-settings-button (frame)
    make(<push-button>, label: "&Advanced...",
	 enabled?: #f,
	 id: list(scepter-client-use-pane, make(<scepter-advanced-settings>)),
	 activate-callback: scepter-advanced-settings-callback);
  pane scepter-server-advanced-settings-button (frame)
    make(<push-button>, label: "&Advanced...",
	 enabled?: #f,
	 id: list(scepter-server-use-pane, make(<scepter-advanced-settings>)),
	 activate-callback: scepter-advanced-settings-callback);
  pane scepter-generate-pane (frame)
    make(<check-box>,
	 child: tabling (rows: 2,
			 y-spacing: 16, x-spacing: 4,
			 y-alignment: #"center")
		  make(<check-button>,
		       label: "Client using", id: #"client",
		       value: #f);
		  frame.scepter-client-use-pane;
		  frame.scepter-client-advanced-settings-button;

		  make(<check-button>,
		       label: "Server using", id: #"server",
		       value: #f);
		  frame.scepter-server-use-pane;
		  frame.scepter-server-advanced-settings-button;
		end,
/*
		vertically (y-spacing: 8)
		  vertically (y-spacing: 4)
		    make(<check-button>, label: "Client", id: #"client",
			 value: #f);
		    indenting () frame.scepter-client-use-pane; end;
		  end;
		  vertically (y-spacing: 4)
		    make(<check-button>, label: "Server", id: #"server",
			 value: #f);
		    indenting () frame.scepter-server-use-pane; end;
		  end;
		end,
*/
	 value-changed-callback:
	   method (cb :: <check-box>)
	     let generate = gadget-value(cb);
	     let client? = member?(#"client", generate);
	     let server? = member?(#"server", generate);
	     gadget-enabled?(frame.scepter-client-use-pane) := client?;
	     gadget-enabled?(frame.scepter-client-advanced-settings-button)
	       := client?;
	     gadget-enabled?(frame.scepter-server-use-pane) := server?;
	     gadget-enabled?(frame.scepter-server-advanced-settings-button)
	       := server?;
	     note-required-field-changed-callback(cb);
	   end);
  pane scepter-basics-page (frame)
    make(<wizard-page>,
	 id: #"scepter-basics-page",
	 child:
	   begin
	     vertically (y-spacing: 16)
               labels
"On this page you must choose a CORBA IDL file (or a blank one will be created)";
"and decide whether to generate a client or server project, or both.";
"These projects will use some or all of the 'protocol', 'stubs' and 'skeletons'";
format-to-string("libraries which %s will generate automatically from the IDL file.",
                 release-product-name());
               end;
	       grouping ("CORBA IDL file",
			 max-width: $fill)
		 vertically (y-spacing: 8)
		   frame.scepter-use-idl-file?-pane;
	           frame.scepter-copy-idl-file?-pane;
		   horizontally (y-alignment: #"bottom")
		     make(<label>, label: "&Location:");
		     frame.scepter-idl-file-pane
		   end;
		 end;
	       end grouping;
	       grouping ("Projects to generate",
			 max-width: $fill)
                 frame.scepter-generate-pane
	       end grouping;
	     end;
	   end);


  //////////////////////////////////////////////////
  // Page: Basic Project Information
  // ---*** Still (maybe) to add "Modules" field.
  //
  slot project-old-name :: false-or(<string>) = #f;
  // --- The next two panes need a value-changed callback as well as a
  // value-changing one because they may be changed other than by typing.
  slot project-library-name :: <string> = "";
  slot project-module-name :: <string> = "";
  slot project-start-function-name :: <string>
    = default-start-function($project-wizard-settings);
  slot project-major-version :: <string> = "1";
  slot project-minor-version :: <string> = "0";
  slot project-compilation-mode :: <symbol>
    = $project-wizard-settings.default-compilation-mode;
  slot project-compilation-mode-required? :: <boolean> = #f;
  slot project-win32-subsystem :: <symbol> = #"console";
  slot project-win32-subsystem-required? :: <boolean> = #f;
  pane project-name-pane (frame)
    make(<text-field>,
	 value-changing-callback: note-project-name-changing-callback,
	 value-changed-callback:
           method (tf)
             note-project-name-changing-callback(tf);
             note-required-field-changed-callback(tf);
           end);
  pane project-location-pane (frame)
    begin
      let pane
	= make(<file-browse-pane>,
	       value: project-wizard-default-location());
      pane.file-browse-text-pane.gadget-value-changed-callback
	:= note-required-field-changed-callback;
      pane.file-browse-function
	:= method (#rest args, #key, #all-keys)
	     ignore(args);
	     let default = gadget-value(pane.file-browse-text-pane);
	     let directory
	       = block ()
		   as(<directory-locator>, default)
		 exception (error :: <locator-error>)
		   #f
		 end;
             let existing-ancestor
	       = directory & find-existing-ancestor(directory);
             default := existing-ancestor & as(<string>, existing-ancestor);
	     choose-directory(title: "Choose a folder for your new project:",
			      default: default,	// #f is allowed for default
			      frame: frame)
	   end;
      pane
    end;
  pane project-target-type-pane (frame)
    make(<radio-box>,
         orientation: #"horizontal", equalize-widths?: #t,
         enabled?:
           ~(target-type-for-project-type
               (gadget-value(frame.project-type-pane))),
         value: #"exe", //---*** should be remembered?
         items: #[#["Application (EXE)",	  #"exe"],
                  #["Dynamic Link Library (DLL)", #"dll"]],
         label-key: first, value-key: second);
  pane project-copy-templates?-pane (frame)
    make(<check-button>,
	 label: "Include any available templates.",
	 value: $project-wizard-settings.use-templates);
/*---*** andrewa: not currently used
  pane project-identity-description (frame)
/*
    make(<text-editor>,
	 read-only?: #t, tab-stop?: #f,
	 scroll-bars: #"vertical",
	 text: "On this page you must choose a name and location for your"
	       " project, and what type of executable to build."
	       "  You can also change some advanced project settings.");
*/
    labels
"On this page you must choose a name and location for your project.";
"";
"If you choose a location first, then enter a name, the Wizard will create";
"a subdirectory of that name.";
    end;
/* For multi-line text:
    make(<multi-line-text-pane>,
	 labels: $project-wizard-strings[#"???"]);
*/
*/
  pane project-advanced-settings-button (frame)
    make(<push-button>, label: "&Advanced...",
	 activate-callback:
	   method (pb)
	     let frame = sheet-frame(pb);
	     let advanced-dialog
	       = make(<advanced-settings-frame>, owner: frame,
		      library-name: frame.project-library-name,
		      module-name: frame.project-module-name,
		      start-function-name: frame.project-start-function-name,
		      major-version: frame.project-major-version,
		      minor-version: frame.project-minor-version,
		      compilation-mode:
			frame.project-compilation-mode,
		      compilation-mode-required?:
			frame.project-compilation-mode-required?,
		      target-type: 
			frame.project-target-type-pane.gadget-value,
		      win32-subsystem:
			frame.project-win32-subsystem,
		      win32-subsystem-required?:
			frame.project-win32-subsystem-required?);
	     let exit-status = start-dialog(advanced-dialog);
	     when (exit-status)
	       // Copy the info into this frame.
	       local maybe-set (frame-slot-setter, adv-slot)
		 let val = gadget-value(advanced-dialog.adv-slot);
		 val & (frame.frame-slot := val);
	       end;
	       maybe-set(project-library-name-setter,
			 project-library-name-pane);
	       maybe-set(project-module-name-setter,
			 project-module-name-pane);
	       maybe-set(project-start-function-name-setter,
			 project-start-function-name-pane);
	       maybe-set(project-major-version-setter,
			 project-major-version-pane);
	       maybe-set(project-minor-version-setter,
			 project-minor-version-pane);
	       maybe-set(project-compilation-mode-setter,
			 project-compilation-mode-pane);
	       maybe-set(project-win32-subsystem-setter,
			 project-win32-subsystem-pane);
	     end;
	   end);
  pane basics-page (frame)
    make(<wizard-page>,
	 id: #"basics-page",
	 child:
	   begin
	     vertically (y-spacing: 8)
	       labels
"On this page you must specify a name and location for your project.";
"You can also change some project settings and choose to include template code,";
"depending on the type of project you are creating.";
               end;
	       grouping ("Project name and location", max-width: $fill)
		 make(<table-layout>,
                      // columns: 2,
                      y-spacing: 8,
                      y-alignment: #[#"bottom", #"bottom"],
		      contents:
			vector(vector(make(<label>, label: "&Name:"),
                                      frame.project-name-pane),
                               vector(make(<label>, label: "&Location:"),
                                      frame.project-location-pane)))
               end grouping;
	       grouping ("Project settings and templates", max-width: $fill)
                 vertically (y-spacing: 8)
                   vertically (y-spacing: 2)
                     labels
"Some types of project can be built as either Applications or DLLs.";
                     end;
                     indenting () frame.project-target-type-pane end;
                   end;
		   vertically (y-spacing: 2)
		     labels
"Click on the 'Advanced...' button if the default project settings aren't suitable.";
		     end;
		     indenting () frame.project-advanced-settings-button end;
		   end;
                   vertically (y-spacing: 2)
                     labels
"Some project types can provide a 'skeleton application' from which you can start.";
                     end;
                     indenting () frame.project-copy-templates?-pane end;
		   end;
                 end
	       end;
	     end;
	   end);


  //////////////////////////////////////////////////
  // Page: Library Use Path.
  //
  pane project-library-use-type (frame)
    make(<radio-box>,
	 child:
	   vertically (y-spacing: 16)
	     vertically (y-spacing: 4)
	       make(<radio-button>, label: "&Minimal", id: #"minimal");
	       indenting ($radio-indent) labels
"Your project will use only the functional-dylan library.  There will be no more";
"more pages for choosing libraries.";
	       end; end;
	     end;
	     vertically (y-spacing: 4)
	       make(<radio-button>, label: "&Simple", id: #"simple");
	       indenting ($radio-indent) labels
"The following pages will lead you through options for using libraries for several";
format-to-string("main areas of support provided by %s.",
                 release-product-name());
	       end; end;
	     end;
	     vertically (y-spacing: 4)
	       make(<radio-button>, label: "&Custom", id: #"custom");
	       indenting ($radio-indent) labels
"The next page will let you choose exactly which libraries your project will use"; 
format-to-string("from the groups of libraries supplied with %s.",
                 release-product-name());
	       end; end;
	     end;
	   end,
	   value: #"simple",
	   value-changed-callback:
	     compose(note-dialog-page-neighbors-changed, sheet-frame));
  pane project-library-use-description (frame)
/*
    make(<text-editor>,
	 read-only?: #t, tab-stop?: #f,
	 scroll-bars: #"vertical",
	 text: "Every Dylan program uses other libaries.  On this page you"
	       " will decide how the Wizard will help you choose which"
	       " libraries your project will use.");
*/
    labels
"Every Dylan program defines one main library that may in turn use other libraries.";
"On this page you will decide how the Wizard will help you choose which libraries";
"your project will use.";
    end;
/* For multi-line text:
    make(<multi-line-text-pane>,
	 labels: $project-wizard-strings[#"???"]);
*/
  pane use-page (frame)
    make(<wizard-page>,
	 id: #"use-page",
	 child:
	   begin
	     vertically (y-spacing: 16)
// For multi-line text:	       with-border (type: #"groove")
		 frame.project-library-use-description;
// For multi-line text:	       end;
	       grouping ("Use libraries", max-width: $fill)
		 frame.project-library-use-type
	       end grouping;
	     end vertically
	   end);

  //////////////////////////////////////////////////
  // Page: IO and System Support
  //
  pane simple-io-and-system-options (frame)
    make(<check-box>,
	 child:
	   vertically (y-spacing: 8)
	     vertically (y-spacing: 4)
	       make(<check-button>,
		    label: "IO (abstract interface to I/O facilities)",
		    id: #"io");
	       indenting ($radio-indent) labels
"Use the IO library.";
"This exports the Format, Format-Out, Print, Standard-IO, and Streams modules.";
	       end; end;
	     end;
	     vertically (y-spacing: 4)
	       make(<check-button>,
                    label: "System (abstract interface to operating system facilities)",
		    id: #"system");
	       indenting ($radio-indent) labels
"Use the System library.";
"This exports the Operating-System, File-System and Date modules.";
	       end; end;
	     end;
	   end);
  pane simple-io-and-system-page (frame)
    make(<wizard-page>,
	 id: #"simple-io-and-system-page",
	 child:
	   begin
	     vertically (y-spacing: 16)
	       labels
"On this page you can choose libraries for access to operating system facilities.";
	       end;
	       grouping ("IO and system support", max-width: $fill)
		 frame.simple-io-and-system-options
	       end;
	     end;
	   end);


  //////////////////////////////////////////////////
  // Page: GUI Support
  //
  // This slot only ever gets updated to #"win32" or #"duim", never back
  // to #f.  It's here so that changing to a non-gui kind of project on
  // page 1 will select #"none" on this page, but changing back won't
  // revert to #"duim" if the user already chose #"win32".
  //
  slot chosen-simple-gui-option :: false-or(<symbol>) = #f;
  pane simple-gui-add-win32-option (frame)
    make(<check-button>,
	 label: "Use Win32 API libraries in addition to DUIM",
	 id: #"win32",
	 enabled?: frame.project-win32-subsystem == #"gui");
  pane simple-gui-option (frame)
    make(<radio-box>,
	 child:
	   vertically (y-spacing: 16)
	     vertically (y-spacing: 4)
	       make(<radio-button>, label: "&None", id: #"none");
	       indenting ($radio-indent) labels
"Do not include any GUI libraries. This is the default for DLLs and for Console";
"Applications (those running in the Win32 Character subsystem).";
	       end; end;
	     end;
	     vertically (y-spacing: 4)
	       make(<radio-button>,
                    label: "Dylan User Interface Manager (DUIM)", id: #"duim");
	       vertically (y-spacing: 2)
		 indenting ($radio-indent) labels
"Use DUIM, a platform-independent GUI toolkit, with a back-end for Win32.";
"This is recommended for all other kinds of projects.";
		 end; end;
		 indenting ($radio-indent) frame.simple-gui-add-win32-option end;
	       end;
	     end;
	     vertically (y-spacing: 4)
	       make(<radio-button>, label: "Win32 API", id: #"win32");
	       indenting ($radio-indent) labels
format-to-string("Use libraries for access to much of the Win32 API via the %s",
                 release-product-name());
"C Foreign Function Interface.";
	       end; end;
	     end;
	   end,
	   value: if (frame.project-win32-subsystem == #"gui")
                  #"duim" else #"none" end,
	   value-changed-callback:
	     method (r-box)
	       let frame = sheet-frame(r-box);
	       // Update computation of next (& prev) page.
	       let gui-option = gadget-value(r-box);
	       // Keep a record of which non-#"none" option was chosen.
	       unless (gui-option == #"none")
		 frame.chosen-simple-gui-option := gui-option;
	       end;
	       gadget-enabled?(frame.simple-gui-add-win32-option)
		 := (gui-option == #"duim");
	     end);
  pane simple-gui-page (frame)
    make(<wizard-page>,
	 id: #"simple-gui-page",
	 child:
	   begin
	     vertically (y-spacing: 16)
	       labels
"On this page you can choose which libraries your project will use for GUI support.";
	       end;
	       grouping ("GUI support", max-width: $fill)
		 frame.simple-gui-option
	       end;
	     end;
	   end);


  //////////////////////////////////////////////////
  // Page: Misc. Enhanced and Internal edition Libraries
  //
  pane network-option (frame)
    make(<check-button>,
	 label: "Network");
  pane ole-automation-option (frame)
    make(<check-button>,
	 label: "OLE Automation");
  pane misc-enhanced-plus-page (frame)
    make(<wizard-page>,
	 id: #"misc-enhanced-plus-page",
	 child:
	   begin
             vertically (y-spacing: 16)
               labels
"On this page you can choose libraries for network support and OLE Automation.";
               end;
               vertically (y-spacing: 8)
		 grouping ("Network support", max-width: $fill)
		   vertically (y-spacing: 4)
		     frame.network-option;
		     indenting ($radio-indent) labels
"Use the Network library.";
"This exports the abstract Sockets module and the Winsock2 module.";
		     end; end;
		   end
		 end;
		 grouping ("OLE Automation support", max-width: $fill)
		   vertically (y-spacing: 4)
		     frame.ole-automation-option;
		     indenting ($radio-indent) labels
"Use the OLE-Automation library.";
"Your project does not have to be an OLE project to use this.";
		     end; end;
		   end
		 end
               end
             end;
	   end);


  //////////////////////////////////////////////////
  // Page: Database Support
  //
  pane simple-databases-option (frame)
    make(<radio-box>,
	 child:
	   vertically (y-spacing: 16)
	     vertically (y-spacing: 4)
	       make(<radio-button>, label: "&None", id: #"none");
	       indenting ($radio-indent) labels
"Do not include any database libraries.  This is the default.";
	       end; end;
	     end;
	     vertically (y-spacing: 4)
	       make(<radio-button>,
		    label: "ODBC-specific support for relational databases",
		    id: #"sql-odbc");
	       indenting ($radio-indent) labels
"Use the SQL-ODBC library, which provides generic relational database support";
"with Microsoft's ODBC as the back-end.  This is recommended for new users.";
	       end; end;
	     end;
	     vertically (y-spacing: 4)
	       make(<radio-button>,
		    label: "DBMS-independent support for relational databases",
		    id: #"sql");
	       indenting ($radio-indent) labels
"Use the SQL library, which provides generic relational database support.";
"Another library in your project must use a library providing a DBMS backend";
"(either the SQL-ODBC library or one you implement for your own DBMS).";
	       end; end;
	     end;
	     vertically (y-spacing: 4)
	       make(<radio-button>,
		    label: "Low-level ODBC support", id: #"odbc-ffi");
	       indenting ($radio-indent) labels
"Use the ODBC-FFI library, which provides a low-level interface to the";
format-to-string("Microsoft ODBC API, via the %s C Foreign Function Interface.",
                 release-product-name());
"This is not recommended for new users.";
	       end; end;
	     end;
	   end,
	   value: #"none");
  pane simple-databases-page (frame)
    make(<wizard-page>,
	 id: #"simple-databases-page",
	 child:
	   begin
	     vertically (y-spacing: 16)
	       labels
"On this page you can choose what libraries to use for database support.";
	       end;
	       grouping ("Database support", max-width: $fill)
	         frame.simple-databases-option
	       end;
             end;
	   end);


  //////////////////////////////////////////////////
  // Page: Custom Library Use page.
  //
  pane project-module-chooser (frame)
    make(<choice-pane>,
	 choice-type-label: "Module");
  pane project-library-chooser (frame)
    make(<choice-pane>,
	 choice-type-label: "Library",
	 next-pane: project-module-chooser(frame));
  pane project-library-group-chooser (frame)
    make(<choice-pane>,
	 choice-type-label: "Library Group",
	 next-pane: project-library-chooser(frame),
	 choices: repository-as-choices());
  pane project-choosers-description (frame)
/*
    make(<text-editor>,
	 read-only?: #t, tab-stop?: #f,
	 scroll-bars: #"vertical",
	 text: $strings-mentioning-environment-name[#"chooser-help"]);
*/
    vertically ()
      labels
"This page lets you choose which groups of libraries to use from the libraries";
format-to-string("provided with %s.",
                 release-product-name());
"";
"Double-click on an item in the list to change whether it is used.";
"";
      end;
    end vertically;
/* For multi-line text:
    make(<multi-line-text-pane>,
	 labels: $project-wizard-strings[#"chooser-description"]);
*/
  pane chooser-page (frame)
    make(<wizard-page>,
	 id: #"chooser-page",
	 child:
	   begin
	     let l-g-chooser = frame.project-library-group-chooser;
	     let l-chooser = frame.project-library-chooser;
	     let m-chooser = frame.project-module-chooser;
	     m-chooser.previous-pane := l-chooser;
	     l-chooser.previous-pane := l-g-chooser;
/*
	     sheet-withdrawn?(l-chooser) := #t;
	     sheet-withdrawn?(m-chooser) := #t;
*/
	     vertically ()
// For multi-line text:	       with-border (type: #"groove")
		 frame.project-choosers-description;
// For multi-line text:	       end
	       horizontally (x-spacing: 8, equalize-heights?: #t,
			     y-alignment: #"bottom")
		 l-g-chooser;
		 l-chooser;
		 m-chooser;
	       end;
	     end
	   end);


  //////////////////////////////////////////////////
  // Page: Project Files -- Common Header Keywords
  //
  pane project-author-pane (frame)
    make(<text-field>,
	 label: "&Author:",
	 value: $project-wizard-settings.default-author);
  pane project-synopsis-pane (frame)
    make(<text-field>,
	 label: "&Synopsis:",
	 value: "A brief description of the project.");
  pane project-copyright-pane (frame)
    make(<text-field>,
	 label: "&Copyright:",
         value: $project-wizard-settings.default-copyright);
  pane project-version-pane (frame)
    make(<text-field>,
	 label: "&Version:",
	 value: $project-wizard-settings.default-version);
  pane project-headers-description (frame)
/*
    make(<text-editor>,
	 read-only?: #t, tab-stop?: #f,
	 scroll-bars: #"vertical",
	 text: "This page lets you include some standard information in the"
	       " files this Wizard will create.  To omit any of these"
	       " keywords, disable them with the corresponding check box.");
*/
    labels
"On this page you can provide standard information in the files the Wizard will create.";
    end;
/* For multi-line text:
    make(<multi-line-text-pane>,
	 labels: $project-wizard-strings[#"headers-page-description"]);
*/
  pane header-page (frame)
    make(<wizard-page>,
	 id: #"header-page",
	 child:
	   begin
	     vertically (y-spacing: 16)
// For multi-line text:	       with-border (type: #"groove")
	       frame.project-headers-description;
// For multi-line text:	       end
	       grouping ("Source file headers", max-width: $fill)
		 vertically (y-spacing: 4)
		   make(<label>,
			label: "For each field, enter the desired "
			       "information or disable it with the check box.");
		   make(<table-layout>,
			columns: 2,
			x-alignment: #[#"left", #"left"],
			y-alignment: #[#"bottom", #"bottom", #"bottom", #"bottom"],
			y-spacing: 8,
			children:
			  vector(make(<text-field-option>, label: "&Synopsis:",
				      enabled?: $project-wizard-settings.show-synopsis,
				      text-field: frame.project-synopsis-pane),
				 frame.project-synopsis-pane,
				 make(<text-field-option>, label: "&Author:",
				      enabled?: $project-wizard-settings.show-author,
				      text-field: frame.project-author-pane),
				 frame.project-author-pane,
				 make(<text-field-option>, label: "&Copyright:",
				      enabled?: $project-wizard-settings.show-copyright,
				      text-field: frame.project-copyright-pane),
				 frame.project-copyright-pane,
				 make(<text-field-option>, label: "&Version:",
				      enabled?: $project-wizard-settings.show-version,
				      text-field: frame.project-version-pane),
				 frame.project-version-pane));
		 end
	       end;
               labels
"You have now finished supplying information about your new project.  Click on";
"'Finish' to create it, 'Back' to go back and change some details, or 'Cancel'";
"to stop without creating the project.";
               end;
	     end;
	   end);
  pages (frame)
    vector(frame.type-page,
           frame.motley-source-page, frame.motley-stubs-page,
	   frame.motley-suffix-page, frame.motley-interfaces-page,
	   frame.scepter-basics-page, // frame.scepter-advanced-page,
	   frame.basics-page, frame.use-page,
	   frame.simple-io-and-system-page, frame.simple-gui-page,
	   frame.misc-enhanced-plus-page, frame.simple-databases-page,
	   frame.chooser-page, frame.header-page);

  // The end :-)
end frame <project-wizard-frame>;

define sealed method make
    (class == <project-wizard-frame>, #rest args, #key, #all-keys)
 => (frame :: <project-wizard-frame>)
  // Initialize "resources" before anything gets made.
  initialize-wizard-images();
  initialize-wizard-strings();
  next-method();
end method make;


define method handle-event
    (frame :: <project-wizard-frame>, event :: <frame-created-event>)
 => ()
// When template projects arrive:  frame-input-focus(frame) := frame.project-custom-type-pane;
  frame-input-focus(frame) := frame.project-name-pane;
  note-page-required-fields-changed(frame);
end method;

define function wizard-motley-type-library-location
    (frame :: <project-wizard-frame>) => (location :: false-or(<pathname>))
  let lb = frame.project-motley-type-libraries-pane;
  let type-library = gadget-value(lb);
  type-library & registry-type-library-path(type-library);
end function;

//---*** This should be in DUIM!
define function note-dialog-page-neighbors-changed
    (dialog :: <project-wizard-frame>) => ()
  let next-page = compute-next-page(dialog);
  dialog-next-page(dialog)     := next-page;
  dialog-previous-page(dialog) := compute-previous-page(dialog);
  dialog-exit-enabled?(dialog) := (next-page == #f);
end function note-dialog-page-neighbors-changed;

define method compute-previous-page
    (dialog :: <project-wizard-frame>)
 => (previous-page :: false-or(<sheet>))
  let current-page = dialog-current-page(dialog);
  let current-page-id = gadget-id(current-page);
  select (current-page-id)
    #"motley-interfaces-page" =>
      select (dialog.project-motley-stubs-pane.gadget-value)
	#"dispatch-clients", #"dispatch-servers" =>
	  dialog.motley-stubs-page;
	#"vtable-interfaces", #"dual-interfaces" =>
	  dialog.motley-suffix-page;
      end select;
    #"scepter-basics-page" =>
      dialog.type-page;
    #"basics-page" =>
      select (gadget-value(dialog.project-type-pane))
        #"motley"  => dialog.motley-interfaces-page;
        #"scepter" => dialog.scepter-basics-page;
        otherwise  => dialog.type-page;
      end;
    #"header-page" =>
      select (gadget-value(dialog.project-library-use-type))
	#"minimal" => dialog.use-page;
	#"custom"  => dialog.chooser-page;
	otherwise =>
	  if (release-contains-edition?(#"enhanced"))
	    dialog.simple-databases-page
	  else
	    // No Databases page in the Basic Edition
	    dialog.simple-gui-page
	  end;
      end;
    #"chooser-page" =>
      dialog.use-page;
    otherwise =>
      next-method();
  end
end method;

define method compute-next-page
    (dialog :: <project-wizard-frame>)
 => (next-page :: false-or(<sheet>))
  let current-page = dialog-current-page(dialog);
  // ---*** current-page should never be #f, but DUIM Wizard code is
  // broken (Bug 1613), so do this until it's fixed.
  when (current-page)
    let current-page-id = gadget-id(current-page);
    select (current-page-id)
      #"type-page" =>
	select (gadget-value(dialog.project-type-pane))
	  #"motley"  => dialog.motley-source-page;
	  #"scepter" => dialog.scepter-basics-page;
	  otherwise  => dialog.basics-page;
	end;
      #"motley-interfaces-page" =>
	dialog.basics-page;
      #"motley-stubs-page" =>
	select (dialog.project-motley-stubs-pane.gadget-value)
	  #"dispatch-clients", #"dispatch-servers" =>
	    dialog.motley-interfaces-page;
	  #"vtable-interfaces", #"dual-interfaces" =>
	    dialog.motley-suffix-page;
	end select;
      #"use-page" =>
	select (gadget-value(dialog.project-library-use-type))
	  #"minimal" => dialog.header-page;
	  #"custom"  => dialog.chooser-page;
	  otherwise  => dialog.simple-io-and-system-page;
	end;
      #"simple-gui-page" =>
        //---*** It would be better to do this by having some "page-hidden?"
        //---*** predicate and, if it's true for the page this would return,
        //---*** then call this function on that to skip over it.  That only
        //---*** works if the first & last pages are never hidden.
	if (release-contains-edition?(#"enhanced"))
	  dialog.misc-enhanced-plus-page
	else
	  // No Databases page in the Basic Edition
	  dialog.header-page
	end;
      #"simple-databases-page" =>
	dialog.header-page;
      otherwise =>
	next-method();
    end
  end
end method;

define method project-wizard-move-to-next-page
    (dialog :: <project-wizard-frame>) => ()
  let current-page = dialog-current-page(dialog);
  let current-page-id = gadget-id(current-page);
  let move? = #t;
  select (current-page-id)
    #"type-page" =>
      // Guarantee that update-wizard-from-type gets called at least once.
      update-wizard-from-type(dialog);
    #"motley-source-page" =>
      let location
        = gadget-value(dialog.project-motley-location-pane.file-browse-text-pane);
      block ()
	// Part of the reason for doing this is for the side-effect that we get
	// an <error> if the file doesn't contain a valid type library.
        let (short-name, interfaces)
          = with-busy-cursor (dialog)
              get-type-library-information(location)
            end;
        // Update the project name to the type-library's short name.
        gadget-value(dialog.project-name-pane, do-callback?: #t) := short-name;
	// Update the interfaces list to the interfaces.
	dialog.project-motley-interfaces-pane.gadget-items := interfaces;
	dialog.project-motley-interfaces-pane.gadget-selection := #();
	dialog.project-motley-interfaces?-pane.gadget-value := #"all";
      exception (<error>)
        move? := #f;
      end;
      unless (move?)
        notify-user("You must choose a valid source of OLE type information.",
                    style: #"information", exit-style: #"ok", owner: dialog);
      end;
    #"motley-suffix-page" =>
      let server-suffix = dialog.motley-server-suffix.gadget-value;
      let client-suffix = dialog.motley-client-suffix.gadget-value;
      if (dialog.motley-client-suffix.gadget-enabled? & 
	  server-suffix = client-suffix)
        notify-user("The server suffix must differ from the client suffix.",
                    style: #"information", exit-style: #"ok", owner: dialog);
	move? := #f;
      end if;
    #"scepter-basics-page" =>
      //---*** hughg: We could perhaps scan the IDL file here with scepter,
      //---*** to see if it looks valid, but I imagine that'd be slower than
      //---*** in Motley's case.
      // Update the project name to the base name of the IDL file.
      //---*** hughg: Unfortunately, that might not be a valid dylan name!
      let idl-file
	= gadget-value(dialog.scepter-use-idl-file?-pane)
	  & gadget-value(dialog.scepter-idl-file-pane.file-browse-text-pane);
      when (idl-file & ~empty?(idl-file))
	if (file-exists?(idl-file))
	  let idl-base = locator-base(as(<file-locator>, idl-file));
	  let idl-base-string = idl-base & as(<string>, idl-base);
	  when (idl-base)
	    gadget-value(dialog.project-name-pane, do-callback?: #t)
	      := idl-base;
	  end;
	else
	  move? := #f;
	  notify-user("The IDL file you have chosen does not exist.",
		      style: #"error", exit-style: #"ok", owner: dialog);
	end;
      end;
    #"basics-page" =>
      let location-string
	= gadget-value(dialog.project-location-pane.file-browse-text-pane);
      debug-assert(~empty?(location-string),
		   "Wizard allowing project creation in directory \"\"");
      let location = as(<file-system-locator>, location-string);
      move?
	:= select (location by instance?)
	     <file-locator> =>
	       if (file-exists?(location))
		 let message
		   = concatenate
		       ("The file '", location-string, "' already exists and is not a directory.\n"
			"You must delete it or enter a different location for the project.\n");
		 notify-user
		   (message, style: #"information", exit-style: #"ok",
		    owner: dialog)
	       else
		 let project-type = gadget-value(dialog.project-type-pane);
		 let directory = location.locator-directory | project-wizard-default-location();
                 // Canonicalize project location string to a directory locator...
                 let name = locator-name(location);
                 let full-dir = if (name)
                                  subdirectory-locator(directory, name)
                                else
                                  directory
                                end;
                 // Update the GUI so user has a clue what's going on, and
                 // so later readers of the gadget value don't have to duplicate
                 // this logic.
                 gadget-value(dialog.project-location-pane.file-browse-text-pane)
                   := as(<string>, full-dir);
		 ensure-filespace-for-type(dialog, project-type, full-dir)
	       end;
	     <directory-locator> =>
	       let project-type = gadget-value(dialog.project-type-pane);
	       ensure-filespace-for-type(dialog, project-type, location);
	   end;
    #"chooser-page" =>
      when (dialog.project-win32-subsystem == #"gui" & 
	    dialog.project-target-type-pane.gadget-value ~= #"dll")
	let library-group-choices
	  = gadget-items
	      (dialog.project-library-group-chooser.choice-list-pane);
	let (duim-group-included?, win32-group-included?) = values(#f, #f);
	for (choice in library-group-choices)
	  when (choice.choice-object.repository-object-id == #"duim")
	    duim-group-included? := choice.choice-included?;
	  end;
	  when (choice.choice-object.repository-object-id == #"win32")
	    win32-group-included? := choice.choice-included?;
	  end;
	end;
	unless (duim-group-included? | win32-group-included?)
	  notify-user("You are creating a project which will run in the Win32"
		      " GUI subsystem\nbut you have not included any libraries"
		      " from the DUIM GUI toolkit or Win32 API groups.\n\n"
		      "You may want to go back and change the libraries your"
		      " project will use,\nor the type of executable it will"
		      " build.",
		      style: #"information", exit-style: #"ok", owner: dialog);
	end;
      end;
    otherwise =>
      #f; // Do nothing
  end;
  when (move?)
    move-to-next-page(dialog)
    // next-method()
  end;
end method;

/* ---*** hughg, 1997/11/20: Not currently used -- remove?
define function find-wizard-page-from-id
    (dialog :: <project-wizard-frame>, id)
 => (page :: false-or(<sheet>))
  find-element(dialog-pages(dialog), method (page) gadget-id(page) = id end)
end function;
*/

define function project-wizard-page-changed-callback
    (dialog :: <project-wizard-frame>) => ()
  note-page-required-fields-changed(dialog)
end function;

define function note-project-name-changing-callback (sheet :: <sheet>)
  let frame = sheet-frame(sheet);
  let old-name = frame.project-old-name;
  let new-name = gadget-value(sheet);
  // Unless they've been set to something other than the project-name,
  // set the library and module name to the project-name.
  when (frame.project-library-name = (old-name | ""))
    frame.project-library-name := new-name;
  end;
  when (frame.project-module-name = (old-name | ""))
    frame.project-module-name := new-name;
  end;
  // Try to update the last directory in the path gadget to match the
  // project name, if the project name is changing; or add the project name
  // as a new directory to the path, if the project name was previously
  // empty.
  let directory-gadget = frame.project-location-pane.file-browse-text-pane;
  frame.project-old-name := if (empty?(new-name)) #f else new-name end;
  // Get the current directory name, ensuring the trailing '/' or '\\'.
  let directory = as(<directory-locator>, gadget-value(directory-gadget));
  let old-dir = as(<string>, directory);
  if (old-name)
    let old-dir-size = size(old-dir);
    let old-name-start = max(old-dir-size - size(old-name) - 1, 0);
    let old-name-end = max(old-dir-size - 1, old-name-start);
    when (case-insensitive-equal
	    (copy-sequence(old-dir, start: old-name-start, end: old-name-end),
	     old-name))
      when (empty?(new-name))
	// We have an extra trailing '\' to remove from the dir name.
	old-name-end := old-name-end + 1;
      end;
      gadget-value(directory-gadget)
	:= replace-subsequence!
	     (old-dir, new-name, start: old-name-start, end: old-name-end);
    end;
  else
    let directory = as(<directory-locator>, concatenate(old-dir, new-name));
    gadget-value(directory-gadget) := as(<string>, directory)
  end;
  note-page-required-fields-changed(frame);
end function;

define function note-required-field-changed-callback (sheet :: <sheet>) 
  note-page-required-fields-changed(sheet-frame(sheet))
end function;


define function note-server-suffix-changed-method (changed) => ()
  let frame = changed.sheet-frame;
  frame.motley-server-suffix-example.gadget-label :=
	format-interface-name-example("server", 
				      frame.motley-server-suffix.gadget-value);
end function;

define function note-client-suffix-changed-method (changed) => ()
  let frame = changed.sheet-frame;
  frame.motley-client-suffix-example.gadget-label :=
      if (frame.motley-client-suffix.gadget-enabled?)
	format-interface-name-example("client", 
				      frame.motley-client-suffix.gadget-value)
      else
	"Client classes will not be generated.  Client methods will "
	"operate on <C-Interface>."
      end if;
end function;


// ---*** This is a "method" instead of a "function" to work around a
// pentium-kan bug as of 1997/09/22, whereby "functions" give "odd
// number of keywords" arguments at runtime.
define method note-page-required-fields-changed
    (dialog :: <project-wizard-frame>,
     #key page :: false-or(<sheet>) = #f)
 => ()
  unless (page)
    page := dialog-current-page(dialog);
  end;
  when (page == dialog-current-page(dialog))
    let got-required?
      = do-note-page-required-fields-changed(dialog, gadget-id(page));
    // ---*** This (or DUIM) needs to cope with the fact that
    // for the last page we really want to affect the "Exit" button.
    dialog-next-enabled?(dialog) := got-required?;
  end;
end method;

define generic do-note-page-required-fields-changed
    (dialog :: <project-wizard-frame>, page-id)
 => (got-required? :: <boolean>);

define method do-note-page-required-fields-changed
    (dialog :: <project-wizard-frame>, page-id)
 => (got-required? :: <boolean>)
  ignore(dialog, page-id);
  #t
end method;

define method do-note-page-required-fields-changed
    (dialog :: <project-wizard-frame>, page-id == #"motley-source-page")
 => (got-required? :: <boolean>)
  let list-library-location = wizard-motley-type-library-location(dialog);
  let text-library-location
    = gadget-value(dialog.project-motley-location-pane.file-browse-text-pane);
  // If the user modifies the text field and it's not the same as the name
  // for the current list selection, cancel the list selection.
  unless (list-library-location = text-library-location)
    gadget-selection(dialog.project-motley-type-libraries-pane) := #[];
  end;
  // If they've selected a Motley project, they must provide a location.
  // When they try to change pages we'll check whether the file's valid.
  ~empty?(text-library-location)
end method;

define method do-note-page-required-fields-changed
    (dialog :: <project-wizard-frame>, page-id == #"scepter-basics-page")
 => (got-required? :: <boolean>)
  let got-idl-file?
    = ~(gadget-value(dialog.scepter-use-idl-file?-pane))
      | ~empty?(gadget-value
		  (dialog.scepter-idl-file-pane.file-browse-text-pane));
  let got-generate? = ~empty?(gadget-value(dialog.scepter-generate-pane));
  got-idl-file? & got-generate?
end method;

define method do-note-page-required-fields-changed
    (dialog :: <project-wizard-frame>, page-id == #"basics-page")
 => (got-required? :: <boolean>)
  // Must provide a name and location.
  let location-string
    = gadget-value
	(dialog.project-location-pane.file-browse-text-pane);
  ~empty?(gadget-value(dialog.project-name-pane))
    & ~empty?(location-string)
end method;

// Add some requirement for the chooser-page?  E.g., must at least include
// dylan or functional-dylan?

// None of the fields for the header-page are required.


/// ----------------------------------------------------------------------
/// INTER-PAGE DEPENDENCIES

define function target-type-for-project-type
    (project-type :: <symbol>)
 => (required-target-type :: false-or(<target-type>),
     recommended-target-type :: false-or(<target-type>))
  let required
    = select (project-type)
        #"gui-exe", #"console-exe", #"ole-server"	=> #"exe";
        #"dll", #"ole-control"				=> #"dll";
        otherwise => #f; // i.e., "nothing required"
      end;
  let recommended
    = select (project-type)
        #"motley" => #"dll";
        otherwise => #f; // i.e., "nothing recommended"
      end;
  values(required, recommended)
end function target-type-for-project-type;

define function compilation-mode-for-project-type
    (project-type :: <symbol>)
 => (required-compilation-mode :: false-or(<symbol>))
  let required
    = select (project-type)
        #"motley"					=> #"tight";
        otherwise => #f; // i.e., "nothing required"
      end;
  required
end function compilation-mode-for-project-type;

define function win32-subsystem-for-project-type
    (project-type :: <symbol>)
 => (required-win32-subsystem :: false-or(<symbol>),
     recommended-win32-subsystem :: false-or(<symbol>))
  let required
    = select (project-type)
	//---*** hughg, 1998/09/08: Note that if we ever support Wizarding
	//---*** OLE Server projects which are in-process servers, and hence
	//---*** DLLs, the constraints for #"ole-server" may need reviewing.
        #"gui-exe", #"ole-server"	=> #"gui";
        #"console-exe"			=> #"console";
        otherwise			=> #f; // i.e., "nothing required"
      end;
  let recommended = #"gui";
  values(required, recommended)
end function win32-subsystem-for-project-type;

//---*** The fact that this callback updates many separate things suggests
//---*** that this would be better done with a "channel" pattern.

//---*** hughg, 1998/09/08: We *really* could do with a "meta-language" for
//---*** describing constraints on project settings, both for use in the
//---*** Wizard and for warning users of "incompatible" changes after the
//---*** project is created.

define function update-wizard-from-type (frame :: <project-wizard-frame>) => ()
  let project-type = gadget-value(frame.project-type-pane);
  // Description
  gadget-value(frame.project-type-description-pane)
    := repository-object-documentation($project-types[project-type]);
  // Target type
  let (required-target-type, recommended-target-type)
    = target-type-for-project-type(project-type);
  let target-type = required-target-type | recommended-target-type;
  when (target-type)
    gadget-value(frame.project-target-type-pane) := target-type;
  end;
  gadget-enabled?(frame.project-target-type-pane) := ~(required-target-type);
  // Compilation Mode
  let required-compilation-mode
    = compilation-mode-for-project-type(project-type);
  when (required-compilation-mode)
    frame.project-compilation-mode := required-compilation-mode;
  end;
  frame.project-compilation-mode-required? := required-compilation-mode & #t;
  // Win32 Subsystem
  let (required-win32-subsystem, recommended-win32-subsystem)
    = win32-subsystem-for-project-type(project-type);
  let win32-subsystem
    = required-win32-subsystem | recommended-win32-subsystem;
  when (win32-subsystem)
    frame.project-win32-subsystem := win32-subsystem;
  end;
  frame.project-win32-subsystem-required? := required-win32-subsystem & #t;
  // Default GUI option
  gadget-value(frame.simple-gui-option, do-callback?: #t)
    := if (frame.project-win32-subsystem == #"gui")
         frame.chosen-simple-gui-option | #"duim";
       else #"none" end;
  //---*** Could enable/disable templates checkbox here.
  // The next page may be different depending on the project type.
  note-dialog-page-neighbors-changed(frame);
  // Check and disable the ole-automation box for motley projects.
  if (project-type == #"motley")
    gadget-value(frame.ole-automation-option) := #t;
    gadget-enabled?(frame.ole-automation-option) := #f;
  else
    //--- Maybe need a "chosen-ole-automation-option" slot, as for the
    //--- simple-gui-option?  Or maybe a more general mechanism overall?
    gadget-value(frame.ole-automation-option) := #f;
    gadget-enabled?(frame.ole-automation-option) := #t;
  end;
end function update-wizard-from-type;



/// ----------------------------------------------------------------------
/// WRITING OUT THE PROJECT

// --- hughg, 1998/09/22: MAYBE-ENSURE-PROJECT-DIRECTORY presents dialogs
// itself, rather than signalling conditions, so if there are several
// problems the user may get several dialogs.  This may turn out to be
// irritating but I can't be bothered to change it right now!
define generic ensure-filespace-for-type
    (dialog :: <project-wizard-frame>, project-type :: <symbol>,
     directory :: <directory-locator>)
 => (okay? :: <boolean>);

define method ensure-filespace-for-type
    (dialog :: <project-wizard-frame>, project-type :: <symbol>,
     directory :: <directory-locator>)
 => (okay? :: <boolean>)
  maybe-ensure-project-directory(directory, owner: dialog)
end method ensure-filespace-for-type;

define method ensure-filespace-for-type
    (dialog :: <project-wizard-frame>, project-type == #"scepter",
     directory :: <directory-locator>)
 => (okay? :: <boolean>)
  let generate = gadget-value(dialog.scepter-generate-pane);
  next-method()
    & block (exit)
	for (using-type in generate)
	  unless (maybe-ensure-project-directory
		    (scepter-using-project-directory(directory, using-type),
		     owner: dialog))
	    exit(#f)
	  end
	end;
	#t
      end
end method ensure-filespace-for-type;


define function handle-write-project-failed-error
    (wpfe :: <write-project-failed-error>, #key frame)
 => ()
  let c = wpfe.condition;
  let c-string
    = block () condition-to-string(c) exception (<condition>) #f end;
  let message
    = concatenate("The Wizard was unable to create all the files for"
		  " your new project",
		  if (c-string)
		    concatenate(" because\n\n", c-string)
		  else
		    "."
		  end,
		  "\n\nDo you want the Wizard to remove any files which"
		  " were created?");
  let remove-files?
    = notify-user(message, style: #"error", exit-style: #"yes-no",
		  owner: frame);
  when (remove-files?)
    block ()
      // ---*** If 'c' is a <file-exists> error, we should really offer
      // an option to force-write, because the user might have spent ages
      // filling in Wizard dialog fields.
      let files-failed = make(<stretchy-vector>);
      for (file in wpfe.files-written)
	block ()
	  delete-file(file);
	exception (<condition>)
	  add!(files-failed, file);
	end;
      end;
      unless (empty?(files-failed))
	let message
	  = apply(concatenate,
		  "An error occurred while the Wizard was trying to"
		  " remove these files:\n\n",
		  map(method (item) concatenate(as(<string>, item), "\n") end,
		      files-failed));
	notify-user
	  (message, style: #"error", exit-style: #"ok", owner: frame);
      end;
    end;
  end;
  #f
end function;


/// ----------------------------------------------------------------------
/// PERSISTENT STATE

define function update-project-wizard-defaults
    (wizard :: <project-wizard-frame>, location :: <directory-locator>) => ()
  let start-function = wizard.project-start-function-name;
  let synopsis = gadget-value(wizard.project-synopsis-pane);
  let author = gadget-value(wizard.project-author-pane);
  let copyright = gadget-value(wizard.project-copyright-pane);
  let version = gadget-value(wizard.project-version-pane);
  let compilation-mode = wizard.project-compilation-mode;
  let use-templates? = gadget-value(wizard.project-copy-templates?-pane);
  let location = as(<string>, location.locator-directory); 
  let settings = $project-wizard-settings;
  let show-synopsis? = ~empty?(synopsis);
  let show-author? = ~empty?(author);
  let show-copyright? = ~empty?(copyright);
  let show-version? = ~empty?(version);
  settings.default-location := location;
  settings.default-compilation-mode := compilation-mode;
  settings.default-start-function := start-function;
  if (show-author?)    settings.default-author    := author    end;
  if (show-copyright?) settings.default-copyright := copyright end;
  if (show-version?)   settings.default-version   := version   end;
  settings.use-templates  := use-templates?;
  settings.show-synopsis  := show-synopsis?;
  settings.show-author    := show-author?;
  settings.show-copyright := show-copyright?;
  settings.show-version   := show-version?;
end function update-project-wizard-defaults;


/// ----------------------------------------------------------------------
/// MAIN ENTRY POINT

define sealed sideways method new-project-wizard
    (#key frame :: false-or(<frame>),
	  title = "New Project",
     #all-keys)
 => (project-locations :: <sequence> /* of <file-locator> */)
  let framem
    = if (frame)
	frame-manager(frame)
      else
	port-default-frame-manager(default-port())
      end;
  let project-locations = #();
  let wizard
    = make(<project-wizard-frame>,
	   title: title, frame-manager: framem, owner: frame,
	   exit-callback:
	     // Done in the exit-callback so that any 'notify-user' calls
	     // can have an owner.
	     method (wizard :: <project-wizard-frame>)
	       let description = make-description-from-wizard-dialog(wizard);
	       block ()
		 project-locations
		   := write-project-from-description(description);
		 let arbitrary-project-location = project-directory(description);
		 when (arbitrary-project-location)
		   update-project-wizard-defaults
		     (wizard, arbitrary-project-location);
		 end;
	       cleanup // ---*** cleanup must precede exception?
		 exit-dialog(wizard);
	       exception (wpfe :: <write-project-failed-error>)
		 handle-write-project-failed-error(wpfe, frame: wizard);
		 #f
	       end
	     end);
  start-dialog(wizard);
  project-locations
end method new-project-wizard;
