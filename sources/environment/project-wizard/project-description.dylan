Module:    environment-project-wizard
Author:    Hugh Greene
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// ISSUES
//
//---*** TEMPLATE PROJECTS (i.e., copy another project and rename bits)


// ---*** I should get these from the new build-system library, or whatever
// it's called, once Roman checks it in.
define constant $user-project-suffix :: <byte-string> = "hdp";
define constant $dylan-source-suffix :: <byte-string> = "dylan";
define constant $idl-suffix :: <byte-string> = "idl";
define constant $spec-suffix :: <byte-string> = "spec";


/// ----------------------------------------------------------------------
/// PROJECT DESCRIPTIONS

define sealed abstract class <project-description> (<object>)
  // Project filename, without extension.
  constant slot project-name :: <string>,
    required-init-keyword: name:;
  // Project library name.
  constant slot project-library-name :: <string>,
    required-init-keyword: library-name:;
  // Type of target file to build
  constant slot project-target-type :: <target-type>,
    required-init-keyword: target-type:;
  // Name of the start function
  constant slot project-start-function-name :: false-or(<string>),
    required-init-keyword: start-function-name:;
  // Path to directory in which to put project.
  constant slot project-directory :: <directory-locator>,
    required-init-keyword: directory:;
  // Keywords used only in the project file's headers.
  constant slot project-main-keywords :: <table>
    = make(<table>), // of: <symbol> -> (<vector> of: <string>)
    init-keyword: main-keywords:;
  // Keywords present in all "Files:" in the project, plus the project file.
  constant slot project-common-keywords :: <table>
    = make(<table>), // of: <symbol> -> (<vector> of: <string>)
    init-keyword: common-keywords:;
  // The "Module:" keyword line for non-library/module-definition files.
  constant slot project-module-keywords :: <table>
    = make(<table>), // of: <symbol> -> (<vector> of: <string>)
    init-keyword: module-keywords:;
end class;

define function project-location
    (description :: <project-description>)
 => (location :: <file-locator>)
  let directory = description.project-directory;
  make(<file-locator>,
       directory: directory,
       base: description.project-name,
       extension: $user-project-suffix);
end function project-location;


/* #### TEMPLATE PROJECTS ###########################
define sealed class <template-project-description> (<project-description>)
  // Path to project file to use as template.
  slot project-template-source :: <string>, // <pathname>/<file-locator>?
    required-init-keyword: template-source:;
end class;
#### TEMPLATE PROJECTS ########################### */

define sealed class <custom-project-description> (<project-description>)
/* --- Not used yet
  slot project-module-names :: <vector> /* of: <string> */,
    required-init-keyword: module-names:;
*/
  // Project module name.
  constant slot project-module-name :: <string>,
    required-init-keyword: module-name:;
  constant slot project-used-libraries :: <sequence>
      /* of: {<choice>:
                object: = library-name :: <string>,
                children: = used-modules :: <vector> of: <string>
             } */,
    required-init-keyword: used-libraries:;
  constant slot project-copy-templates? :: <boolean>,
    required-init-keyword: copy-templates?:;
end class;

/// Motley
//--- hughg, 1998/09/18: Urk!  This Motley class and code decides where to
//--- put its various extra files in a rather different way from existing
//--- code.  Humph.  Dunno which way round is better, but it's messily
//--- inconsistent (as usual in this code!).
define sealed class <motley-project-description>
    (<custom-project-description>)
  constant slot project-motley-spec-file :: <file-locator>,
    init-value: as(<file-locator>, "type-library.spec"),
    init-keyword: motley-spec-file:;
  constant slot project-motley-type-library :: <file-locator>,
    required-init-keyword: motley-type-library:;
  constant slot project-motley-module-name :: <string>,
    init-value: "type-library-module",
    init-keyword: motley-module-name:;
  constant slot project-motley-module-file :: <file-locator>,
    init-value: as(<file-locator>, "type-library-module.dylan"),
    init-keyword: motley-module-file:;
  constant slot project-motley-generate :: <symbol>,
    required-init-keyword: motley-generate:;
  constant slot project-motley-stub-file :: <file-locator>,
    init-value: as(<file-locator>, "stubs.dylan"),
    init-keyword: motley-stub-file:;
  constant slot project-motley-interfaces :: false-or(<sequence>) = #f,
    init-keyword: motley-interfaces:;
  constant slot project-motley-server-suffix
      :: false-or(<string>) = #f, init-keyword: motley-server-suffix:;
  constant slot project-motley-client-suffix
      :: false-or(<string>) = #f, init-keyword: motley-client-suffix:;
end class <motley-project-description>;

/// Scepter
define sealed class <scepter-project-description>
    (<custom-project-description>)
  slot project-scepter-idl-file :: false-or(<file-locator>),
    required-init-keyword: scepter-idl-file:;
  constant slot project-scepter-copy-idl-file? :: <boolean>,
    required-init-keyword: scepter-copy-idl-file?:;
  constant slot project-scepter-client
      :: false-or(<scepter-using-project-description>),
    init-value: #f,
    init-keyword: scepter-client:;
  constant slot project-scepter-server
      :: false-or(<scepter-using-project-description>),
    init-value: #f,
    init-keyword: scepter-server:;
end class <scepter-project-description>;

define method initialize
    (description :: <scepter-project-description>,
     #key, #all-keys)
  next-method();
  let client = description.project-scepter-client;
  when (client)
    client.project-scepter-parent-description := description;
  end;
  let server = description.project-scepter-server;
  when (server)
    server.project-scepter-parent-description := description;
  end;
end method initialize;

define class <scepter-using-project-description>
    (<custom-project-description>)
  // This locator is relative to the project-directory.
  constant slot project-scepter-spec-file :: <file-locator>,
    init-keyword: scepter-spec-file:;
/* --- hughg, 1998/09/22: We never actually use this, yet.
  constant slot project-scepter-using-type :: <scepter-using-type>,
    required-init-keyword: scepter-using-type:;
*/
  constant slot project-scepter-use :: <scepter-use-type>,
    required-init-keyword: scepter-use:;
  constant slot project-scepter-advanced-settings
      :: <scepter-advanced-settings>,
    required-init-keyword: scepter-advanced-settings:;
  slot project-scepter-parent-description
      :: false-or(<scepter-project-description>),
    init-value: #f;
end class <scepter-using-project-description>;

// This takes a locator indicating a "parent" directory (we just call
// LOCATOR-DIRECTORY-PATH on this, so it may be a directory locator or a
// locator of a file in that directory) and the type of "using" project
// (#"client" or #"server") and returns a locator to the directory for
// the "using" project (by adding "client" or "server" as a subdirectory
// of the PARENT-LOCATION).
define function scepter-using-project-directory
    (directory :: <directory-locator>, using-type :: <scepter-using-type>)
 => (using-directory :: <directory-locator>)
  let type-string = as(<string>, using-type);
  subdirectory-locator(directory, type-string)
end function scepter-using-project-directory;

define method make
    (class == <scepter-using-project-description>,
     #rest initargs,
     #key name :: <string>,
          library-name :: <string>,
          directory :: <directory-locator>,
          main-keywords :: <table>,
          module-name :: <string>,
          module-keywords :: <table>,
          scepter-using-type :: <scepter-using-type>,
     #all-keys)
 => (supd :: <scepter-using-project-description>)
  let type-string :: <string> = as(<string>, scepter-using-type);
  let type-string-suffix :: <string> = concatenate("-", type-string);
  // Add a subdirectory onto the parent's directory.
  let using-directory
    = scepter-using-project-directory(directory, scepter-using-type);
  // Make the locator for the spec file, which is relative to the directory.
  let scepter-spec-file
    = make(<file-locator>, base: name, extension: $spec-suffix);
  // Fix up the "Library:" header in the project file.
  let using-library-name :: <string>
    = concatenate(library-name, type-string-suffix);
  let using-main-keywords = shallow-copy(main-keywords);
  using-main-keywords[#"Library"] := using-library-name;
  // Fix up the "Module:" header as well as the module definition's name.
  let using-module-name :: <string>
    = concatenate(module-name, type-string-suffix);
  let using-module-keywords = shallow-copy(module-keywords);
  using-module-keywords[#"Module"] := using-module-name;
  // Override some keywords; apart from that, we'll be passed the same initargs
  // as our parent project.
  apply(next-method, class,
        name:              concatenate(name, type-string-suffix),
        library-name:      using-library-name,
        directory:         using-directory,
        main-keywords:     using-main-keywords,
        module-name:       using-module-name,
        module-keywords:   using-module-keywords,
        scepter-spec-file: scepter-spec-file,
        initargs)
end method make;



/// ----------------------------------------------------------------------
/// PROJECT DESCRIPTION GENERATION

define function collect-used-libraries
    (library-groups :: <sequence>) // of: <choice>
 => (libraries :: <sequence>) // of: <choice>
  let libraries = #();
  for (group :: <choice> in library-groups)
    when (group.choice-included?)
      for (library :: <choice> in group.choice-children)
        when (library.choice-included?)
          libraries := pair(library, libraries);
        end;
      end;
    end;
  end;
  libraries
end function collect-used-libraries;

define function libraries-from-project-type
    (wizard :: <project-wizard-frame>)
 => (libraries :: <sequence> /* of: <choice> */)
  let libraries :: <list> = #();
  local method add-new-library-choice (library-name :: <symbol>) => ()
    let choice = make-repository-choice($project-libraries[library-name]);
    libraries := add!(libraries, choice)
  end method;
  let project-type = gadget-value(wizard.project-type-pane);
  let gui-type
    = (gadget-value(wizard.project-library-use-type) == #"simple")
        & gadget-value(wizard.simple-gui-option);
  select (project-type)
    #"ole-server" => // Use DUIM-OLE-Server or OLE-Server
      select (gui-type)
        #"none", #f => #f; // Do nothing ---*** add "you need a gui" file?
        #"duim"     => add-new-library-choice(#"duim-ole-server");
        #"win32"    => add-new-library-choice(#"ole-server");
      end;
      add-new-library-choice(#"com");
      add-new-library-choice(#"ole");
      add-new-library-choice(#"c-ffi");
    #"ole-control" => // Use DUIM-OLE-Control or OLE-Control-Framework
      select (gui-type)
        #"none", #f => #f; // Do nothing ---*** add "you need a gui" file?
        #"duim"     => add-new-library-choice(#"duim-ole-control");
        #"win32"    => add-new-library-choice(#"ole-control-framework");
      end;
      add-new-library-choice(#"com");
      add-new-library-choice(#"ole");
      add-new-library-choice(#"c-ffi");
    #"motley" =>
      add-new-library-choice(#"common-dylan");
      add-new-library-choice(#"ole-automation");
      add-new-library-choice(#"c-ffi");
    #"scepter" =>
      add-new-library-choice(#"dylan-orb");
    otherwise => #f; // Do nothing.
  end;
  libraries
end function libraries-from-project-type;

define function libraries-from-use-path
    (wizard :: <project-wizard-frame>)
 => (libraries :: <sequence> /* of: <choice> */)
  select (gadget-value(wizard.project-library-use-type))
    #"minimal" =>
      let libraries
        = vector(make-repository-choice($project-libraries[#"common-dylan"]));
      libraries;
    #"simple" =>
      let library-group-choices = repository-as-choices();
      // NOTE: The order in which these inclusions and exclusions are done
      // below is important, as some later ones override earlier ones.
      ////// IO and System
      for (option in gadget-value(wizard.simple-io-and-system-options))
        library-choice-included?
          (library-group-choices, option, option) := #t;
      end;
      ////// Other enhanced-plus options
      // Network
      when (gadget-value(wizard.network-option))
        library-choice-included?
          (library-group-choices, #"network", #"network") := #t;
      end;
      // OLE-Automation
      // (We only want some of the OLE libraries at this point.)
      library-group-choice-included?
        (library-group-choices, #"ole", recursive?: #t) := #f;
      when (gadget-value(wizard.ole-automation-option))
        library-choice-included?
          (library-group-choices, #"ole", #"ole-automation") := #t;
      end;
      ////// GUI Support
      let gui-type = gadget-value(wizard.simple-gui-option);
      select (gui-type)
        #"none"  => #f; // Do nothing
        #"duim"  => library-group-choice-included?
                      (library-group-choices, #"duim") := #t;
                    when (gadget-value(wizard.simple-gui-add-win32-option))
                      library-group-choice-included?
                        (library-group-choices, #"win32") := #t;
                    end;
        #"win32" => library-group-choice-included?
                      (library-group-choices, #"win32") := #t;
      end;
      ////// Database Support
      // (We only want some of the libraries from the Databases group.)
      let databases-option = gadget-value(wizard.simple-databases-option);
      unless (databases-option == #"none")
        library-group-choice-included?
          (library-group-choices, #"databases", recursive?: #t) := #f;
        library-choice-included?
          (library-group-choices, #"databases", databases-option) := #t;
      end;
      collect-used-libraries(library-group-choices);
    #"custom" =>
      let library-group-choices
        = gadget-items(wizard.project-library-group-chooser.choice-list-pane);
      collect-used-libraries(library-group-choices);
    otherwise =>
      // Should never happen, unless DUIM does something wrong or I make
      // an inconsistent change to my code.
      error("Project Wizard is trying to make a project using no libraries.");
  end
end function libraries-from-use-path;

define function get-used-libraries
    (wizard :: <project-wizard-frame>)
 => (libraries :: <sequence> /* of: <choice> */)
  let basic-libraries = libraries-from-use-path(wizard);
  let project-type-libraries = libraries-from-project-type(wizard);

  // Combine the two sequences of library choices, merging the lists of
  // libraries/modules and by logical-OR-ing the "included?" flag for
  // libraries and modules.
  union(basic-libraries, project-type-libraries,
        test: rcurry(union-choice-inclusion!, test: \=, recursive?: #t))
end function;

define macro add-all!
  { add-all!(?list:expression) } => { ?list }
  { add-all!(?list:expression, ?:expression, ?rest:*) }
    => { add!(add-all!(?list, ?rest), ?expression) }
  { add-all!(?list:expression, ?key:expression ?value:expression, ?rest:*) }
    => { add-all!(?list, ?key, ?value, ?rest) }
end macro add-all!;

define function make-description-from-wizard-dialog
    (wizard :: <project-wizard-frame>)
 => (description :: <project-description>);
  local method value (slot :: <function>)
          gadget-value(wizard.slot)
        end method,
        method maybe-insert-slot-value
                   (table :: <table>, key, slot :: <function>, #key default)
          let val = value(slot);
          if (val)
            if (instance?(val, <collection>) & empty?(val))
              when (default) table[key] := default; end;
            else
              table[key] := val;
            end;
          else
            when (default) table[key] := default; end;
          end;
        end method;
  let class = <custom-project-description>;
/* #### TEMPLATE PROJECTS ###########################
  let class
    = if (value(project-custom-type-pane))
        <custom-project-description>;
      else
        <template-project-description>;
      end;
#### TEMPLATE PROJECTS ########################### */
  let main-keywords = make(<table>, size: 8);
  // Project name
  let name = value(project-name-pane);
  main-keywords[#"Library"] := name;
  // ---*** What if coercion fails?
  // Project location
  let directory
    = as(<directory-locator>,
         value(compose(file-browse-text-pane, project-location-pane)));
  // Library and module names
  let library-name = wizard.project-library-name;
  when (empty?(library-name))
    library-name := name;
  end;
  let module-name = wizard.project-module-name;
  when (empty?(module-name))
    module-name := name;
  end;
  // Target type
  let target-type = value(project-target-type-pane);
  // Start Function
  let start-function-name
    = if (target-type = #"exe") wizard.project-start-function-name
      else #f end;
  // Build settings
  main-keywords[#"Compilation-Mode"] := wizard.project-compilation-mode;
  main-keywords[#"Major-Version"] := wizard.project-major-version;
  main-keywords[#"Minor-Version"] := wizard.project-minor-version;
  main-keywords[#"Target-Type"]
    := if (target-type == #"dll") #"dll" else #"executable" end;
  when (wizard.project-win32-subsystem == #"gui")
    // The default at link-time is non-gui, for which we do nothing.
    main-keywords[#"Linker-Options"] := vector("$(guilflags)");
  end;
  // Debug settings
  main-keywords[#"Start-Function"] := start-function-name;
  // Libraries Used
  let used-libraries = get-used-libraries(wizard);
  // Copy template code?
  let use-type = value(project-library-use-type);
  let copy-templates? = value(project-copy-templates?-pane);
  // Set up common keywords.
  let common-keywords = make(<table>, size: 4);
  maybe-insert-slot-value
    (common-keywords, #"Author", project-author-pane);
  maybe-insert-slot-value
    (common-keywords, #"Synopsis", project-synopsis-pane);
  maybe-insert-slot-value
    (common-keywords, #"Copyright", project-copyright-pane);
  maybe-insert-slot-value
    (common-keywords, #"Version", project-version-pane);
  // Set up module keyword.
  let module-keywords = make(<table>, size: 1);
  module-keywords[#"Module"] := module-name;

  let keywords =
    list(
      name: name, directory: directory,
/* #### TEMPLATE PROJECTS ###########################
      template-source:
        gadget-value
          (wizard.project-template-location-pane.file-browse-text-pane),
#### TEMPLATE PROJECTS ########################### */
      library-name: library-name, module-name: module-name,
      target-type: target-type, start-function-name: start-function-name,
      copy-templates?: copy-templates?, used-libraries: used-libraries,
      main-keywords: main-keywords, common-keywords: common-keywords,
      module-keywords: module-keywords
    );

  let project-type = wizard.project-type-pane.gadget-value;
  //--- hughg, 1998/09/18: I feel this should be rearranged to be handled
  //--- by dispatch on the "class" variable.
  select (project-type)
    #"motley" =>
      let motley-type-library
        = as(<file-locator>,
             value(compose(file-browse-text-pane,
                           project-motley-location-pane)));
      let motley-generate = project-motley-stubs-pane.value;
      let interfaces =
        select (project-motley-interfaces?-pane.value)
          #"all" => #();
          #"selected" => project-motley-interfaces-pane.value;
        end select;
      let server-suffix = motley-server-suffix.value;
      let client-suffix = wizard.motley-client-suffix.gadget-enabled? &
                          motley-client-suffix.value;
      class := <motley-project-description>;
      keywords := add-all!(keywords,
        motley-type-library: motley-type-library,
        motley-generate: motley-generate,
        motley-interfaces: interfaces,
        motley-server-suffix: server-suffix, motley-client-suffix: client-suffix
      );
    #"scepter" =>
      let scepter-generate = value(scepter-generate-pane);
      let client :: false-or(<scepter-using-project-description>)
        = when (member?(#"client", scepter-generate))
            apply(make, <scepter-using-project-description>,
                  scepter-using-type: #"client",
                  scepter-use: value(scepter-client-use-pane),
                  scepter-advanced-settings:
                    second(gadget-id(wizard.scepter-client-advanced-settings-button)),
                  keywords);
          end;
      let server :: false-or(<scepter-using-project-description>)
        = when (member?(#"server", scepter-generate))
            apply(make, <scepter-using-project-description>,
                  scepter-using-type: #"server",
                  scepter-use: value(scepter-server-use-pane),
                  scepter-advanced-settings:
                    second(gadget-id(wizard.scepter-server-advanced-settings-button)),
                  keywords);
          end;
      class := <scepter-project-description>;
      keywords := add-all!(keywords,
        scepter-idl-file:
          value(scepter-use-idl-file?-pane)
          & as(<file-locator>,
               value(compose(file-browse-text-pane, scepter-idl-file-pane))),
        scepter-copy-idl-file?: value(scepter-copy-idl-file?-pane),
        scepter-client: client,
        scepter-server: server
      );
    otherwise => #f; // Do nothing.
  end;

  // Return the description
  apply(make, class, keywords);
end function;


/// ----------------------------------------------------------------------
/// PROJECT FILE GENERATION

// ---*** ISSUES
// - More useful handling of file-creation errors, e.g., offer to write
//   with a different name.

define constant $project-file-header-order :: <vector>
    = #[#"Format-Version", // This must ALWAYS be first.
        #"Library", #"Module",
        #"Synopsis", #"Author", #"Copyright", #"Version",
        #"Major-Version", #"Minor-Version",
        #"Files", #"C-Header-Files", #"C-Source-Files",
        #"C++-Source-Files", #"RC-Files",
        #"C-Libraries",        #"C-Object-Files",
        #"Linker-Options", #"Base-Address", #"Executable",
        #"Debug-Command", #"Debug-Arguments"];

define constant $scepter-spec-file-header-order :: <vector>
    = #[#"Origin", // This must ALWAYS be first.
        #"IDL-File",
        #"Protocol", #"Stubs", #"Skeletons"];

define sealed class <write-project-failed-error> (<error>)
  sealed constant slot files-written :: <stretchy-vector> /* of: <file-locator> */,
    required-init-keyword: files-written:;
  sealed constant slot condition :: false-or(<condition>) = #f,
    init-keyword: condition:;
end class;

// This will return #f only if the project cannot be created.
define sealed generic write-project-from-description
    (description :: <project-description>,
     #key files-written :: false-or(<stretchy-vector>))
 => (project-locations :: <sequence> /* of: <file-locator> */,
     files-written :: <stretchy-vector> /* of: <file-locator> */);

define sealed method write-project-from-description
    (description :: <project-description>,
     #key files-written: files-written-already :: false-or(<stretchy-vector>))
 => (project-locations :: <sequence>,
     files-written :: <stretchy-vector>);
  let files-written = files-written-already | make(<stretchy-vector>);
  block ()
    let project-locations
      = do-write-project-from-description
          (description, files-written);
    values(project-locations, files-written)
  exception (c :: type-union(<stream-error>, <file-error>, <file-system-error>))
    error(make(<write-project-failed-error>, files-written: files-written,
               condition: c));
  end
end method;


define sealed generic do-write-project-from-description
    (description :: <project-description>,
     files-written :: <stretchy-vector>)
 => (project-locations :: <sequence> /* of: <file-locator> */);

// ---*** What if the destination directory doesn't exist?

/* #### TEMPLATE PROJECTS ###########################
define constant $define-library-string :: <byte-string> = "define library ";
define constant $define-library-string-size :: <integer> = 14;

define method do-write-project-from-description
    (description :: <template-project-description>,
     files-written :: <stretchy-vector>)
 => (project-locations :: <sequence>)
  let to-name = description.project-name;
  // ---*** What if locator conversion fails?
  let from = as(<file-locator>, description.project-template-source);
  let to-directory = description.project-directory;
  let to-loc = description.project-location;
  let files = #f;

  // Read'n'write the project file, overriding the name and maj/min-version.
  with-open-interchange-file
      (from-stream = from, from-headers = #f,
       direction: #"input" /*, if-exists: #"signal"*/)
    let main-keywords = description.project-main-keywords;
    files := element(main-keywords, #"Files", default: #f);
    //---*** What about the file-format version?
    for (key in #[#"Library", #"Major-Version", #"Minor-Version"])
      let new-value = main-keywords[key];
      when (new-value)
        from-headers[key] := new-value;
      end;
    end;
    write-interchange-file(to-loc, from-headers, #f,
                           header-order: $project-file-header-order);
    add!(files-written, to-loc);
  end;

  // Read'n'write each source file, overriding any lines containing
  //   "define library <old-name>" to the new name.
  let library-renamed? = #f;
  local method copy-renaming-library
            (from :: <file-locator>, to :: <file-locator>)
          with-open-interchange-file
              (from-stream = from, from-headers = #f, direction: #"input")
            with-open-interchange-file
                (to-stream = to, to-headers = from-headers,
                 direction: #"output", if-exists: #"signal",
                 header-order: $project-file-header-order)
              let (line, nl?) = values(#f, #f);
              while (begin
                       let (_line, _nl?)
                         = read-line(from-stream, on-end-of-stream: #f);
                       line := _line;
                       nl? := _nl?;
                       line
                     end)
                unless (library-renamed?)
                  // Check if we need to substitute the library name.
                  let pos = subsequence-position(line, $define-library-string);
                  when (pos)
                    // Replace the rest of the line with our new name.
                    // We include the last ' ' to ensure the replacement
                    // start index is within line.
                    line := replace-subsequence!
                              (line, concatenate(" ", to-name),
                               start: pos + $define-library-string-size);
                    library-renamed? := #t;
                  end;
                end;
                write(to-stream, line);
                when (nl?) new-line(to-stream); end;
              end;
            end;
          end;
        end method;

  when (files)
    // If the "from" project had no files, that's a bug of sorts -- maybe
    // we should notify the user?
    let from-directory = from.locator-directory;
    let to-directory   = to.locator-directory;
    for (file in files)
      let from-location
        = make(<file-locator>,
               directory: from-directory,
               base:      file,
               extension: $dylan-source-suffix);
      let to-location
        = make(<file-locator>,
               directory: to-directory,
               base:      file,
               extension: $dylan-source-suffix);
      copy-renaming-library(from-location, to-location);
      add!(files-written, to-location);
    end;
  end;

  // ---*** What if we're about to copy over (an) existing file(s)?  Have
  // to delete all the project files we just copied.

  vector(to-loc)
end method;
#### TEMPLATE PROJECTS ########################### */

define method copy-library-templates
    (description :: <custom-project-description>,
     files-written :: <stretchy-vector>)
 => (template-file-names :: <sequence> /* of: <string> */,
     real-templates-copied? :: <boolean>)
  let template-files-written = make(<stretchy-vector>);
  let real-templates-copied? :: <boolean> = #f;
  let templates-directory = release-source-templates-directory();
  if (templates-directory)
    let name = description.project-name;
    let directory = description.project-directory;
    let main-keywords = description.project-main-keywords;
    let common-keywords = description.project-common-keywords;
    let module-keywords = description.project-module-keywords;
    let module-name = description.project-module-name;
    let libraries = description.project-used-libraries;

    local
      // Keep track of the base names of the files we create or copy here,
      // so they can be added to the project's "files:" entry.
      method note-file-written (location :: <file-locator>) => ()
        add!(files-written, location);
        add!(template-files-written, locator-base(location));
      end,
      // Copy an interchange-format file from one locator to another,
      // overriding/inserting the "Module:" header line we want.
      method copy-changing-module
          (from :: <file-locator>, to :: <file-locator>) => ()
/* ---*** with-open-interchange-file doesn't support input yet, so
// all we can do is copy from a headerless file.
        with-open-interchange-file
            (from-stream = from, from-headers = #f, direction: #"input")
          from-headers[#"Module"]
            := element(module-headers, #"Module", default: #f);
*/
        with-open-file (from-stream = from, direction: #"input")
          with-open-interchange-file
              (to-stream = to,
               to-headers = vector(common-keywords, module-keywords),
               direction: #"output", if-exists: #"signal",
               header-order: $project-file-header-order)
            block (done)
              while (#t)
                let (line, nl?)
                  = read-line(from-stream, on-end-of-stream: #f);
                unless (line) done() end;
                write(to-stream, line);
                when (nl?) new-line(to-stream); end;
              end;
            end;
          end;
        end;
      end,
      // Copy all the dylan files in the directory to the project dir.
      method copy-template-source
          (from-directory :: <directory-locator>,
           to-directory :: <directory-locator>)
       => ()
        do-directory
          (method (directory-name :: <pathname>, file-name :: <string>, file-type :: <file-type>)
             when (file-type == #"file")
               let file-locator
                 = make(<file-locator>,
                        directory: from-directory,
                        name:      file-name);
               when (case-insensitive-equal(locator-extension(file-locator),
                                            $dylan-source-suffix))
                 let to-locator
                   = make(<file-locator>,
                          directory: to-directory,
                          name:      file-name);
                 copy-changing-module(file-locator, to-locator);
                 note-file-written(to-locator);
               end;
             end;
           end,
           from-directory);
        real-templates-copied? := #t;
      end;

    // Create "application-info" file, which defines constants for templates.
    let project-info-location
      = make(<file-locator>,
             directory: directory,
             base:      concatenate(name, "-info"),
             extension: $dylan-source-suffix);
    with-open-interchange-file
        (stream = project-info-location,
         headers = vector(common-keywords, module-keywords),
         direction: #"output", if-exists: #"signal",
         header-order: $project-file-header-order)
      format(stream,
  "define constant $application-name :: <byte-string> = \"%s\";\n",
  name);
  /*
      format(stream,
  "define constant $application-author :: <byte-string> = \"%s\";\n",
  element(common-keywords, #"Author", default: "Unknown"));
      format(stream,
  "define constant $application-copyright :: <byte-string> = \"%s\";\n",
  element(common-keywords, #"Copyright", default: "No copyright."));
  */
      format(stream,
  "define constant $application-major-version :: <byte-string> = \"%s\";\n",
  element(main-keywords, #"Major-Version", default: "0"));
      format(stream,
  "define constant $application-minor-version :: <byte-string> = \"%s\";\n",
  element(main-keywords, #"Minor-Version", default: "0"));
      format(stream,
  "\ndefine method application-full-name () => (full-name :: <byte-string>)\n"
  "  concatenate($application-name, \" Version \",\n"
  "              $application-major-version, \".\",\n"
  "              $application-minor-version)\n"
  "end method application-full-name;\n"
             );
    end;
    note-file-written(project-info-location);

    // Given the selected libraries, choose for files to copy (from repository?)
    // Copy files from release-templates-directory().
    // - streams
    // - Console EXE (command-line/env-vars, formatting, maybe file-dir stuff?)
    // - GUI apps: DUIM, DUIM+Win32, Win32-without-DUIM
    //   ^^^ If these all implement the same GUI differently, the DB examples
    //       could use that code.
    // - OLE server: DUIM, Win32
    // - OLE control: DUIM, Win32
    //   ^^^ For server + control, can we integrate with DUIM-vs-Win32 & DB?
    // - OLE Automation: no example yet.
    // - DB: SQL-ODBC, SQL(?), ODBC-FFI(?)
    // ---*** Could do with a more systematic way to decide which template
    // code to copy across.
    // ---*** For all of these choices, the user might have used the group
    // but excluded libraries (or used libraries but excluded modules).  To
    // do this properly, we should check for that, but this'll do for now.

    // ---*** We only have a DUIM example for 1.0 Beta 2.
    when (find-repository-choice(libraries, #"duim", if-not-exists: #f)
            & ~find-repository-choice(libraries, #"ole", if-not-exists: #f))
      // ---*** Making this locator should be handled in the release-info
      // library.
      let duim-template-directory
        = subdirectory-locator(as(<directory-locator>, templates-directory),
                               "duim");
      copy-template-source(duim-template-directory, directory);
    end;

    // Return filenames as <string>s, stripped of path and extension;
    // plus whether we actually copied any files (as opposed to just
    // creating the "-info" file).
    // ---*** Maybe this should also return binding-names for module exports?
    values(template-files-written, real-templates-copied?)
  else
    values(#[], #f)
  end
end method copy-library-templates;

define method write-main-files
    (description :: <custom-project-description>,
     files-written :: <stretchy-vector>,
     #key start-template? :: <boolean>)
 => ()
  let name = description.project-name;
  let directory = description.project-directory;
  let start-function-name = description.project-start-function-name;
  let common-keywords = description.project-common-keywords;
  let module-keywords = description.project-module-keywords;

  // Create main source file(s).
  // ---*** Do we want them to be called
  //   "[<project>-]{implemetation,interface}.dylan"
  // or just one file called
  //   "<project>.dylan"
  // ?  For now, I'm doing the latter, as it's simplest.
  let main-file-location
    = make(<file-locator>,
           directory: directory,
           base:      name,
           extension: $dylan-source-suffix);
  //---*** hughg, 1998/03/20: Make this "define method", as "define
  //---*** function"s aren't found as initial breakpoints just now.
  // For EXE projects we provide a start function, for DLLs we don't.
  with-open-interchange-file
      (stream = main-file-location,
       headers = vector(common-keywords, module-keywords),
       direction: #"output", if-exists: #"signal",
       header-order: $project-file-header-order)
    if (description.project-target-type == #"exe" & start-function-name)
      format(stream, "define method %s () => ()\n", start-function-name);
      format(stream, if (start-template?)
                       "  start-template();\n"
                     else
                       "  // Your program starts here...\n"
                     end);
      format(stream, "end method %s;\n", start-function-name);
      format(stream, "\nbegin\n  %s();\nend;\n", start-function-name);
    else
      write-line(stream, "begin");
      write-line(stream, "  // Library initialization starts here ...");
      when (start-template?)
        write-line(stream, "  start-template();")
      end;
      write-line(stream, "end;");
    end;
  end with-open-interchange-file;
  add!(files-written, main-file-location);
end method write-main-files;

define method do-write-project-from-description
    (description :: <custom-project-description>,
     files-written :: <stretchy-vector>)
 => (project-locations :: <sequence>)
  // Extract common info.
  let name                = description.project-name;
  let directory           = description.project-directory;
  let location            = description.project-location;
  let library-name        = description.project-library-name;
  let module-name         = description.project-module-name;
  let common-keywords     = description.project-common-keywords;
  let module-keywords     = description.project-module-keywords;
  let dylan-user-keywords = make(<table>);
  let include-templates?  = description.project-copy-templates?;
  //---*** hughg, 1998/09/18: We should really rearrange this function into
  //---*** some "functional protocol" so that project-type-specific stuff
  //---*** can be done in separate methods which are combined correctly
  //---*** simply by dispatch.
  let motley? = instance?(description, <motley-project-description>);
  let motley-module-name = motley? & description.project-motley-module-name;
  let scepter-using?
    = instance?(description, <scepter-using-project-description>);
  dylan-user-keywords[#"Module"] := "dylan-user";

  // Copy template code
  let (template-file-names, real-templates-copied?)
    = if (include-templates?)
        copy-library-templates(description, files-written);
      else
        values(#[], #f)
      end;

  // Create project file.
  let main-keywords = description.project-main-keywords;
  // This version of the Wizard generates project files of format
  // version "2".
  main-keywords[#"Format-Version"] := "2";
  // ---*** This should be done last, and be based on 'files-written'.
  main-keywords[#"Files"]
    := concatenate(vector("library", "module"), template-file-names,
                   vector(name));
  //---*** hughg, 1998/09/18: This Motley stuff should probably be set up
  //---*** at the stage of determining the init-keywords, in MAKE-DESCRIPTION-
  //---*** FROM-WIZARD-DIALOG.
  when (motley?)
    main-keywords[#"Other-files"]
      := as(<string>, description.project-motley-spec-file);
  end;
  //--- hughg, 1998/09/18: Err, but this Scepter stuff hackily shouldn't,
  //--- because the same projects-keywords <table> may be used twice,
  //--- once for the client and once for the server!  Yuk.
  let scepter-used-namespace-name :: false-or(<string>) = #f;
  when (scepter-using?)
    //---*** hughg, 1998/09/22: Ideally this would be a relative pathname,
    //---*** but I don't know how to make relative locators from absolutes.
    main-keywords[#"Other-files"]
      := as(<string>, description.project-scepter-spec-file);
    let idl-name
      = locator-base(description.project-scepter-parent-description.
                       project-scepter-idl-file);
    scepter-used-namespace-name
      := concatenate(idl-name, "-",
                     as(<string>, description.project-scepter-use));
    // Advanced settings
    let settings
      = description.project-scepter-advanced-settings;
    local method add-option
        (options :: <string>, new-option :: <string>)
     => (options :: <string>)
      if (empty?(options))
        new-option
      else
        concatenate(options, " ", new-option)
      end if
    end method;
    let debug-arguments :: <string> = "";
    when (settings.scepter-advanced-trace?)
      debug-arguments := add-option(debug-arguments, "-ORBtrace");
    end;
    when (settings.scepter-advanced-no-co-location?)
      debug-arguments := add-option(debug-arguments, "-ORBno-co-location");
    end;
    when (settings.scepter-advanced-debug?)
      debug-arguments := add-option(debug-arguments, "-ORBdebug");
    end;
    let port = settings.scepter-advanced-port;
    when (port)
      debug-arguments := add-option(debug-arguments,
                                    format-to-string("-ORBport %d", port));
    end;
    unless (empty?(debug-arguments))
      main-keywords[#"Debug-Arguments"] := debug-arguments;
    end;
  end;
  write-interchange-file
    (location, vector(main-keywords, common-keywords), #f,
     header-order: $project-file-header-order,
     if-exists: #"signal");
  add!(files-written, location);

  // Create library file.
  let libraries = description.project-used-libraries;
  let library-location
    = make(<file-locator>,
           directory: directory,
           base:      "library",
           extension: $dylan-source-suffix);
  with-open-interchange-file
      (stream = library-location,
       headers = vector(dylan-user-keywords, common-keywords),
       direction: #"output", if-exists: #"signal",
       header-order: $project-file-header-order)
    format(stream, "define library %s\n", library-name);
    for (library-choice in libraries)
      format(stream, "  use %s;\n", library-choice.choice-object.repository-object-id);
    end;
    when (scepter-using?)
      format(stream, "  use %s;\n", scepter-used-namespace-name);
    end;
    format(stream, "\n  // Add any more module exports here.\n");
    format(stream, "  export %s;\n", module-name);
    format(stream, "end library %s;\n", library-name);
  end;
  add!(files-written, library-location);

  // Create module file.
  // ---*** What about separate interface and implementation modules?
  let module-location
    = make(<file-locator>,
           directory: directory,
           base:      "module",
           extension: $dylan-source-suffix);
  with-open-interchange-file
      (stream = module-location,
       headers = vector(dylan-user-keywords, common-keywords),
       direction: #"output", if-exists: #"signal",
       header-order: $project-file-header-order)
    format(stream, "define module %s\n", module-name);
    for (library-choice in libraries)
      for (module-choice in library-choice.choice-children)
        let module-name = module-choice.choice-object;
        when (module-choice.choice-included?)
          format(stream, "  use %s;\n", module-name);
        end;
      end;
    end;
    when (motley?)
      format(stream, "  use %s, export: all;\n\n", motley-module-name);
    end;
    when (scepter-using?)
      format(stream, "  use %s;\n", scepter-used-namespace-name);
    end;
    format(stream, "\n  // Add binding exports here.\n\n");
    format(stream, "end module %s;\n", module-name);
  end;
  add!(files-written, module-location);

  // Create Motley spec file, if necessary.
  when (motley?)
    //---*** hughg, 1998/09/18: This should use WRITE-INTERCHANGE-FILE
    let motley-spec-location
      = merge-locators(as(<file-locator>, description.project-motley-spec-file),
                       directory);
    with-open-file (spec = motley-spec-location,
                    direction: #"output", if-exists: #"signal")
      format(spec, "Origin: COM-type-library\n");
      format(spec, "Type-library: %s\n"
                   "Module: %s\n"
                   "Module-file: %s\n"
                   "Generate: %s\n"
                   "Stub-file: %s\n",
             as(<string>, description.project-motley-type-library),
             description.project-motley-module-name,
             as(<string>, description.project-motley-module-file),
             as(<string>, description.project-motley-generate),
             as(<string>, description.project-motley-stub-file));
      select (description.project-motley-generate)
        #"vtable-interfaces", #"dual-interfaces" =>
          if (description.project-motley-server-suffix &
              description.project-motley-server-suffix ~= "")
            format(spec, "Server-suffix: %s\n",
                   description.project-motley-server-suffix);
          end if;
          if (description.project-motley-client-suffix)
            format(spec, "Client-suffix: %s\n",
                   description.project-motley-client-suffix);
          end if;
        otherwise => #f;
      end select;
      let prefix = "Interfaces: ";
      for (iface in description.project-motley-interfaces | #())
        format(spec, "%s%s\n", prefix, iface);
        prefix := "            ";
      end for;
    end with-open-file;
    add!(files-written, motley-spec-location);
  end;

  // Create Scepter spec file, if necessary.
  when (scepter-using?)
    let scepter-spec-location
      = merge-locators(as(<file-locator>, description.project-scepter-spec-file),
                       directory);
    let scepter-spec-keywords = make(<table>, size: 3);
    scepter-spec-keywords[#"Origin"] := "OMG-IDL";
    // Store the IDL file location relative to this project's directory.
    let idl-file
      = description.project-scepter-parent-description.
          project-scepter-idl-file;
    idl-file := relative-locator(idl-file, directory);
    scepter-spec-keywords[#"IDL-File"] := as(<string>, idl-file);
    scepter-spec-keywords[description.project-scepter-use] := "yes";
    write-interchange-file
      (scepter-spec-location, vector(scepter-spec-keywords), #f,
       header-order: $scepter-spec-file-header-order,
       if-exists: #"signal");
    add!(files-written, scepter-spec-location);
  end;

  // Write the main file(s) for the project, which the user will extend.
  write-main-files(description, files-written,
                   start-template?:
                     (include-templates? & real-templates-copied?));

  vector(location)
end method;

// Scepter "top level" "non-projects".
define method do-write-project-from-description
    (description :: <scepter-project-description>,
     files-written :: <stretchy-vector>)
 => (project-locations :: <sequence>)
  let directory = description.project-directory;
  // If the user supplied an IDL file,
  let idl-file = description.project-scepter-idl-file;
  if (idl-file)
    // Then copy the IDL file into the project directory, if required;
    let idl-file-in-project-directory
      = make(<file-locator>,
             directory: directory,
             name:      locator-name(idl-file));
    //--- Maybe we should SIMPLIFY-LOCATOR on these for the \= call?
    // Note that we *don't* add the IDL file to the list of files written
    // if it was already there, otherwise the Wizard may delete it while
    // recovering from other errors!

    // Note: the Wizard already checked that the source file exists.
    if ((idl-file ~= idl-file-in-project-directory)
          & description.project-scepter-copy-idl-file?)
      copy-file(idl-file, idl-file-in-project-directory,
                if-exists: #"signal");
      description.project-scepter-idl-file := idl-file-in-project-directory;
      add!(files-written, idl-file-in-project-directory);
    end if;
  else
    // Else write a blank IDL file and put its location in the description.
    let name = description.project-name;
    idl-file
      := make(<file-locator>,
              directory: directory,
              base:      name,
              extension: $idl-suffix);
    with-open-file (file = idl-file,
                    direction: #"output",
                    if-exists: #"signal")
      format(file, "// IDL file for project '%s'.\n\n", name);
    end;
    description.project-scepter-idl-file := idl-file;
    add!(files-written, idl-file);
  end;

  let projects = make(<stretchy-vector>);

  // Write the client project, if requested.
  let client = description.project-scepter-client;
  when (client)
    concatenate!
      (projects,
       write-project-from-description(client, files-written: files-written));
  end;

  // Write the server project, if requested.
  let server = description.project-scepter-server;
  when (server)
    concatenate!
      (projects,
       write-project-from-description(server, files-written: files-written));
  end;

  projects
end method do-write-project-from-description;

