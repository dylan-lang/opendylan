Module: user-projects
Author: Roman Budzianowski
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $user-project-suffix   = "hdp";
define constant $lid-project-suffix    = "lid";
define constant $binary-project-suffix = "ddb";

define constant $dylan-makefile = "dylanmakefile.mkf";

define constant $dylan-file-type       = #"dylan";
define constant $C-source-file-type    = #"C";
define constant $C-header-file-type    = #"h";
define constant $C-object-file-type    = #"obj";
define constant $C-libraries-file-type = #"lib";
define constant $rc-file-type          = #"rc";
define constant $ico-file-type         = #"ico";
define constant $bmp-file-type         = #"bmp";

define constant $jam-file-type         = #"jam";

define constant $include-file-types = type-union(singleton($C-header-file-type),
                                                 singleton($ico-file-type),
                                                 singleton($bmp-file-type));

define constant $project-file-type  = #"hdp";
define constant $database-file-type = #"ddb";

define constant $project-file-types = type-union(singleton($project-file-type),
                                                 singleton($database-file-type));

define class <find-project-location-restart> (<simple-restart>)
  constant slot condition-project-location :: <file-locator>,
    required-init-keyword: location:;
end;

define class <project-not-found> (<simple-error>)
  constant slot condition-project-name :: <symbol>,
    required-init-keyword: project-name:;
end;

define method make
    (class == <project-not-found>, #rest keys, #key project-name :: <symbol>)
 => (error :: <project-not-found>)
  apply(next-method, class,
        format-string: "Project %= was not found",
        format-arguments: vector(project-name),
        keys)
end method make;

define method print-object (c :: <find-project-location-restart>,
                            stream :: <stream>)
 => ();
  format(stream, "Project location restart: %s\n", c.condition-project-location);
end method;

define class <yes-or-no-condition> (<simple-condition>)
end;

define method make(condition :: subclass(<yes-or-no-condition>),
                   #rest keys, #key yes-or-no)
 => (condition :: <yes-or-no-condition>)
  apply(next-method, condition,
        format-string: yes-or-no, format-arguments: vector(), keys)
end;

define method print-object (c :: <yes-or-no-condition>,
                            stream :: <stream>)
 => ();
  let text = apply(format-to-string, c.condition-format-string,
                   c.condition-format-arguments);
  format(stream, "Yes-or-no: %s\n", text);
end method;

define class <duplicate-project-condition> (<yes-or-no-condition>)
  constant slot duplicate-project-key :: <symbol>,
    required-init-keyword: key:;
end;

define class <cannot-open-project-file-condition> (<simple-condition>)
  constant slot condition-project-file-location :: <file-locator>,
    required-init-keyword: project-file-location:;
end;

// TO DO: not correct ?
define function project-file-location(project :: <lid-project>)
 => (file :: <file-locator>)
  project.project-lid-location
end function;

// system-projects are registry projects
// at this time they can be read-only or editable if they are in
// the personal registry
// at some point we probably should get rid of personal registries
// then we can uncomment the two methods below
//
define class <system-project> (<registry-project>, <interactive-project>)

end;

define sealed domain make(singleton(<system-project>));
define sealed domain initialize(<system-project>);

/*
define method project-personal-library?(project :: <system-project>)
  #f
end;
*/

// user projects are editable, can reside anywhere on disk
//
define sealed class <user-project> (<user-disk-project-layout>,
                                    <lid-project>,
                                    <interactive-project>)
  constant slot %source-record-table = make(<string-table>);
  constant slot %subproject-files = make(<stretchy-vector>);
  slot %user-project-used-projects = make(<table>);
  slot %compiled-source-records = #();
  slot %used-projects-cache = #f;
  slot %tools-cache = #f;
  slot %project-read-only? = #f;
end;

define sealed domain make(singleton(<user-project>));
define sealed domain initialize(<user-project>);

define function %set-target-values(c, processor, operating-system)
 => (processor, operating-system);
  unless (processor & operating-system)
    let (default-processor, default-os) = default-platform-info();
    unless (processor) processor := default-processor end;
    unless (operating-system) operating-system := default-os end;
  end;
  values(processor, operating-system)
end;

define generic project-browsing-context(project :: <project>)
 => (context);

define method project-browsing-context(project :: <user-project>)
 => (context);
  project.project-execution-context
    |
    project.ensure-project-database
end;

define method project-browsing-context(project :: <system-project>)
 => (context);
  project.project-execution-context
    |
  project.ensure-project-database
end;

define method note-project-loaded (project :: <user-project>)
// fixup the library name in the hdp/makefile file
  let context = project.project-current-compilation-context;
  let name = compilation-context-library-name(context);
  let old-name = project.project-lid-library-name;
  if(name)
    let symbol-name :: <symbol> = as(<symbol>, name);
    unless(symbol-name == old-name)
      project.project-lid-library-name := symbol-name;
      debug-out(#"project-manager", "Changing library name keyword to %s", name);
    end;
  end;
end method;

define constant $replace-project-string =
  "Project defining library %s is already open as %s\nReplace it ?";

define function %project-replace-project-ask(project :: <project>,
                                             close? :: <boolean>)
 => (yes-or-no :: <boolean>, project :: false-or(<project>));
  debug-out(#"project-manager", "Asking if to replace %= %s", project, project.project-name);
  let key = project.project-library-name;
  let text = format-to-string($replace-project-string,
                              key, project.project-location);
  let condition = make(<duplicate-project-condition>,
                       key: key,
                       yes-or-no: text);

  let yes? =
    signal(condition);
  if(yes? & close?)
    // close? is acted on only if answer is yes
      %close-project(project);
      values(#t, #f)
  else
    values(yes?, project)
  end;
end;

define generic project-replace-project-with?(c :: subclass(<project>),
                                             project :: <project>,
                                             #key, #all-keys)
 => (yes-or-no :: <boolean>, project :: false-or(<project>));

define method project-replace-project-with?(c == <system-project>,
                                            project :: <user-project>,
                                            #key
                                            key :: <symbol>,
                                            close? = #t)
 => (yes-or-no :: <boolean>, project :: false-or(<project>));
  user-warning("Cannot replace user project in %s with system project %s",
               project.user-disk-project-file, key);
  values(#f, project)
end;

define method project-replace-project-with?(c == <system-project>,
                                            project :: <system-project>,
                                            #key
                                            key :: <symbol>,
                                            close? = #t)
 => (yes-or-no :: <boolean>, project :: false-or(<project>));
  if(close?)
    %close-project(project);
    values(#t, #f);
  else
    values(#t, project)
  end;
end;

define method project-replace-project-with?(c == <user-project>,
                                            project :: <user-project>,
                                            #key
                                            project-file :: <file-locator>,
                                            close? = #t)
 => (yes-or-no :: <boolean>, project :: false-or(<project>));

  if(project.user-disk-project-file = project-file)
    debug-out(#"project-manager",
              "project file %s is the same as %s",
              project-file, project.user-disk-project-file);
    values(#f, project)
  else
    %project-replace-project-ask(project, close?)
  end;
end method;

define method project-replace-project-with?(c == <user-project>,
                                            project :: <system-project>,
                                            #key always-replace-system? = #t,
                                            project-file :: <file-locator>,
                                            close? = #t, force? = #f)
 => (yes-or-no :: <boolean>, project :: false-or(<project>));

  if(always-replace-system?)
    %close-project(project);
    values(#t, #f)
  else
    %project-replace-project-ask(project, close?)
  end;
end;

define method project-replace-project-with?(c == <user-project>,
                                            project :: <binary-project>,
                                            #key always-replace-system? = #t,
                                            project-file :: <file-locator>,
                                            close? = #t, force? = #f)
 => (yes-or-no :: <boolean>, project :: false-or(<project>));

  if(always-replace-system?)
    %close-project(project);
    values(#t, #f)
  else
    %project-replace-project-ask(project, close?)
  end;
end;

define method project-replace-project-with?(c == <binary-project>,
                                            project :: <system-project>,
                                            #key always-replace-system? = #t,
                                            project-file :: <file-locator>,
                                            close? = #t, force? = #f)
 => (yes-or-no :: <boolean>, project :: false-or(<project>));

  if(always-replace-system?)
    %close-project(project);
    values(#t, #f)
  else
    %project-replace-project-ask(project, close?)
  end;
end;

define method project-replace-project-with?(c == <binary-project>,
                                            project :: <user-project>,
                                            #key always-replace-system? = #t,
                                            project-file :: <file-locator>,
                                            close? = #t, force? = #f)
 => (yes-or-no :: <boolean>, project :: false-or(<project>));

  %project-replace-project-ask(project, close?)
end;

define method project-replace-project-with?(c == <binary-project>,
                                            project :: <binary-project>,
                                            #key always-replace-system? = #t,
                                            project-file :: <file-locator>,
                                            close? = #t, force? = #f)
 => (yes-or-no :: <boolean>, project :: false-or(<project>));
  if(always-replace-system?)
    %close-project(project);
    values(#t, #f)
  else
    %project-replace-project-ask(project, close?)
  end;

end;

define generic replace-project-with?(c :: subclass(<project>),
                                     #key, #all-keys)
 => (yes-or-no :: <boolean>, project :: false-or(<project>), key);

define method replace-project-with?(c :: subclass(<project>),
                                    #rest keys,
                                    #key processor, operating-system,
                                    project-file, force? = #f, dont-replace? = #f,
                                    key, #all-keys)
 => (yes-or-no :: <boolean>, project :: false-or(<project>), key);
  debug-assert(project-file | key);
  let (processor, operating-system) =
    %set-target-values(c, processor, operating-system);
  let key = if(key) key else library-name-from-file(project-file) end;
  let project = key & find-platform-project(key, processor, operating-system);
  // TO DO: when replacing project the new one will not have an owner
  // until next compilation - is this a problem ?
  if(project)
    debug-out(#"project-manager",
              "Deciding if %= %s should be replaced",
              project, project.project-name);
    if(force?)
      // 'force?' implies 'close?'
      %close-project(project);
      values(#t, #f)
    elseif(~dont-replace?)
      let (yes?, opened-project) =
        apply(project-replace-project-with?, c, project, keys);
      values(yes?, opened-project, key)
    else
      values(#f, project, key)
    end; //
  else
    values(key & #t, #f, key)
  end
end method;

define method project-read-only?-setter(flag, project :: <system-project>)
  // cannot change the registry projects
  project.project-personal-library?
end;

define method project-read-only?-setter(flag, project :: <user-project>)
  project.%project-read-only? := flag;
  save-project(project)
end;

define function library-name-from-file
    (loc :: <file-locator>)
 => (name :: false-or(<symbol>));
  project-data-from-file(loc)
end;

define function project-data-from-file
    (loc :: <file-locator>)
 => (name :: false-or(<symbol>),
     project-class :: false-or(<class>),
     init-keyword-or-false);
  let extension = locator-extension(loc);
  select (extension by \=)
    $lid-project-suffix =>
      values(%library-name-from-file(<lid-project>, loc),
             <lid-project>, #f);
    $user-project-suffix =>
      values(%library-name-from-file(<lid-project>, loc),
             <user-project>, project-file:);
    $binary-project-suffix =>
      values(%library-name-from-file(<binary-project>, loc),
             <binary-project>, database:);
    otherwise =>
      values(#f, #f, #f);
  end;
end;

define generic %library-name-from-file
    (type :: subclass(<project>), file :: <file-locator>)
 => (name :: false-or(<symbol>));

define method project-remove-build-products(project :: <user-project>,
                                            #key recursive? = #f);
  next-method();
  project-flush-caches(project, recursive?: #f);
end;

//---*** andrewa: not currently used
// define constant user-project-keywords = project-lid-file-info;

// the default class is <project> since the make method on <project> decides
// what kind of project will be actually created
//
*default-project-class* := <project>;

define sideways method set-default-platform-info (processor, operating-system)
  let platform = platform-namestring(processor, operating-system);
  environment-variable("OPEN_DYLAN_PLATFORM_NAME") := platform
end method;

define sideways method default-platform-info
    () => (processor-name, os-name)
  let platform = environment-variable("OPEN_DYLAN_PLATFORM_NAME");
  unless (platform) platform := $platform-name end;
  platform-namestring-info(platform)
end method;

define method project-personal-library?(project :: <user-project>)
 => (is-personal? :: <boolean>)
  ~project.%project-read-only?
end;

// this method checks if the project of that name is already open
// if not it calls next-method which will go trough the process of creating it
// from scratch
// this method assumes that project files are named the same as libraries
//

define method make-project (c == <user-project>,
                            #key key = #f,
                            source-record-class = <file-source-record>,
                            project-file, parent = #f,
                            load-namespace? = #f,
                            processor = #f, operating-system = #f, mode)
 => (project :: <project>);
  let (processor, operating-system) =
    %set-target-values(c, processor, operating-system);

  unless(key)
    key := library-name-from-file(project-file);
  end;


  debug-assert(key, "Non existing project file");
  debug-assert(~find-platform-project(key, processor, operating-system),
               "cannot replace project");
  let project = next-method(c,
                            key: key,
                            source-record-class: source-record-class,
                            project-file: project-file,
                            parent: parent,
                            load-namespace?: load-namespace?,
                            processor: processor,
                            operating-system: operating-system,
                            mode: mode);

  debug-assert(instance?(project, <user-project>),
               "In make-project(<user-project>)");
  project
end method;

// this is the main make method for creating projects

// if the project is not found it will raise an error
// and establish a restart for the user to submit an explicit path

define sideways method make (class == <project>,
                    #rest keys, #key key,
                    parent = #f,
                    processor = #f, operating-system = #f, #all-keys)
 => (project :: <project>)
  block()
    // We get here through the following paths:
    // 1. Open project with key argument
    // 2. through make-used-project when the search there failed
    /*
    let directory = working-directory();

    // there is a slight chance that we will search twice in the same path
    // if the working-directory is a parent of using project directory
    // and we got here through 2.
    let local-search-path = if(directory)
                              list(directory, directory.locator-directory)
                            else
                              #()
                            end;

    let search-path = if (user-registry-path())
                        pair(user-registry-path().locator-directory,
                             local-search-path)
                      else
                        local-search-path
                      end;
    */
    // standard search path if set and then our path above
    let project-path = search-for-project(key);
    /*---*** andrewa: this slows everything down too much
      // this will accept an empty path
      | search-for-project(key, search-path: search-path);
    */

    let project =
      if(project-path)
        apply(make, <user-project>, project-file: project-path, keys)
      else
        // first try registries
        apply(make, <system-project>, keys)
      end;
    project
  exception(<registry-entry-not-found-error>)
    // this is the last resort for automatic finding of a project
    let binary-project = find-binary-project(key, processor: processor,
                                             operating-system: operating-system);
    binary-project | signal(make(<project-not-found>, project-name: key));
  exception(restart :: <find-project-location-restart>)
    let project-location = restart.condition-project-location;
    apply(primitive-make-project-from-file, project-location, parent: parent, keys);
  end;
end method;

// this is a simple new-user-project functionality
//
define function new-user-project
    (name :: <string>, location :: <directory-locator>)
 => (project :: <user-project>)
  let project-location
    = make(<file-locator>,
           directory: location,
           base:      name,
           extension: $user-project-suffix);
  let lid-location
    = make(<file-locator>,
           directory: location,
           base:      name,
           extension: $lid-project-suffix);
  let library-file-name = concatenate(name,"-library");
  // create prj file
  with-open-file(stream = project-location, direction: #"output")
    save-single-value(stream, #"Library", name);
    save-list-value(stream, #"Files", list(library-file-name));
  end;

  // create library file
  let library-locator
    = make(<file-locator>,
           directory: location,
           base:      library-file-name,
           extension: *dylan-source-suffix*);
  with-open-file(stream = library-locator, direction: #"output")
    format(stream,
           "Module: dylan-user\n\ndefine library %s\n\tuse common-dylan;\nexport %s;\nend library;\n\ndefine module %s\n\tuse common-dylan;\nend module;\n",
           name, name, name);
  end;
  make-project(<user-project>, project-file: project-location);
end;

// external interface
// this method converts a lid-project into a user-project
define method import-lid-project
    (lid-location :: <file-locator>, #key to-file)
  let project-location
    = to-file
        & merge-locators(as(<file-locator>, to-file),
                         lid-location);
  let project
    = begin
        let (yes?, project) =
          replace-project-with?(<user-project>,
                                // we pass lid-location on purpose
                                // since project-location doesn't exist yet
                                project-file: lid-location);
        if(yes?)
          let p = %import-lid-project(lid-location, to-file: project-location);
          p
        else
          user-warning("Project %s has not been imported", lid-location);
          project
        end
      end;
  when (project & project.project-namespace-loaded)
    close-unused-projects();
  end;
  project
end;

// internal method for use inside 'make' methods
define method %import-lid-project(lid-location :: <file-locator>,
                                  #rest keys,
                                    #key to-file = #f,
                                  make-method = make-project,
                                  #all-keys)
 => (project :: false-or(<user-project>));
  let project-location
    = to-file
        | make(<file-locator>,
               directory: lid-location.locator-directory,
               base:      lid-location.locator-base,
               extension: $user-project-suffix);
  let ok? =
    block()
      copy-file(lid-location, project-location, if-exists: #"replace");
      file-property(project-location, #"writeable?") := #t;
      #t
    exception(e :: <file-system-error>)
      user-warning("Project %s has not been imported due to file system error",
                   lid-location);
      apply(user-warning, e.condition-format-string, e.condition-format-arguments);
      #f
    end;

  if(ok?)
    debug-out(#"project-manager",
              "Importing %s to %s", as(<string>, lid-location),
              as(<string>, project-location));

    apply(make-method, <user-project>, project-file: project-location, keys);
  else
    #f
  end;
end;

// this method is used when we know for sure that no project defining
// the same library is open - this can happen when the compiler is asking
// for a subproject
define method make-project-from-file
    (file :: <file-locator>, #rest keys, #key, #all-keys)
 => (project :: false-or(<project>));
  apply(primitive-make-project-from-file, file, make-method: make-project, keys)
end;

// this method is a primitive version of the above
// this method uses make by default to make it possible to call it from within make
define method primitive-make-project-from-file(file :: <file-locator>,
                                               #rest keys, #key make-method = make,
                                               #all-keys)
 => (project :: false-or(<project>));
  let (key, project-class, init-keyword) =
    project-data-from-file(file);
  if(key)
    select(project-class)
      <lid-project> => apply(%import-lid-project, file,
                             make-method: make-method, keys);
      <user-project>, <binary-project> =>
        apply(make-method, project-class, init-keyword, file, key: key, keys);
      otherwise => #f;
    end
  end
end;

// This method goes through the replace-project-with? protocol
// internal interface used for opening subprojects
define method open-project-from-file(location :: <file-locator>,
                                     #rest keys, #key, #all-keys)
 => (project :: false-or(<project>));

  let (key, project-class, init-keyword) =
    project-data-from-file(location);
  if(key)
    let (yes?, opened-project, key) =
      apply(replace-project-with?, project-class,
            key: key, project-file: location, keys);

    if(yes?)
      apply(make-project, project-class, init-keyword, location, key: key, keys)
    else
      opened-project
    end
  else
    #f
  end
end;

define method close-project(project :: <user-project>, #key system?)
 => (ok? :: <boolean>);
  let ok? = next-method();
  if(ok? & ~project.project-namespace-loaded)
    let user-projects = project.project-user-projects;
    debug-out(#"project-manager", "Closing subprojects of user-project %s: %s\n",
              project.project-name, map(project-name, user-projects));
    do(close-project, user-projects)
  end;
  ok?
end;

// this is an external interface
define sideways method open-project
    (project-file-location :: <file-locator>)
 => (project :: false-or(<user-project>));
  let extension = locator-extension(project-file-location);
  select (extension by \=)
    $user-project-suffix => open-hdp-project(project-file-location);
    $lid-project-suffix  => import-lid-project(project-file-location);
    otherwise            => #f
  end;
end;

define function open-hdp-project
    (project-file-location :: <file-locator>)
 => (project :: false-or(<user-project>));
//  debug-assert(locator-extension(project-file-location) = $user-project-suffix,
//             "%s is not a project file", project-file-location);

  let (processor, operating-system) = values(#f, #f);
  let project-location = project-file-location;

  let project
    = begin
        let (yes?, opened-project) =
          replace-project-with?(<user-project>, project-file: project-location);
        if(yes?)
          let project = make-project(<user-project>, project-file: project-location);
          project
        else
          debug-out(#"project-manager",
                    "Open-project: returning already opened project %= %s",
                    opened-project, opened-project & opened-project.project-name);
          if(opened-project)
            debug-out(#"project-manager",
                      "The project is already open as %s ",
                      project-location)
          else
            user-warning("Couldn't open project in %s ", project-location)
          end;
          opened-project
        end;
      end;
  when (project & project.project-namespace-loaded)
    close-unused-projects();
  end;
  project
end;

define function %cached-subprojects(project :: <user-project>)
 => (projects :: <sequence>);
  let table = project.%user-project-used-projects;
  let keys = table.key-sequence;
  map(method(k) table[k] end, keys);
end;

define method all-used-projects(project :: <user-project>, #key system?)
 => (projects :: <sequence>);
  ignore(system?);
  if(project.project-namespace-loaded)
    next-method();
  else
    // project not compiled
    %cached-subprojects(project)
  end;
end;

define method directly-used-projects(project :: <user-project>, #key system?)
 => (projects :: <sequence>);
  ignore(system?);
  if(project.project-namespace-loaded)
    next-method();
  else
    // project not compiled
    %cached-subprojects(project)
  end
end;

// TO DO: save and lookup in the cache %user-project-used-projects
// this cache is curently filled by the compiler calls
// NO -  we have to use %subproject-files since we know they were added
// explictly by the user
define function project-user-projects(project :: <project>)
 => (projects :: <sequence>);

  let used-projects = make(<stretchy-vector>);
  let project-directory = project.user-disk-project-file.locator-directory;
  for(f in project.%subproject-files)
    let project-location = merge-locators(f, project-directory);
    let project-file = as(<string>, project-location);

    unless(member?(project-file, project-build-property(project, #"broken-files") | #[], test: \=))
      let library-name = library-name-from-file(f);
      let used-project =
        // with-compiler-transaction is used to avoid making the projects top level
        with-compiler-transaction
          lookup-named-project(library-name, create?: #f);
        end;
      used-project & add!(used-projects, used-project)
    end;
  end;
  used-projects

end;

define method note-project-made(project :: <user-project>, #key parent) => ();
  let subprojects = project-keyword-property(project,
                                             #"subprojects", default: #());
  for(s in subprojects)
    project-add-file(project, s, save?: #f);
  end;

  project-initialize-caches(project);
end;

define method project-key? (project :: <user-project>,
                            key :: <symbol>)
                          => key?;
  project.project-lid-library-name == key
    |
    project.project-name == key
end method;

define method project-build-location
    (project :: <user-project>)
 => (location :: false-or(<directory-locator>));
  project-personal-library?(project) & next-method()
end;

// TO DO: this should be changed to reflect the hdp file name
define method project-registered-name (project :: <user-project>)
 => (name :: <symbol>);
  project.project-lid-library-name
//  locator-base(project.project-file-location)
end;

define method project-name
    (project :: <user-project>) => (name :: <symbol>)
  as(<symbol>, locator-base(project.project-file-location))
end method project-name;

define method project-compiler-source-files
    (project :: <user-project>)
 => (location :: false-or(<sequence>));
  project.project-source-files;
end;

define method project-compiler-source-location
    (project :: <user-project>)
 => (location :: <directory-locator>);
  project.project-source-location;
end;

define generic project-source-record-location(project :: <project>,
                                              sr :: <source-record>)
 => (location :: false-or(<file-locator>));

define method project-source-record-location(project :: <project>,
                                             sr :: <source-record>)
 => (location :: false-or(<file-locator>));
  sr.source-record-location
end;

define method project-source-record-location(project :: <user-project>,
                                             sr :: <file-source-record>)
 => (location :: false-or(<file-locator>));
  sr.source-record-location
end;

define function project-id-source-record(project :: <user-project>, id) => sr;
  let table = project.%source-record-table;
  let str = as(<string>, id);
  let record = element(table, str, default: #f);
  record |
    (table[str] :=
       id-as-source-record(project-source-record-class(project),
                           project,
                           project-source-location(project), id))
end function;

define method update-project-files (project :: <user-project>) => ();
  // do nothing - we don't allow for changes of HDP files
  // outside of the environment
  // well, now we do ;-)
end;

define method note-loading-namespace(project :: <user-project>) => ();
  if(project-dynamic-environment(#"compiler-transaction"))
  // we are starting a compiler operation
    debug-assert(~project.project-execution-context,
                 "Cannot compile %s while connected to the application",
                 project.project-name);

    let other-files = project-build-property(project, #"other-files") | #[];
    local method process-file(f)
            let full-path = merge-locators(as(<file-locator>, f),
                                           project.project-source-location);
            let tool-name :: false-or(<symbol>) =
              tool-name-from-specification(full-path);
            let tool = tool-name & tool-find(tool-name);
            if(tool)
              let tool-cache = element(project.%tools-cache, tool-name, default: #f);
              local method last-run-date(f, cache)
                      block(found)
                        debug-out(#"project-manager", "Looking for %s in tool cache\n",
                                  as(<string>, f));
                        for(e in cache, el from 0 by 1)
                          debug-out(#"project-manager", "Checking %s in tool cache\n",
                                    as(<string>, first(e)));
                          if(first(e) = f)
                            found(second(e), el)
                          end
                        end;
                        values(#f, #f)
                      end;
                    end;

              let (last-run, el) = if(tool-cache)
                                     last-run-date(full-path, tool-cache);
                                   else
                                     project.%tools-cache[tool-name] := make(<stretchy-vector>);
                                     values(#f, #f);
                                   end;
              block(return)
                let handler <tool-warning-condition>
                  = method(e, next-handler)
                        user-warning("%s", condition-to-string(e));
                        unless(tool-warning-recoverable?(e))
                          return()
                        end;
                    end;
                let handler <tool-yes-no-question>
                  = method(e, next-handler)
                        let question = apply(format-to-string, e.condition-format-string,
                                             e.condition-format-arguments);
                        signal(make(<yes-or-no-condition>,
                                    yes-or-no: question))
                    end;
                let (success?, hdp-modified?, new-projects)
                  = tool(full-path, project.user-disk-project-file, last-run);
                if(success?)
                  if(el)
                    (project.%tools-cache[tool-name])[el] := list(full-path, current-date())
                  else
                    add!(project.%tools-cache[tool-name], list(full-path, current-date()))
                  end;
                  hdp-modified? & project-read-project-file(project);

                end;
              end;
            end;
          end;

    do(process-file, other-files);

  end;
end;

// this function re-reads the project file
define function project-read-project-file(project :: <user-project>) => ();
  let (library-name, files, properties) = read-lid-data(project.user-disk-project-file);
  project-source-files(project) := files;
  project-lid-file-info(project) := properties;
  reinitialize-lid-project(project);
  let subprojects = project-keyword-property(project,
                                             #"subprojects", default: #());
  for(s in subprojects)
    project-add-file(project, s, save?: #f);
  end;

end;

// Compute the sources records to give the compiler.
define method project-current-source-records (project :: <user-project>)
 => sr*;
  block()
    compute-compiler-source-records(project)
// TO DO: should we catch it ?
//  exception (<file-does-not-exist-error>)

  end;
end method;

define method note-compiled-definitions(project :: <user-project>)
  //---*** andrewa: we don't need to save the project, as the dynamic
  //---*** information is all in the caches.
  // save-project(project);
  save-project-caches(project);
  next-method();
end;

/* TO DO: doesn't work in the emulator
define constant <standard-keyword> = <standard-lid-keyword>;

define generic project-keyword-setter(val :: type-union(<string>, <sequence>),
                                      project :: <project>,
                                      key :: type-union(<symbol>, <standard-keyword>))
 => (val :: type-union(<string>, <sequence>));

define method project-keyword-setter(val :: <string>, project :: <user-project>,
                                     key :: <symbol>) => (val :: <string>);
  project.user-project-keywords[key] := list(val);
  val
end;

define method project-keyword-setter(val :: <string>, project :: <user-project>,
                                     key :: <standard-keyword>) => (val :: <string>);
  error("Internal: Cannot set standard keyword through this interface");
  val
end;

define method project-keyword-setter(val :: <sequence>, project :: <user-project>,
                                     key :: <symbol>) => (val :: <sequence>);
  project.user-project-keywords[key] := val
end;

define method project-keyword-setter(val :: <sequence>, project :: <user-project>,
                                     key :: <standard-keyword>) => (val :: <sequence>);
  error("Internal: Cannot set standard keyword through this interface");
  val
end;

define generic project-keyword(project :: <project>,
                               key :: <symbol>) => (val :: <sequence>);

define method project-keyword(project :: <project>,
                              key :: <symbol>) => (val :: <sequence>);
  element(project.user-project-keywords, key, default: #f);
end;

define generic project-keywords(project :: <project>) => (keywords :: <sequence>);

define method project-keywords(project :: <user-project>) => (keywords :: <sequence>);
  project.user-project-keywords.key-sequence
end;

define generic project-remove-keyword(project :: <project>, key :: <symbol>);

define method project-remove-keyword(project :: <user-project>, key :: <symbol>);
  remove-key!(project.user-project-keywords, key)
end;

*/
