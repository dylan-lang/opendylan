Module:   projects-implementation
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define open class <project-warning> (<simple-warning>)
end;

define open class <project-serious-warning> (<project-warning>)
end;

define open class <project-fatal-error> (<project-serious-warning>)
end;

define sealed primary abstract class <base-project> (<object>)
  // Table mapping source record ids to source records.
  constant slot project-source-record-table = make(<string-table>);
  slot project-current-compilation-context = #f;
  slot project-namespace-loaded :: <boolean> = #f;
  slot %project-owners :: <vector> = make(<stretchy-vector>);
  slot %project-top-level? :: <boolean> = #f;
  slot %database-in-memory :: <boolean> = #f;
  slot %database-in-memory-current :: <boolean> = #f;
  slot %database-saved :: <boolean> = #t;
  // andrewa: this isn't used anywhere, can we remove it?
  // slot %database-complete :: <boolean> = #f;
end;

define open abstract class <project> (<base-project>)
end class;

define open generic project-name(project :: <project>)
 => (name :: false-or(<symbol>));

define open generic project-read-only? (project :: <project>) => (flag :: <boolean>);

define open generic project-read-only?-setter (flag, project :: <project>);


define open generic project-location (project :: <project>)
 => (location :: false-or(<file-locator>));

define open generic project-platform-name (project :: <project>)
 => platform-name;

define open generic project-platform-name-setter (platform-name, project :: <project>);

define open generic project-compilation-mode (project :: <project>)
 => mode;

define open generic project-compilation-mode-setter (mode, project :: <project>);

define open generic project-compiler-back-end (project :: <project>)
 => back-end;

define open generic project-compiler-back-end-setter (back-end, project :: <project>);

define open generic project-build-location
    (project :: <project>)
 => (location :: false-or(<directory-locator>));

define open generic project-build-location-setter
    (location :: false-or(<directory-locator>), project :: <project>)
 => (location :: false-or(<directory-locator>));

define open generic project-database-location
    (project :: <project>)
 => (location :: <file-locator>);

define open generic project-database-location-setter
    (location :: false-or(<file-locator>), project :: <project>)
 => (location :: false-or(<file-locator>));

define open generic project-profile-location
    (project :: <project>)
 => (location :: false-or(<file-locator>));

define open generic project-profile-location-setter
    (location :: false-or(<file-locator>), project :: <project>)
 => (location :: false-or(<file-locator>));

define open generic project-library-loose-bindings (project :: <project>)
 => bindings;

define open generic
    project-library-loose-bindings-setter (bindings :: <sequence>, project :: <project>)
 => bindings;

define open generic project-major-version (project :: <project>)
 => (version :: <integer>);

define open generic project-major-version-setter (version :: <integer>, project :: <project>)
 => (version :: <integer>);

define open generic project-minor-version (project :: <project>)
 => (version :: <integer>);

define open generic project-minor-version-setter (version :: <integer>, project :: <project>)
 => (version :: <integer>);

define open generic project-library-pack (project :: <project>)
 => (library-pack :: <integer>);

define method project-library-pack (project :: <project>) => (library-pack :: <integer>)
  0
end method project-library-pack;

define open generic project-library-pack-setter
    (library-pack :: <integer>, project :: <project>)
 => (library-pack :: <integer>);

define constant <project-target-type> = one-of(#"executable", #"dll");

define open generic project-target-type(project :: <project>)
 => (type :: <project-target-type>);

define open generic project-target-type-setter
    (type :: <project-target-type>, project :: <project>)
 => (type :: <project-target-type>);

define open generic project-library-name (project :: <project>)
 => (name :: <symbol>);

define open generic project-executable-name (project :: <project>)
 => (name :: <string>);

define open generic project-executable-name-setter (name :: <string>, project :: <project>)
 => (name :: <string>);

/*
  // Compiler callbacks
  // These are defined in the compiler to avoid module circularities.

  // Return the context for the used library of this project
  define open generic used-library-context
    (context, used-library-dylan-name, #key canonicalize?) => used-c;

// Return the source record given a source-record-id
define generic project-record-id-source-record (project, id) => sr;

// Return the id given a source record
define generic project-source-record-id (project, sr) => id;

// Returns the source record name relative to project
define generic project-source-record-name (project, sr) => name-or-false;

define open generic note-definitions-updated(context);

// Return the version of a library/project
define open generic project-library-version (project)
 => (major-ver, minor-ver);

// implemented by the compiler
define open generic project-used-library-version (context, used-context)
 =>  (major-ver, minor-ver, time-stamp);

// Return inter library binding
define open generic project-inter-library-binding (project, used-project) =>
    (binding :: one-of(#"tight", #"loose"));

*/

define open generic note-loading-namespace (project :: <project>) => ();

define method note-loading-namespace (project :: <project>) => ();
// do nothing
end;

// Return true if the library is a user library as opposed to a system
// library.
define compiler-open generic project-personal-library? (project)
  => (is-personal? :: <boolean>);

define compiler-open generic project-personal-library?-setter
    (is-personal?, project :: <project>);

define variable *default-compilation-mode* = #"tight";

define variable *default-inter-library-binding* = #"tight";

define variable *default-library-major-version* = 0;
define variable *default-library-minor-version* = 0;
define variable *default-library-library-pack*  = 0;

define variable *session-properties* = make(<table>);

// this is external interface to return the set of records on disk
define function project-source-records (project :: <project>)
 => (records :: <sequence>);
  project-verify-source-records(project)
end;

define generic
    project-source-canonical-source-record (project :: <project>, source)
 => (record :: false-or(<source-record>), modified? :: <boolean>);

// this api returns "raw" sources, whatever it means
define open generic project-dylan-sources (project :: <project>)
 => (records :: <sequence>);

// this is external interface to return the set of records as known to the compiler
define open generic project-canonical-source-records (project :: <project>)
 => (records :: <sequence>);

define method project-canonical-source-records (project :: <project>)
 => (records :: <sequence>)
  let context = project.project-current-compilation-context;
  if (context)  compilation-context-sources(context) else #() end
end;

define open generic project-remove-build-products
    (project :: <base-project>, #key recursive?);

define class <system-project-not-usable> (<simple-error>)
end;

define method make (class == <system-project-not-usable>,
                    #key project :: <project>)
 => (error :: <system-project-not-usable>)
  next-method(class,
              format-string: "System project %s doesn't have a full database",
              format-arguments: vector(project))
end method;

define method condition-unusable-project (c :: <system-project-not-usable>)
 => (project :: <project>)
  first(condition-format-arguments(c))
end;

// Returns the current project sources
define open generic project-current-source-records (project :: <project>) => sr*;

define open generic project-source-location (project :: <project>)
 => (location :: <directory-locator>);

// Returns the class to use for source records in this project.  The class
// is used both to create new source records and to determine the protocol
// for converting a source-record-id to a source record.
define open generic project-source-record-class
    (project :: <project>) => (res :: <class>);

// [Optional] Called after project has been parsed.
define open generic note-project-loaded (project :: <project>);

define method note-project-loaded (project :: <project>) end;

// [Optional] Called before compiling definitions.
define open generic note-compiling-definitions (project :: <project>);

define method note-compiling-definitions (project :: <project>) end;

// [Optional] Called after compiling definitions.
define open generic note-compiled-definitions (project);

define method note-compiled-definitions (project :: <project>) end;

define method note-compiled-definitions (project :: <string>)
  note-compiled-definitions(lookup-named-project(project));
end;

// [Optional] Called after database saved
define open generic note-database-saved (project :: <base-project>);

define method note-database-saved (project :: <base-project>) end;

// [Optional] Called when database on disk is out of date
define open generic note-database-unsaved (project :: <base-project>);

define method note-database-unsaved (project :: <base-project>)
  project-message(project, "Database file for project %s is not up to date", project)
end;

// [Optional] Called when database is invalidated
define open generic note-database-invalidated (project :: <base-project>);

define method note-database-invalidated (project :: <base-project>)
  debug-out(#"project-manager", "Database for project %s has changed", project.project-name)
end;

// [Optional] Hook to allow projects to have different names ("keys")
// from their dylan library names.  If defined, should return some object
// which identifies the used project.
// The default is to just use the library name.
define open generic used-library-project-key
    (project :: <project>, used-library-dylan-name :: <symbol>)
 => key;

define method used-library-project-key
    (project :: <project>, used-library-dylan-name :: <symbol>)
 => (key)
  used-library-dylan-name
end method;

// True if the specified key identifies this project.  key is either
// a symbol (from the listener interface) or something returned by
// by a call to used-library-project-key (on a different project).
define open generic project-key? (project :: <project>, key) => key?;

// Create a new subproject of project, identified by key (which is
// as returned by a call to used-library-project-key for some used
// library of the project) and platform info.  Only called if subproject
// identified by key isn't already open.
define open generic make-used-project
    (project :: <project>, key, platform-name)
 => (project :: <project>);

// Class of projects to create from the listener interface.
define variable *default-project-class* = #f;

// [Optional] A sequence of alternating keyword and value pairs for component
// creation from the current compilation context.  Default method returns #()
define open generic project-build-settings
    (project :: <project>)
 => (res :: <sequence>);

define method project-build-settings
    (project :: <project>)
 => (res :: singleton(#()))
  #()
end method;

// [Optional] Access to global, per-session properties applying to all projects.
define open generic session-property
    (key :: <symbol>)
 => (value);

define method session-property
    (key :: <symbol>)
 => (value)
  element(*session-properties*, key, default: #f)
end method;

define open generic session-property-setter
    (value, key :: <symbol>)
 => (value);

define method session-property-setter
    (value, key :: <symbol>)
 => (value)
  element(*session-properties*, key) := value;
end method;

// [Optional] Access to some optional static properties of the project.
define open generic project-keyword-property
    (project :: <project>, key :: <symbol>, #key default)
 => (value);

define method project-keyword-property
    (project :: <project>, key :: <symbol>, #key default = unsupplied())
 => (value)
  if (supplied?(default))
    default
  else
    error("Unknown project property %s", key)
  end;
end method project-keyword-property;

define generic project-build-property
    (project :: <base-project>, key :: <symbol>)
 => (property);

define method project-build-property
    (project :: <base-project>, key :: <symbol>)
 => (property)
  #f
end;

define generic project-build-property-setter
    (property, project :: <base-project>, key :: <symbol>)
 => (property);

define method project-build-property-setter
    (property, project :: <base-project>, key :: <symbol>)
 => (property)
  #f
end;

// Called when the default target platform gets changed.
define open generic note-platform-change
    (project :: <project>, platform-name);

// Called when a used project is opened
define open generic note-used-project (project :: <project>, subproject :: <project>);

define open generic all-used-projects (project :: <project>, #key system?)
 => (projects :: <sequence>);

define method all-used-projects (project :: <project>, #key system? = #t)
 => (projects :: <sequence>)
  if (project.project-namespace-loaded)
    let all =
      map(compilation-context-project,
          tail(all-known-compilation-contexts(project.project-current-compilation-context)));
    if (system?)
      all
    else
      choose(method (p) p.project-personal-library? end, all)
    end
  else
    #[]
  end;
end;

define open generic directly-used-projects (project :: <project>, #key system?)
 => (projects :: <sequence>);

define method directly-used-projects (project :: <project>, #key system? = #t)
 => (projects :: <sequence>)
  if (project.project-namespace-loaded)
    let all =
      map(compilation-context-project,
          used-compilation-contexts(project.project-current-compilation-context));
    if (system?)
      all
    else
      choose(method (p) p.project-personal-library? end, all)
    end
  else
    #[]
  end;
end;

define open generic open-project (key) => (project :: false-or(<project>));

define open generic close-project (key, #key system?) => (closed? :: <boolean>);

define open generic note-project-closed (project :: <base-project>) => ();

define method note-project-closed (project :: <base-project>)
 => ();
  // do nothing
end method;

define open generic project-close-compilation-contexts (project :: <project>);

define method project-close-compilation-contexts (project :: <project>)
  let context = project.project-current-compilation-context;
  if (context)
    close-compilation-context(context);
    project.project-current-compilation-context := #f;
  end
end;
