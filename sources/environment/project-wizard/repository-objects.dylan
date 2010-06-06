Module:    repository
Author:    Hugh Greene
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// ----------------------------------------------------------------------
/// REPOSITORY-OBJECT
/// Root class for objects in repository

define sealed abstract class <repository-object> (<object>)
  sealed constant each-subclass slot
      repository-object-class-lookup-table :: <table>,
    init-keyword: lookup-table:;
  sealed constant slot repository-object-id :: <symbol>,
    required-init-keyword: id:;
  sealed constant slot repository-object-label :: <string>,
    init-keyword: label:;
  sealed constant slot repository-object-documentation :: <string>,
    init-keyword: documentation:;
end class;

define sealed method make
    (class :: subclass(<repository-object>),
     #rest initargs,
     #key id, label,
     #all-keys)
 => (object :: <repository-object>)
  unless (label)
    label := string-capitalize(as(<string>, id));
  end;
  apply(next-method, class, label: label, initargs)
end method;

define sealed method initialize
    (object :: <repository-object>, #key id :: <symbol>)
  next-method();
  //---*** If we ever dynamically extend the repository, this should be
  //---*** a call to error, not an "assert".
  let lookup-table = object.repository-object-class-lookup-table;
  assert(element(lookup-table, id, default: #f) == #f,
         "Attempt to add an object to the Wizard repository with"
         " the same ID as an existing object of the same class.");
  lookup-table[id] := object;
end method;

define sealed method \<
    (ro1 :: <repository-object>, ro2 :: <repository-object>)
 => (less? :: <boolean>)
  ro1.repository-object-label < ro2.repository-object-label
end method;

define sealed method \=
    (ro1 :: <repository-object>, ro2 :: <repository-object>)
 => (equal? :: <boolean>)
  ro1.repository-object-id = ro2.repository-object-id
end method;

/// ----------------------------------------------------------------------
/// PROJECT-LIBRARY-GROUPS

define constant $project-library-groups :: <table> = make(<table>);

define sealed class <project-library-group> (<repository-object>)
// This fails due to Bug 3605
//  inherited slot repository-object-class-lookup-table,
//    init-value: $project-library-groups;
  keyword lookup-table:, init-value: $project-library-groups;
  sealed slot project-library-group-libraries :: <vector>,
    required-init-keyword: libraries:;
end class;


/// ----------------------------------------------------------------------
/// PROJECT-LIBRARIES

define constant $project-libraries :: <table> = make(<table>);

define sealed class <project-library> (<repository-object>)
// This fails due to Bug 3605
//  inherited slot repository-object-class-lookup-table,
//    init-value: $project-libraries;
  keyword lookup-table:, init-value: $project-libraries;
  sealed slot project-library-modules :: <vector>,
    required-init-keyword: modules:;
  sealed constant slot project-library-packs :: false-or(<sequence>),
    init-value: #f,
    init-keyword: library-packs:;
end class;

define function library-packs (packs :: <sequence>) => (packs :: <sequence>)
  packs
end function library-packs;

// Specify the modules as a vector of symbols which are the names of each
// module.  To exclude a module from a library by default, put the symbol
// inside in a list.  See the generic-arithmetic library for an example.

/*

/// ----------------------------------------------------------------------
/// PROJECT-FILES

define constant $project-files :: <table> = make(<table>);

define sealed class <project-file> (<repository-object>)
// This fails due to Bug 3605
//  inherited slot repository-object-class-lookup-table,
//    init-value: $project-files;
  keyword lookup-table:, init-value: $project-files;
  sealed slot project-file-name :: <string>, required-init-keyword: name:;
  sealed slot project-file-base :: <string>, required-init-keyword: base:;
  sealed slot project-file-extension :: <string>, required-init-keyword: extension:;
  sealed slot project-file-headers :: <vector>, required-init-keyword: headers:;
end class;


/// ----------------------------------------------------------------------
/// PROJECT-FILE-HEADERS

define constant $project-file-headers :: <table> = make(<table>);

define sealed class <project-file-header> (<repository-object>)
// This fails due to Bug 3605
//  inherited slot repository-object-class-lookup-table,
//    init-value: $project-file-headers;
  keyword lookup-table:, init-value: $project-headers;
  sealed slot project-file-header-default-value :: <vector>,
    required-init-keyword: libraries:;
end class;

*/

/// ----------------------------------------------------------------------
/// PROJECT-TYPES

define constant $project-types :: <table> = make(<table>);

define variable *project-type-order* :: <integer> = 0;

define sealed class <project-type> (<repository-object>)
// This fails due to Bug 3605
//  inherited slot repository-object-class-lookup-table,
//    init-value: $project-types;
  keyword lookup-table:, init-value: $project-types;
  //--- Could we encode project setting requirements/recommendations here?
  // This is used to impose a visual order on the project types in the
  // list in the Wizard GUI.  The MAKE method below ensures they're ordered
  // as they're created in the top-level initialization below.
  constant slot project-type-order :: <integer>,
      required-init-keyword: order:;
end class;

define sealed method make
    (class == <project-type>,
     #rest initargs,
     #key order,
     #all-keys)
 => (object :: <project-type>)
  ignore(order);
  let order = *project-type-order*;
  *project-type-order* := *project-type-order* + 1;
  apply(next-method, class, order: order, initargs)
end method;


/// ----------------------------------------------------------------------
/// THE REPOSITORY AS <CHOICE>S
/// The functions hereunder control what is made visible from the
/// repository, in terms of "release-edition-type()".

/// Making <choice>s from the repository.
// --- Note: "map(..., as(<vector>, ...))" is used 'cos map-as fails in
// the emu.

define sealed generic make-repository-choice
    (object :: <object>, #key included?)
 => (choice :: <choice>);

define function make-repository-choices
    (objects :: <collection>)
 => (choices :: <vector>)
  local method include-object? (o)
    if (instance?(o, <list>)) o := o.head; end if;
    // We include an object if
    //   - it's not a <repository-object> (i.e., it's a module);
    //   - it's to be available in the edition of this release; and
    //     - either it's not a library;
    //     - or (it *is* a library and) it's library pack is
    //       installed in this release.
    if (instance?(o, <repository-object>))
      ~instance?(o, <project-library>)
	| begin
	    let packs = o.project-library-packs;
	    ~packs | every?(release-contains-library-pack?, packs)
	  end
    else
      #t
    end
  end method;
  let objects-for-this-release
    = choose(include-object?, as(<vector>, objects));
  sort!(map(make-repository-choice, objects-for-this-release))
end function;

define method make-repository-choice
    (object :: <object>, #key included? = #t)
 => (choice :: <choice>)
  // This method will be used for modules.  We assume that people
  // want all the modules in a library unless they say otherwise,
  // for convenience.
  make(<choice>, object: object, included?: included?)
end method;

define method make-repository-choice
    (list :: <list>, #key included?)
 => (choice :: <choice>)
  // This method is used hackily for the occasional case where we want to
  // default some modules in a library to not-included.
  ignore(included?);
  make-repository-choice(head(list), included?: #f)
end method;

define method make-repository-choice
    (group :: <project-library-group>, #key included? = #f)
 => (choice :: <choice>)
  local
    method lookup-library (lib-name) => (lib)
      if (instance?(lib-name, <list>))
	let lib = element($project-libraries, head(lib-name), default: #f);
	lib & list(lib)
      else
	element($project-libraries, lib-name, default: #f)
      end
    end method;
  make(<choice>, object: group, included?: included?,
       children:
	 make-repository-choices
	   (map(method (lib-name)
		  lookup-library(lib-name)
		    | error("Library %s could not be found for group %s",
			    lib-name, repository-object-label(group))
		end,
		group.project-library-group-libraries)))
end method;

define method make-repository-choice
    (library :: <project-library>, #key included? = #t)
 => (choice :: <choice>)
  // We assume that people want all the modules in a library unless
  // they say otherwise, for convenience.
  make(<choice>, object: library, included?: included?,
       children:
	 make-repository-choices(library.project-library-modules))
end method;

define function find-repository-choice
    (repository-choices :: <sequence>, id :: <symbol>,
     #key if-not-exists :: false-or(singleton(#"signal")) = #"signal")
 => (choice :: false-or(<choice>))
  find-element(repository-choices,
	       method (choice)
		 choice.choice-object.repository-object-id == id
	       end)
    | when (if-not-exists == #"signal")
	error("Wizard error: the repository object %s could not be found", id)
      end;
end function;

define function repository-as-choices
    ()
 => (choices :: <vector> /* of: <choice> */)
  let choices = make-repository-choices($project-library-groups);
  // Include the Core group by default to get the (functional-)dylan library!
  let core-group = find-repository-choice(choices, #"core");
  core-group.choice-included? := #t;
  choices
end function;

define function library-group-choice-included?-setter
    (included? :: <boolean>, group-choices :: <sequence>, id :: <symbol>,
     #key recursive? = #f)
 => (included? :: <boolean>)
  let group-choice = find-repository-choice(group-choices, id);
  choice-included?-setter(included?, group-choice, recursive?: recursive?)
end function;

define function library-choice-included?-setter
    (included? :: <boolean>, group-choices :: <sequence>,
     group-id :: <symbol>, library-id :: <symbol>,
     #key recursive? = #f)
 => (included? :: <boolean>)
  let group-choice
    = find-repository-choice(group-choices, group-id);
  when (included?)
    // Include the group, non-recursively, in case it isn't already in.
    choice-included?-setter(included?, group-choice);
  end;
  let library-choice
    = find-repository-choice(group-choice.choice-children, library-id);
  choice-included?-setter(included?, library-choice, recursive?: recursive?)
end function;

