Module:    release-info-internals
Synopsis:  Functional Developer release information
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Useful constants
define constant $library-pack-extension = "dlp";


/// Release info
define class <release-info> (<object>)
end class <release-info>;

define generic info-name
    (info :: <release-info>) => (name :: false-or(<symbol>));
define generic info-title
    (info :: <release-info>) => (title :: <string>);
define generic info-description
    (info :: <release-info>) => (description :: false-or(<string>));

define method info-description
    (info :: <release-info>) => (title :: <string>)
  ""
end method info-description;


/// Named release info
define class <named-release-info> (<release-info>)
  constant slot info-name :: <symbol>,
    required-init-keyword: name:;
end class <named-release-info>;

define method info-title
    (info :: <named-release-info>) => (title :: <string>)
  as-lowercase(as(<string>, info.info-name))
end method info-title;


/// Described release info

define class <described-release-info> (<named-release-info>)
  constant slot info-title :: <string> = "",
    init-keyword: title:;
  constant slot info-description :: <string> = "",
    init-keyword: description:;
end class <described-release-info>;


/// Libraries

define constant *libraries* :: <object-table> = make(<object-table>);

define class <library-info> (<described-release-info>)
  slot info-library-pack :: false-or(<library-pack-info>) = #f,
    init-keyword: library-pack:;
  constant slot info-categories :: <vector> = #[],
    init-keyword: categories:;
  constant slot info-project :: false-or(<string>) = #f,
    init-keyword: project:;
  constant slot info-modules :: <sequence> = #[],
    init-keyword: modules:;
  constant slot info-requires :: <sequence> = #[],
    init-keyword: requires:;
  constant slot info-source-directory :: false-or(<directory-locator>) = #f,
    init-keyword: source-directory:;
  slot info-binary :: false-or(<library-binary-info>) = #f,
    init-keyword: binary:;
  slot info-binary-name :: false-or(<string>) = #f,
    init-keyword: binary-name:;
  slot info-merge-parent :: false-or(<library-info>) = #f,
    init-keyword: merge-parent:;
  slot info-database :: false-or(<string>) = #f,
    init-keyword: database:;
  slot info-lib :: false-or(<string>) = #f,
    init-keyword: lib:;
  slot info-releases :: <vector> = #[],
    init-keyword: releases:;
end class <library-info>;

define method initialize
    (info :: <library-info>, #key) => ()
  next-method();
  *libraries*[info.info-name] := info
end method initialize;

define class <library-binary-info> (<release-info>)
  constant slot info-binary-name :: <string>,
    required-init-keyword: binary-name:;
  constant slot info-merged-libraries :: <sequence> = #[],
    init-keyword: merged-libraries:;
end class <library-binary-info>;

define class <library-release-info> (<release-info>)
  constant slot info-product :: <string>,
    required-init-keyword: product:;
  constant slot info-version :: <string>,
    required-init-keyword: version:;
  constant slot info-platform :: <string>,
    required-init-keyword: platform:;
  constant slot info-relative-location :: <directory-locator>,
    required-init-keyword: relative-location:;
  constant slot info-binary :: false-or(<library-binary-info>) = #f,
    init-keyword: binary:;
  constant slot info-database :: <string>,
    required-init-keyword: database:;
  constant slot info-lib :: <string>,
    required-init-keyword: lib:;
end class <library-release-info>;

define method find-library-info
    (name :: <string>) => (library :: false-or(<library-info>))
  find-library-info(as(<symbol>, name))
end method find-library-info;

define method find-library-info
    (name :: <symbol>) => (library :: false-or(<library-info>))
  element(*libraries*, name, default: #f)
end method find-library-info;

define method info-location
    (info :: <library-info>) => (location :: <file-locator>)
  /*---*** This is ultimately the right implementation
  merge-locators(make(<file-locator>, 
		      name: info.info-project,
		      directory: info.info-source-directory),
		 info.info-library-pack.info-location)
  */
  merge-locators(make(<file-locator>, 
		      name: info.info-project,
		      directory: info.info-source-directory),
		 release-sources-directory())
end method info-location;

define method info-location
    (info :: <example-info>) => (location :: <file-locator>)
  merge-locators(make(<file-locator>, 
		      name: info.info-project,
		      directory: info.info-source-directory),
		 release-examples-directory())
end method info-location;

define method info-location
    (info :: <test-suite-info>) => (location :: <file-locator>)
  merge-locators(make(<file-locator>, 
		      name: info.info-project,
		      directory: info.info-source-directory),
		 release-sources-directory())
end method info-location;


/// Library Packs

define constant *library-packs* :: <object-table> = make(<object-table>);

define class <library-pack-info> (<described-release-info>)
  constant slot info-manual :: false-or(<string>) = #f,
    init-keyword: manual:;
  constant slot info-pack-number :: false-or(<integer>) = #f,
    init-keyword: number:;
  constant slot info-required? :: <boolean> = #f,
    init-keyword: required?:;
  constant slot info-author :: false-or(<string>) = #f,
    init-keyword: author:;
  constant slot info-company :: false-or(<string>) = #f,
    init-keyword: company:;
  constant slot info-copyright :: false-or(<string>) = #f,
    init-keyword: copyright:;
end class <library-pack-info>;

define method initialize
    (info :: <library-pack-info>, #key) => ()
  next-method();
  *library-packs*[info.info-name] := info;
  if (info.info-pack-number)
    install-numbered-library-pack-info(info)
  end
end method initialize;

define function installed-library-packs
    () => (library-packs :: <sequence>)
  let library-packs = make(<simple-object-vector>, size: size(*library-packs*));
  for (library-pack in *library-packs*,
       i from 0)
    library-packs[i] := library-pack
  end;
  sort!(library-packs, 
	test: method (info1 :: <library-pack-info>, info2 :: <library-pack-info>)
		info1.info-name < info2.info-name
	      end)
end function installed-library-packs;

define class <basic-library-pack-info> (<library-pack-info>)
  constant slot info-location :: <directory-locator>,
    required-init-keyword: location:;
  constant slot info-libraries :: <vector>,
    required-init-keyword: libraries:;
  constant slot info-examples :: <vector>,
    required-init-keyword: examples:;
  constant slot info-test-suites :: <vector>,
    required-init-keyword: test-suites:;
end class <basic-library-pack-info>;

define class <numbered-library-pack-info> (<library-pack-info>)
end class <numbered-library-pack-info>;

define constant $maximum-library-packs :: <integer> = $machine-word-size;

define constant $release-library-packs
  = make(<simple-object-vector>, size: $maximum-library-packs + 1, fill: #f);

define function install-numbered-library-pack-info
    (info :: <library-pack-info>) => ()
  let pack :: <integer> = info.info-pack-number;
  assert(pack > 0 & pack <= $maximum-library-packs,
	 "Library pack number must be between 1 and %d, inclusive", $maximum-library-packs);
  $release-library-packs[pack] := info
end function install-numbered-library-pack-info;

define method find-library-pack-info
    (pack :: <symbol>) => (info :: false-or(<library-pack-info>))
  element(*library-packs*, pack, default: #f)
    | block (return)
	for (i :: <integer> from 1 to $maximum-library-packs)
	  let info :: false-or(<library-pack-info>) = $release-library-packs[i];
	  when (info)
	    when (info.info-name = pack)
	      return(info)
	    end
	  end
	end;
	return(#f)
      end
end method find-library-pack-info;

define method find-library-pack-info
    (pack :: <integer>) => (info :: false-or(<library-pack-info>))
  pack > 0 & pack <= $maximum-library-packs 
    & $release-library-packs[pack]
end method find-library-pack-info;

define macro library-pack-definer
  { define library-pack ?pack-name:name 
      ?options:*
    end }
    => { let info
           = make(<numbered-library-pack-info>,
		  name: ?#"pack-name",
		  ?options,
		  title: ?"pack-name" ## " Library Pack",
		  description: "{None}");
         install-numbered-library-pack-info(info) }
end macro library-pack-definer;

define method read-library-pack
    (locator :: <file-locator>) => (info :: <library-pack-info>)
  let xml = read-xml-document(locator);
  let node = xml.document-element;
  let name = as(<symbol>, node-attribute(node, "name"));
  let number = node-attribute(node, "number");
  let library-nodes = select-nodes(node, "libraries/library");
  let library-pack
    = make(<basic-library-pack-info>,
	   name: name,
	   title: node-attribute(node, "title"),
	   number: number & string-to-integer(number),
	   description: select-node-text(node, "description") | "",
	   author: select-node-text(node, "author"),
	   company: select-node-text(node, "company"),
	   copyright: select-node-text(node, "copyright"),
	   location: locator-directory(locator),
	   libraries: map(interpret-library-xml, library-nodes),
	   examples: map(method (node :: <xml-node>)
			   interpret-library-xml(node, class: <example-info>)
			 end,
			 select-nodes(node, "examples/library")),
	   test-suites: map(method (node :: <xml-node>)
			      interpret-library-xml(node, class: <test-suite-info>)
			    end,
			    select-nodes(node, "test-suites/library")));
  for (library :: <library-info> in library-pack.info-libraries,
       library-node :: <xml-node> in library-nodes)
    let releases
      = map(method (release-node :: <xml-node>) => (info :: <library-release-info>)
	       interpret-library-release-xml(library.info-name, release-node)
	     end,
	     select-nodes(library-node, "release"));
    if (~empty?(releases))
      let this-release = releases[0];
      let binary = this-release.info-binary;
      let binary-name
	= if (binary) 
	    binary.info-binary-name
	  else 
	    format-to-string("%s.%s", library.info-name, "dll")
	  end;
      library.info-library-pack := library-pack;
      library.info-releases     := releases;
      library.info-binary       := binary;
      library.info-binary-name  := binary-name;
      library.info-database     := this-release.info-database;
      library.info-lib          := this-release.info-lib;
    end
  end;
  for (example :: <example-info> in library-pack.info-examples)
    example.info-library-pack := library-pack
  end;
  for (test-suite :: <test-suite-info> in library-pack.info-test-suites)
    test-suite.info-library-pack := library-pack
  end;
  library-pack
end method read-library-pack;

define method interpret-library-xml
    (node :: <xml-node>, 
     #key class :: subclass(<library-info>) = <library-info>)
 => (info :: <library-info>)
  let sources-node = select-single-node(node, "sources");
  let name = as(<symbol>, node-attribute(node, "name"));
  make(class,
       name: name,
       title: node-attribute(node, "title") | as-lowercase(as(<string>, name)),
       categories: map(node-text, select-nodes(node, "category")),
       description: select-node-text(node, "description") | "",
       source-directory: as(<directory-locator>, node-attribute(sources-node, "location")),
       project: select-node-text(sources-node, "project"))
end method interpret-library-xml;

define method interpret-library-release-xml
    (name :: <symbol>, node :: <xml-node>) => (info :: <library-release-info>)
  let binary-node = select-single-node(node, "binary");
  make(<library-release-info>,
       product: node-attribute(node, "product"),
       version: node-attribute(node, "version"),
       platform: node-attribute(node, "platform"),
       relative-location: as(<directory-locator>,
			     format-to-string("build/%s/Releases/Functional Developer 2.1",
					      name)),
       binary: binary-node & interpret-library-binary-xml(name, binary-node),
       database: select-node-text(node, "database") | format-to-string("%s.%s", name, "ddb"),
       lib: select-node-text(node, "lib") | format-to-string("%s.%s", name, "lib"))
end method interpret-library-release-xml;

define method interpret-library-binary-xml
    (name :: <symbol>, node :: <xml-node>) => (info :: <library-binary-info>)
  let merge-parent = element(*libraries*, name);
  make(<library-binary-info>,
       binary-name: node-attribute(node, "file"),
       merged-libraries: map(method (merge-node :: <xml-node>)
			       let library-name = as(<symbol>, merge-node.node-text);
			       let library = element(*libraries*, library-name);
			       library.info-merge-parent := merge-parent;
			       library
			     end,
			     select-nodes(node, "merge")))
end method interpret-library-binary-xml;

// Install the default library packs
if (file-exists?(release-library-packs-directory()))
  do-directory(method(directory :: <directory-locator>, name :: <string>, type)
                   if (type == #"directory")
                     let xml-directory
                       = subdirectory-locator(directory, name);
                     let xml-locator
                       = make(<file-locator>,
                              //---*** UNIX/Linux filesystem is case sensitive;
                              //       we need to clean this up!
                              base: as-lowercase(name),
                              extension: $library-pack-extension,
                              directory: xml-directory);
                     if (file-exists?(xml-locator))
                       read-library-pack(xml-locator);
                     end if;
                   end if;
               end,
               release-library-packs-directory());
end if;


/// Merged library DLL information

define method merged-project-name
    (library :: <symbol>) => (merged-library :: <symbol>)
  let info = find-library-info(library);
  let merge-parent = info & info.info-merge-parent;
  if (merge-parent)
    merge-parent.info-name
  else
    library
  end
end method merged-project-name;

define method merged-project-libraries
    (library :: <symbol>)
 => (parent :: <symbol>, libraries :: <sequence>)
  let library-info = find-library-info(library);
  let parent-info =
    if (library-info) library-info.info-merge-parent | library-info end;
  let parent-binary-info = parent-info & parent-info.info-binary;
  let parent = if (parent-info) parent-info.info-name else library end;
  values(parent,
	 if (parent-binary-info)
           map(info-name, parent-binary-info.info-merged-libraries);
	 else
	   #[]
	 end)
end method merged-project-libraries;


/// Library category handling

define class <library-category-info> (<release-info>)
  constant slot info-name :: <symbol>,
    required-init-keyword: name:;
  constant slot info-title :: <string>,
    required-init-keyword: title:;
  constant slot info-subcategories :: <sequence> = make(<stretchy-object-vector>),
    init-keyword: subcategories:;
  constant slot info-libraries :: <sequence> = make(<stretchy-object-vector>),
    init-keyword: libraries:;
end class <library-category-info>;

define method info-description
    (info :: <library-category-info>) => (description :: <string>)
  format-to-string("%s libraries", info.info-title)
end method info-description;

define method installed-library-categories
    (libraries-function :: <function>) => (categories :: <sequence>)
  let root-category :: <library-category-info>
    = make(<library-category-info>, name: #"root", title: "Root");
  for (library-pack-info :: <library-pack-info> in installed-library-packs())
    for (library-info :: <library-info> in library-pack-info.libraries-function)
      for (category-name :: <string> in library-info.info-categories)
	let parent-category :: <library-category-info> = root-category;
	for (level-title :: <string> in %split(category-name, '/'))
	  let level-name :: <symbol> = as(<symbol>, level-title);
	  let category :: <library-category-info>
	    = block (return)
		for (category-info :: <library-category-info> in parent-category.info-subcategories)
		  if (category-info.info-name == level-name)
		    return(category-info)
		  end
		end;
		let category :: <library-category-info>
		  = make(<library-category-info>, 
			 name:  level-name,
			 title: level-title);
		add!(parent-category.info-subcategories, category);
		category
	      end;
	  parent-category := category
	end;
	add!(parent-category.info-libraries, library-info)
      end
    end
  end;
  root-category.info-subcategories
end method installed-library-categories;

//---*** cgay: This is a copy of split from functional-extensions
//       (now common-extensions) but the original has now diverged
//       from this one.
define sealed method %split
    (string :: <byte-string>, character :: <byte-character>,
     #key start :: <integer> = 0,
          end: _end :: <integer> = string.size,
          trim? :: <boolean> = #t)
 => (strings :: <stretchy-object-vector>)
  let old-position :: <integer> = start;
  let end-position :: <integer> = _end;
  let new-position :: <integer> = old-position;
  let results :: <stretchy-object-vector> = make(<stretchy-object-vector>);
  local method add-substring
	    (start :: <integer>, _end :: <integer>, #key last? :: <boolean> = #f) => ()
	  if (trim?)
	    while (start < _end & string[start] = ' ')
	      start := start + 1
	    end;
	    while (start < _end & string[_end - 1] = ' ')
	      _end := _end - 1
	    end
	  end;
	  // Don't ever return just a single empty string
	  if (~last? | start ~== _end | ~empty?(results))
	    add!(results, copy-sequence(string, start: start, end: _end))
	  end
	end method add-substring;
  while (new-position < end-position)
    if (string[new-position] = character)
      add-substring(old-position, new-position);
      new-position := new-position + 1;
      old-position := new-position
    else
      new-position := new-position + 1;
    end
  end;
  add-substring(old-position, new-position, last?: #t);
  results
end method %split;


/// Examples

define class <example-info> (<library-info>)
end class <example-info>;

define class <test-suite-info> (<library-info>)
end class <test-suite-info>;

/*--- Remove this when sure...
define class <group-info> (<described-release-info>)
end class <group-info>;

define class <library-group-info> (<group-info>)
  constant slot info-libraries :: <sequence>,
    required-init-keyword: libraries:;
end class <library-group-info>;

define class <dll-group-info> (<group-info>)
  constant slot info-dll-name :: <string>,
    required-init-keyword: dll-name:;
  constant slot info-libraries :: <sequence>,
    required-init-keyword: libraries:;
end class <dll-group-info>;

define class <project-info> (<described-release-info>)
  slot info-group :: <group-info>, init-keyword: group:;
end class <project-info>;

define method info-edition
    (example :: <example-info>) => (edition :: <symbol>)
  example.info-group.info-edition
end method info-edition;

define method info-library-pack
    (example :: <example-info>) => (library-pack :: false-or(<integer>))
  example.info-group.info-library-pack
end method info-library-pack;

define method info-available?
    (example :: <example-info>) => (available? :: <boolean>)
  info-available?(example.info-group)
end method info-available?;

define function example-location
    (info :: <example-info>) => (location :: false-or(<string>))
  let project = info-project(info);
  let examples-directory = release-examples-directory();
  if (project & examples-directory)
    as(<string>,
       merge-locators(as(<file-locator>, project), examples-directory))
  end
end function example-location;


/// Example groups

define macro example-group-definer
  { define ?edition:name example-group ?group-name:name (?options:*)
      ?examples:*
    end }
    => { let info
           = make(<example-group-info>,
		  name: ?#"group-name",
		  edition: ?#"edition",
		  examples: vector(?examples),
		  ?options);
         install-example-group(info) }
 options:
  { } => { }
  { ?option:*, ... }
    => { ?option, ... }
 option:
  { library-pack: ?pack:name }
    => { library-pack: library-pack-number(?#"pack") }
  { library-pack: ?pack:expression }
    => { library-pack: library-pack-number(?pack) }
  { ?other:* }
    => { ?other }
 examples:
  { } => { }
  { ?example:*; ... }
    => { ?example, ... }
 example:
  { example ?name:expression, ?example-options:* }
    => { make(<example-info>,
	      name: ?#"name",
	      ?example-options) }
end macro example-group-definer;

define class <example-group-info> (<group-info>)
  constant slot info-edition :: <symbol>,
    required-init-keyword: edition:;
  constant slot info-library-pack :: false-or(<integer>) = #f,
    init-keyword: library-pack:;
  constant slot info-examples :: <sequence>,
    required-init-keyword: examples:;
end class <example-group-info>;

define method initialize
    (group :: <example-group-info>, #key) => ()
  next-method();
  let examples = group.info-examples;
  for (example :: <example-info> in examples)
    example.info-group := group
  end
end method initialize;

define method info-available?
    (group :: <example-group-info>) => (available? :: <boolean>)
  let edition      = group.info-edition;
  let library-pack = group.info-library-pack;
  release-contains-edition?(edition)
    & (~library-pack | release-contains-library-pack?(library-pack))
end method info-available?;

define constant $root-example-group =
    make(<example-group-info>,
         edition: #"basic",
         name: #"root-example-group",
         title: "Examples",
         description: "Functional Developer example projects",
         examples: make(<stretchy-vector>));

define function release-example-groups () => (groups :: <sequence>)
  convert-example-info($root-example-group, release-examples-directory());
  info-examples($root-example-group)
end function release-example-groups;

define function install-example-group
    (info :: <example-group-info>) => ()
  let name = info-name(info);
  let groups = info-examples($root-example-group);
  remove!(groups, name,
	  test: method (info, name)
		  info-name(info) == name
		end);
  add!(groups, info)
end function install-example-group;


/// DLL groupings

define constant $release-dll-groups = make(<stretchy-vector>);

define function release-dll-groups () => (groups :: <sequence>)
  $release-dll-groups
end function release-dll-groups;

define function install-dll-group
    (info :: <dll-group-info>) => ()
  let name = info-name(info);
  remove!($release-dll-groups, name,
	  test: method (info, name)
		  info-name(info) == name
		end);
  add!($release-dll-groups, info)
end function install-dll-group;

define function find-dll-group-info
    (name :: <symbol>) => (dll-group :: false-or(<dll-group-info>))
  block (return)
    for (dll-group :: <dll-group-info> in $release-dll-groups)
      when (info-name(dll-group) == name)
	return(dll-group)
      end
    end
  end
end function find-dll-group-info;

define macro dll-group-definer
  { define dll-group ?group-name:name (?options:*)
      ?libraries:*
    end }
    => { let info
           = make(<dll-group-info>,
		  name: ?#"group-name",
		  libraries: vector(?libraries),
		  ?options,
		  title: ?"group-name",
		  description: "{None}");
         install-dll-group(info) }
 libraries:
  { } => { }
  { ?library:*; ... }
    => { ?library, ... }
 library:
  { library ?name:expression, ?library-options:* }
    => { make(<library-info>,
	      name: ?#"name",
	      ?library-options,
	      title: ?"name",
	      description: "{None}",
	      modules: #[]) }
end macro dll-group-definer;

define macro renamed-dll-definer
  { define renamed-dll ?group-name:name = ?dll-name:expression }
    => { let info
           = make(<dll-group-info>,
		  name: ?#"group-name",
		  title: ?"group-name",
		  dll-name: ?dll-name,
		  libraries: #[],
		  description: "{None}");
         install-dll-group(info) }
end macro renamed-dll-definer;

define macro renamed-dlls-definer
  { define renamed-dlls ()
      ?renamed-dll:*
    end }
    => { ?renamed-dll }
  renamed-dll:
    { } => { }
    { ?group-name:name = ?dll-name:expression; ... }
      => { define renamed-dll ?group-name = ?dll-name; ... }
end macro renamed-dlls-definer;


/// Library Groups
define constant $release-library-groups = make(<stretchy-vector>);

define function release-library-groups () => (groups :: <sequence>)
  $release-library-groups
end function release-library-groups;

define function install-library-group
    (info :: <library-group-info>) => ()
  let name = info-name(info);
  remove!($release-library-groups, name,
	  test: method (info, name)
		  info-name(info) == name
		end);
  add!($release-library-groups, info)
end function install-library-group;

define macro library-group-definer
  { define library-group ?group-name:name (?options:*)
      ?libraries:*
    end }
    => { let info
           = make(<library-group-info>,
		  name: ?#"group-name",
		  libraries: vector(?libraries),
		  ?options);
         install-library-group(info) }
 libraries:
  { } => { }
  { ?library:*; ... }
    => { ?library, ... }
 library:
  { library ?name:expression, ?library-options:* }
    => { make(<library-info>,
	      name: ?#"name",
	      ?library-options) }
end macro library-group-definer;
*/

