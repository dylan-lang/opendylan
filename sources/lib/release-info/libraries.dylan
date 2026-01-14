Module:    release-info
Synopsis:  Functional Developer release information
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
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
       binary-name: node-attribute(node, "file"))
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
