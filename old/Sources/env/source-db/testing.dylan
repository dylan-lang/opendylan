Module: source-db
Author: Roman Budzianowski
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/***
One way to test the source-db is to use the lib-manager
? require: lib-manager;
? in: lib-manager;
? init-dylan-world();
? dl := find-dylan-library(#"dylan-extensions");
? load-library-sources(dl);
 You can print the source-db:

? describe(dl.library-source, verbose?: #"full");

change a file in the library and update the sources

? update-library-sources(dl);

or you can update one file:

? file-loc := merge(directory(dl.lid-path), as(<locator>, filename)); if it works

? update-dylan-library-file(dl.library-source, as(<locator>, file-loc));

print the source system:

? describe(dl.library-source, verbose?: #"full");

***/

define method build-file-locator(directory :: <string>,
				 name :: <string>,
				 suffix :: <string>)
  let dir-locator = as(<locator>, directory);
  let file-locator = make(<locator>, base: name, extension: suffix);
  merge(dir-locator, file-locator);
end;

define variable *the-source-system* = make(<db-source-system>, name: "test-system");
define variable *a-snapshot* = #f;

define method reinit()
  *the-source-system* := make(<db-source-system>, name: "test-system");
end;

define method test-source-db()
  let sys = *the-source-system*;
  let loc = as(<physical-locator>, build-file-locator("", "test-file", "dylan"));
  update-or-boot-file(sys, loc);
  describe(sys, verbose?: full:);
end;

define method test-branch(#key branch-name = "test-branch")
  let sys = *the-source-system*;
  let test-branch = create-branch(sys, branch-name);
  sys.branch-list := pair(test-branch, sys.branch-search-list);
// or 
// sys.branch-list := #("test-branch", "base");
  test-source-db();
end;

  
define method test-snapshot()
  let sys = *the-source-system*;
  create-snapshot(sys, "test-snapshot");
  test-branch(branch-name: "after-snapshot");
  *a-snapshot* := sys.snapshots[#"test-snapshot"];
  if(*a-snapshot*)
    describe(*a-snapshot*, verbose?: full:);
  else
    format(*standard-output*, "Error in test-snapshot\n");
  end;
end;

define variable *ver* = as(<integer>, '1');

define method test-version()
  let sys = *the-source-system*;
  let (meta, base-branch) = find-file(sys, "test-file.dylan");
  let section = retrieve-one-section(sys, "method1");
  let text = shallow-copy(section.section-text);
  *ver* := *ver* + 1;
  text[59] := as(<character>, *ver*);
  update-file-section(meta, base-branch, section, text);
  describe(sys, verbose?: full:);
end;

  
 
