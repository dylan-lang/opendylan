module: dfmc-debug
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// f :: (ld:: <library-description>, sr :: <source-record>, l :: <locator>) => ()
define function walk-library-sources 
    (name :: <string>,  f :: <function>, #key used-too? = #f)
  load-library(as-lowercase(name));
  let ld :: <library-description> 
    = lookup-library-description(as-lowercase(name));
  let all-lds = 
    if (used-too?)
      pair(ld, all-used-library-descriptions(ld));
    else
      list(ld);
    end if;
  for (ld in all-lds)
    let lib-crs = library-description-compilation-records(ld);
    let lib-srs = map(compilation-record-source-record, lib-crs);
    let source-locs = map(source-record-location, lib-srs);
    do(curry(f, ld), lib-srs, source-locs);
  end for;
end function;

// ex:  run-command-on-sources("dir", "play")
define function run-command-on-sources(command :: <string>, lib-name :: <string>)
  local method f (ld :: <library-description>, sr :: <source-record>, l :: <locator>) => ()
    let command-string = format-to-string("%s %s", command, l);
    format-out("doing %s ...\n", command-string);
    run-application(command-string);
  end;
  walk-library-sources(lib-name, f, used-too?: #t);
end function;

define function run-command-on-source-directories(command :: <string>, lib-name :: <string>)
  let cur-ld = #f;
  local method f (ld :: <library-description>, sr :: <source-record>, l :: <locator>) => ()
    if (~cur-ld | cur-ld ~= ld)
      let dir = as(<string>, locator-directory(l));
      let command-string = format-to-string("%s %s", command, dir);
      format-out("doing %s ...\n", command-string);
      run-application(command-string);
      cur-ld := ld;
    end if;
  end;
  walk-library-sources(lib-name, f, used-too?: #t);
end function;
