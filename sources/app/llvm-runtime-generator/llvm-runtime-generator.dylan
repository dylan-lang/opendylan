Module:       llvm-runtime-generator
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              Additional code is Copyright 2010 Gwydion Dylan Maintainers
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define function generate-runtime
    (architecture :: <symbol>, os :: <symbol>) => ();
  with-booted-dylan-context
    without-dependency-tracking
      let back-end :: <llvm-back-end>
        = find-back-end-object(#"llvm", architecture, os);
      with-back-end-initialization(back-end)
        let m = make(<llvm-module>,
                     name: "runtime",
                     target-triple: llvm-back-end-target-triple(back-end),
                     data-layout: llvm-back-end-data-layout(back-end));
        back-end.llvm-builder-module := m;
        
        // FIXME do the real work here...
        
        let output-basename
          = format-to-string("%s-%s-runtime", architecture, os);
        let output-locator
          = make(<file-locator>, base: output-basename, extension: "bc");
        llvm-save-bitcode-file(m, output-locator);
      end with-back-end-initialization;
    end without-dependency-tracking;
  end;
end function;

block ()
  let arguments = application-arguments();
  if (arguments.size = 1)
    let name = arguments[0];
    // Split into processor and os portions
    let separator-position = position(name, '-');
    let processor-name = copy-sequence(name, end: separator-position);
    let os-name = copy-sequence(name, start: separator-position + 1);
    // Generate runtime support for the requested platofmr
    generate-runtime(as(<symbol>, processor-name), as(<symbol>, os-name));
  else
    format(*standard-error*, "Usage: llvm-runtime-generator processor-os\n");
    exit-application(1);
  end if;
exception (e :: <simple-error>)
  format(*standard-error*, "%s\n", e);
  exit-application(1);
end;
