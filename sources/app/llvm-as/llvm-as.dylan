Module:    llvm-as
Author:    Peter S. Housel
Copyright:    Original Code is Copyright 2009-2018 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method main () => ()
  let parser = make(<command-line-parser>,
                    min-positional-options: 1,
                    max-positional-options: 1);
  add-option(parser,
             make(<flag-option>,
                  names: #("b", "output-builder"),
                  default: #f,
                  variable: "WHAT",
                  help: "Output builder code to standard output"));
  add-option(parser,
             make(<parameter-option>,
                  names: #("o"),
                  variable: "FILE",
                  help: "Override bitcode output filename"));

  block ()
    parse-command-line(parser, application-arguments(),
                       description: "assemble LLVM assembly language");
    let filename = parser.positional-options[0];
    let module = make(<llvm-module>, name: filename);
    with-open-file(stream = filename)
      llvm-asm-parse(module, stream);
    end;
    let outfilename = get-option-value(parser, "o");
    if (outfilename)
      llvm-save-bitcode-file(module, outfilename);
    else
      let locator = as(<file-locator>, filename);
      let output-locator
        = make(<file-locator>,
               directory: locator.locator-directory,
               base: locator.locator-base,
               extension: "bc");
      llvm-save-bitcode-file(module, output-locator)
    end if;
    if (get-option-value(parser, "output-builder"))
      llvm-write-builder(module, *standard-output*);
    end if;

    exit-application(0);
  exception (e :: <simple-error>)
    format(*standard-error*, "%s\n", e);
    force-output(*standard-output*);
    exit-application(1);
  end;
end method main;

begin
  main();
end;
