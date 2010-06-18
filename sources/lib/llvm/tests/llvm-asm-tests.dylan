Module:       llvm-test-suite
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2009 Gwydion Dylan Maintainers
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define macro llvm-asm-suite-definer
  { define llvm-asm-suite ?suite-name:name (?keyword-args:*) ?options end }
    => { define variable ?suite-name
           = make-suite(?"suite-name", 
		        method ()
		          llvm-asm-suite-components(?"suite-name", ?options)
		        end,
                        ?keyword-args) }
options:
    { } => { }
    { ?option; ... } => { ?option, ... }
    
option:
    { directory ?:expression } => { ?expression }
end macro llvm-asm-suite-definer;

define llvm-asm-suite llvm-asm-suite ()
end llvm-asm-suite;

define function llvm-asm-suite-components
    (name :: <string>, #rest directories)
 => (components :: <sequence>);
  ignore(directories);
  block (return)
    let llvm-tests-path :: false-or(<string>)
      = environment-variable("LLVM_TEST");
    if (~llvm-tests-path)
      signal("The LLVM_TEST environment variable is not set");
      return(#[]);
    end if;
    let llvm-tests-directory :: <directory-locator>
      = as(<directory-locator>, llvm-tests-path);

    let components = make(<stretchy-object-vector>);

    let directory-worklist :: <deque> = make(<deque>);
    push-last(directory-worklist, llvm-tests-directory);
    
    while (~empty?(directory-worklist))
      let suite-directory = pop(directory-worklist);

      let tests = make(<stretchy-object-vector>);
      do-directory(method (directory :: <directory-locator>, name, type)
                     if (type == #"file")
                       let file-locator = as(<file-locator>, name);
                       if (file-locator.locator-extension = "ll")
                         let test-function
                           = make-llvm-test-function(directory, file-locator);
                         if (test-function)
                           let test
                             = make(<test>,
                                    name: name,
                                    function: test-function);
                           add!(tests, test);
                         end if;
                       end if;
                     elseif (type == #"directory" & name[0] ~= '.')
                       push-last(directory-worklist,
                                 merge-locators(as(<directory-locator>, name),
                                                directory));
                     end if;
                   end method,
                   suite-directory);

      unless (empty?(tests))
        let directory-name = suite-directory.locator-path.last;
        let suite = make-suite(concatenate("llvm-", directory-name, "-suite"),
                               always(tests));
        add!(components, suite);
      end unless;
    end while;

    components
  end block
end function;

define function make-llvm-test-function
    (directory :: <directory-locator>, file-locator :: <file-locator>)
 => (test-function :: false-or(<function>));
  let merged-file-locator = merge-locators(file-locator, directory);

  let marked-not?
    = with-open-file(stream = merged-file-locator)
        peek(stream, on-end-of-stream: #f) == ';'
          & subsequence-position(read-line(stream) | "", "; RUN: not ")
      end with-open-file;

  if (marked-not?)
    method ()
      let module
        = make(<llvm-module>, name: as(<string>, merged-file-locator));
      with-open-file (stream = merged-file-locator)
        check-condition(format-to-string("Parse %s detects errors",
                                         file-locator),
                        <error>, llvm-asm-parse(module, stream));
      end with-open-file;
    end method
  else
    method ()
      let reference :: <string>
        = with-application-output (stream = "llvm-as | llvm-dis",
                                   under-shell?: #t,
                                   input: merged-file-locator,
                                   error: #"null")
            read-to-end(stream)
          end;
      
      let module
        = make(<llvm-module>, name: as(<string>, merged-file-locator));

      let parsed? = #f;
      with-open-file (stream = merged-file-locator)
        check-no-errors(format-to-string("Parse %s", file-locator),
                        begin
                          llvm-asm-parse(module, stream);
                          parsed? := #t;
                        end);
      end with-open-file;

      let saved? = #f;
      let bc-pathname = "out.bc"; // FIXME
      if (parsed?)
        check-no-errors(format-to-string("Write .bc for %s", file-locator),
                        begin
                          llvm-save-bitcode-file(module, bc-pathname);
                          saved? := #t;
                        end);
      end if;

      if (saved?)
        check-equal(format-to-string("Disassembly of .bc for %s"
                                       " matches reference", file-locator),
                    reference,
                    with-application-output (stream = "llvm-dis",
                                             input: bc-pathname,
                                             error: #"null")
                      read-to-end(stream)
                    end);
      end if;
    end method
  end if
end function;
