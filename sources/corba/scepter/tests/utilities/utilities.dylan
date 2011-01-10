Module: scepter-tests
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define macro sequence
  { sequence(?class:expression, ?elements:*) }
  =>
  { begin
      let seq = make(?class);
      ?elements;
      seq;
    end }

  elements:
  { } => { }
  { ?element:expression , ?elements:* } => { seq := add!(seq, ?element); ?elements }
end macro;

define class <test-scepter> (<scepter>)
end class;

define method scepter-default-back-end-class (scepter :: <test-scepter>)
 => (back-end-class :: subclass(<scepter-back-end>));
  scepter-back-end-class("dump");
end method;

define method scepter-default-front-end-class (scepter :: <test-scepter>)
 => (front-end-class :: subclass(<scepter-front-end>));
  scepter-front-end-class("file");
end method;  

define method scepter-report-condition (scepter :: <test-scepter>, condition :: <simple-condition>)
 => ()
  let stream = *standard-error*;
  idl-condition-header(stream, condition);
  format(stream, "unrecoverable internal error: ");
  apply(format, stream, condition-format-string(condition), condition-format-arguments(condition));
end method;

define method scepter-report-condition (scepter :: <test-scepter>, condition :: <idl-condition>)
 => ()
  let stream = *standard-error*;
  idl-condition-header(stream, condition);
  idl-condition-title(stream, condition);
  idl-condition-body(stream, condition);
end method;

define method idl-condition-header (stream :: <stream>, condition :: <idl-condition>)
  format(stream, "\n%s: ", "Test Scepter");
  if (condition.idl-condition-source)
    format(stream, "%s: ", as(<string>, condition.idl-condition-source));
  end if;
end method;

define method idl-condition-header (stream :: <stream>, condition :: <condition>)
  let scepter = get-scepter();
  format(stream, "\n%s: ", "Test Scepter");
  if (scepter.scepter-source)
    format(stream, "%s: ", as(<string>, scepter.scepter-source));
  end if;
end method;

define method scepter-format-progress-start (scepter :: <test-scepter>, control-string, #rest args)
  format(*standard-error*, "\n");
  apply(format, *standard-error*, control-string, args);
  format(*standard-error*, "...");
end method;

define method scepter-format-progress-end (scepter :: <test-scepter>, result, control-string, #rest args)
  apply(scepter-format-progress-start, scepter, control-string, args);
  if (result)
    format(*standard-error*, "Done.");
  else
    format(*standard-error*, "Aborted.");
  end if;
end method;  

set-scepter(make(<test-scepter>, directory: working-directory()));

define method scepter-reset-options (scepter :: <test-scepter>)
  scepter-initialize-options(scepter);
  scepter-informative?(scepter) := #t;
end method;

define method scepter-process-options (scepter :: <test-scepter>)
 => ()
  // do nothing
end method;

define method scepter-parse-options (scepter :: <test-scepter>)
 => ()
  // do nothing
end method;

define class <idl-test-file> (<object>)
  constant slot idl-test-file-name :: <string>, required-init-keyword: name:;
  constant slot idl-test-file-original :: <string>, required-init-keyword: original:;
  constant slot idl-test-file-regression :: false-or(<string>) = #f, init-keyword: regression:;
  constant slot idl-test-file-setup :: <function> = identity, init-keyword: setup:;
end class;

define method idl-test-file-original-name (file :: <idl-test-file>)
  concatenate(file.idl-test-file-name, ".idl");
end method;

define method idl-test-file-new-name (file :: <idl-test-file>)
  concatenate(file.idl-test-file-name, ".new");
end method;

define method idl-test-file-regression-name (file :: <idl-test-file>)
  concatenate(file.idl-test-file-name, ".old");
end method;

define method test-idl-file (files :: <table>,
                             filename :: <string>,
                             #key regression? :: <boolean>)
  let scepter = get-scepter();
  with-progress (scepter, "Testing %s", filename)                             
    let file = files[as(<symbol>, filename)];
    scepter-sources(scepter) := sequence(<stretchy-vector>, file.idl-test-file-original-name);
    scepter-front-end(scepter) := make(scepter-front-end-class("file"), scepter: scepter);
    scepter-back-ends(scepter) := sequence(<deque>, make(scepter-back-end-class("dump"), scepter: scepter));
    (file.idl-test-file-setup)(scepter);
    scepter-invoke(scepter);
    if (regression?)
      unless (file.idl-test-file-regression)
	error(make(<simple-error>,
		   format-string: "no regression test contents for %s",
		   format-arguments: vector(filename)));
      end unless;
      compare-tokens(file.idl-test-file-new-name, file.idl-test-file-regression-name);
    else
      compare-tokens(file.idl-test-file-new-name, file.idl-test-file-original-name);
    end if;
  end with-progress;
end method;

define method compare-tokens (new-file :: <string>, old-file :: <string>)
  let scepter = get-scepter();
  with-progress(scepter, "Comparing %s and %s", new-file, old-file)
    block (return)
      with-open-file(new-stream = new-file, direction: #"input")
	let new-token-stream = #f;
	block ()
	  new-token-stream := make-token-stream(new-stream, new-file);
	  let new-token = read-element(new-token-stream);
	  with-open-file(old-stream = old-file, direction: #"input")
	    let old-token-stream = #f;
	    block ()
	      old-token-stream := make-token-stream(old-stream, old-file);
	      let old-token = read-element(old-token-stream);
	      until (new-token = $lexer-eoi-token | old-token = $lexer-eoi-token)
		unless ((new-token.idl-parser-tag == old-token.idl-parser-tag) &
			(new-token.idl-lexer-string = old-token.idl-lexer-string))
                  format(scepter.scepter-output, "\nTest Failure: IDL dump mismatch in new file \"%s\" at line %d: %s",
                         new-file, new-token.source-line, new-token.lexer-string);
		  return(#f);
		end unless;
		new-token := read-element(new-token-stream);
		old-token := read-element(old-token-stream);
	      end until;
	      unless (new-token = $lexer-eoi-token & old-token = $lexer-eoi-token)
		return(#f);
	      end unless;
	    cleanup
	      close(old-token-stream);
	    end block;
	  end with-open-file;
	cleanup
	  close(new-token-stream);
	end block;
      end with-open-file;
      return(#t);
    end block;
  end with-progress;
end method;

define method add-idl-file!(files :: <table>,
                            filename :: <string>,
                            original :: <string>,
                            #key
                            regression :: false-or(<string>) = #f,
                            setup :: <function> = identity)
  files[as(<symbol>, filename)] := 
    make(<idl-test-file>, name: filename, original: original, regression: regression, setup: setup);
end method;

define method setup-idl-files (files :: <table>)
  do(do-setup-1-idl-file, files);
end method;

define method do-setup-1-idl-file (file :: <idl-test-file>)
  with-open-file (stream = file.idl-test-file-original-name, direction: #"output")
    write(stream, file.idl-test-file-original);
  end with-open-file;
  if (file.idl-test-file-regression)
    with-open-file (stream = file.idl-test-file-regression-name, direction: #"output")
      write(stream, file.idl-test-file-regression);
    end with-open-file;
  end if;
end method;  

/*
define method setup-1-idl-file (files :: <table>, file-name :: <string>)
  do-setup-1-idl-file(files[as(<symbol>, file-name)]);
end method;
*/

define method cleanup-idl-files (files :: <table>)
  do(do-cleanup-1-idl-file, files);
end method;

define method do-cleanup-1-idl-file (file :: <idl-test-file>)
  let original-name = file.idl-test-file-original-name;
  if (file-exists?(original-name))
    delete-file(original-name);
  end if;
  let new-name = file.idl-test-file-new-name;
  if (file-exists?(new-name))
    delete-file(new-name);
  end if;
  if (file.idl-test-file-regression)
    let regression-name = file.idl-test-file-regression-name;
    if (file-exists?(regression-name))
      delete-file(regression-name);
    end if;
  end if;
end method;

/*
define method cleanup-1-idl-file (files :: <table>, file-name :: <string>)
  do-cleanup-1-idl-file(files[as(<symbol>, file-name)]);
end method;
*/

define method test-damaged-idl-file (files :: <table>, filename :: <string>)
  let file = files[as(<symbol>, filename)];
  run-damaged(file.idl-test-file-original-name, #(deletion:, insertion:));
  #t; // unless it crashes it worked!
end method;

define method run-damaged (file-name :: <string>, failure-modes :: <sequence>, #key start = 0, limit = #f)
  let scepter = get-scepter();
  let token-count = count-tokens-in(file-name);
  scepter-sources(scepter) := sequence(<stretchy-vector>, file-name);
  for (fm in failure-modes)
    format(*standard-error*, "\n\nFailure Mode: %=", fm);
    scepter-failure-mode(scepter) := fm;
    for (ft from start to (limit | token-count))
      format(*standard-error*, "\n\nFailure Point: %=", ft);
      scepter-failure-token(scepter) := ft;
      scepter-invoke(scepter);
    end for;
  end for;
end method;

define method count-tokens-in (file-name :: <string>)
  let count = 0;
  with-open-file(stream = file-name, direction: #"input")
    let token-stream = #f;
    block ()
      token-stream := make-token-stream(stream, file-name);
      let token = read-element(token-stream);
      until (token = $lexer-eoi-token)
        count := count + 1;
        token := read-element(token-stream);
      end until;
    cleanup
      close(token-stream);
    end block;
  end with-open-file;
  count;
end method;

