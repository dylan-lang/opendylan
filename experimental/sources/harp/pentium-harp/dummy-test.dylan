module: pentium-harp-test
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// Define a backend itself;


define variable pentium-backend = make(<pentium-back-end>);

define variable pb = pentium-backend;

define variable ext1 = ins--constant-ref(pb, "dylanXinternalXexternal_sym_1");
// define variable ext2 = ins--constant-ref(pb, "dylanXinternalXexternal_sym_2");
define variable int1 = ins--constant-ref(pb, "dylanXinternalXinternal_sym_1");
// define variable int2 = ins--constant-ref(pb, "dylanXinternalXinternal_sym_2");
define variable lit1 = ins--constant-ref(pb, "lit_1");
// define variable lit2 = ins--constant-ref(pb, "lit_2");

/// Some file tests ...

define class <complex-test> (<object>)
  constant slot test-function :: <function>, init-keyword: function:;
  constant slot test-start-line :: <integer>, init-keyword: start:;
  constant slot test-end-line :: <integer>, init-keyword: end:;
  constant slot test-invoker-args :: <sequence>, init-keyword: args:;
end class;

define method do-file-test 
    (be :: <pentium-back-end>, 
     filename :: <string>,
     outputter-options :: <vector>,
     data-function :: <function>,
     #rest code-tests) => ()
  let outputter = apply(make-harp-outputter, be, filename, outputter-options);
  block ()
    output-header(be, outputter);
    output-data-start(be, outputter);
    data-function(be, outputter);
    output-code-start(be, outputter);
    for (code-test in code-tests)
      let (code-function, keys, source-locator)
        = if (instance?(code-test, <complex-test>))
            let start-line = code-test.test-start-line;
            let end-line = code-test.test-end-line;
            let code-function = code-test.test-function;
            let args = code-test.test-invoker-args;
            let locator = locator-for-test(filename, start-line, end-line);
            values(code-function, args, locator);
          else values(code-test, #[], #f);
          end if;
      apply(run-test, 
            code-function, 
            outputter: outputter, 
            source-locator: source-locator,
            back-end: be, 
            keys);
    end for;
    output-footer(be, outputter);
  cleanup
    close-harp-outputter(be, outputter);
  end block;
end method;


define method external1 (be :: <pentium-back-end>, outputter)
  output-external(be, outputter, ext1);
end method;

define method internal1 (be :: <pentium-back-end>, outputter)
  output-variable(be, outputter, int1, 1, public?: #t);
end method;

define method literal1 (be :: <pentium-back-end>, outputter)
  output-variable(be, outputter, lit1, 1, public?: #f);
end method;

define method null-data (be :: <pentium-back-end>, outputter)
end method;


define macro lambda-test-definer
  { define lambda-test ?:name (?args:*) 
      name ?string:expression; 
      start-line ?start-line:expression;
      end-line ?end-line:expression;
      ?:body 
    end }
    => { define method ?name ## "-test" (?args) ?body end;
         define method ?name (#rest args)
           make(<complex-test>,
                function: ?name ## "-test",
                start: ?start-line,
                end: ?end-line,
                args: apply(vector, name: ?string, args));
         end }
end macro;

define lambda-test return-external1 (be :: <pentium-back-end>)
  name "ext1_iep"; 
  start-line 0; 
  end-line 0;

  let regs = be.registers;
  ins--move(be, regs.reg-result, ext1);
  ins--rts-and-drop(be, 0);
end;


     
define lambda-test return-internal1 (be :: <pentium-back-end>)
  name "int1_iep"; 
  start-line 0; 
  end-line 0;
  let regs = be.registers;
  ins--move(be, regs.reg-result, int1);
  ins--rts-and-drop(be, 0);
end;
     

/// Source Code Locations:
/// As a temporary test, source code locations are supplied as a list
/// in the following form:-
///   (pos-in-function, start-of-function-in-file, end-of-function-in-file)



define lambda-test return-literal1 (be :: <pentium-back-end>)
  name "lit1_iep"; 
  start-line 5; 
  end-line 10;
  let regs = be.registers;
  ins--scl(be, scl(1), #[]);
  ins--move(be, regs.reg-result, lit1);
  ins--scl(be, scl(3), #[]);
  ins--rts-and-drop(be, 0);
end;
  

define lambda-test call-literal1 (be :: <pentium-back-end>)
  name "lit1_caller_iep"; 
  start-line 13;
  end-line 20;

  with-harp (be)
    named reg zero, the-result;
    result result;

    ins--move(be, zero, 0);
    ins--scl(be, scl(2), vector(zero));
    ins--call(be, ins--constant-ref(be, "lit1_iep"), 0);
    ins--move(be, the-result, result);
    ins--scl(be, scl(3), vector(zero, the-result));
    ins--move(be, result, the-result);
    ins--scl(be, scl(5), vector(the-result));
    ins--rts-and-drop(be, 0);
  end with-harp;
end;

   
define lambda-test return-effective-address (be :: <pentium-back-end>)
  name "lea_iep"; 
  start-line 0; 
  end-line 0;

  with-harp (be)
    arg0 arg0;
    result result;
    tag tag1;
    named reg r1, r2;

    ins--tag(be, tag1);
    ins--move(be, r1, arg0);
    ins--lea0(be, r2, tag1);
    ins--add(be, result, r1, r2);
    ins--rts-and-drop(be, 0);
  end with-harp;
end;
    
define lambda-test return-effective-address2 (be :: <pentium-back-end>)
  name "lea_iep"; 
  start-line 0;
  end-line 0;

  with-harp (be)
    arg0 arg0;
    result result;
    tag tag1, tag2;
    named reg r1, r2, r3;

    ins--tag(be, tag1);
    ins--move(be, r1, arg0);
    ins--lea0(be, r2, tag1);
    ins--lea0(be, r3, tag2);
    ins--add(be, r1, r1, r3);
    ins--add(be, result, r1, r2);
    ins--tag(be, tag2);
    ins--rts-and-drop(be, 0);
  end with-harp;
end; 


define method file-test-1 (#rest all-keys, #key type, print-harp? = #t) => ()
  do-file-test(make(<pentium-back-end>),
               "test1",
               apply(vector, print-harp?: print-harp?, all-keys),
               external1,
               return-external1());
end method;

define method file-test-2 (#rest all-keys, #key type, print-harp? = #t) => ()
  do-file-test(make(<pentium-back-end>),
               "test2",
               apply(vector, print-harp?: print-harp?, all-keys),
               internal1,
               return-internal1());
end method;

define method file-test-3 (#rest all-keys, #key type, print-harp? = #t) => ()
  do-file-test(make(<pentium-back-end>),
               "test3",
               apply(vector, print-harp?: print-harp?, all-keys),
               literal1,
               return-literal1(),
               call-literal1());
end method;

define method file-test-4 (#rest all-keys, #key type, print-harp? = #t) => ()
  do-file-test(make(<pentium-back-end>),
               "test4",
               apply(vector, print-harp?: print-harp?, all-keys),
               null-data,
               return-effective-address());
end method;

define method file-test-5 (#rest all-keys, #key type, print-harp? = #t) => ()
  do-file-test(make(<pentium-back-end>),
               "test5",
               apply(vector, print-harp?: print-harp?, all-keys),
               null-data,
               return-effective-address2());
end method;

// second generation lambda tests .....

define method set-function-name
    (be :: <pentium-back-end>, name :: <byte-string>) 
  be.variables.function-name  := name;
end method;


define method run-test 
    (test :: <function>, 
     #key outputter = make-interactive-outputter(),
          source-locator = #f,
          static = #f,
          name = "dummy",
          back-end = pb)
  let be :: <harp-back-end> = pb;
  invoke-harp(back-end, test, name, 
              outputter: outputter, 
              source-locator: source-locator, 
              static: static,
              harp-debug: #t);
end method;

define method test0 (be :: <pentium-back-end>)
  let regs = be.registers;
  set-function-name(be, "_Tony0");
  ins--move(be, regs.reg-result, 99);
  ins--rts-and-drop(be, 0);
end;
     

define method test1 (be :: <pentium-back-end>)
  set-function-name(be, "_Tony1");
  ins--jmp(be, ins--constant-ref(be, "Tony0"), 0);
end;
     

define method test2 (be :: <pentium-back-end>)
  set-function-name(be, "_Tony2");
  ins--adjust-stack(be, 4);
  ins--call(be, ins--constant-ref(be, "Funny"), 0);
  ins--jmp(be, ins--constant-ref(be, "Tony0"), 0);
end;
     

define method test3 (be :: <pentium-back-end>)
  set-function-name(be, "_sdi_test");
  let argc = be.registers.reg-arg-count;
  let tag = make-tag(be);
  ins--bne(be, tag, argc, 0);
  for (i from 0 below 40)
    ins--add(be, argc, argc, 123456);
    ins--bne(be, tag, argc, 0);
  end for;
  ins--tag(be, tag);
  ins--jmp(be, ins--constant-ref(be, "Tony0"), 0);
end;
     

define method test4 (be :: <pentium-back-end>)
  let addr-tag = make-tag(be);
  set-function-name(be, "_Tony2");
  ins--adjust-stack(be, 4);
  ins--lea0(be, be.registers.reg-arg0, addr-tag);
  ins--call(be, ins--constant-ref(be, "Funny"), 1);
  ins--tag(be, addr-tag);
  ins--jmp(be, ins--constant-ref(be, "Tony0"), 0);
end;
     

define method test5 (be :: <pentium-back-end>)
  with-harp (be)
    tag addr-tag, finished-tag;

    set-function-name(be, "_Tony2");
    ins--lea0(be, be.registers.reg-arg0, addr-tag);
    ins--call(be, ins--constant-ref(be, "primitive_build_up_frame"), 1);
    ins--call(be, ins--constant-ref(be, "protected_form"), 0);
    ins--call(be, ins--constant-ref(be, "primitive_up_cleanup"), 0);
    ins--control-flow-link(be, addr-tag);
    ins--bra(be, finished-tag);
    ins--tag(be, addr-tag);
    ins--call(be, ins--constant-ref(be, "cleanup_form"), 0);
    ins--end-cleanup(be, finished-tag);
    ins--tag(be, finished-tag);
    ins--rts(be);
  end with-harp;
end;

define method test6 (be :: <pentium-back-end>)
  let regs = be.registers;
  let df = make-df-register(be);
  set-function-name(be, "_CallAlien1");
  be.variables.compiling-call-in := #t;
  ins--dmove(be, regs.reg-float-arg0, ins--df-indirect-constant-ref(be, "PI"));
  ins--call-alien(be, ins--constant-ref(be, "_sinh"), 0);
  ins--dmove(be, df, regs.reg-c-float-result);
  ins--dmove(be, regs.reg-float-result, df);
  ins--rts(be);
end;
     
define method test7 (be :: <pentium-back-end>)
  let regs = be.registers;
  with-harp (be)
    named dfreg df1;
    named reg rr1, rr2, rr3, rr4, rr5, rr6, rr7, rr8;

    set-function-name(be, "_CallAlien2");
    be.variables.compiling-call-in := #t;
    ins--load-stack-arg-n(be, rr1, 0);
    ins--load-stack-arg-n(be, rr2, 1);
    ins--add(be, rr3, rr1, rr1);
    ins--add(be, rr4, rr3, rr2);
    ins--add(be, rr5, rr3, rr2);
    ins--add(be, rr6, rr4, rr5);
    ins--add(be, rr8, rr3, rr6);
    ins--add(be, rr7, rr2, rr5);
    ins--add(be, rr8, rr8, rr7);
    ins--add(be, rr8, rr8, rr1);
    ins--move(be, regs.reg-c-result, rr8);
    ins--reset-values(be);
    ins--rts(be);
  end with-harp;
end;
     
     

define method test8 (be :: <pentium-back-end>)
  set-function-name(be, "_Tony8");
  ins--adjust-stack(be, 4);
  ins--scl(be, "Line 1", #[]);
  ins--call(be, ins--constant-ref(be, "Funny"), 0);
  ins--scl(be, "Line 2", #[]);
  ins--jmp(be, ins--constant-ref(be, "Tony0"), 0);
end;


define method defasm-test0 (be :: <pentium-back-end>)
  let regs = be.registers;
  let vars = be.variables;
  with-harp (be)
    named reg rr1, rr2, rr3, rr4, rr5, rr6, rr7;

    vars.compiling-defasm := #t;
  
    ins--rem(be, "This is a quick remark");
    ins--move(be, rr3, regs.reg-arg0);
    ins--load-stack-arg-n(be, rr1, 0);
    ins--load-stack-arg-n(be, rr2, 1);
   
    ins--ld(be, rr5, rr1, 56);
    ins--move(be, rr6, rr1);
  
    ins--call(be, rr6, 0);
  
    ins--store-stack-arg-n(be, rr2, 1);
    ins--store-stack-arg-n(be, rr6, 0);
    ins--move(be, regs.reg-arg0, rr3);
    ins--move(be, rr7, rr5);
  
    ins--jmp(be, rr7, 1);
    set-function-name(be, "_DEFASM0");
  end with-harp;
end;
     

/// dummy support for source code locators


define class <dummy-source-record> (<source-record>)
  constant slot source-record-name :: <byte-string>, required-init-keyword: name:;
end class;

define abstract class <dummy-source-locator> (<object>)
  constant slot locator-start-line, required-init-keyword: start-line:;
end class;

define class <dummy-absolute-source-locator> (<dummy-source-locator>)
  constant slot locator-source-record, required-init-keyword: record:;
  constant slot locator-end-line, required-init-keyword: end-line:;
end class;

define class <dummy-relative-source-locator> (<dummy-source-locator>)
end class;


define method locator-as-absolute-source-position 
    (locator :: <dummy-absolute-source-locator>) 
    => (source-pos :: <absolute-source-position>)
  make(<absolute-source-position>,
       source-record: locator.locator-source-record, 
       start-offset: locator.locator-start-line, 
       end-offset: locator.locator-end-line)
end method;


define method locator-for-test
    (name :: <byte-string>, start-line :: <integer>, end-line :: <integer>)
    => (locator)
  if (end-line = 0)
    #f
  else
    let dname = concatenate(name, ".dylan");
    let record = make(<dummy-source-record>, name: dname);
    make(<dummy-absolute-source-locator>, record: record, 
         start-line: start-line, end-line: end-line);
  end if;
end method;



define method scl 
    (line :: <integer>)
    => (scl :: <dummy-relative-source-locator>)
  make(<dummy-relative-source-locator>, start-line: line);
end method;


define method make-relative-source-position 
    (abs :: <absolute-source-position>, 
     locator :: <dummy-relative-source-locator>, 
     code-pos :: <integer>)
     => (source-pos :: <relative-source-position>)
  make(<relative-source-position>,
       line-number: locator.locator-start-line,
       code-position: code-pos)
end method;
