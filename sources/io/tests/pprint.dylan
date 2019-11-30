Module:       io-test-suite
Synopsis:     IO library test suite
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// Pretty stream testing

register-stream-class-info("<pretty-stream>", <pretty-stream>,
                           input-stream?: #f,
                           output-stream?: #t,
                           element-type: <object>);

define constant $pretty-printing-test-string
  = "This is an extremely long paragraph that should be"
    " pretty printed into a number of lines of text. This"
    " is some padding to the paragraph to ensure that it"
    " is long enough.";

define sideways method make-stream-tests-of-size
    (class :: subclass(<pretty-stream>), stream-size :: <integer>)
 => (streams :: <sequence>)
  let class-info = stream-class-info(class);
  let character-sequence = copy-sequence($pretty-printing-test-string, end: stream-size);
  let tests :: <stretchy-object-vector> = make(<stretchy-object-vector>);
  local method add-pretty-stream-test
            (contents :: <sequence>)
         => (test :: <stream-test-info>)
          let test
            = make(<stream-test-info>,
                   test-name: format-to-string("%s size %d",
                                               class-info.info-class-name,
                                               stream-size),
                   class-info: class-info,
                   contents: contents,
                   direction: #"output",
                   make-function: method ()
                                    let target-stream
                                      = make(<sequence-stream>, direction: #"output");
                                    make(<pretty-stream>,
                                         sequence: contents,
                                         target: target-stream);
                                  end);
          add!(tests, test);
          test
        end method add-pretty-stream-test;
  add-pretty-stream-test(character-sequence);
  tests
end method make-stream-tests-of-size;


/// Pretty printing interface testing

define macro with-pretty-print-to-string
  { with-pretty-print-to-string (?stream:name, #rest ?options:*)
      ?body:body
    end }
 => { let ss :: <string-stream> = make(<string-stream>, direction: #"output");
      let ?stream :: <pretty-stream>
        = make(<pretty-stream>, target: ss, ?options);
      ?body;
      close(?stream);
      stream-contents(ss, clear-contents?: #f) }
end macro with-pretty-print-to-string;

define test test-*print-miser-width* ()
  //---*** Fill this in...
end test;

define test test-*default-line-length* ()
  //---*** Fill this in...
end test;

define test test-pprint-logical-block ()
  //---*** Fill this in...
end test;

define test test-pprint-newline ()
  //---*** Fill this in...
end test;

define test test-pprint-indent ()
  //---*** Fill this in...
end test;

define test test-pprint-tab ()
  // Adapted from XP's tests
  local
    method do-tab-a (kind, colnum :: <integer>, colinc :: <integer>)
      with-pretty-print-to-string (ps)
        write(ps, "tes");
        printing-logical-block (ps)
          write(ps, "t");
          pprint-tab(kind, colnum, colinc, ps);
          write(ps, "a");
        end;
      end
    end;
  assert-equal("test a",
               do-tab-a(#"line", 1, 1));
  assert-equal("test    a",
               do-tab-a(#"line", 8, 1));
  assert-equal("test    a",
               do-tab-a(#"line", 8, 3));
  assert-equal("test  a",
               do-tab-a(#"line", 0, 3));
  assert-equal("test    a",
               do-tab-a(#"line", 0, 4));
  assert-equal("test a",
               do-tab-a(#"line", 0, 5));

  assert-equal("test a",
               do-tab-a(#"line-relative", 1, 1));
  assert-equal("test        a",
               do-tab-a(#"line-relative", 8, 1));
  assert-equal("test        a",
               do-tab-a(#"line-relative", 8, 3));
  assert-equal("test           a",
               do-tab-a(#"line-relative", 8, 5));
  assert-equal("test  a",
               do-tab-a(#"line-relative", 0, 3));
  assert-equal("testa",
               do-tab-a(#"line-relative", 0, 4));

  local
    method do-tab-b (kind, colnum :: <integer>, colinc :: <integer>)
      with-pretty-print-to-string (ps)
        write(ps, "test");
        pprint-tab(kind, colnum, colinc, ps);
        write(ps, "a");
      end
    end;
  assert-equal("test a",
               do-tab-b(#"section", 1, 1));
  assert-equal("test    a",
               do-tab-b(#"section", 8, 1));
  assert-equal("test    a",
               do-tab-b(#"section", 8, 3));
  assert-equal("test  a",
               do-tab-b(#"section", 0, 3));
  assert-equal("test    a",
               do-tab-b(#"section", 0, 4));
  assert-equal("test a",
               do-tab-b(#"section", 0, 5));

  assert-equal("test a",
               do-tab-b(#"section-relative", 1, 1));
  assert-equal("test        a",
               do-tab-b(#"section-relative", 8, 1));
  assert-equal("test        a",
               do-tab-b(#"section-relative", 8, 3));
  assert-equal("test           a",
               do-tab-b(#"section-relative", 8, 5));
  assert-equal("test  a",
               do-tab-b(#"section-relative", 0, 3));
  assert-equal("testa",
               do-tab-b(#"section-relative", 0, 4));

  local
    method do-tab-c (newline? :: <boolean>, kind,
                     colnum :: <integer>, colinc :: <integer>)
      with-pretty-print-to-string (ps)
        write(ps, "fo-");
        if (newline?)
          pprint-newline(#"mandatory", ps);
        end if;
        printing-logical-block (ps)
          write(ps, "test");
          pprint-tab(kind, colnum, colinc, ps);
          write(ps, "a");
        end;
      end
    end;
  assert-equal("fo-test a",
               do-tab-c(#f, #"section", 1, 1));
  assert-equal("fo-test    a",
               do-tab-c(#f, #"section", 8, 1));
  assert-equal("fo-test    a",
               do-tab-c(#f, #"section", 8, 3));
  assert-equal("fo-test  a",
               do-tab-c(#f, #"section", 0, 3));
  assert-equal("fo-test    a",
               do-tab-c(#f, #"section", 0, 4));
  assert-equal("fo-test a",
               do-tab-c(#f, #"section", 0, 5));

  assert-equal("fo-\ntest  a",
               do-tab-c(#t, #"line", 0, 3));
  assert-equal("fo-\ntest    a",
               do-tab-c(#t, #"line", 0, 4));
  assert-equal("fo-\ntest a",
               do-tab-c(#t, #"line", 0, 5));

  assert-equal("fo-test a",
               do-tab-c(#f, #"section-relative", 1, 1));
  assert-equal("fo-test        a",
               do-tab-c(#f, #"section-relative", 8, 1));
  assert-equal("fo-test        a",
               do-tab-c(#f, #"section-relative", 8, 3));
  assert-equal("fo-test           a",
               do-tab-c(#f, #"section-relative", 8, 5));
  assert-equal("fo-test  a",
               do-tab-c(#f, #"section-relative", 0, 3));
  assert-equal("fo-testa",
               do-tab-c(#f, #"section-relative", 0, 4));
  assert-equal("fo-test   a",
               do-tab-c(#f, #"line", 6, 4));
  assert-equal("fo-test  a",
               do-tab-c(#f, #"line", 6, 3));
end test;

define test test-printing-logical-block ()
  //---*** Fill this in...
end test;

define test test-basic-logical-block ()
  // Based on the example on page 7 of XP: A Common Lisp Pretty Printing System
  local
    method basic-boston ()
      with-pretty-print-to-string (ps)
        write(ps, "+ ");
        printing-logical-block (ps)
          write(ps, "Roads ");
          printing-logical-block (ps)
            write(ps, "ELM, ");
            pprint-newline(#"fill", ps);
            write(ps, "COTTONWOOD");
          end;
          write(ps, " ");
          pprint-newline(#"fill", ps);
          write(ps, " Town ");
          printing-logical-block (ps)
            write(PS, "BOSTON")
          end
        end;
        write(ps, " +");
      end
    end;
  assert-equal("+ Roads ELM, COTTONWOOD  Town BOSTON +",
               basic-boston());
  assert-equal("+ Roads ELM, COTTONWOOD\n"
               "   Town BOSTON +",
               dynamic-bind (*default-line-length* = 25)
                 basic-boston()
               end);
  assert-equal("+ Roads ELM,\n"
               "        COTTONWOOD\n"
               "   Town BOSTON +",
               dynamic-bind (*default-line-length* = 21)
                 basic-boston()
               end);
end test;

define test test-delimited-logical-block ()
  local
    method delimited-boston ()
      with-pretty-print-to-string (ps)
        write(ps, "+ ");
        printing-logical-block (ps)
          write(ps, "Roads ");
          printing-logical-block (ps, prefix: "[", suffix: "]")
            write(ps, "ELM, ");
            pprint-newline(#"fill", ps);
            write(ps, "COTTONWOOD");
          end;
          write(ps, " ");
          pprint-newline(#"fill", ps);
          write(ps, " Town ");
          printing-logical-block (ps)
            write(PS, "BOSTON")
          end
        end;
        write(ps, " +");
      end
    end;
  assert-equal("+ Roads [ELM, COTTONWOOD]  Town BOSTON +",
               delimited-boston());
  assert-equal("+ Roads [ELM,\n"
               "         COTTONWOOD]\n"
               "   Town BOSTON +",
               dynamic-bind (*default-line-length* = 25)
                 delimited-boston()
               end);
end test;

define test test-line-prefix-logical-block ()
  local
    method line-prefix-boston ()
      with-pretty-print-to-string (ps)
        printing-logical-block (ps, per-line-prefix: ";;; ")
          write(ps, "Roads ");
          printing-logical-block (ps, per-line-prefix: "= ")
            write(ps, "ELM, ");
            pprint-newline(#"fill", ps);
            write(ps, "COTTONWOOD");
          end;
          write(ps, " ");
          pprint-newline(#"fill", ps);
          write(ps, " Town ");
          printing-logical-block (ps)
            write(PS, "BOSTON")
          end
        end;
        close(ps);
      end
    end;
  assert-equal(";;; Roads = ELM, COTTONWOOD  Town BOSTON",
               line-prefix-boston());
  assert-equal(";;; Roads = ELM,\n"
               ";;;       = COTTONWOOD\n"
               ";;;  Town BOSTON",
               dynamic-bind (*default-line-length* = 25)
                 line-prefix-boston()
               end);
end test;

define test test-misering-logical-block ()
  // Based on the example on page 10 of XP: A Common Lisp Pretty Printing System
  local
    method misering-test ()
      with-pretty-print-to-string (ps)
        printing-logical-block (ps, prefix: "(", suffix: ")")
          write(ps, "LIST ");
          pprint-newline(#"miser", ps);
          printing-logical-block (ps)
            write(ps, "FIRST ");
            pprint-newline(#"linear", ps);
            write(ps, "SECOND ");
            pprint-newline(#"linear", ps);
            write(ps, "THIRD");
          end;
        end;
        close(ps);
      end
    end;
  assert-equal("(LIST FIRST\n"
               "      SECOND\n"
               "      THIRD)",
               dynamic-bind (*default-line-length* = 10,
                             *print-miser-width* = 8)
                 misering-test()
               end);
  assert-equal("(LIST\n"
               " FIRST\n"
               " SECOND\n"
               " THIRD)",
               dynamic-bind (*default-line-length* = 10,
                             *print-miser-width* = 9)
                 misering-test()
               end);
end test;

define test test-pprint-misc-indenting ()
  assert-equal("foo a\n"
               "        b",
               dynamic-bind (*default-line-length* = 20)
                 with-pretty-print-to-string (ps)
                   printing-logical-block (ps)
                     write(ps, "foo ");
                     pprint-indent(#"current", 4, ps);
                     write(ps, "a");
                     pprint-newline(#"mandatory", ps);
                     write(ps, "b");
                   end;
                 end
               end);

  assert-equal("foo a\n"
               "\n"
               "        b",
               dynamic-bind (*default-line-length* = 20)
                 with-pretty-print-to-string (ps)
                   printing-logical-block (ps)
                     write(ps, "foo ");
                     pprint-indent(#"current", 4, ps);
                     write(ps, "a");
                     pprint-newline(#"mandatory", ps);
                     pprint-newline(#"mandatory", ps);
                     write(ps, "b");
                   end;
                 end
               end);

  let lots = make(<string>, size: 300, fill: ' ');
  assert-equal(format-to-string("foo a\n%sb\n%s     c", lots, lots),
               dynamic-bind (*default-line-length* = 400)
                 with-pretty-print-to-string (ps)
                   printing-logical-block (ps)
                     write(ps, "foo ");
                     pprint-indent(#"block", 300, ps);
                     write(ps, "a");
                     pprint-newline(#"mandatory", ps);
                     write(ps, "b");
                     pprint-indent(#"block", 305, ps);
                     pprint-newline(#"mandatory", ps);
                     write-element(ps, 'c');
                   end;
                 end
               end);
end test;

define test test-pprint-buffers ()
  assert-equal($pretty-printing-test-string,
               dynamic-bind (*default-line-length* = 70)
                 with-pretty-print-to-string (ps)
                   printing-logical-block (ps)
                     write(ps, $pretty-printing-test-string, start: 0, end: 80);
                     write(ps, $pretty-printing-test-string, start: 80);
                   end;
                 end
               end);
  assert-equal($pretty-printing-test-string,
               dynamic-bind (*default-line-length* = 240)
                 with-pretty-print-to-string (ps)
                   printing-logical-block (ps)
                     write(ps, $pretty-printing-test-string, start: 0, end: 80);
                     write(ps, $pretty-printing-test-string, start: 80);
                   end;
                 end
               end);
end test;

define suite pprint-test-suite ()
  test test-*default-line-length*;
  test test-*print-miser-width*;
  test test-basic-logical-block;
  test test-delimited-logical-block;
  test test-line-prefix-logical-block;
  test test-misering-logical-block;
  test test-pprint-buffers;
  test test-pprint-indent;
  test test-pprint-logical-block;
  test test-pprint-misc-indenting;
  test test-pprint-newline;
  test test-pprint-tab;
  test test-printing-logical-block;
end;
