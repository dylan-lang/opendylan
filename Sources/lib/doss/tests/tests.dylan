Module:    doss-tests
Language:  infix-dylan
Author:    Eliot Miranda
Synopsis:  Dylan Object Storage System; DOSS Testing/Debugging
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// debugging/testing code

define variable dosstest = #f;
define variable dim = #f;
define variable dom = #f;
define variable cts = #f;

define method clean-doss-tests ()
  dosstest := #f;
  cts := #f;
  dim := #f;
  dom := #f;
end method;

define variable $use-files = #t;

define method use-files ()
  $use-files := #t;
end method;

define method dont-use-files ()
  $use-files := #f;
end method;

// import-cl-functions(system(call-system-showing-output)(as: unix-system));

define method test-doss (o)
  if (instance?(dosstest,<string>))
    format-out("testing %=\n",dosstest);
  end if;
  let dd = make(<doss-dumper>,
                stream:  if ($use-files)
                           make (<file-stream>, locator: ".dosstest",
				 direction: #"output",
				 element-type: <byte>,
				 if-exists: #"truncate")
                         else
                           make (<sequence-stream>,
				 contents: make(<byte-vector>, size: 0),
				 direction: #"output")
                         end if);
  dom := dd;
  my-format("~%~% test-doss ~S from stream ~A~%", dosstest, dd.stream);
  my-format("~%");
  store-object(o, dd);

//  my-format("post-store-object position ~D ~%", dd.stream.position);
//  if ($use-files)
//    unix-system("ls -l .dosstest");
//  end if;
    close(dd.stream);
    let s = make (<file-stream>, locator: ".dosstest",
  				 direction: #"input",
   				 element-type: <byte>);
  cts := copy-sequence(s.stream-contents, start: dd.header-size);
//  my-format("post-contents position ~D ~%", dd.stream.position);
//  if ($use-files)
//    unix-system("ls -l .dosstest");
//  end if;
  close(s);
//  my-format("post-close position ~D ~%", dd.stream.position);
//  if ($use-files)
//    unix-system("ls -l .dosstest");
//  end if;
  let dl = make(<doss-loader>,
                stream: if ($use-files)
                           make (<file-stream>, locator: ".dosstest",
				 direction: #"input",
				 element-type: <byte>)
                         else
                           make (<sequence-stream>, contents: cts,
				 direction: #"input")
                         end if);
  dim := dl;
//  my-format("~%");
  values(dl.fetch-object, o, cts, dl.stream.close);
end method;



define method test-doss-out (o)
  let dd = make(<doss-dumper>,
		stream: make (<sequence-stream>,
			      contents: make(<byte-vector>, size: 0),
			      direction: #"input-output"));
  dom := dd;
  store-object(o, dd);
  cts = stream-contents(dd.stream);
  cts
end method;

define method do-doss-test(o)
  block (return)
    let reconstruction = test-doss(o);
    format-out("%= ->\n%=\n",o,reconstruction);
    reconstruction
  exception (c :: <error>)
    format-out("%=\n",c);
    format-out("\ntest %= BOMBED OUT\n\n", dosstest);
    close(dom.stream);
    close(dim.stream);
    //if ($use-files)
    //  close-open-streams ();
    //end if;
    return("bombed out, mate, dinnit. tough.");
  end
end method;

// Whole load of tests for doss system.

define method do-doss-tests ()
    dosstest := "integers";
    map(do-doss-test, #[0, 1, -1, 127, 128, 129, -127, -128, -129, 32766, 32767, 32768, 32769, -32766, -32767, -32768, -32769, 65534, 65535, 65536, 65537, -65534, -65535, -65536, -65537]);
    dosstest := "constants";
    map(do-doss-test, #[#t, #f, 'A', 'a']);
    dosstest := "#()";
    do-doss-test(#());
    dosstest := "string";
    do-doss-test("foo");
    dosstest := "symbol";
    do-doss-test(as(<symbol>, "foo"));
    dosstest := "a class: <vector>";
    do-doss-test(<vector>);
    dosstest := "a class: <table>";
    do-doss-test(<table>);
    dosstest := "a class: <doss-policy>";
    do-doss-test(<doss-policy>);
    dosstest := "#[]";
    do-doss-test(#[]);
    dosstest := "#[1]";
    do-doss-test(#[1]);
    dosstest := "#(1)";
    do-doss-test(#(1));
    dosstest := "#[1,1]";
    do-doss-test(#[1,1]);
    dosstest := "make(<vector>,size: 20,fill: 1)";
    do-doss-test(make(<vector>,size: 20,fill: 1));
    dosstest := "make(<stretchy-vector>,size: 20,fill: 1)";
    do-doss-test(make(<stretchy-vector>,size: 20,fill: 1));
    dosstest := "pair 1 2";
    do-doss-test(pair(1,2));
    dosstest := "vector of constants";
    do-doss-test(#[1, 2, 3, 'A', 'b', 'C', "foo", "bar", "baz", #[1, 2, 3], #"foo"]);
    dosstest := "list of constants";
    do-doss-test(#(1, 2, 3, 'A', 'b', 'C', "foo", "bar", "baz", #[1, 2, 3], #"foo"));
    dosstest := "vector with shared substructure";
    begin
      let v = make(<vector>,size: 10,fill: "foo");
      for (i from 1 below 10 by 2)
        v[i] := "bar";
      end;
      do-doss-test(v);
    end;
    dosstest := "element";
    do-doss-test(element);
    dosstest := "<equal-table>";
    do-doss-test(<equal-table>);
    dosstest := "make <equal-table>";
    do-doss-test(make(<equal-table>));
    dosstest := "make <object-table>";
    do-doss-test(make(<object-table>));
    dosstest := "vector of object-tables";
    begin
      let a = make(<vector>, size: 3);
      a[0] := make(<object-table>, size: 512);
      a[1] := make(<object-table>, size: 512);
      a[2] := make(<object-table>, size: 512);
      do-doss-test(a);
    end;
    dosstest := "object-table with shared substructure";
    begin
      let a = make(<object-table>);
      let bar = "bar";
      a[shallow-copy("foo")] := bar;
      a[shallow-copy("foo")] := bar;
      a[shallow-copy("foo")] := bar;
      do-doss-test(a);
    end;
    dosstest := "vector of object-tables with shared substructure";
    begin
      let a = make(<vector>, size: 3);
      let bar = "bar";
      for (i from 0 below 3)
        a[i] := make(<object-table>, size: 512);
        a[i][shallow-copy("foo")] := bar;
        a[i][shallow-copy("foo")] := bar;
        a[i][shallow-copy("foo")] := bar;
      end for;
      do-doss-test(a);
    end;
    dosstest := "make <doss-dumper>";
    do-doss-test(make(<doss-dumper>));
// Modules aren't interesting.
//    dosstest := "module doss";
//    do-doss-test(find-translator-module(as(<symbol>, "doss")));
    dosstest := "first (slot-descriptors <equal-table>)";
    do-doss-test(first(<equal-table>.slot-descriptors));
    dosstest := "doss-dumper that has dumped <equal-table>";
    do-doss-test(begin
                let dd = make(<doss-dumper>,
                              stream: make (<sequence-stream>,
					    contents: make(<byte-vector>, size: 128),
					    direction: #"input-output"));
                    store-object(<equal-table>, dd);
                dd;
              end);
    dosstest := "doss-dumper that has dumped an <equal-table>";
    do-doss-test(begin
                let dd = make(<doss-dumper>,
                              stream: make (<sequence-stream>,
					    contents: make(<byte-vector>, size: 128),
					    direction: #"input-output"));
                store-object(make(<equal-table>), dd);
                dd;
              end);
end method;

do-doss-tests();

// eof
