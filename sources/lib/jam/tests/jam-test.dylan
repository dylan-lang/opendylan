Module:       jam-test-suite
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2004 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define sideways method make-test-instance
    (class :: subclass(<jam-state>))
 => (jam :: <jam-state>)
  make(class)
end method make-test-instance;

define test test-<jam-state> ()
  //---*** Fill this in...
end test;

define test test-jam-state-copy ()
  //---*** Fill this in...
end test;

define test test-jam-variable ()
  //---*** Fill this in...
end test;

define test test-jam-variable-setter ()
  //---*** Fill this in...
end test;

define test test-jam-expand-arg-simple-variable-expansion ()
  let jam = make-test-instance(<jam-state>);
  jam-variable(jam, "X") := #("a", "b", "c");

  check-equal("$(X)",
              jam-expand-arg(jam, "$(X)"),
              #("a", "b", "c"));
  check-equal("t$(X)",
              jam-expand-arg(jam, "t$(X)"),
              #("ta", "tb", "tc"));
  check-equal("$(X)z",
              jam-expand-arg(jam, "$(X)z"),
              #("az", "bz", "cz"));
  check-equal("$(X)-$(X)",
              jam-expand-arg(jam, "$(X)-$(X)"),
              #("a-a", "a-b", "a-c",
                "b-a", "b-b", "b-c",
                "c-a", "c-b", "c-c"));
end test;

define test test-jam-expand-arg-two-level-variable-expansion ()
  let jam = make-test-instance(<jam-state>);
  jam-variable(jam, "X") := #("a", "b", "c");
  jam-variable(jam, "Y") := #("1", "2");
  jam-variable(jam, "Z") := #("X", "Y");

  check-equal("$($(Z))",
              jam-expand-arg(jam, "$($(Z))"),
              #("a", "b", "c", "1", "2"));
end test;

define test test-jam-expand-arg-null-elements ()
  let jam = make-test-instance(<jam-state>);

  jam-variable(jam, "X") := #("a", "");
  jam-variable(jam, "Y") := #("", "1");

  check-equal("$(X)",
              jam-expand-arg(jam, "$(X)"),
              #("a", ""));
  check-equal("$(Y)",
              jam-expand-arg(jam, "$(Y)"),
              #("", "1"));
  check-equal("$(Z)",
              jam-expand-arg(jam, "$(Z)"),
              #());

  check-equal("*$(X)$(Y)*",
              jam-expand-arg(jam, "*$(X)$(Y)*"),
              #("*a*", "*a1*", "**", "*1*"));
  check-equal("*$(X)$(Z)*",
              jam-expand-arg(jam, "*$(X)$(Z)*"),
              #());
end test;

define test test-jam-expand-arg-element-selection ()
  let jam = make-test-instance(<jam-state>);

  jam-variable(jam, "X") := #("A", "B", "C", "D", "E", "F", "G", "H");
  jam-variable(jam, "Selected") := #("3", "7", "8");

  check-equal("$(X[$(Selected)])",
              jam-expand-arg(jam, "$(X[$(Selected)])"),
              #("C", "G", "H"));

  jam-variable(jam, "foo") := #("1", "2", "3", "4", "5", "6", "7");
  check-equal("$(foo[3-5)",
              jam-expand-arg(jam, "$(foo[3-5)"),
              #("3", "4", "5"));
end test;

define test test-jam-expand-arg-B-expansion-and-replacement ()
  //---*** Fill this in...
end test;

define test test-jam-expand-arg-S-expansion-and-replacement ()
  //---*** Fill this in...
end test;

define test test-jam-expand-arg-d-expansion-and-replacement ()
  //---*** Fill this in...
end test;

define test test-jam-expand-arg-P-expansion-and-replacement ()
  //---*** Fill this in...
end test;

define test test-jam-expand-arg-G-expansion-and-replacement ()
  let jam = make-test-instance(<jam-state>);

  jam-variable(jam, "g0") := #("A");
  jam-variable(jam, "g1") := #("<A>");
  jam-variable(jam, "g2") := #("<B>B");

  check-equal("1: $(g0:G)", jam-expand-arg(jam, "$(g0:G)"), #(""));
  check-equal("2: $(g1:G)", jam-expand-arg(jam, "$(g1:G)"), #("<A>"));
  check-equal("3: $(g2:G)", jam-expand-arg(jam, "$(g2:G)"), #("<B>"));

  check-equal("4: $(g0:G=wham)",
              jam-expand-arg(jam, "$(g0:G=wham)"),
              #("<wham>A"));
  check-equal("5: $(g0:G=)",
              jam-expand-arg(jam, "$(g0:G=)"), #("A"));
  check-equal("6: $(g0:G=<)",
              jam-expand-arg(jam, "$(g0:G=<)"), #("<>A"));
  check-equal("7: $(g0:G=<>)",
              jam-expand-arg(jam, "$(g0:G=<>)"), #("<>A"));
  check-equal("8: $(g0:G=<wham>)",
              jam-expand-arg(jam, "$(g0:G=<wham>)"), #("<wham>A"));

  check-equal("9: $(g1:G=wham)",
              jam-expand-arg(jam, "$(g1:G=wham)"), #("<wham>"));
  check-equal("10: $(g1:G=)",
              jam-expand-arg(jam, "$(g1:G=)"), #(""));
  check-equal("11: $(g1:G=<)",
              jam-expand-arg(jam, "$(g1:G=<)"), #("<>"));
  check-equal("12: $(g1:G=<>)",
              jam-expand-arg(jam, "$(g1:G=<>)"), #("<>"));
  check-equal("13: $(g1:G=<wham>)",
              jam-expand-arg(jam, "$(g1:G=<wham>)"), #("<wham>"));

  check-equal("14: $(g2:G=wham)",
              jam-expand-arg(jam, "$(g2:G=wham)"), #("<wham>B"));
  check-equal("15: $(g2:G=<)",
              jam-expand-arg(jam, "$(g2:G=<)"), #("<>B"));
  check-equal("16: $(g2:G=)",
              jam-expand-arg(jam, "$(g2:G=)"), #("B"));
  check-equal("17: $(g2:G=<>)",
              jam-expand-arg(jam, "$(g2:G=<>)"), #("<>B"));
  check-equal("18: $(g2:G=<wham>)",
              jam-expand-arg(jam, "$(g2:G=<wham>)"), #("<wham>B"));

  check-equal("19: $(g2:G=<wham>whir)",
              jam-expand-arg(jam, "$(g2:G=<wham>whir)"), #("<wham>whir>B"));
end test;

define test test-jam-expand-arg-U-expansion-and-replacement ()
  //---*** Fill this in...
end test;

define test test-jam-expand-arg-L-expansion-and-replacement ()
  //---*** Fill this in...
end test;

define test test-jam-expand-arg-multi-modifier-expansion ()
  //---*** Fill this in...
end test;

define test test-jam-expand-arg-R-replacement ()
  //---*** Fill this in...
end test;

define test test-jam-expand-arg-E-replacement ()
  //---*** Fill this in...
end test;

define test test-jam-expand-arg-with-J-modifier ()
  let jam = make-test-instance(<jam-state>);
  jam-variable(jam, "join") := #("one", "two", "three");

  check-equal("$(join:J)",
              jam-expand-arg(jam, "$(join:J)"),
              #("onetwothree"));
  check-equal("$(join:J=!)",
              jam-expand-arg(jam, "$(join:J=!)"),
              #("one!two!three"));
end test;

define test test-jam-expand-arg-two-level-expansion-with-modifier ()
  let jam = make-test-instance(<jam-state>);

  jam-variable(jam, "grelge:froop") := #("toads");
  jam-variable(jam, "q") := #("grelge:froop");
  check-equal("$($(q):U)",
              jam-expand-arg(jam, "$($(q):U)"),
              #("TOADS"));
end test;

define test test-jam-expand-arg-Q-expansion ()
  //---*** Fill this in...
end test;

define test test-jam-expand-arg-@-expansion ()
  //---*** Fill this in...
end test;

define test test-jam-expand-list ()
  let jam = make-test-instance(<jam-state>);
  jam-variable(jam, "X") := #("This", "is");
  jam-variable(jam, "Y") := #("A", "TEST");

  check-equal("$(X:U) $(Y:L)",
              jam-expand-list(jam, #("$(X:U)", "$(Y:L)")),
              #("THIS", "IS", "a", "test"));
end test;

define test test-jam-read-file ()
  //---*** Fill this in...
end test;

define test test-jam-read ()
  //---*** Fill this in...
end test;

define test test-jam-state-stale? ()
  let jam = make-test-instance(<jam-state>);
  assert-false(jam-state-stale?(jam),
               "pristine jam states are fresh");
  let a = as(<file-locator>, "a.jam");
  with-open-file (s = a, direction: #"output")
    write(s, "Echo hello A ;");
  end;
  jam-read-file(jam, a);
  assert-false(jam-state-stale?(jam),
               "unmodified jamfiles keep state fresh");
  sleep(2.0);
  with-open-file (s = a, direction: #"output")
    write(s, "Echo hello A Modified ;");
  end;
  assert-true(jam-state-stale?(jam),
              "modified jamfiles make state stale");
  delete-file(a);
  assert-true(jam-state-stale?(jam),
              "deleted jamfiles make state stale");
end test;

define test test-jam-rule ()
  //---*** Fill this in...
end test;

define test test-jam-rule-setter ()
  //---*** Fill this in...
end test;

define test test-jam-invoke-rule ()
  //---*** Fill this in...
end test;

define test test-jam-target-variable-setter ()
  //---*** Fill this in...
end test;

define test test-jam-target-variable ()
  //---*** Fill this in...
end test;

define test test-jam-target-bind ()
  //---*** Fill this in...
end test;

define test test-jam-target-build ()
  //---*** Fill this in...
end test;

define test test-jam-shell ()
  let jam = make-test-instance(<jam-state>);
  check-equal("SHELL",
              #["value\n"],
              jam-invoke-rule(jam, "SHELL", #["echo value"]));
  check-equal("SHELL no-output",
              #[""],
              jam-invoke-rule(jam, "SHELL", #["echo value"], #["no-output"]));
  check-equal("SHELL strip-eol",
              #["value"],
              jam-invoke-rule(jam, "SHELL", #["echo value"], #["strip-eol"]));
  check-equal("SHELL exit-status",
              #["value\n", "0"],
              jam-invoke-rule(jam, "SHELL", #["echo value"], #["exit-status"]));
  check-equal("SHELL combined options",
              #["value", "0"],
              jam-invoke-rule(jam, "SHELL", #["echo value"],
                              #["strip-eol"], #["exit-status"]));
end test;

define test test-jam-split ()
  let jam = make-test-instance(<jam-state>);
  check-equal("Split indivisible",
              #["I", "like", "peas."],
              jam-invoke-rule(jam, "Split",
                              #["I", "like", "peas."],
                              #[" "]));
  check-equal("Split straightforward",
              #["I", "like", "peas."],
              jam-invoke-rule(jam, "Split",
                              #["I like peas."],
                              #[" "]));
  check-equal("Split multiple delimiters",
              #["I", "like", "peas"],
              jam-invoke-rule(jam, "Split",
                              #["I like peas."],
                              #[" ."]));
  check-equal("Split not",
              #["No", "splits", "here"],
              jam-invoke-rule(jam, "Split",
                              #["No", "splits", "here"],
                              #["|"]));
  check-equal("Split libs",
              #["No", "splits", "here"],
              jam-invoke-rule(jam, "Split",
                              #["No", "splits", "here"],
                              #["|"]));
  check-equal("Split pkg-config",
              #["-lgobject-2.0", "-lglib-2.0"],
              jam-invoke-rule(jam, "Split",
                              #["-lgobject-2.0 -lglib-2.0 "],
                              #[" "]));
end test;
