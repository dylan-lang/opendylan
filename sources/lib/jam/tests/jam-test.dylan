Module:       jam-test-suite
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2004 Gwydion Dylan Maintainers
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library-spec jam ()
  module jam;
end library-spec jam;

define module-spec jam ()
  instantiable class <jam-state> (<object>);

  function jam-state-copy(<jam-state>) => (<jam-state>);

  function jam-variable(<jam-state>, <string>, #"key", #"default")
   => (false-or(<sequence>));
  function jam-variable-setter (false-or(<sequence>), <jam-state>, <string>)
   => (false-or(<sequence>));

  function jam-expand-arg(<jam-state>, <string>) => (<sequence>);
  function jam-expand-list(<jam-state>, <sequence>) => (<sequence>);

  function jam-read(<jam-state>, <string>, false-or(<locator>)) => ();
  function jam-read-file(<jam-state>, <locator>) => ();

  function jam-rule(<jam-state>, <string>) => (false-or(<function>));
  function jam-rule-setter(false-or(<function>), <jam-state>, <string>)
    => (false-or(<function>));

  function jam-invoke-rule(<jam-state>, <string>, #"rest") => (<sequence>);

  function jam-target-variable
      (<jam-state>, <string>, <string>, #"key", #"default")
   => (false-or(<sequence>));
  function jam-target-variable-setter
      (false-or(<sequence>), <jam-state>, <string>, <string>)
   => (false-or(<sequence>));

  function jam-target-build
      (<jam-state>, <sequence>, #"key", #"force?", #"progress-callback")
   => (<boolean>);
end module-spec jam;

define sideways method make-test-instance
    (class :: subclass(<jam-state>))
 => (jam :: <jam-state>)
  make(class)
end method make-test-instance;

define jam class-test <jam-state> ()
  //---*** Fill this in...
end class-test <jam-state>;

define jam function-test jam-state-copy ()
  //---*** Fill this in...
end function-test jam-state-copy;

define jam function-test jam-variable ()
  //---*** Fill this in...
end function-test jam-variable;

define jam function-test jam-variable-setter ()
  //---*** Fill this in...
end function-test jam-variable-setter;

define jam function-test jam-expand-arg ()
  begin 
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

    jam-variable(jam, "Y") := #("1", "2");
    jam-variable(jam, "Z") := #("X", "Y");

    check-equal("$($(Z))",
                jam-expand-arg(jam, "$($(Z))"),
                #("a", "b", "c", "1", "2"));
  end;

  begin
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
  end;

  begin
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
  end;

  begin
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
  end;

  begin
    let jam = make-test-instance(<jam-state>);
    jam-variable(jam, "join") := #("one", "two", "three");

    check-equal("$(join:J)",
                jam-expand-arg(jam, "$(join:J)"),
                #("onetwothree"));
    check-equal("$(join:J=!)",
                jam-expand-arg(jam, "$(join:J=!)"),
                #("one!two!three"));
  end;

  begin
    let jam = make-test-instance(<jam-state>);

    jam-variable(jam, "grelge:froop") := #("toads");
    jam-variable(jam, "q") := #("grelge:froop");
    check-equal("$($(q):U)",
                jam-expand-arg(jam, "$($(q):U)"),
                #("TOADS"));
  end;
end function-test jam-expand-arg;

define jam function-test jam-expand-list ()
  let jam = make-test-instance(<jam-state>);
  jam-variable(jam, "X") := #("This", "is");
  jam-variable(jam, "Y") := #("A", "TEST");

  check-equal("$(X:U) $(Y:L)",
              jam-expand-list(jam, #("$(X:U)", "$(Y:L)")),
              #("THIS", "IS", "a", "test"));
end function-test jam-expand-list;

define jam function-test jam-read-file ()
  //---*** Fill this in...
end function-test jam-read-file;

define jam function-test jam-read ()
  //---*** Fill this in...
end function-test jam-read;

define jam function-test jam-rule ()
  //---*** Fill this in...
end function-test jam-rule;

define jam function-test jam-rule-setter ()
  //---*** Fill this in...
end function-test jam-rule-setter;

define jam function-test jam-invoke-rule ()
  //---*** Fill this in...
end function-test jam-invoke-rule;

define jam function-test jam-target-variable-setter ()
  //---*** Fill this in...
end function-test jam-target-variable-setter;

define jam function-test jam-target-variable ()
  //---*** Fill this in...
end function-test jam-target-variable;

define jam function-test jam-target-build ()
  //---*** Fill this in...
end function-test jam-target-build;

