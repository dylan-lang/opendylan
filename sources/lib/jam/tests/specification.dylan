Module:       jam-test-suite
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2004 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define interface-specification-suite jam-specification-suite ()
  instantiable class <jam-state> (<object>);

  function jam-state-copy(<jam-state>) => (<jam-state>);

  function jam-variable(<jam-state>, <string>, #"key", #"default")
   => (false-or(<sequence>));
  function jam-variable-setter (false-or(<sequence>), <jam-state>, <string>)
   => (false-or(<sequence>));

  function jam-expand-arg(<jam-state>, <jam-arg>) => (<sequence>);
  function jam-expand-list(<jam-state>, <sequence>) => (<sequence>);

  function jam-read(<jam-state>, <string>, false-or(<locator>)) => ();
  function jam-read-file(<jam-state>, <locator>) => ();

  function jam-state-stale?(<jam-state>) => (<boolean>);

  function jam-rule(<jam-state>, <string>) => (false-or(<function>));
  function jam-rule-setter(<function>, <jam-state>, <string>)
    => (<function>);

  function jam-invoke-rule(<jam-state>, <string>, #"rest") => (<sequence>);

  function jam-target-variable
      (<jam-state>, <string>, <string>, #"key", #"default")
   => (false-or(<sequence>));
  function jam-target-variable-setter
      (false-or(<sequence>), <jam-state>, <string>, <string>)
   => (false-or(<sequence>));

  function jam-target-bind
      (<jam-state>, <string>) => (<physical-locator>, <object>);

  function jam-target-build
      (<jam-state>, <sequence>, #"key", #"force?", #"progress-callback")
   => (<boolean>);
end jam-specification-suite;

define suite jam-test-suite ()
  suite jam-specification-suite;

  test test-<jam-state>;
  test test-jam-expand-arg-@-expansion;
  test test-jam-expand-arg-B-expansion-and-replacement;
  test test-jam-expand-arg-E-replacement;
  test test-jam-expand-arg-G-expansion-and-replacement;
  test test-jam-expand-arg-L-expansion-and-replacement;
  test test-jam-expand-arg-P-expansion-and-replacement;
  test test-jam-expand-arg-Q-expansion;
  test test-jam-expand-arg-R-replacement;
  test test-jam-expand-arg-S-expansion-and-replacement;
  test test-jam-expand-arg-U-expansion-and-replacement;
  test test-jam-expand-arg-d-expansion-and-replacement;
  test test-jam-expand-arg-element-selection;
  test test-jam-expand-arg-multi-modifier-expansion;
  test test-jam-expand-arg-null-elements;
  test test-jam-expand-arg-simple-variable-expansion;
  test test-jam-expand-arg-two-level-expansion-with-modifier;
  test test-jam-expand-arg-two-level-variable-expansion;
  test test-jam-expand-arg-with-J-modifier;
  test test-jam-expand-list;
  test test-jam-invoke-rule;
  test test-jam-read-file;
  test test-jam-read;
  test test-jam-rule-setter;
  test test-jam-rule;
  test test-jam-state-copy;
  test test-jam-state-stale?;
  test test-jam-target-bind;
  test test-jam-target-build;
  test test-jam-target-variable-setter;
  test test-jam-target-variable;
  test test-jam-variable-setter;
  test test-jam-variable;
end suite;
