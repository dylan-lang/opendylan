Module:       jam-internals
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2004 Gwydion Dylan Maintainers
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <jam-state> (<object>)
  constant slot %jam-variables :: <string-table> = make(<string-table>);
  constant slot %jam-rules :: <string-table> = make(<string-table>);
  constant slot %jam-actions :: <string-table> = make(<string-table>);
  constant slot %jam-targets :: <string-table> = make(<string-table>);
end class;

define method jam-state-copy(jam :: <jam-state>) => (jam :: <jam-state>);
  let new-jam = make(<jam-state>);
  for (value keyed-by variable in jam.%jam-variables)
    new-jam.%jam-variables[variable] := value;
  end for;
  for (rule-function keyed-by rule-name in jam.%jam-rules)
    new-jam.%jam-rules[rule-name] := rule-function;
  end for;
  for (action keyed-by action-name in jam.%jam-actions)
    new-jam.%jam-actions[action-name] := action;
  end for;
  for (target keyed-by target-name in jam.%jam-targets)
    new-jam.%jam-targets[target-name] := jam-target-copy(jam, new-jam, target);
  end for;
  new-jam
end method;

define method jam-variable
    (jam :: <jam-state>, name :: <byte-string>,
     #key default :: false-or(<sequence>) = #[])
 => (value :: false-or(<sequence>));
  element(jam.%jam-variables, name, default: default)
end method;

define method jam-variable-setter
    (value :: false-or(<sequence>), jam :: <jam-state>, name :: <byte-string>)
 => (value :: false-or(<sequence>));
  if (value)
    jam.%jam-variables[name] := value;
  else
    remove-key!(jam.%jam-variables, name);
    #f
  end if
end method;

define method jam-rule
    (jam :: <jam-state>, name :: <byte-string>)
 => (rule :: false-or(<function>));
  element(jam.%jam-rules, name, default: #f);
end method;

define method jam-rule-setter
    (value :: <function>, jam :: <jam-state>, name :: <byte-string>)
 => (value :: <function>);
  element(jam.%jam-rules, name) := value;
end method;

// initialize
//
// Install the built-in rule function definitions
//
define method initialize (jam :: <jam-state>, #key #all-keys) => ();
  // Dependency building
  jam-rule(jam, "Depends") := jam-rule(jam, "DEPENDS") := jam-builtin-depends;
  jam-rule(jam, "Includes") := jam-rule(jam, "INCLUDES") :=
    jam-builtin-includes;

  // Modifying update determination
  jam-rule(jam, "Always") := jam-rule(jam, "ALWAYS") := jam-builtin-always;
  jam-rule(jam, "Leaves") := jam-rule(jam, "LEAVES") := jam-builtin-leaves;
  jam-rule(jam, "NoCare") := jam-rule(jam, "NOCARE") := jam-builtin-nocare;
  jam-rule(jam, "NotFile") := jam-rule(jam, "NOTFILE") := jam-builtin-notfile;
  jam-rule(jam, "NoUpdate") := jam-rule(jam, "NOUPDATE") :=
    jam-builtin-noupdate;
  jam-rule(jam, "Temporary") := jam-rule(jam, "TEMPORARY") :=
    jam-builtin-temporary;

  // Utilities
  jam-rule(jam, "Glob") := jam-rule(jam, "GLOB") := jam-builtin-glob;
  jam-rule(jam, "Match") := jam-rule(jam, "MATCH") := jam-builtin-match;
end method;
