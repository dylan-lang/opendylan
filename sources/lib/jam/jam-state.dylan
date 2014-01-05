Module:       jam-internals
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2004 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <jam-state> (<object>)
  constant slot %jam-variables :: <string-table> = make(<string-table>);
  constant slot %jam-rules :: <string-table> = make(<string-table>);
  constant slot %jam-actions :: <string-table> = make(<string-table>);
  constant slot %jam-targets :: <string-table> = make(<string-table>);
  constant slot %jam-random :: <random> = make(<random>);
  constant slot %jam-temporary-files :: <deque> = make(<deque>);
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
    (jam :: <jam-state>, name :: <string>,
     #key default :: false-or(<sequence>) = #[])
 => (value :: false-or(<sequence>));
  element(jam.%jam-variables, name, default: default)
end method;

define method jam-variable-setter
    (value :: false-or(<sequence>), jam :: <jam-state>, name :: <string>)
 => (value :: false-or(<sequence>));
  if (value)
    jam.%jam-variables[name] := value;
  else
    remove-key!(jam.%jam-variables, name);
    #f
  end if
end method;

define method jam-rule
    (jam :: <jam-state>, name :: <string>)
 => (rule :: false-or(<function>));
  element(jam.%jam-rules, name, default: #f);
end method;

define method jam-rule-setter
    (value :: <function>, jam :: <jam-state>, name :: <string>)
 => (value :: <function>);
  element(jam.%jam-rules, name) := value;
end method;

define constant $temporary-file-range :: <integer> = 36 ^ 5;

define method jam-new-temporary-file
    (jam :: <jam-state>)
 => (stream :: <file-stream>, locator :: <file-locator>);
  let directory :: false-or(<directory-locator>) = temp-directory();
  block (return)
    while (#t)
      block (again)
        let filename
          = concatenate("jam",
                        integer-to-string(random($temporary-file-range,
                                                 random: jam.%jam-random),
                                          base: 36, size: 5),
                        ".tmp");
        let locator
          = make(<file-locator>, name: filename, directory: directory);
        let stream
          = make(<file-stream>,
                 locator: locator, direction: #"output",
                 if-exists: #"signal");
        push-last(jam.%jam-temporary-files, locator);
        return(stream, locator);
      exception (e :: <file-exists-error>)
        again();
      end block;
    end while;
  end block;
end method;

define method jam-clean-temporary-files
    (jam :: <jam-state>)
 => ();
  while (~empty?(jam.%jam-temporary-files))
    delete-file(pop(jam.%jam-temporary-files));
  end while;
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
