Module:    dfmc-environment-reports
Synopsis:  DFMC report generation utilities
Author:    Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Coerce a symbol to a string
//---*** cpage: Currently, this is necessary because the default format for "%s"
//              produces all-caps; this may be an anomally of the emulator.
define macro as-string
  { as-string (?:expression) } => { as-lowercase(as(<string>, ?expression))  };
end macro as-string;

// Return the name of a definition (source-form)
define function definition-name
    (definition :: <source-form>)
 => (name :: false-or(<symbol>))
  let variables = definition.source-form-defined-variables;
  if (variables & size(variables) >= 1)
    variables[0].variable-name
  end if
end function definition-name;

// Extract the name of a keyword
//---*** cpage: This whole function is a kludge. dfmc-browser-support currently
//              has a bug in which it returns instances of some class other than
//              <symbol> for "keywords". Here, we fudge up a string, then parse
//              for the keyword name.
define function keyword-name (keyword) => (name :: <symbol>)
  if (instance?(keyword, <symbol>))
    key
  else
//    debug-out(#"dfmc-environment-reports",
//              "Error: %= is not an instance of <symbol>",
//              keyword);
    // Two formats are possible: '#P{ #"keyword" }' '#P{ keyword: }'
    let name = format-to-string("%=", keyword);
    let key-start = subsequence-position(name, "\"");
    let key-end = 0;
    if (key-start)
      key-end := subsequence-position(name, "\"", count: 2);
    else
      key-start := subsequence-position(name, " ");
      key-end := subsequence-position(name, ":");
    end if;
    let key-name = copy-sequence(name, start: key-start + 1, end: key-end);
    as(<symbol>, key-name)
  end block;
end function keyword-name;
