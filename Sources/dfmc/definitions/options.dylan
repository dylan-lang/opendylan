Module: dfmc-definitions
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define dood-class <option> (<object>)
  lazy constant slot option-debug-name,
    required-init-keyword: debug-name:;
  lazy constant slot option-symbol,
    required-init-keyword: symbol:;
  lazy constant slot option-constraint,
    required-init-keyword: constraint:;
  lazy constant slot option-excludes-thunk,
    required-init-keyword: excludes-thunk:;
end dood-class;

ignore(option-debug-name);

define method option-excludes (option :: <option>)
  option-excludes-thunk(option)()
end method;

define macro option-definer
  { define option ?:name => ?:symbol \:: ?constraint:name ?excludes:* ; end }
    => { define constant ?name
           = make(<option>,
                  debug-name:     ?#"name",
                  symbol:         ?symbol,
                  constraint:     ?#"constraint",
                  excludes-thunk: method () ?excludes end); }
excludes:
  { }
    => { #() }
  { excludes ?names:* }
    => { list(?names) }
names:
  { }
    => { }
  { ?:name, ... }
    => { ?name, ... }
end macro;

define program-warning <malformed-option>
  slot condition-variable-name,
    required-init-keyword: variable-name:;
  format-string 
    "Malformed key/value option in %= - ignoring.";
  format-arguments 
    variable-name;
end program-warning;

define program-warning <unrecognized-options>
  slot condition-variable-name,
    required-init-keyword: variable-name:;
  slot condition-options,
    required-init-keyword: options:;
  format-string 
    "Unrecognized options %= specified in the definition of %= "
    "- ignoring.";
  format-arguments 
    options, variable-name;
end program-warning;

define program-warning <contradictory-option>
  slot condition-option,
    required-init-keyword: option:;
  slot condition-excluded,
    required-init-keyword: excluded:;
  slot condition-variable-name,
    required-init-keyword: variable-name:;
  format-string 
    "Use of the option \"%=\" excludes the use of \"%=\" in the definition of %=."
    "Ignoring \"%=\".";
  format-arguments 
    option, excluded, variable-name, excluded;
end program-warning;

define program-warning <duplicated-options>
  slot condition-variable-name,
    required-init-keyword: variable-name:;
  slot condition-options,
    required-init-keyword: options:;
  format-string 
    "The options %= are specified more than once in the definition of %= "
    "- using the first value of each.";
  format-arguments 
    options, variable-name;
end program-warning;

define method parse-options (options, options-fragment, name)
  // Set up a table indexed by key for recognising declared options.
  let val-table = make(<table>);
  for (option in options)
    let val-collector = make(<deque>);
    val-table[option-symbol(option)] := val-collector;
  end;
  collecting (unrecognized)
    macro-case (options-fragment)
      { ?options:* }
        => #f;
    options:
      { }
        => #();
      { ?key:symbol ?val:*, ... }
        => begin
             // TODO: EMULATOR: Remove this keyword/symbol hack.
             let key = as-keyword(fragment-value(key));
             let val-collector = element(val-table, key, default: #f);
             if (val-collector)
               push-last(val-collector, val);
             else
               collect-into(unrecognized, key);
             end;
           end;
      { ?other:*, ... }
        => note(<malformed-option>,
                source-location: fragment-source-location(options-fragment),
                variable-name:   name);
    end;
    // Was there anything we weren't expecting?
    if (~empty?(collected(unrecognized)))
      note(<unrecognized-options>,
           source-location: fragment-source-location(options-fragment),
           variable-name:   name,
           options:         collected(unrecognized));
    end;
    // Are any of the options specified exlusive of any others?
    let problems = options-contradictory?(options, val-table);
    if (problems)
      for (problem-pair in problems)
        note(<contradictory-option>,
             source-location: fragment-source-location(options-fragment),
             variable-name:   name,
             option: option-symbol(head(problem-pair)),
             excluded: option-symbol(tail(problem-pair)));
        // null out the excluded option
        val-table[option-symbol(tail(problem-pair))] := make(<deque>);
      end for;
    end if;
    // Do any of the options appear more than once?
    let problems = options-duplicated?(options, val-table);
    if (problems)
      note(<duplicated-options>,
           source-location: fragment-source-location(options-fragment),
           variable-name:   name,
           options:         map(option-symbol, problems));
    end;
    parse-option-values(options, val-table);
  end;
end method;

define method parse-option-values (options, val-table)
  collecting (as <vector>)
    for (option in options)
      let vals = element(val-table, option-symbol(option));
      if (~empty?(vals))
        let val = vals.first;
        collect(option-symbol(option));
        collect(parse-option-value(option-constraint(option), val));
      end;
    end;
  end;
end method;

// gts,10/97:  used to return a flat collection of options and their
// excluded counterparts.  Now return a collection of contradictory pairs.

define method options-contradictory? (options, val-table)
  collecting (contradictory)
    for (option in options)
      let vals = element(val-table, option-symbol(option));
      if (~empty?(vals))
        for (excluded in option-excludes(option))
          if (~empty?(element(val-table, option-symbol(excluded))))
            collect-into(contradictory, pair(option, excluded));
            // collect-into(contradictory, option);
            // collect-into(contradictory, excluded);
          end;
        end;
      end;
    finally
      if (empty?(collected(contradictory)))
        #f;
      else
        collected(contradictory);
      end;
    end;
  end;
end method;

define method options-duplicated? (options, val-table)
  collecting (duplicated)
    for (option in options)
      let vals = element(val-table, option-symbol(option));
      if (size(vals) > 1)
        collect-into(duplicated, option);
      end;
    finally
      if (empty?(collected(duplicated)))
        #f;
      else
        collected(duplicated);
      end;
    end;
  end;
end method;

// A tool for parsing any property list to a compile-stage property list
// of keyword/unparsed-fragment pairs.

// TODO: Lose the call to as-keyword here when we go native.

define method parse-property-list (list-fragment) => (list)
  macro-case (list-fragment)
    { ?properties:* }
      => properties
  properties:
    { }
      => #();
    { ?key:symbol ?val:expression, ... }
      => pair(as-keyword(as(<symbol>, key)), pair(val, ...))
  end;
end method;

//// Option parsers.

define method parse-option-value (constraint, fragment)
  fragment
end method;

define method parse-option-value (constraint == #"name", fragment)
  macro-case (fragment) { ?:name } => name end
end method;

define method parse-option-value (constraint == #"expression", fragment)
  let result = macro-case (fragment) { ?:expression } => expression end;
  result
end method;

define method parse-option-value (constraint == #"symbol", fragment)
  let result = macro-case (fragment) { ?:symbol } => symbol end;
  result
end method;

define method parse-option-value (constraint == #"name->symbol", fragment)
  macro-case (fragment) { ?:name } => { ?#"name" } end
end method;

// eof
