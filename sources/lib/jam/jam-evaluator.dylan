Module:       jam-internals
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2004 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Variable expansion

// jam-expand-arg
//
// Expand a single argument according to variable expansion rules.
//
define generic jam-expand-arg
    (jam :: <jam-state>, arg :: <jam-arg>, #key start, end: _end)
 => (result :: <sequence>);

define method jam-expand-arg
    (jam :: <jam-state>, arg :: <byte-string>,
     #key start :: <integer> = 0, end: _end :: <integer> = arg.size)
 => (result :: <sequence>);
  let arg-markers
    = make(<bit-set>,
           upper-bound-hint: _end,
           members: range(from: start, below: _end));
  jam-expand-arg-aux(jam, arg, arg-markers, start, _end);
end method;

define constant $empty-bit-set = make(<bit-set>);

define method am-extract
    (arg :: <byte-string>, arg-markers :: <bit-set>,
     start :: <integer>, _end :: <integer>)
 => (extracted :: <byte-string>, extracted-markers :: <bit-set>);
  if(start = _end)
    values("", $empty-bit-set)
  else
    let new-markers = make(<bit-set>, upper-bound-hint: _end - start);
    for (index :: <integer> from start below _end)
      if (member?(index, arg-markers))
        set-add!(new-markers, index - start);
      end if;
    end for;
    values(copy-sequence(arg, start: start, end: _end), new-markers)
  end if
end method;

define method jam-expand-arg-aux
    (jam :: <jam-state>, arg :: <byte-string>, arg-markers :: <bit-set>,
     start :: <integer>, _end :: <integer>)
 => (result :: <sequence>, markers :: <sequence>);
  block (return)
    for (index :: <integer> from start below _end - 1)
      if (arg[index] == '$' & arg[index + 1] == '(')
        let (prefix, prefix-markers)
          = am-extract(arg, arg-markers, start, index);
        iterate loop(var-index :: <integer> = index + 2,
                     parenthesis-depth :: <integer> = 1)
          if (var-index = _end)
            let (vars, vars-markers)
              = jam-expand-arg-aux(jam, arg, arg-markers,
                                   index + 2, var-index);
            let (results, markers)
              = jam-expand-arg-product(jam, prefix, prefix-markers,
                                       vars, vars-markers,
                                       #[""], vector($empty-bit-set));
            return(results, markers);
          elseif (arg[var-index] == ')')
            if (parenthesis-depth = 1)
              let (vars, vars-markers)
                = jam-expand-arg-aux(jam, arg, arg-markers,
                                     index + 2, var-index);
              let (suffixes, suffixes-markers)
                = jam-expand-arg-aux(jam, arg, arg-markers,
                                     var-index + 1,  _end);
              let (results, markers)
                = jam-expand-arg-product(jam, prefix, prefix-markers,
                                         vars, vars-markers,
                                         suffixes, suffixes-markers);
              return(results, markers);
            else
              loop(var-index + 1, parenthesis-depth - 1);
            end if;
          elseif (arg[var-index] == '(')
            loop(var-index + 1, parenthesis-depth + 1);
          else
            loop(var-index + 1, parenthesis-depth);
          end if;
        end iterate;
      end if;
    end for;
    if (start = 0 & _end = arg.size)
      values(vector(arg), vector(arg-markers))
    else
      let (extracted, extracted-markers)
        = am-extract(arg, arg-markers, start, _end);
      values(vector(extracted), vector(extracted-markers))
    end
  end block
end method;

// jam-expand-arg-product()
//
// Implement the  variables * values * suffixes product for variable
// expansions.
//
define method jam-expand-arg-product
    (jam :: <jam-state>,
     prefix :: <byte-string>, prefix-markers :: <bit-set>,
     vars :: <sequence>, vars-markers :: <sequence>,
     suffixes :: <sequence>, suffixes-markers :: <sequence>)
 => (result :: <sequence>, markers :: <sequence>);
  let result = make(<stretchy-vector>);
  let markers = make(<stretchy-vector>);
  for (var :: <byte-string> in vars, var-markers :: <bit-set> in vars-markers)
    let contents
      = block(return)
          for (c :: <character> in var, i :: <integer> from 0)
            if (c == ':' & member?(i, var-markers))
              let var-name = copy-sequence(var, end: i);
              let contents = jam-variable(jam, var-name, default: #f);
              return(jam-expand-arg-colon(jam, contents, var, i + 1));
            elseif (c == '[' & member?(i, var-markers))
              let var-name = copy-sequence(var, end: i);
              return(jam-expand-arg-bracket(jam, jam-variable(jam, var-name),
                                            var, i + 1));
            end if;
          end for;
          jam-variable(jam, var);
        end block;
    for (component in contents)
      for (suffix :: <byte-string> in suffixes,
           suffix-markers :: <bit-set> in suffixes-markers)
        add!(result, concatenate(prefix, component, suffix));

        let result-markers = make(<bit-set>);
        copy-bit-set!(result-markers, prefix-markers);
        for (_ keyed-by index :: <integer> in suffix-markers)
          set-add!(result-markers, prefix.size + component.size + index);
        end for;
        add!(markers, result-markers);
      end for;
    end for;
  end for;
  values(result, markers)
end method;

// jam-expand-arg-colon
//
// Modify the variable expansion using one of the available modifier letters.
//
define function jam-expand-arg-colon
    (jam :: <jam-state>,
     contents :: false-or(<sequence>),
     variable :: <byte-string>,
     i :: <integer>)
 => (result :: <sequence>);
  let variable-size :: <integer> = variable.size;
  if (i < variable-size)
    let modifier = variable[i];
    let replace?
      = i + 1 < variable.size
      & element(variable, i + 1) == '=';
    let contents = contents | #[];

    if (modifier == 'E')
      if (~empty?(contents))
        contents
      elseif (replace?)
        vector(copy-sequence(variable, start: i + 2))
      else
        #[""]
      end
    elseif (modifier == 'J')
      let joiner =
        if (replace?) copy-sequence(variable, start: i + 2) else "" end;
      for (component in contents,
           first? = #t then #f,
           result = first(contents)
             then if (first?)
                    result
                  else
                    concatenate(result, joiner, component)
                  end)
      finally
        vector(result)
      end for
    elseif (modifier == '@')
      if (empty?(contents))
        #[]
      else
        let (stream :: <file-stream>, locator :: <file-locator>)
          = jam-new-temporary-file(jam);
        block ()
          for (component in contents)
            write(stream, component);
            new-line(stream);
          end for;
        cleanup
          close(stream);
        end;
        vector(as(<byte-string>, locator))
      end if
    else
      let func =
        select (modifier)
          // B - Filename base
          'B' =>
            if (replace?)
              method (name :: <byte-string>) => (modified :: <byte-string>);
                let locator = as(<file-locator>, strip-grist(name));
                as(<byte-string>,
                   make(<file-locator>,
                        directory: locator.locator-directory,
                        base: copy-sequence(variable, start: i + 2),
                        extension: locator.locator-extension))
              end
            else
              method (name :: <byte-string>) => (extracted :: <byte-string>);
                as(<file-locator>, strip-grist(name)).locator-base;
              end
            end if;
            
          // S - Filename suffix
          'S' =>
            if (replace?)
              method (name :: <byte-string>) => (modified :: <byte-string>);
                let locator = as(<file-locator>, strip-grist(name));
                  
                as(<byte-string>,
                   make(<file-locator>,
                        directory: locator.locator-directory,
                        base: locator.locator-base,
                        extension: copy-sequence(variable, start: i + 3)))
              end
            else
              method (name :: <byte-string>) => (extracted :: <byte-string>);
                let locator = as(<file-locator>, strip-grist(name));
                if (locator.locator-extension)
                  concatenate(".",
                              as(<file-locator>,
                                 strip-grist(name)).locator-extension)
                else
                  ""
                end if
              end
            end if;
            
          // D - Directory path
          'D', 'P' =>
            if (replace?)
              method (name :: <byte-string>) => (modified :: <byte-string>);
                let locator = as(<file-locator>, strip-grist(name));
                add-grist(name,
                          as(<byte-string>,
                             make(<file-locator>,
                                  directory:
                                    as(<directory-locator>,
                                       copy-sequence(variable, start: i + 2)),
                                  base: locator.locator-base,
                                  extension: locator.locator-extension)))
                         end
            else
              method (name :: <byte-string>) => (extracted :: <byte-string>);
                let locator = as(<file-system-locator>, strip-grist(name));
                add-grist(name,as(<byte-string>, locator.locator-directory | ""))
              end
            end if;

          // R - Root directory path
          'R' =>
            if (replace?)
              let new-root = as(<directory-locator>,
                                copy-sequence(variable, start: i + 2));
              method (name :: <byte-string>) => (modified :: <byte-string>);
                as(<byte-string>,
                   merge-locators(as(<file-system-locator>, name),
                                  new-root))
              end method;
            else
              always("")
            end if;

          // G - "Grist"
          'G' =>
            if (i + 2 = variable-size)
              strip-grist
            elseif (replace?)
              let new-start
                = if (variable[i + 2] == '<') i + 3 else i + 2 end if;
              let new-end
                = if (variable[variable-size - 1] == '>')
                    variable-size - 1
                  else
                    variable-size
                  end;
              let grist
                = copy-sequence(variable, start: new-start, end: new-end);
              method (name :: <byte-string>) => (modified :: <byte-string>);
                  concatenate("<", grist, ">", strip-grist(name))
              end
            else
              extract-grist
            end if;
          

          // U - Uppercased expansion
          'U' =>
            as-uppercase;

          // L - Lowercased expansion
          'L' =>
            as-lowercase;

          // Q - Quote argument
          'Q' =>
            quote-argument;

          otherwise =>
            error("unknown variable modifier '%c' in '%s'",
                  modifier, variable);
        end select;
      let contents = map(func, contents);
      if (replace?)
        contents
      else
        jam-expand-arg-colon(jam, contents, variable, i + 1)
      end if
    end if
  else
    contents | #[]
  end if
end function;

// jam-expand-arg-bracket
//
// Extract a range of values from a variable expansion
//
define function jam-expand-arg-bracket
    (jam :: <jam-state>,
     contents :: <sequence>,
     variable :: <byte-string>,
     start :: <integer>)
 => (result :: <sequence>);
  let variable-size :: <integer> = variable.size;
  if (start < variable-size)
    let (n, after-n) = string-to-integer(variable, start: start, default: 0);
    if (n < 1 | n > contents.size)
      #[]
    elseif (after-n >= variable-size)
      vector(contents[n - 1])
    elseif (variable[after-n] == ']')
      let result = vector(contents[n - 1]);
      if (after-n + 1 < variable-size & variable[after-n + 1] == ':')
        jam-expand-arg-colon(jam, result, variable, after-n + 2)
      else
        result
      end if
    elseif (variable[after-n] == '-')
      if (after-n + 1 < variable-size & variable[after-n + 1] == ']')
        let result = copy-sequence(contents, start: n - 1);
        if (after-n + 2 < variable-size & variable[after-n + 2] == ':')
          jam-expand-arg-colon(jam, result, variable, after-n + 3)
        else
          result
        end if
      else
        let (m, after-m)
          = string-to-integer(variable, start: after-n + 1, default: 0);
        let result
          = if (m < 1 | m > contents.size)
              copy-sequence(contents, start: n - 1);
            else
              copy-sequence(contents, start: n - 1, end: m);
            end if;
        if (after-m + 1 < variable-size
              & variable[after-m] == ']'
              & variable[after-m + 1] == ':')
          jam-expand-arg-colon(jam, result, variable, after-m + 2)
        else
          result
        end if
      end if
    else
      vector(contents[n - 1])
    end if
  else
    vector(contents[0])
  end if;
end function;

// jam-expand-arg
//
// Expand bracketed statements within argument lists.
//
define method jam-expand-arg
    (jam :: <jam-state>, arg :: <jam-statement>, #key start, end: _end)
 => (result :: <sequence>);
  evaluate-statement(jam, arg)
end method;


/// Argument list expansion.

// jam-expand-list
//
// Expand each argument in an argument list
//
define generic jam-expand-list
    (jam :: <jam-state>, list :: <sequence>)
 => (result :: <sequence>);

define method jam-expand-list
    (jam :: <jam-state>, list :: <sequence>)
 => (result :: <sequence>);
  let result = make(<stretchy-vector>);
  for (arg in list)
    concatenate!(result, jam-expand-arg(jam, arg));
  end for;
  result
end method;


/// Statement evaluation

// evaluate-statement
//
// Evaluate a Jam statement.
//
define generic evaluate-statement
    (jam :: <jam-state>, statement :: <jam-statement>)
 => (result :: <sequence>);

define method evaluate-statement
    (jam :: <jam-state>, statement :: <jam-block-statement>)
 => (result :: <sequence>);
  // Dynamically bind block's local variables
  let vars = jam-expand-list(jam, statement.block-local-vars);
  let outer-values
    = map(method (v :: <byte-string>) jam-variable(jam, v, default: #f) end, vars);
  let inner-values = jam-expand-list(jam, statement.block-local-values);
  let result = #[];
  block ()
    // Initialize local variables
    do(method (v :: <byte-string>) => ();
         jam-variable(jam, v) := inner-values;
       end,
       vars);
    // Evaluate each statement in the block; the resulting value is that
    // of the last statement.
    for (statement :: <jam-statement> in statement.block-statements)
      result := evaluate-statement(jam, statement);
    end for;
  cleanup
    // Restore the previous state of each variable
    do(method (v :: <byte-string>, w :: false-or(<sequence>)) => ();
         jam-variable(jam, v) := w;
       end,
       vars,
       outer-values);
  end block;
  result
end method;

define method evaluate-statement
    (jam :: <jam-state>, statement :: <jam-invocation-statement>)
 => (result :: <sequence>);
  let rulenames = jam-expand-arg(jam, statement.invocation-rulename);
  let fields = map(curry(jam-expand-list, jam), statement.invocation-fields);

  if (rulenames.size = 1)
    apply(jam-invoke-rule, jam, rulenames.first, fields)
  else
    let result = make(<stretchy-vector>);
    for (rulename in rulenames)
      concatenate!(result, apply(jam-invoke-rule, jam, rulename, fields));
    end for;
    result
  end if
end method;

define method evaluate-statement
    (jam :: <jam-state>, statement :: <jam-assignment-statement>)
 => (result :: <sequence>);
  let vars = jam-expand-arg(jam, statement.assignment-variable);
  let values = jam-expand-list(jam, statement.assignment-values);
  for (var in vars)
    select (statement.assignment-kind)
      #"=" =>
        jam-variable(jam, var) := values;
      #"+=" =>
        jam-variable(jam, var) := concatenate(jam-variable(jam, var), values);
      #"?=" =>
        unless (jam-variable(jam, var, default: #f))
          jam-variable(jam, var) := values;
        end;
    end select;
  end for;
  values
end method;

define method evaluate-statement
    (jam :: <jam-state>, statement :: <jam-on-assignment-statement>)
 => (result :: <sequence>);
  let vars = jam-expand-arg(jam, statement.assignment-variable);
  let values = jam-expand-list(jam, statement.assignment-values);
  let targets = jam-expand-list(jam, statement.assignment-targets);
  for (var in vars)
    for (target in targets)
      select (statement.assignment-kind)
        #"=" =>
          jam-target-variable(jam, target, var) := values;
        #"+=" =>
          jam-target-variable(jam, target, var)
            := concatenate(jam-target-variable(jam, target, var), values);
        #"?=" =>
          unless (jam-target-variable(jam, target, var, default: #f))
            jam-target-variable(jam, target, var) := values;
          end;
      end select;
    end for;
  end for;
  values
end method;

define class <jam-break-condition> (<condition>)
  constant slot break-condition-values :: <sequence>,
    required-init-keyword: values:;
end class;

define method evaluate-statement
    (jam :: <jam-state>, statement :: <jam-break-statement>)
 => (result :: <sequence>);
  let values = jam-expand-list(jam, statement.break-values);
  signal(make(<jam-break-condition>, values: values));

  error("break statement occurred outside a Jam 'while' or 'for' statement")
end method;

define class <jam-continue-condition> (<condition>)
  constant slot continue-condition-values :: <sequence>,
    required-init-keyword: values:;
end class;

define method evaluate-statement
    (jam :: <jam-state>, statement :: <jam-continue-statement>)
 => (result :: <sequence>);
  let values = jam-expand-list(jam, statement.continue-values);
  signal(make(<jam-continue-condition>, values: values));

  error("continue statement occurred outside a Jam 'while' or 'for' statement")
end method;

define method evaluate-statement
    (jam :: <jam-state>, statement :: <jam-for-statement>)
 => (result :: <sequence>);
  let list-values = jam-expand-list(jam, statement.for-values);
  block (break)
    let result = #[];
    for (value in list-values)
      jam-variable(jam, statement.for-var) := vector(value);
      block (continue)
        for (statement in statement.for-statements)
          result := evaluate-statement(jam, statement);
        end for
      exception (continue-condition :: <jam-continue-condition>)
        result := continue-condition.continue-condition-values;
        continue();
      exception (break-condition :: <jam-break-condition>)
        break(break-condition.break-condition-values);
      end block
    finally
      result
    end for
  end block
end method;

define method evaluate-statement
    (jam :: <jam-state>, statement :: <jam-switch-statement>)
 => (result :: <sequence>);
  let values = jam-expand-list(jam, statement.switch-values);

  let the-value = element(values, 0, default: "");
  
  block (return)
    for (the-case in statement.switch-cases)
      if (the-case.case-match-function(the-value))
        let result = #[];
        for (statement in the-case.case-statements)
          result := evaluate-statement(jam, statement);
        end for;
        return(result);
      end if;
    end for;
    #[]
  end block;
end method;

define method evaluate-statement
    (jam :: <jam-state>, statement :: <jam-if-statement>)
 => (result :: <sequence>);
  let cond = evaluate-expression(jam, statement.if-condition);
  if (~every?(empty?, cond))
    let result = #[];
    for (statement in statement.if-statements)
      result := evaluate-statement(jam, statement);
    end for;
    result
  elseif (statement.else-statement)
    evaluate-statement(jam, statement.else-statement)
  else
    #[]
  end
end method;

define method evaluate-statement
    (jam :: <jam-state>, statement :: <jam-while-statement>)
 => (result :: <sequence>);
  block (break)
    let result = #[];
    iterate loop()
      let cond = evaluate-expression(jam, statement.while-condition);
      if (~every?(empty?, cond))
        block (continue)
          for (statement in statement.while-statements)
            result := evaluate-statement(jam, statement);
          end for
        exception (continue-condition :: <jam-continue-condition>)
          result := continue-condition.continue-condition-values;
          continue();
        exception (break-condition :: <jam-break-condition>)
          break(break-condition.break-condition-values);
        end block;
        loop()
      end if;
    end iterate;
    result
  end block
end method;

define method evaluate-statement
    (jam :: <jam-state>, statement :: <jam-on-statement>)
 => (result :: <sequence>);
  let targets = jam-expand-arg(jam, statement.on-targets);
  if (~targets.empty?)
    with-jam-target (jam, jam-target(jam, targets[0]))
      evaluate-statement(jam, statement.on-statement);
    end;
  else
    #[]
  end if
end method;

define class <jam-return-condition> (<condition>)
  constant slot return-condition-values :: <sequence>,
    required-init-keyword: values:;
end class;

define constant $rule-params
  = #["<", ">", "1", "2", "3", "4", "5", "6", "7", "8", "9"];

define method evaluate-statement
    (jam :: <jam-state>, statement :: <jam-ruledef-statement>)
 => (result :: <sequence>);
  let params = statement.ruledef-params;
  let all-params = concatenate(params, $rule-params);
  let statements = statement.ruledef-statements;

  local
    method rule-function
        (jam :: <jam-state>, #rest lol) => (result :: <sequence>);
      // Dynamically bind the rule's parameters
      let outer-values
        = map(method (v :: <byte-string>) jam-variable(jam, v, default: #f) end,
              all-params);

      // Initialize the parameter values
      jam-variable(jam, "<") := if (lol.size > 0) lol[0] else #f end;
      jam-variable(jam, ">") := if (lol.size > 1) lol[1] else #f end;
      for (i from 0, param in #["1", "2", "3", "4", "5", "6", "7", "8", "9"])
        jam-variable(jam, param) := if (lol.size > i) lol[i] else #f end;
      end for;
      for (i from 0, param in params)
        jam-variable(jam, param) := if (lol.size > i) lol[i] else #f end;
      end for;
      
      block (return)
        let result = #[];
        for (statement in statements)
          result := evaluate-statement(jam, statement);
        end for;
        result
      cleanup
        // Restore the previous state of each parameter variable
        do(method (v :: <byte-string>, w :: false-or(<sequence>)) => ();
             jam-variable(jam, v) := w;
           end,
           all-params,
           outer-values);
      exception (return-condition :: <jam-return-condition>)
        return(return-condition.return-condition-values);
      end block
    end method;
  
  jam-rule(jam, statement.ruledef-name) := rule-function;
  #[];
end method;

define method evaluate-statement
    (jam :: <jam-state>, statement :: <jam-return-statement>)
 => (result :: <sequence>);
  let values = jam-expand-list(jam, statement.return-values);
  signal(make(<jam-return-condition>, values: values));

  error("return statement occurred outside a Jam rule definition");
end method;

define method evaluate-statement
    (jam :: <jam-state>, statement :: <jam-actiondef-statement>)
 => (result :: <sequence>);
  let bindlist
    = if (statement.actiondef-bindlist)
        jam-expand-list(jam, statement.actiondef-bindlist)
      else
        #[]
      end if;
  let action = apply(make, <jam-action>,
                     name: statement.actiondef-name,
                     bindlist:, bindlist,
                     commands:, statement.actiondef-commands,
                     statement.actiondef-flags);
  jam.%jam-actions[statement.actiondef-name] := action;
  #[]
end method;

define method evaluate-statement
    (jam :: <jam-state>, statement :: <jam-include-statement>)
 => (result :: <sequence>);
  let includes = jam-expand-list(jam, statement.include-list);
  for (target-name in includes)
    let (locator, target) = jam-target-bind(jam, target-name);
    if (file-exists?(locator))
      jam-read-file(jam, locator)
    elseif (target.target-allow-nonexistent?)
      #f;
    else
      error(make(<file-does-not-exist-error>, locator: locator));
    end if;
  end for;
  #[]
end method;


/// if/while condition evaluation

// $jam-false - constant
//
// The "canonical" Jam Boolean false value.
//
define constant $jam-false = #[];

// $jam-false - constant
//
// The "canonical" Jam Boolean true value
//
define constant $jam-true = #["1"];

// evaluate-expression
//
// Evaluates a condition expression.
//
define generic evaluate-expression
    (jam :: <jam-state>, statement :: <jam-expression>)
 => (result :: <sequence>);

define method evaluate-expression
    (jam :: <jam-state>, statement :: <jam-leaf-expression>)
 => (result :: <sequence>);
  let arg = jam-expand-arg(jam, statement.leaf-argument);
  if (statement.leaf-list)
    let list = jam-expand-list(jam, statement.leaf-list);
    if (every?(method (a) member?(a, list, test: \=) end, arg))
      $jam-true
    else
      $jam-false
    end if
  else
    arg
  end
end method;

define method evaluate-expression
    (jam :: <jam-state>, statement :: <jam-not-expression>)
 => (result :: <sequence>);
  let left = evaluate-expression(jam, statement.composite-left);
  if (every?(empty?, left))
    $jam-true
  else
    $jam-false
  end
end method;

define method evaluate-expression
    (jam :: <jam-state>, statement :: <jam-and-expression>)
 => (result :: <sequence>);
  let left = evaluate-expression(jam, statement.composite-left);
  if (every?(empty?, left))
    left
  else
    evaluate-expression(jam, statement.composite-right);
  end if
end method;

define method evaluate-expression
    (jam :: <jam-state>, statement :: <jam-or-expression>)
 => (result :: <sequence>);
  let left = evaluate-expression(jam, statement.composite-left);
  if (every?(empty?, left))
    evaluate-expression(jam, statement.composite-right);
  else
    left
  end if
end method;

define method evaluate-expression
    (jam :: <jam-state>, statement :: <jam-eq-expression>)
 => (result :: <sequence>);
  let left = evaluate-expression(jam, statement.composite-left);
  let right = evaluate-expression(jam, statement.composite-right);
  if (left = right) $jam-true else $jam-false end if
end method;

define method evaluate-expression
    (jam :: <jam-state>, statement :: <jam-ne-expression>)
 => (result :: <sequence>);
  let left = evaluate-expression(jam, statement.composite-left);
  let right = evaluate-expression(jam, statement.composite-right);
  if (left ~= right) $jam-true else $jam-false end if
end method;

/* ### FIXME
define class <jam-lt-expression> (<jam-composite-expression>) end;
define class <jam-le-expression> (<jam-composite-expression>) end;
define class <jam-gt-expression> (<jam-composite-expression>) end;
define class <jam-ge-expression> (<jam-composite-expression>) end;
*/


/// Rule invocation interface

define method jam-invoke-rule
    (jam :: <jam-state>, rulename :: <byte-string>, #rest lol)
 => (result :: <sequence>);
  let action = element(jam.%jam-actions, rulename, default: #f);

  if (action)
    apply(jam-invoke-action, jam, action, lol);
  end if;
    
  let rule-function = jam-rule(jam, rulename);
  if (rule-function)
    apply(rule-function, jam, lol)
  elseif (action)
    #[]
  else
    signal("Invoking undefined Jam rule %s", rulename);
    #[]
  end if
end method;
