module:          devel-dbg-ui
synopsis:        Describing expressions to be evaluated
author:          Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// <DEBUGGER-EXPRESSION>
//    An expression to be evaluated in the debugger.

define abstract class <debugger-expression> (<object>)

  slot expression-evaluated? :: <boolean>,
    init-value: #f,
    init-keyword: expression-evaluated?:;

  slot expression-has-lvalue? :: <boolean>,
    init-value: #f,
    init-keyword: expression-has-lvalue?:;

  slot expression-value :: <remote-value>,
    init-keyword: expression-value:;

  slot expression-lvalue :: <remote-location>,
    init-keyword: expression-lvalue:;

end class;


///// <SIMPLE-EXPRESSION>
//    An expression that can be evaluated without any runtime code being
//    executed, such as an address/integer/character literal, or a symbolic
//    name that can just be looked up.

define abstract class <simple-expression> (<debugger-expression>)
end class;


///// <FUNCTION-EXPRESSION>
//    An expression that requires runtime code to be executed. This is a
//    function and a sequence of arguments.

define class <function-expression> (<debugger-expression>)

  constant slot function-expression :: <simple-expression>,
    required-init-keyword: function:;

  constant 
    slot function-arguments-list :: <sequence>, // of <simple-expression>
    required-init-keyword: arguments:;

end class;


///// <ILLEGAL-SIMPLE-EXPRESSION>
//    Something that the parser can return when it goes tits-up

define class <illegal-simple-expression> (<simple-expression>)
end class;


///// <LITERAL-EXPRESSION>
//    Made up of a single token: address, character, integer...

define class <literal-expression> (<simple-expression>)

  constant slot literal-token :: <token>,
    required-init-keyword: token:;

end class;


///// <ADDRESS-LITERAL-EXPRESSION>
//    An address literal in hex.

define class <address-literal-expression> (<literal-expression>)
end class;


///// <REGISTER-LITERAL-EXPRESSION>
//    An expression of the form [<register-name>]

define class <register-literal-expression> (<literal-expression>)
end class;


///// <HISTORY-VARIABLE-EXPRESSION>
//    A history variable.

define class <history-variable-expression> (<literal-expression>)
end class;


///// <LOCAL-VARIABLE-EXPRESSION>
//    A variable on the stack.

define class <local-variable-expression> (<literal-expression>)
end class;


///// <INTEGER-LITERAL-EXPRESSION>
//    An integer literal in decimal.

define class <integer-literal-expression> (<literal-expression>)
end class;


///// <DYLAN-KEYWORD-LITERAL-EXPRESSION>

define class <dylan-keyword-literal-expression> (<literal-expression>)
end class;


///// <EMPTY-LIST-LITERAL-EXPRESSION>

define class <empty-list-literal-expression> (<literal-expression>)
end class;


///// <BOOLEAN-TRUE-EXPRESSION>

define class <boolean-true-expression> (<literal-expression>)
end class;


///// <BOOLEAN-FALSE-EXPRESSION>

define class <boolean-false-expression> (<literal-expression>)
end class;


///// <CHARACTER-LITERAL-EXPRESSION>

define class <character-literal-expression> (<literal-expression>)
end class;


///// <SYMBOLIC-NAME-EXPRESSION>

define class <symbolic-name-expression> (<simple-expression>)

  constant slot symbol :: <string>,
    required-init-keyword: symbol:;

  constant slot dll-context :: false-or(<remote-library>),
    init-keyword: dll-context:,
    init-value: #f;

  constant slot lookup-in-stack? :: <boolean>,
    init-value: #f,
    init-keyword: lookup-in-stack?:;

end class;


///// <DYLAN-SYMBOLIC-NAME-EXPRESSION>

define class <dylan-symbolic-name-expression> (<symbolic-name-expression>)

  constant slot name-context :: <dylan-name-context>,
    required-init-keyword: context:;

end class;


///// SELECT-MOST-APPROPRIATE-THREAD
//    Whenever a thread-local variable, or a register is looked up, this
//    function decides the thread context.

define method select-most-appropriate-thread
    (application :: <application>) => (thread :: <remote-thread>)
  if ((application.selected-thread > 0) &
      (application.selected-thread <=
         size(application.application-thread-table)))
    application.application-thread-table[application.selected-thread - 1]
  else
    application.stopped-thread
  end if
end method;


///// EVALUATE-SIMPLE-EXPRESSION
//    Any instance of <simple-expression> can be reduced to a <remote-value>
//    within a debugger transaction.

define method evaluate-simple-expression
  (application :: <application>, expression :: <address-literal-expression>)
       => ()
  unless (expression.expression-evaluated?)
    let hex-string-with-zero-x = expression.literal-token.representation;
    let string-sz = size(hex-string-with-zero-x) - 2;
    let hex-string = make(<byte-string>, size: string-sz);
    for (i from 0 below string-sz)
      hex-string[i] := hex-string-with-zero-x[i + 2];
    end for;
    expression.expression-evaluated? := #t;
    expression.expression-value := 
      string-as-remote-value(application.debug-target-access-path,
                             hex-string,
                             16);
    expression.expression-has-lvalue? := #t;
    expression.expression-lvalue := expression.expression-value;
  end unless;
end method;

define method evaluate-simple-expression
  (application :: <application>, expression :: <character-literal-expression>)
    => ()
  unless (expression.expression-evaluated?)
    expression.expression-evaluated? := #t;
    expression.expression-value :=
      character-as-tagged-remote-value
        (expression.literal-token.representation[1]);
    expression.expression-has-lvalue? := #t;
    expression.expression-lvalue := expression.expression-value;
  end unless;
end method;

define method evaluate-simple-expression
  (application :: <application>, expression :: <integer-literal-expression>)
    => ()
  unless (expression.expression-evaluated?)
    expression.expression-evaluated? := #t;
    expression.expression-value :=
      integer-as-tagged-remote-value
        (decimal-string-to-integer(expression.literal-token.representation));
    expression.expression-has-lvalue? := #t;
    expression.expression-lvalue := expression.expression-value;
  end unless;
end method;

define method evaluate-simple-expression
  (application :: <application>, 
   expression :: <dylan-keyword-literal-expression>) => ()
  unless (expression.expression-evaluated?)
    let keyword-address = 
      resolve-dylan-keyword(application,
        generate-actual-keyword(expression.literal-token));
    if (keyword-address)
      expression.expression-evaluated? := #t;
      expression.expression-value := keyword-address;
    end if;
  end unless;
end method;

define method evaluate-simple-expression
  (application :: <application>, expression :: <history-variable-expression>)
     => ()
  unless (expression.expression-evaluated?)
    expression.expression-evaluated? := #t;
    expression.expression-value :=
      retrieve-history-value(
        previous-result-string-to-integer
          (expression.literal-token.representation));
  end unless;
end method;

define method evaluate-simple-expression
  (application :: <application>, expression :: <boolean-true-expression>)
     => ()
  unless (expression.expression-evaluated?)
    let (boolean-true, boolean-false) =
       dylan-runtime-boolean-markers(application);
    expression.expression-evaluated? := #t;
    expression.expression-value := boolean-true;
  end unless;
end method;

define method evaluate-simple-expression
  (application :: <application>, expression :: <boolean-false-expression>)
     => ()
  unless (expression.expression-evaluated?)
    let (boolean-true, boolean-false) =
       dylan-runtime-boolean-markers(application);
    expression.expression-evaluated? := #t;
    expression.expression-value := boolean-false;
  end unless;
end method;

define method evaluate-simple-expression
  (application :: <application>, 
   expression :: <empty-list-literal-expression>)
     => ()
  unless (expression.expression-evaluated?)
    let context = make(<dylan-name-context>);
    let empty-list = #f;
    context.context-library := "dylan";
    context.context-module := "internal";
    empty-list := resolve-dylan-name(application, "%empty-list", context,
                                     indirect?: #f);
    if (empty-list)
      expression.expression-evaluated? := #t;
      expression.expression-value := empty-list;
    end if;
  end unless;
end method;

define method evaluate-simple-expression
  (application :: <application>, expression :: <local-variable-expression>)
    => ()
  unless (expression.expression-evaluated?)
    if (*open-application*)
      if (size(*open-application*.current-stack) > 0)
        let i = *open-application*.current-frame-index;
        if (i > 0)
          let fr = *open-application*.current-stack[i - 1];
          if (instance?(fr, <call-frame>))
            let found = #f;
            let i = 0;
            let limit = 0;
            let (live-variables, live-values) =
              live-frame-lexical-variables(*open-application*, fr);
            limit := size(live-variables);
            while ((~found) & (i < limit))
              let lexname = 
                mangle-local-dylan-name
                  (as-lowercase(expression.literal-token.representation));
              if ((lexname = live-variables[i].lexical-variable-name) |
                  (expression.literal-token.representation =
                             live-variables[i].lexical-variable-name))
                found := #t;
                expression.expression-evaluated? := #t;
                expression.expression-value := live-values[i];
                expression.expression-has-lvalue? := #t;
                expression.expression-lvalue := 
                  live-variables[i].lexical-variable-address;
              else
                i := i + 1;
              end if
            end while
          end if
        end if
      end if
    end if;
  end unless;
end method;

define method evaluate-simple-expression
  (application :: <application>, expression :: <register-literal-expression>)
    => ()
  let found-register = #f;
  let regname = 
    generate-actual-register-name(expression.literal-token);

  local method set-if-correct (r :: <unassigned-remote-register>)
          if (as-uppercase(r.register-name) = regname)
            found-register := r;
          end if;
        end method;

  unless (expression.expression-evaluated?)
    do-registers(set-if-correct, application.debug-target-access-path);
    if (found-register)
      if (application.stopped-thread)
        let reg-active = 
          active-register(application.debug-target-access-path,
                          select-most-appropriate-thread(application),
                          found-register);
        let val = read-value(application.debug-target-access-path, reg-active);
        expression.expression-value := val;
        expression.expression-evaluated? := #t;
        expression.expression-has-lvalue? := #t;
        expression.expression-lvalue := reg-active;
      end if
    end if
  end unless
end method;

define method evaluate-simple-expression
  (application :: <application>, expression :: <symbolic-name-expression>)
     => ()

  local method found-in-stack-frame?
           (frame :: <call-frame>) => (well? :: <boolean>)
          if (dylan-call-frame?(application, frame))
            #f
          else
            let (vars, vals) =
              live-frame-lexical-variables(application, frame);
            block (return)
              for (i from 0 below vars.size)
                if (vars[i].lexical-variable-name = expression.symbol)
                  expression.expression-evaluated? := #t;
                  expression.expression-has-lvalue? := #t;
                  expression.expression-value := vals[i];
                  expression.expression-lvalue :=
                    vars[i].lexical-variable-address;
                  return(#t)
                end if;
              end for;
              return(#f);
            end block;
          end if;
        end method;

  local method found-on-stack? () => (well? :: <boolean>)    
          let found-it? = #f;              
          if (size(*open-application*.current-stack) > 0)
            let i = *open-application*.current-frame-index;
            if (i > 0)
              let fr = *open-application*.current-stack[i - 1];
              if (instance?(fr, <call-frame>) & found-in-stack-frame?(fr))
                found-it? := #t;
              end if
            end if
          end if;
          found-it?
        end method;

  unless (expression.expression-evaluated?)
    (expression.lookup-in-stack? & found-on-stack?()) |
    block ()
      let sym = 
        if (expression.dll-context)
          symbol-table-find-symbol(application.debug-target-symbol-table,
                                   expression.symbol,
                                   library: expression.dll-context);
        else
          symbol-table-find-symbol(application.debug-target-symbol-table,
                                   expression.symbol)
        end if;
      if (sym)
        expression.expression-evaluated? := #t;
        expression.expression-has-lvalue? := #t;
        expression.expression-value := sym.remote-symbol-address;
        expression.expression-lvalue := sym.remote-symbol-address;
      end if;
    end block;
  end unless;
end method;

define method evaluate-simple-expression
  (application :: <application>, 
   expression :: <dylan-symbolic-name-expression>)
     => ()

  local method thread-localise-if-necessary
           (name :: <string>, context :: <dylan-name-context>,
            value-obtained :: <remote-value>, addr-obtained :: <remote-value>)
               => (value-sanitized :: <remote-value>,
                   addr-sanitized :: <remote-value>)
          let path = application.debug-target-access-path;
          let thread = select-most-appropriate-thread(application);
          let tlv = thread-current-local-variables(application, thread);
          let object-tag =
            remote-value-low-order-bits(value-obtained, 2);
          if (object-tag == 0)
            let tlv-size = as-remote-value((tlv.size + 2) * 4);
            if (remote-value-<=(value-obtained, tlv-size))
              let index = truncate/(as-integer(value-obtained), 4) - 2;
              let addr-sanitized = head(tlv[index]);
              let value-sanitized = read-value(path, addr-sanitized);
              debugger-message("NB: %s in %s:%s appears to be a "
                               "thread-local variable.",
                               name, context.context-module,
                               context.context-library);
              debugger-message("  (Using value for selected thread).");
              values(value-sanitized, addr-sanitized);
            else
              values(value-obtained, addr-obtained)
            end if;
          else
            values(value-obtained, addr-obtained)
          end if;
        end method;

  local method found-static-in-this-context?
           (context :: <dylan-name-context>) => (well? :: <boolean>)
          let (direct-val, direct-address) =
            if (expression.dll-context)
              resolve-dylan-name(application, as-lowercase(expression.symbol),
                                 context, indirect?: #f,
                                 library: expression.dll-context)
            else
              resolve-dylan-name(application, as-lowercase(expression.symbol),
                                 context, indirect?: #f)
            end if;
          if (direct-val & direct-address)
            let (sv, sa) = 
              thread-localise-if-necessary(expression.symbol, context,
                                           direct-val, direct-address);
            expression.expression-evaluated? := #t;
            expression.expression-has-lvalue? := #t;
            expression.expression-value := sv;
            expression.expression-lvalue := sa;
            #t
          else
            #f
          end if;
        end method;

  local method found-dynamic-in-this-context?
           (context :: <dylan-name-context>) => (well? :: <boolean>)
          let (indirect-val, indirect-address) =
            if (expression.dll-context)
              resolve-dylan-name(application, as-lowercase(expression.symbol),
                                 context, indirect?: #t,
                                 library: expression.dll-context)
            else
              resolve-dylan-name(application, as-lowercase(expression.symbol),
                                 context, indirect?: #t)
            end if;
          if (indirect-val & indirect-address)
            let (sv, sa) = 
              thread-localise-if-necessary(expression.symbol, context,
                                           indirect-val, indirect-address);
            expression.expression-evaluated? := #t;
            expression.expression-has-lvalue? := #t;
            expression.expression-value := sv;
            expression.expression-lvalue := sa;
            #t
          else
            #f
          end if;
        end method;

  local method found-in-stack-frame?
           (frame :: <call-frame>) => (well? :: <boolean>)
          if (dylan-call-frame?(application, frame))
            let (names, types, models, vals, locations) =
              active-dylan-lexical-variables(application, frame);
            block (return)
              for (i from 0 below names.size)
                if (names[i] = expression.symbol)
                  expression.expression-evaluated? := #t;
                  expression.expression-has-lvalue? := #t;
                  expression.expression-value := vals[i];
                  expression.expression-lvalue := locations[i];
                  return(#t)
                end if;
              end for;
              return(#f);
            end block;
          else
            let (vars, vals) =
              live-frame-lexical-variables(application, frame);
            block (return)
              for (i from 0 below vars.size)
                if (vars[i].lexical-variable-name = expression.symbol)
                  expression.expression-evaluated? := #t;
                  expression.expression-has-lvalue? := #t;
                  expression.expression-value := vals[i];
                  expression.expression-lvalue :=
                    vars[i].lexical-variable-address;
                  return(#t)
                end if;
              end for;
              return(#f);
            end block;
          end if;
        end method;

  local method found-on-stack? () => (well? :: <boolean>)    
          let found-it? = #f;              
          if (size(*open-application*.current-stack) > 0)
            let i = *open-application*.current-frame-index;
            if (i > 0)
              let fr = *open-application*.current-stack[i - 1];
              if (instance?(fr, <call-frame>) & found-in-stack-frame?(fr))
                found-it? := #t;
              end if
            end if
          end if;
          found-it?
        end method;

  unless (expression.expression-evaluated?)
    let dylan-context = make(<dylan-name-context>,
                             library: "dylan", module: "dylan");
    (expression.lookup-in-stack? & found-on-stack?()) |
    found-static-in-this-context?(expression.name-context) |
    found-dynamic-in-this-context?(expression.name-context) |
    found-static-in-this-context?(dylan-context) |
    found-dynamic-in-this-context?(dylan-context) |
    block (return)
      debugger-message("WARNING: Could not evaluate %s in the supplied "
                       "namespace. Searching...",
                       expression.symbol);
      for (context in application.application-likely-namespaces)
        if (found-static-in-this-context?(context) |
            found-dynamic-in-this-context?(context))
          debugger-message("NB: Using the value of %s in %s:%s",
                           expression.symbol, 
                           context.context-module,
                           context.context-library);
          return(#t);
        end if;
      end for;
      debugger-message("WARNING: Could not evaluate the variable %s "
                       "in any namespace.", expression.symbol);
      return(#f);
    end block;
  end unless;
end method;

define method evaluate-simple-expression
   (application :: <application>, expression :: <illegal-simple-expression>)
      => ()
  expression.expression-evaluated? := #t;
  expression.expression-value := as-remote-value(0);
end method;


///// EVALUATE-AS-FAR-AS-POSSIBLE
//    A <function-expression> cannot be evaluated without running
//    dylan code in the runtime. However, such expressions are restricted
//    to having arguments which are <simple-expressions>, and which can
//    therefore be evaluated. This function evaluates all of the
//    <simple-expression>s that make up a <function-call-expression>, and
//    returns #t if (and only if) they all evaluate legally.

define method evaluate-as-far-as-possible
    (application :: <application>, expression :: <function-expression>)
       => (ok? :: <boolean>)
  evaluate-simple-expression(application, expression.function-expression);
  if (expression.function-expression.expression-evaluated?)
    let all-ok? = #t;
    for (arg-expression in expression.function-arguments-list)
       evaluate-simple-expression(application, arg-expression);
       all-ok? := all-ok? & arg-expression.expression-evaluated?;
    end for;
    all-ok?;
  else
    #f
  end if
end method;


///// COMPUTE-FUNCTION-BREAKPOINT-TARGETS
//    The destination(s) of a breakpoint is given by a <simple-expression>.

define method compute-function-breakpoint-targets
   (application :: <application>, expression :: <simple-expression>)
      => (targets :: <sequence>, objects :: <sequence>, bp-types :: <sequence>)
  evaluate-simple-expression(application, expression);
  if (expression.expression-evaluated?)
    compute-general-breakpoint-targets
      (application, expression.expression-value)
  else
    values(#[], #[], #[])
  end if
end method;

define method compute-general-breakpoint-targets
   (application :: <application>, value :: <remote-value>)
 => (targets :: <sequence>, objects :: <sequence>, bp-types :: <sequence>)

  let classification = classify-runtime-value(application, value);
  let targets = #[];
  let bp-types = #[];
  let objects = #[];

  select (classification)
    #"dylan-generic-function" =>
      let method-list = dylan-generic-function-methods(application, value);
      targets := make(<vector>, size: method-list.size);
      objects := make(<vector>, size: method-list.size);
      bp-types := make(<vector>, 
                       size: method-list.size, 
                       fill: #"dylan-method-iep");
      for (i from 0 below method-list.size)
        let (signature, breakpoint-address, dummy) =
          remote-method-inspect(application, method-list[i]);
        targets[i] := breakpoint-address;
        objects[i] := method-list[i];
      end for;

    #"dylan-method" =>
      let (signature, breakpoint-address, dummy) =
         remote-method-inspect(application, value);
      targets := vector(breakpoint-address);
      bp-types := vector(#"dylan-method-iep");
      objects := vector(value);

    otherwise =>
      let (sym, offset) =
        symbol-table-symbol-relative-address
          (application.debug-target-symbol-table, value);
      if (sym & (offset == 0))
        if (sym.remote-symbol-language == $symbol-language-Dylan)
          targets := vector(value);
          bp-types := vector(#"dylan-method-iep");
          objects := vector(#f);
	else
          targets := vector(value);
          bp-types := vector(#"foreign");
          objects := vector(#f);
	end if
      else
        targets := vector(value);
        bp-types := vector(#"direct");
        objects := vector(#f);
      end if;
  end select;
  values(targets, objects, bp-types);
end method;


