Module: cpp-internal
Author: Toby Weinberg
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

ignorable(source-stream-setter, operands-setter, operators-setter);

// This file contains definitions supporting cpp's #if, #else etc.
// conditional compilation directives.  It also contains an evaluator for
// compile time constant expressions.

// Conditional compilation state is kept on the skip-stack which is one of
// the slot of <cpp-stream>.  The top of stack (or empty) determines the
// current skip-state.  Values pushed onto the skip-stack must be one
// of #"read-to-end", #"read-to-else", #"skip-to-end" or #"skip-to-else".
// These correspond in the obvious ways to true or false if or else
// clauses.  Every #if or #ifdef pushes one of these tokens onto the
// skip-stack.  Every #endif pops one.  The only interesting point is that
// if the state is skip then a newly encountered #if or #ifdef must only push
// a #"skip-to-end".  #else and #elif are ignored when the state is
// #"skip-to-end".

define function read-cpp-if (stream :: <cpp-stream>, if-type :: <symbol>)
 => ();
  if (~empty?(stream.skip-stack) &
	((stream.skip-stack.first == #"skip-to-end") |
	   (stream.skip-stack.first == #"skip-to-else")))
    push(stream.skip-stack, #"skip-to-end");
    // This will continue in the cpp-skip loop
  elseif(evaluate-constant-expression?(stream, if-type))
    push(stream.skip-stack, #"read-to-else");
  else
    push(stream.skip-stack, #"skip-to-else");
    cpp-skip(stream);
  end if;
end function;

define method read-cpp-else (stream :: <cpp-stream>)
 => ();
  // check for garbage on the rest of the line
  get-next-token(stream);
  skip-to-end-of-line-and-warn(stream, "#else");
  if ( ~ empty?(stream.skip-stack))
    select (stream.skip-stack.first)
      #"skip-to-end" => ; // Don't do anything
      #"skip-to-else" =>
	pop(stream.skip-stack);
	push(stream.skip-stack, #"read-to-end");
	// this will exit the skip loop
      #"read-to-end" => 
	cpp-error(stream,
		  "#else with no matching #if: skip-stack is read-to-end"); 
      #"read-to-else" => 
	pop(stream.skip-stack);
	push(stream.skip-stack, #"skip-to-end");
	cpp-skip(stream);
    end select;
  else
    cpp-error(stream,
	      "#else with no matching #if: skip-stack is empty"); 
  end if;
end method;

define function read-cpp-elif (stream :: <cpp-stream>)
 => ();
  if ( ~ empty?(stream.skip-stack))
    select (stream.skip-stack.first)
      #"skip-to-end" => ; // Don't do anything
      #"skip-to-else" =>
	pop(stream.skip-stack);
	read-cpp-if(stream, #"if");
      #"read-to-end" => 
	cpp-error(stream,
		  "#elif with no matching #if: skip-state is read-to-end");
      // this is so because only #else generates read-to-end
      #"read-to-else" => 
	pop(stream.skip-stack);
	push(stream.skip-stack, #"skip-to-end");
	cpp-skip(stream);
    end select;
  else
    cpp-error(stream,
	      "#else with no matching #if: skip-stack is empty"); 
  end if;
end function;

define function read-cpp-endif (stream :: <cpp-stream>)
 => ();
  // check for garbage on the rest of the line
  get-next-token(stream);
  skip-to-end-of-line-and-warn(stream, "#endif");
  if ( ~ empty?(stream.skip-stack))
    pop(stream.skip-stack);
  else
    cpp-error(stream,
	      "#endif with no matching #if: skip-stack is empty"); 
  end if;
end function;

define method cpp-skip (stream :: <cpp-stream>) => ();
  while (~empty?(stream.skip-stack) &
	   ((stream.skip-stack.first == #"skip-to-end") |
	      (stream.skip-stack.first == #"skip-to-else")))
    get-next-token(stream);
    select (stream.current-token by instance?)
      <pound> => 
	// If the last-token was a <new-line> and there is a define word
	// following, then this is a cpp directive.  
	if (instance?(stream.last-token, <new-line>))
	  get-next-non-space-token(stream);
	  if(instance?(stream.current-token, <identifier>))
	    select (stream.current-token.lexer-string by \=)
	      "if" => read-cpp-if(stream, #"if");
	      "ifdef" => read-cpp-if(stream, #"ifdef");
	      "ifndef" => read-cpp-if(stream, #"ifndef");
	      "elif" => read-cpp-elif(stream);
	      "else" => read-cpp-else(stream);
	      "endif" => read-cpp-endif(stream);
	      otherwise => ; // don't do anything
	    end select;
	  end if;
	end if;
      <new-line> =>
	// set last-token to the <new-line> so that we correctly expand
	// lines beginning with #.
	stream.last-token := stream.current-token;
      <eoi> =>
	// If the current inner stream is empty this might be an included
	// file.  If so pop out to the enclosing include file's stream
	// otherwise it's really the end...
	if(empty?(stream.inner-stream-stack))
	  cpp-error(stream,
		    "end of input while expecting #endif");
	else
	  stream.inner-stream := pop(stream.inner-stream-stack);
	end if;
	stream.last-token := make(<new-line>);
      otherwise => ;
	// do nothing
    end select;
  end while;
end method;

define method evaluate-constant-expression?
    (stream :: <cpp-stream>, if-type == #"if") => (result :: <boolean>);
  let expression-as-tokens = make(<deque>);
  for (expanded-token = read-element(stream, return-new-line?: #t,
				     evaluate-defined-expression?: #t)
	 then read-element(stream, return-new-line?: #t,
			   evaluate-defined-expression?: #t),
       until: instance?(expanded-token, <new-line>))
    push-last(expression-as-tokens, expanded-token);
  finally 
    push-last(expression-as-tokens, expanded-token);  // push the new-line 
  end for;
  0 ~= constant-expression-to-integer(stream, expression-as-tokens)
end method;

define method evaluate-constant-expression?
    (stream :: <cpp-stream>, if-type == #"ifdef") => (result :: <boolean>);
  get-next-non-space-token(stream);
  if (instance?(stream.current-token, <identifier>))
    ~ ( ~ element(stream.macro-definitions, 
		  stream.current-token.lexer-string,
		  default: #f))
  else
    cpp-error(stream,
	      "#ifdef not followed by an identifier");
  end if
end method;

define method evaluate-constant-expression?
    (stream :: <cpp-stream>, if-type == #"ifndef") => (result :: <boolean>);
  get-next-non-space-token(stream);
  if (instance?(stream.current-token, <identifier>))
    ~ element(stream.macro-definitions, stream.current-token.lexer-string,
	      default: #f)
  else
    cpp-error(stream,
	      "#ifdef not followed by an identifier");
  end if
end method;

// Expression evaluator for compile time constant expressions allowable in
// #if cpp directives.  The stream parameter is here to allow for
// creating constant expression evaluators specific to particuuar
// subclasses of cpp-streams (if that turns out to be necessary).  Also to
// allow for evaluation of identifiers as defined or not.  This is
// a generic ansi expression evaluator...

// Simple shift reduce precedence evaluator.  There are two stacks,
// operands and operators.  The basic rules are:
// 
// 1. Push an operator of lower precedence than the top of the operator
//    stack => reduce (pop the top operator and it's operands, evaluate
//    it and push the result onto the operand stack) and try again.
// 2. Push an operator of higher precedence than the top of the operator
//    stack => shift (push the operator).
// 3. Push an operator of the same precedence:
//      if left associative => reduce
//      if right associative => shift
// 
// The beginning and end of expression are special operators with
// precedence 0.  Open and close parenthesis also have precedence 0 but
// pushing the open parenthesis doesn't cause a reduce.  Identifiers are
// assumed to be undefined macros whose value is 0 except for the
// identifier "defined" which is filtered out before the tokens are
// expanded in read-element as defined on cpp-streams.  Pushing two
// operands in a row is always an error.  Pushing two operators in a row is
// only an error if the second operator can't be interpreted as a prefix
// operator.
// 
// The precedence and associativity of tokens as operators is defined in
// token.dylan.

define constant $literal-one = 
  make(<decimal-integer-literal>, lexer-string: "1");
define constant $literal-zero = 
  make(<decimal-integer-literal>, lexer-string: "0");

// This function is called in read-element for <cpp-stream>s when the
// evaluate-defined-expression? keyword is #t
define function evaluate-defined-expression(stream :: <cpp-stream>)
 => (result :: <decimal-integer-literal>);
  let result = $literal-zero;
  get-next-non-space-token(stream);
  select (stream.current-token by instance?)
    <identifier> => 
        if (element(stream.macro-definitions, 
		    stream.current-token.lexer-string,
		    default: #f))
	  result := $literal-one;
	end if;
    <open-parenthesis> => 
      get-next-non-space-token(stream);
      if (instance?(stream.current-token, <identifier>))
        if (element(stream.macro-definitions, 
		    stream.current-token.lexer-string,
		    default: #f))
	  result := $literal-one;
	end if;
      else
	cpp-error(stream,
		  "Badly formed \"defined\" in #if constant expression");
      end if;
      get-next-non-space-token(stream); 
      unless (instance?(stream.current-token, <close-parenthesis>))
	cpp-error(stream,
		  "Badly formed \"defined\" in #if constant expression");
      end unless;
    otherwise =>
      cpp-error(stream,
		"Badly formed \"defined\" in #if constant expression");
  end select;
  result
end function;

define class <begin-expression> (<symbol-token>)
  constant slot precedence :: <integer> = 0;
  constant slot associativity :: <associativity-type> = #"right";
end;

define constant $begin-expression = make(<begin-expression>);

define class <end-expression> (<symbol-token>)
  constant slot precedence :: <integer> = 0;
  constant slot associativity :: <associativity-type> = #"right";
end;

define constant $end-expression = make(<end-expression>);

define class <constant-expression-evaluator> (<object>)
  slot operands :: <deque>, init-function: curry(make, <deque>);
  slot operators :: <deque>, init-function: curry(make, <deque>);
  slot last-pushed :: one-of(#"operand", #"operator"), 
    init-value: #"operator";
  slot source-stream :: <cpp-stream>, 
    required-init-keyword: source-stream:;
end class;

define method initialize 
    (evaluator :: <constant-expression-evaluator>, #key)
 => ();
  next-method();
  push(evaluator.operators, $begin-expression);
end method;

// constant expression evaluator top-level
define method constant-expression-to-integer
    (stream :: <cpp-stream>, expression-as-tokens :: <deque>)
  => (result :: <integer>)
  let evaluator = make(<constant-expression-evaluator>,
		       source-stream: stream);
  for (token in expression-as-tokens)
    evaluate-token(token, evaluator)
  end for;
  pop(evaluator.operands);
end method;

define method evaluate-token 
    (token :: <literal-token>, evaluator :: <constant-expression-evaluator>)
 => ();
  if(evaluator.last-pushed ~= #"operand")
    push(evaluator.operands, constant-value(token));
    evaluator.last-pushed := #"operand";
  else
    cpp-error(evaluator.source-stream, 
	      "Error in #if constant expression: two operands in a row");
  end if;
end method;

define method evaluate-token 
    (token :: <identifier>, evaluator :: <constant-expression-evaluator>)
 => ();
  if (evaluator.last-pushed = #"operand")
    cpp-error(evaluator.source-stream, 
	      "Error in #if constant expression: two operands in a row");
  else
    push(evaluator.operands, constant-value(token));
    evaluator.last-pushed := #"operand";
  end if;
end method;

define method evaluate-token
    (token :: <new-line>, evaluator :: <constant-expression-evaluator>)
 => ();
  push-operator(evaluator, $end-expression);
end method;

define method evaluate-token
    (token :: <open-parenthesis>, 
     evaluator :: <constant-expression-evaluator>)
 => ();
  // Don't do the usual operator push because open parenthesis shouldn't
  // force a reduce even though it has precedence 0.
  if (  evaluator.last-pushed ~= #"operand")
    push(evaluator.operators, token);
    evaluator.last-pushed := #"operator";
  else
    cpp-error(evaluator.source-stream,
	    "Error in constant expression: open parenthesis follows operand");
  end if;
end method;

define method evaluate-token
    (token :: <close-parenthesis>, 
     evaluator :: <constant-expression-evaluator>)
 => ();
    push-operator(evaluator, token );
end method;

// Special methods for dealing with unary operators
define constant $unary-plus = make(<unary-plus>);

define method evaluate-token
    (token :: <plus>, evaluator :: <constant-expression-evaluator>)
 => ();
  push-operator(evaluator,
		if (evaluator.last-pushed == #"operator") $unary-plus
		else token end if);
end method;

define constant $unary-minus = make(<unary-minus>);

define method evaluate-token
    (token :: <minus>, evaluator :: <constant-expression-evaluator>)
 => ();
  push-operator(evaluator,
		if (evaluator.last-pushed == #"operator") $unary-minus
		else token end if);
end method;

define method evaluate-token
    (token :: <tilde>, evaluator :: <constant-expression-evaluator>)
 => ();
  push-operator(evaluator, token);
end method;

define method evaluate-token
    (token :: <not>, evaluator :: <constant-expression-evaluator>)
 => ();
  push-operator(evaluator, token);
end method;

// Catch-all for evaluating binary operators
define method evaluate-token
    (token :: <binary-operator>, 
     evaluator :: <constant-expression-evaluator>)
 => ();
  if ( evaluator.last-pushed = #"operand")
    push-operator(evaluator, token);
  else
    if (instance?(evaluator.operators.first, <begin-expression>))
      cpp-error(evaluator.source-stream,
	      "#if constant expression begins with a binary operator");
    else
      cpp-error(evaluator.source-stream,
	        "Error in #if constant expression: two binary operators in a row");
    end if;
  end if;
end method;

define method evaluate-token
    (token :: <symbol-token>, 
     evaluator :: <constant-expression-evaluator>)
 => ();
  cpp-error(evaluator.source-stream,
	    "Error in #if constant expression: unrecognized operator");
end method;


// Termination is delicate for this loop.  End expression and close
// parenthesis force reduction of anything but open parenthesis and begin
// expression, because all have precedence 0 but associativity is right.
// Tricky that. Close parenthesis can have right associativity because any
// expression in parentheses is always reduced before another close
// parenthesis can be pushed.
define method push-operator 
    (evaluator :: <constant-expression-evaluator>, new-operator :: <token>)
  => ();
  while ((new-operator.precedence < evaluator.operators.first.precedence)
	   | ((new-operator.precedence = evaluator.operators.first.precedence)
		& (new-operator.associativity = #"left")))
    cpp-reduce(pop(evaluator.operators), evaluator);
  end while;
  select(new-operator by instance?)
    <close-parenthesis> =>
      // attempting to push a <close-parenthesis> should reduce everything
      // but an <open-parenthesis> or a <begin-expression> so:
      if(instance?(evaluator.operators.first, <open-parenthesis>))
	pop(evaluator.operators);
	// The value of the expression between the parentheses should be on
	// the top of the stack.
	evaluator.last-pushed := #"operand";
      else
	cpp-error(evaluator.source-stream,
		  "close parenthesis without matching open parenthesis");
      end if;
    <end-expression> =>
      unless(instance?(evaluator.operators.first, <begin-expression>))
	cpp-error(evaluator.source-stream,
		  "end of constant expression of operand stack isn't empty");
      end unless;
    otherwise =>
      push(evaluator.operators, new-operator);
      evaluator.last-pushed := #"operator";
  end select;
end method;

// These are all slightly wrong, should either use machine word library
// operations or if that isn't possible callout to C routines (which is
// probably the right way to do this (sigh))
define method cpp-reduce
    (operator :: <colon>, evaluator :: <constant-expression-evaluator>)
 => ();
  let operand-3 = pop(evaluator.operands);
  let operand-2 = pop(evaluator.operands);
  let operand-1 = pop(evaluator.operands);
  unless (instance?(pop(evaluator.operators), <question>))
    cpp-error(evaluator.source-stream,
	      ": not matched with ?");
  end unless;
  push(evaluator.operands,
       if (operand-1 ~= 0) operand-2 else operand-3 end if);
end method;

define method cpp-reduce
    (operator :: <unary-operator>, evaluator :: <constant-expression-evaluator>)
 => ();
  let operand-1 = pop(evaluator.operands);
  push(evaluator.operands,
       select (operator by instance?)
	 <unary-plus> => operand-1;
	 <unary-minus> => (- operand-1);
	 <not> => if (operand-1 = 0) 1 else 0 end if;
	 <tilde> => lognot(operand-1)
       end select);
  evaluator.last-pushed := #"operand";
end method;

define method cpp-reduce
    (operator :: <binary-operator>, 
     evaluator :: <constant-expression-evaluator>)
 => ();
  let operand-2 = pop(evaluator.operands);
  let operand-1 = pop(evaluator.operands);
  push(evaluator.operands,
       select (operator by instance?)
	 <and-and> => 
	   if ((operand-1 ~= 0) & (operand-2 ~= 0)) 1 else 0 end if;
	 <or-or> => 
	   if ((operand-1 ~= 0) | (operand-2 ~= 0)) 1 else 0 end if;
	 <not-equal> => 
	   if (operand-1 ~= operand-2) 1 else 0 end if;
	 <equal-equal> => 
	   if (operand-1 = operand-2) 1 else 0 end if;
	 <less-than-or-equal> => operand-1 <= operand-2;
	 <less-than> => operand-1 < operand-2;
	 <greater-than-or-equal> => operand-1 >= operand-2;
	 <greater-than> => operand-1 > operand-2;
	 <and> => logand(operand-1, operand-2);
	 <or> => logior(operand-1, operand-2);
	 <carat> => logxor(operand-1, operand-2); // bit-wise xor
	 <left-shift> => ash(operand-1, operand-2);
	 // This is wrong: ash right shifts the same value as the sign bit,
	 // but C >> shifts in 0s and behavior is not defined for signed
	 // first operands.
	 <right-shift> => ash(operand-1, -operand-2);
	 <plus> => operand-1 + operand-2;
	 <minus> => operand-1 - operand-2;
	 <star> => operand-1 * operand-2;
	 <divide> => floor/(operand-1, operand-2); // ????
	 <remainder> => 
	   begin let (quo, rem) = floor/(operand-1, operand-2); rem end;
       end select);
  evaluator.last-pushed := #"operand";
end method;

define method test-ansi-cpp-evaluator (string)
  let stream = make-test-cpp-stream (string);
  let expression-as-tokens = make(<deque>);
  for (expanded-token = read-element(stream, return-new-line?: #t,
				     evaluate-defined-expression?: #t)
	 then read-element(stream, return-new-line?: #t,
			   evaluate-define-expression?: #t),
       until: instance?(expanded-token, <new-line>))
    push-last(expression-as-tokens, expanded-token);
  finally 
    push-last(expression-as-tokens, expanded-token);  // push the new-line 
  end for;
  constant-expression-to-integer(stream, expression-as-tokens)
end method;
