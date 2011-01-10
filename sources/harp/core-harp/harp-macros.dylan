module:    base-harp
Synopsis:  General macro & utility support for HARP
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


//// Low level accessors for the instruction vector

// It is important that this code is as efficient as possible
// and vector bounds are guaranteed elsewhere, so inline element
// and do no bound-checks


define inline function ins-op
    (sv :: <instructions-vector>, ins :: <integer>) => (op :: <op>)
  without-bounds-checks
    sv[ins + instruction-op-index];
  end;
end function;


define inline function ins-op-setter
    (new :: <op>, sv :: <instructions-vector>, ins :: <integer>) => (op :: <op>)
  without-bounds-checks
    sv[ins + instruction-op-index] := new;
  end;
end function;


define inline function ins-tag
    (sv :: <instructions-vector>, ins :: <integer>) => (tag)
  without-bounds-checks
    sv[ins + instruction-special-index];
  end;
end function;


define inline function ins-tag-setter
    (new, sv :: <instructions-vector>, ins :: <integer>) => (tag)
  without-bounds-checks
    sv[ins + instruction-special-index] := new;
  end;
end function;



//// Constant function generation

define macro constant-fn
  { constant-fn(?:body) }
    => { begin
           let the-constant = ?body;
           method (backend, ins) the-constant end;
         end }
end macro;



//// Iterate over the instructions in the basic-block

define macro for-instructions-in-basic-block
  { for-instructions-in-basic-block (?ins:name in ?block:expression)
      ?:body
    end }
    => 
    { begin
        let $bb$ = ?block;
        let $start$ = $bb$.bb-start;
        let $finish$ = $bb$.bb-end;
        for (?ins :: <integer> from $start$ below $finish$ by instruction-size)
          ?body
        end
      end }
end macro;


//// Iterate backwards over the instructions in the basic-block

define macro for-instructions-in-basic-block-backwards
  { for-instructions-in-basic-block-backwards (?ins:name in ?block:expression)
      ?:body
    end }
    => 
    { begin
        let $bb$ = ?block;
        let $start$ = $bb$.bb-start;
        let $finish$ = $bb$.bb-end;
        for (?ins :: <integer> from ($finish$ - instruction-size) to $start$ by (- instruction-size))
          ?body
        end
      end }
end macro;



define macro set-def
  { set-def(?new-val:expression) }
    =>
  {
   element-no-bounds-check(?=def-vector, ?=def-index) := ?new-val
   }
end macro;

define macro set-use
  { set-use(?new-val:expression) }
    =>
  {
   element-no-bounds-check(?=use-vector, ?=use-index) := ?new-val 
   }
end macro;



/// Iterate over the defs of an instruction. The value of the def can be
/// altered by going (set-def new-value) in the body. 


define macro for-instruction-defs
  { for-instruction-defs (?def:name in ?sv-ins:expression at ?instruction:expression)
      ?:body
    end }
    => 
    { begin
	let def-index :: <integer> = ?instruction + instruction-defs-index;
	let def-vector :: <instructions-vector> = ?sv-ins;
	let count :: <integer> = instruction-defs-slots - 1;
	iterate process-defs
	  (?=def-index :: <integer> = def-index,
           ?=def-vector :: <simple-object-vector> = def-vector,
           count :: <integer> = count)
	  if (count >= 0)
	    let ?def = element-no-bounds-check(?=def-vector, ?=def-index);
	    if (?def)
	      if (instance?(?def, <simple-object-vector>))
		let def-vector :: <simple-object-vector> = ?def;
		process-defs(0, def-vector, def-vector.size - 1)
	      else
		?body;
		process-defs(?=def-index + 1, ?=def-vector, count - 1)
	      end if;
	    else
	      process-defs(?=def-index + 1, ?=def-vector, count - 1)
	    end if;
	  end if;
	end iterate
      end }
end macro;


/// Iterate over the uses of an instruction. The value of the use can be
/// altered by going (set-use new-value) in the body.


define macro for-instruction-uses
  { for-instruction-uses (?use:name in ?sv-ins:expression at ?instruction:expression)
      ?:body
    end }
    => 
    { begin
	let use-index :: <integer> = ?instruction + instruction-uses-index;
	let use-vector :: <instructions-vector> = ?sv-ins;
	let count :: <integer> = instruction-uses-slots - 1;
	iterate process-uses
	  (?=use-index :: <integer> = use-index,
	   ?=use-vector :: <simple-object-vector> = use-vector,
	   count :: <integer> = count)
	  if (count >= 0)
	    let ?use = element-no-bounds-check(?=use-vector, ?=use-index);
	    if (?use)
	      if (instance?(?use, <simple-object-vector>))
		let use-vector :: <simple-object-vector> = ?use;
		process-uses(0, use-vector, use-vector.size - 1)
	      else 
		?body;
		process-uses(?=use-index + 1, ?=use-vector, count - 1)
	      end if;
	    end if;
	  end if;
        end iterate
      end }
end macro;


//// Structured accessor for the operands of an instruction.
//// The details depend on the instruction type.


define macro ins-operand-element
  // Pattern where the number of this type of operand is variable
  { ins-operand-element
      (?sv:expression, ?ins:expression,
       #f, ?n:expression,
       ?base:expression, ?slots:expression,
       ?setval)
  }
    => { begin
           let element = element-no-bounds-check;
           let element-setter = element-no-bounds-check-setter;
           let pos = ?ins + (?base - 1);
           if ((?n < ?slots) |
               ((?n == ?slots) & ~(instance?(?sv[pos + ?n], <simple-object-vector>))))
             // the operand is in-line in the main vector
             ?sv[pos + ?n] ?setval
           else
             // The operand is in an out-of-line vector
             let ovec :: <simple-object-vector> = ?sv[pos + ?slots];
             ovec[?n - ?slots] ?setval
           end if;
         end
       }

  // Pattern where the number of this type of operand is fixed
  { ins-operand-element
      (?sv:expression, ?ins:expression,
       ?total:expression, ?n:expression,
       ?base:expression, ?slots:expression,
       ?setval)
  }
    => { begin
           let element = element-no-bounds-check;
           let element-setter = element-no-bounds-check-setter;
           let pos = ?ins + (?base - 1);
           if ((?total <= ?slots) | (?n < ?slots))
             // the operand is in-line in the main vector
             ?sv[pos + ?n] ?setval
           else
             // The operand is in an out-of-line vector
             let ovec :: <simple-object-vector> = ?sv[pos + ?slots];
             ovec[?n - ?slots] ?setval
           end if;
         end
       }

setval:
  { } => { }

  { ?newval:expression } => { := ?newval }

end macro;





// Historically USE was a Dylan reserved word. As a (now entrenched) dirty hack, 
// we use UZE instead

// Use instruction-specific low-level accessor macros to guarantee
// that everything is running inline at runtime

define macro with-xyz-macro-definer
  { define with-xyz-macro ?:name ?defs:expression, ?uses:expression end }
    => { define macro ?name ## "-def"
	     { ?name ## "-def" (\?n:expression) }
	     =>
	     {
	      ins-operand-element(\?=$sv-instructions$, \?=$sv-instruction$, ?defs, \?n,
				  instruction-defs-index, 
				  instruction-defs-slots)
	     }
         end macro;

         define macro ?name ## "-def-setter"
	     { ?name ## "-def-setter" (\?val:expression, \?n:expression) }
	     =>
	     {
	      ins-operand-element(\?=$sv-instructions$, \?=$sv-instruction$, ?defs, \?n,
				  instruction-defs-index, 
				  instruction-defs-slots,
				  \?val)
	     }
         end macro;

         define macro ?name ## "-uze"
	     { ?name ## "-uze" (\?n:expression) }
	     =>
	     {
	      ins-operand-element(\?=$sv-instructions$, \?=$sv-instruction$, ?uses, \?n,
				  instruction-uses-index, 
				  instruction-uses-slots)
	     }
         end macro;

         define macro ?name ## "-uze-setter"
	     { ?name ## "-uze-setter" (\?val:expression, \?n:expression) }
	     =>
	     {
	      ins-operand-element(\?=$sv-instructions$, \?=$sv-instruction$, ?uses, \?n,
				  instruction-uses-index, 
				  instruction-uses-slots,
				  \?val)
	     }
         end macro;

         define macro "with-" ## ?name 
           { "with-" ## ?name (\?sv:expression at \?ins:expression)
                \?body:body 
              end }
           => { begin
		  let \?=$sv-instructions$ :: <instructions-vector> = \?sv;
		  let \?=$sv-instruction$ :: <integer> = \?ins;
                  \?body
                end 
              }
         end macro }
end macro;


define macro with-txyz-macro-definer
  { define with-txyz-macro ?:name ?defs:expression, ?uses:expression end }
    => { define with-xyz-macro ?name ?defs, ?uses end;
         define with-xyz-macro "t" ## ?name ?defs, ?uses end }
end macro;


//// The macros which create or update the ops in the instruction set

/// Macro op-element behaves like element, but on instruction sets rather
/// than sequences. The keys are (unmangled) names of the instructions.


define macro op-element
  { op-element(?ins-set:expression, ?op-name:name) }
    => { "harp-" ## ?op-name (?ins-set) }
end macro;


define macro op-element-setter
  { op-element-setter(?newval:expression, ?ins-set:expression, ?op-name:name) }
    => { "harp-" ## ?op-name (?ins-set) := ?newval }
end macro;


define macro update-op
  { update-op ?the-op:expression end }
    => { }

  { update-op ?the-op:expression, ?modifiers:* end }
    => { modify-op(?the-op, ?modifiers) }

modifiers:
  { } => { }
  { ?key:token, ?val:expression, ... }
    => { as(<symbol>, ?key), ?val, ... }
end macro;


define macro with-ops-in-aux
  { with-ops-in-aux ?instruction-set:name (?names-as-calls)
      (?bindings)
      (?assignments)
    end }
    => { begin
           let inst-set = ?instruction-set;
           ?bindings;
           local method do-op (op :: <op>) => ()
                   update-op op ?assignments end
                 end method;
           ?names-as-calls
         end
       }

bindings:
  { } => { }

  { ?:name, ?val:expression ; ... }
    => { let "evaluated-" ## ?name = ?val; ... }

assignments:
  { } => { }

  { ?:name, ?val:expression ; ... }
    => { , ?#"name", "evaluated-" ## ?name ... }

names-as-calls:
  { } => { }

  { ?:name, ... }
    => { do-op(op-element(inst-set, ?name)); ... }

end macro;




define macro with-ops-in
  { with-ops-in ?instruction-set:name (?names:*)
      ?slots-and-vals:*
    end }
    => { with-ops-in-aux ?instruction-set (?names)
           (?slots-and-vals)
           (?slots-and-vals)
         end }

slots-and-vals:
  { } => { }

  { ?:name := ?val:expression ; ... }
    => { ?name, ?val; ... }

end macro;


/// for the reverse ops
//// NB the reverse OP is named by it's accessor function


define macro mark-reverse-ops
  { mark-reverse-ops (?instruction-set:expression)
      ?ops:*
    end }
    => { begin
           let inst-set = ?instruction-set;
           ?ops
         end }
ops:
  { } => { }

  { ?op1:name <-> ?op2:name; ... }
    => { with-ops-in inst-set (?op1) reverse-op := "harp-" ## ?op2 end;
         with-ops-in inst-set (?op2) reverse-op := "harp-" ## ?op1 end; 
         ...
        }
end macro;




//// Define the macros which create the ins--name generic functions
//// for each op ...

define macro xyz-definer-macro-definer
  { define xyz-definer-macro ?type:name (?args:*) }
    => { define macro "define-" ## ?type
           { "define-" ## ?type \?class:name () end }
             => {  }
           { "define-" ## ?type \?class:name (\?op:name, \?other-ops:*) end }
             => { define ins-method \?op (backend :: \?class, ?args)
                    let $op$ = op-element(backend.instructions, \?op);
                    "output-" ## ?type(backend, $op$, ?args)
                  end;
                  "define-" ## ?type \?class (\?other-ops) end
                }
         end macro }
end macro;


define macro ins-method-definer
  { define ins-method ?:name ?stuff:* end }
    => { define method "ins--" ## ?name ?stuff end }
end macro;


/// define spread-function defines spreads for each type of instruction


define macro spread-function-definer
  { define spread-function ?:name (?args:*) ?:body end }
    => { define ?name spread-function ?name (?args) ?body end }

  { define ?type:name spread-function ?:name 
        (?backend:name :: ?be-class,
         ?op:name :: ?op-class,
         ?fn:name :: ?fn-class,
         ?ins:name :: ?ins-class,
         ?sv-ins:*) 
      ?:body 
    end }
    => { define function "spread-" ## ?name 
             (?backend :: ?be-class, ?op :: ?op-class, ?fn :: ?fn-class, 
              ?sv-ins :: <instructions-vector>, ?ins :: ?ins-class) 
           "with-" ## ?type (?sv-ins at ?ins)
             ?body 
           end
         end }

sv-ins:
  { } => { $sv-ins$ }
  { \#key sv-ins } => { ?=sv-ins }
  { \#key sv-ins: ?sv:name } => { ?sv }


be-class:
  { <object> } => { <harp-back-end> }
  { ?anything:* } => { ?anything }

fn-class:
  { <object> } => { <function> }
  { ?anything:* } => { ?anything }

ins-class:
  { <object> } => { <integer> }
  { ?anything:* } => { ?anything }

op-class:
  { <object> } => { <op> }
  { ?anything:* } => { ?anything }


end macro;


/* Example usage of define spread-function :
   
The macro behaves like define method, accepting the required 3 arguments. 
If a binding is required for sv-ins, then then it can be considered that the 
value was given as a keyword argument with key sv-ins.

Examples:

define spread-function du (backend, fn, ins)
  fn(backend, def(1), uze(1));
end;

define spread-function tu (backend, fn, ins, #key sv-ins) 
  fn(backend, ins-tag(sv-ins, ins), uze(1));
end;

define tu spread-function contrived tu (backend, fn, ins, #key sv-ins: sv) 
  fn(backend, ins-tag(sv, ins), uze(1));
end;



*/


/// define register-function defines a function for clashes etc.
/// for each type of instruction



define macro register-function-definer

  { define ?type:name register-function ?:name 
        (?backend:name :: ?be-class, ?ins:name :: ?ins-class,
         ?sv-ins:*) 
      ?:body 
    end }
    => { define function ?name 
             (?backend :: ?be-class, ?ins :: ?ins-class) 
           let ?sv-ins :: <instructions-vector> = ?backend . variables . sv-instructions;
           "with-" ## ?type (?sv-ins at ?ins)
             ?body 
           end
         end }

sv-ins:
  { } => { $sv-ins$ }
  { \#key sv-ins } => { ?=sv-ins }
  { \#key sv-ins: ?sv:name } => { ?sv }

be-class:
  { <object> } => { <harp-back-end> }
  { ?anything:* } => { ?anything }

ins-class:
  { <object> } => { <integer> }
  { ?anything:* } => { ?anything }

end macro;



// define-output-function defines a function to output instructions
// and arguments into the instructions vector

define macro output-function-definer
  { define output-function ?:name (?args:*) ?:body end }
    => { define function "output-" ## ?name (?args) ?body end }
end macro;




// define-instruction-function defines the compiler-visible function
// used to generate code for each OP.
//
// define instruction-function allows a named binding for the op 
// to be passed via #key. The op: key must come first.





define macro maybe-bind-variable
  { maybe-bind-variable ?var:name (?init:expression) ?:body end }
    => { let ?var = ?init; ?body }
  { maybe-bind-variable (?init:expression) ?:body end } => { ?body }
end macro;


define macro define-instruction-function-aux

  { define-instruction-function-aux ?:name ?ins:name
        (?backend:name :: ?be-class:expression, ?processed-args:*) (?op-binding)
        => (?results:*)
      ?:body 
    end }
    => { define method "ins--" ## ?name 
             (?backend :: ?be-class ?processed-args) => (?results)
           maybe-bind-variable ?op-binding 
                (?backend . instructions . "harp-" ## ?ins)
             ?body;
           end;
         end }

op-binding:
// Unpick { arg, #key op, foo} into { op  }
// or     { arg, #key op: op-name, foo} into { op-name  }
// or     { arg, #key foo} into { }
  { } => { }
  { \#rest ?rest:variable, ... } => { ... }
  { \#key ?op-arg:* } => { ?op-arg }
  { ?arg:variable, ... } => { ... }

op-arg:
  { op, ?other-args:* } => { ?=op }
  { op: ?op:name, ?other-args:* } => { ?op }
  { ?other-args:* } => { }


processed-args:
// Unpick {arg1, arg2, #key op, foo} into { , arg1, arg2, #key foo }
// or     {arg1, arg2, #key op, foo} into { , arg1, arg2 }
  { } => {  }
  { \#rest ?rest:variable, ... } => { , #rest ?rest, ... } 
  { \#key ?op-or-key-args:* } => { ?op-or-key-args }
  { ?arg:name :: ?type:expression, ... } => { , ?arg :: ?type ... }

op-or-key-args:
  { } => { }
  { op, ?other-args:* } => { ?other-args }
  { op: ?op:name, ?other-args:* } => { ?other-args }
  { ?other-args:* } => { ?other-args }

other-args:
  { } => { }
  { ?key-args:* } => { , #key ?key-args }

be-class:
  { <object> } => { <harp-back-end> }
  { ?anything:* } => { ?anything }

end macro;



define macro instruction-function-definer

  { define ?ins:name instruction-function ?:name (?args:*)
      ?:body 
    end }
    => { define-instruction-function-aux ?name ?ins (?args) (?args) => ()
           ?body 
         end }

  { define ?ins:name instruction-function ?:name (?args:*) => (?results:*)
      ?:body 
    end }
    => { define-instruction-function-aux ?name ?ins (?args) (?args) => (?results)
           ?body 
         end }

  { define instruction-function ?:name ?rest:* end }
    => { define ?name instruction-function ?name ?rest end }

end macro;




// harp-out may be used by backend templates to make use of 
// other templates

define macro harp-out
  { harp-out (?backend:expression)
      ?instructions:*
    end }
    => { begin
           let $instructions$ = ?backend . instructions;
           ?instructions
         end }

instructions:
  { } => { }
  { ?op:name(?be:expression, ?args:*) ; ... }
    => { begin
           let $op$ = $instructions$ . "harp-" ## ?op;
           $op$ . op-code-gen-fn (?be, $op$, ?args) 
         end ; ... }

end macro;



/*

example usage in infix Dylan:

harp-out (backend)
  and(backend, pregs.reg_tmp1, s1, s2);
  move(backend, d1, pregs.tmp1);
end harp-out;

NB individual backends may decide to specialize a macro to avoid passing
the backend set each time. 

*/


//  harp-reapply may be used by a template with the SELF option
//  to reapply the template.


define macro harp-reapply
  { harp-reapply(?backend:expression, ?op:expression, ?args:*) }
    => { let $op$ = ?op;
         $op$ . op-code-gen-fn (?backend, $op$, ?args) }
end macro;


define macro emit
  { emit(?backend:expression, ?emissions:*) }
    => { begin
           let $backend$ = ?backend;
           ?emissions
         end }

emissions:
  { } => { }
  { ?word:expression, ... } => { emit-1($backend$, ?word); ... }

end macro;



// call-instruction may be used to call the instruction generators


define macro call-instruction
  { call-instruction(?:name, ?args:*) }
    => { "ins--" ## ?name (?args) }
end macro;





// ENSURE-MREG moves the register into a temporary if it is not a real
// register already. The next available temporary from a supplied
// variable containing a list or temporaries is used. If none are
// available, #f is returned. 


define macro ensure-mreg
  { ensure-mreg (?backend:expression, ?reg:expression, ?temps:name) }
    => { m-ref(?reg) | if (?temps . empty?)
                         #f
                       else 
                         let tmp = ?temps.head;
                         ?temps := ?temps.tail;
                         harp-out (?backend) move(?backend, tmp, ?reg) end;
                         tmp
                       end }
end macro;



//// SDI support


define macro loop-sdis

  { loop-sdis (?sdi:name  at ?index:name 
               from ?from:expression below ?below:expression 
               in ?vec:expression)
      ?:body
    end }
    => { for (?index :: <integer> from ?from below ?below)
           let ?sdi = ?vec[?index];
           ?body
         end }

  { loop-sdis (?sdi:name at ?index:name 
               below ?below:expression
               in ?vec:expression)
      ?:body
    end }
    => { loop-sdis (?sdi at ?index from 0 below ?below in ?vec) ?body end }

end macro;


// Ensure local bindings of the instructions vector
// keep up with growths in the vector that occur in
// body of code

define macro preserving-instructions
  { preserving-instructions (?sv:name)
      ?:body
    end }
    =>
    { begin

      let body-result = begin ?body end;

      ?sv := ?=vars.sv-instructions;

      body-result

      end }
end macro;

