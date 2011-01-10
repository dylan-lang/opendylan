module:    harp-op
Synopsis:  The <op> class definition.
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define constant nil-fn = 
  method (back-end, ins)
    #();
  end;

define constant false-fn = 
  method (back-end, ins)
    #f
  end;

define method no-template (#rest args)
  error("No backend template for args %=.", args);
end;


// Spread functions take parameters 
//  (fn :: <function>, vec :: <stretchy-vector>, ins :: <integer>)
define constant <spread-fn> = <function>; 

// Register type functions take parameters
// (backend :: <harp-back-end>, ins :: <integer>);
define constant <reg-fn> = <function>;   

define primary open class <op> (<object>)
  //// Target machine independent slots ...

  slot op-name :: <string>, init-keyword: name:;

  slot op-properties :: <integer> = 0;

  //// Target machine specific slots...

  slot op-disallow-fn :: <reg-fn>, init-value: nil-fn;
  // those real registers that are zapped by this ins and cannot 
  // be used for arguments

  slot op-prefer-fn :: <reg-fn>, init-value: nil-fn;

  slot op-clash-fn :: <reg-fn>, init-value: nil-fn;

  slot op-destroys-fn :: <reg-fn>, init-value: nil-fn;
  // those real registers that are zapped by this instruction

  slot op-c-preserved-destroys-fn :: <reg-fn>, init-value: nil-fn;
  // those registers that are zapped by this instruction, even though
  // they might not have been allocated, and are also preserved registers 
  // in C's calling convention

  slot op-implicit-uses :: <reg-fn>, init-value: nil-fn;

  slot op-spread :: <spread-fn>;

  slot op-code-gen-fn :: <function>, init-value: no-template;

  slot op-reverse-op, init-value: #F;	// #F or an accessor function

  slot op-constant-fold-op, init-value: #F;	
  // #F or a Dylan function for constant folding

  slot op-info, init-value: #f;
  // A slot that may be used by backends as required.
end;


define leaf packed-slots op-properties (<op>, <object>)

  boolean slot op-eliminatable = #f;
  // when #t, can zap this op if its defs aren't used

  boolean slot op-flag = #f;
  // added for stack ops, can be used for other purposes

  boolean slot op-no-schedule = #f;

  boolean slot op-bb-ender = #f;
  // when t prevents bb from being stitched

  boolean slot op-external-transfer = #f;
  // when T prevents bb containing this op from being coalesced.

  boolean slot op-does-jump = #f;
  // When true, the uses and defs are analysed specially.

  boolean slot op-for-effective-address = #f;
  // When true, the op does an effective address operation.

  boolean slot op-is-move = #f;
  // When true, the op is a MOVE type instruction.

  boolean slot op-is-rem = #f;
  // When true, the op is a REM type instruction.

  boolean slot op-is-scl = #f;
  // When true, the op is an SCL type instruction

  boolean slot op-stack-dependent = #f;
  // When true, the op is stack / leaf-case dependent

  boolean slot op-keep-virtual = #f;
  // When true, don't remove the virtual register information
  // from the instruction vector before invoking the templates

end packed-slots;


define macro op-slots
  { op-slots () }
    => {  name,
          eliminatable,
          disallow-fn,
          prefer-fn,
          clash-fn,
          code-gen-fn,
          spread,
          implicit-uses,
          reverse-op,
          constant-fold-op,
          flag,
          no-schedule,
          bb-ender,
          destroys-fn,
          c-preserved-destroys-fn,
          external-transfer,
          does-jump,
          for-effective-address,
          is-move,
          is-rem,
          is-scl,
          stack-dependent,
          keep-virtual,
          info
        }
end macro;


define macro copy-op-slots
  { copy-op-slots (?old:name, ?slots:macro) }
    => { let old = ?old;
         let new = make(old.object-class);
         ?slots;
         new }

slots:
  { } => { }
  { ?:name, ... } 
    =>  { new . "op-" ## ?name := old . "op-" ## ?name ; ... }

end macro;


// USE SHALLOW-COPY

define method copy-op (old :: <op>) => (new :: <op>)
  copy-op-slots(old, op-slots())
end method;



define macro op-modifier-fn-aux
  { op-modifier-fn-aux ?:name (?keys:*) (?updates:*) end }
    => { define method ?name 
             (op :: <op>, #key ?keys) => ()
           ?updates
         end method }

keys:
  { } => { }
  { ?:name, ... } => { ?name = $unsupplied, ... }

updates:
  { } => { }
  { ?:name, ... } 
    => { if (?name.supplied?) 
           op . "op-" ## ?name := ?name 
         end ; ... }
  
end macro;

define macro op-modifier-fn-definer
  { define op-modifier-fn ?:name (?slots:macro) }
    => { op-modifier-fn-aux ?name 
           (?slots)
           (?slots)
         end }
end macro;

define op-modifier-fn modify-op (op-slots());





define method make-op (name :: <string>) => (new :: <op>)
  make(<op>, name: name);
end;


////// can be changed to other test, so the flag can be used for other 
////// things
////// 

define constant stack-op? = op-flag;
define constant stack-op?-setter = op-flag-setter;




