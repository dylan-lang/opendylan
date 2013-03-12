Module: dfmc-java-back-end
Author: Mark Tillotson
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



define constant $initial-string-stream-contents-size = 10000;

define sealed class <java-back-end> (<back-end>)
  slot %platform-name  = #f,     init-keyword: platform-name:;
  slot %operating-system-name = #f;
  slot %processor-name = #f;
  slot lambda-stream   = make(<string-stream>,
                              direction: #"output",
                              contents:  make(<byte-string>,
                              size:      $initial-string-stream-contents-size));
  slot mangle-buffer   = make(<stretchy-vector>);
  slot current-module  = #f;
end;

define method initialize (back-end :: <java-back-end>, #key, #all-keys) => ()
  next-method ();
  stream-contents (back-end.lambda-stream, clear-contents?: #t);
end method;

define variable *java-back-end* = make (<java-back-end>);

default-back-end() := *java-back-end*; // !@#$ hack

// eof


define function pprint-dfms (dfm :: <computation>, last :: false-or (<computation>), depth :: <integer>)
  while (dfm & (dfm ~== last))
    pprint-dfm (dfm, depth);
    dfm := dfm.next-computation
  end
end;

define sealed generic pprint-dfm (dfm :: <computation>, depth :: <integer>) => ();

define function indent (depth :: <integer>)
  for (i from 0 below depth)
    format-out ("   ")
  end
end;


define function maybe-assign-temp (dfm :: <computation>) => ()
  let  temp = dfm.temporary;
  if (temp)
    pprint-val (temp);
    format-out (" := ")
  end
end;

// default method
define method pprint-dfm (dfm :: <computation>, depth :: <integer>) => ()
  indent (depth);
  maybe-assign-temp (dfm);
  format-out ("%s", dfm.object-class);
  format-out ("\n")
end;

define method pprint-dfm (dfm :: <if-merge>, depth :: <integer>) => ()
//  indent (depth);
//  format-out ("// redundant <if-merge>\n")
end;
define method pprint-dfm (dfm :: <loop-merge>, depth :: <integer>) => ()
  indent (depth);
  format-out ("// redundant <loop-merge>\n")
end;
define method pprint-dfm (dfm :: <bind-exit-merge>, depth :: <integer>) => ()
//  indent (depth);
//  format-out ("// redundant <bind-exit-merge>\n")
end;

define method pprint-dfm (dfm :: <if>, depth :: <integer>) => ()
  indent (depth);
  maybe-assign-temp (dfm);
  format-out ("IF (%s)\n", dfm.test);
  let  merge = dfm.next-computation;
  if (~ instance? (merge, <if-merge>))
    error ("missing <if-merge>")
  end;
  if (dfm.consequent == merge & dfm.alternative == merge & merge.temporary == #f)
    error ("<if> node missing both arms")
  end;
  if (dfm.temporary)
    error ("<if> node with temporary")
  end;
  pprint-dfms (dfm.consequent, merge, depth + 1);
  if (merge.temporary)
    indent (depth + 1);
    pprint-val (merge.temporary);
    format-out (" := ");
    pprint-val (merge.merge-left-value);
    format-out ("\n")
  end;
  indent (depth); format-out ("ELSE\n");
  pprint-dfms (dfm.alternative, merge, depth + 1);
  if (merge.temporary)
    indent (depth + 1);
    pprint-val (merge.temporary);
    format-out (" := ");
    pprint-val (merge.merge-right-value);
    format-out ("\n")
  end;
  indent (depth); format-out ("END IF\n")
end;

define variable *loop-namer* = make (<object-table>);

define method pprint-dfm (dfm :: <bind-exit>, depth :: <integer>) => ()
  let  es = dfm.entry-state;
  let  block-tag = size (*loop-namer*);
  *loop-namer* [es] := block-tag;
  indent (depth);
  let  maybe-merge = dfm.next-computation;
  if (dfm.temporary)
    format-out ("#| ");
    pprint-val (maybe-merge.temporary);
    format-out (" := |# ");
  end;
  format-out ("BIND-EXIT ("); pprint-val (es); format-out (") %s\n", block-tag);
  pprint-dfms (dfm.body, #f, depth + 1);
  indent (depth); format-out ("END BIND-EXIT\n");
  remove-key! (*loop-namer*, es);
  #f
end;

define method pprint-dfm (dfm :: <unwind-protect>, depth :: <integer>) => ()
  let  es = dfm.entry-state;
  let  block-tag = size (*loop-namer*);
  *loop-namer* [es] := block-tag;
  indent (depth);
  maybe-assign-temp (dfm);
  format-out ("UNWIND-PROTECT ("); pprint-val (es); format-out (") %s protected=", block-tag);
  pprint-val (dfm.protected-temporary);
  format-out ("\n");
  pprint-dfms (dfm.body, #f, depth + 1);
  if (dfm.temporary)
    indent (depth);
    format-out ("#| ");
    pprint-val (dfm.temporary);
    format-out (" := ");
    pprint-val (dfm.protected-temporary);
    format-out (" |#\n")
  end;
  indent (depth); format-out ("CLEANUP:\n");
  pprint-dfms (dfm.cleanups, #f, depth + 1);
  indent (depth); format-out ("END UNWIND-PROTECT\n");
  remove-key! (*loop-namer*, es);
  #f
end;

define method pprint-dfm (dfm :: <end-exit-block>, depth :: <integer>) => ()
  if (dfm.next-computation | dfm.temporary)
    error ("<end-exit-block> with unexpected next or temp")
  end;
  let  be = dfm.entry-state.me-block;
  let  maybe-merge = be.next-computation;
  if (instance? (maybe-merge, <bind-exit-merge>))
    indent (depth);
    pprint-val (maybe-merge.temporary);
    format-out (" #| := ");
    pprint-val (be.temporary);
    format-out (" |# := ");
    pprint-val (maybe-merge.merge-right-value);
    format-out ("\n")
  end
end;

define method pprint-dfm (dfm :: <end-protected-block>, depth :: <integer>) => ()
  if (dfm.next-computation | dfm.temporary)
    error ("<end-protected-block> with unexpected next or temp")
  end
end;

define method pprint-dfm (dfm :: <end-cleanup-block>, depth :: <integer>) => ()
  if (dfm.next-computation | dfm.temporary)
    error ("<end-cleanup-block> with unexpected next or temp")
  end
end;

define method pprint-dfm (dfm :: <temporary-transfer-computation>, depth :: <integer>) => ()
  indent (depth);
  maybe-assign-temp (dfm);
  pprint-val (dfm.computation-value);
  format-out ("\n")
end;

define method pprint-dfm (dfm :: <set!>, depth :: <integer>) => ()
  indent (depth);
  pprint-val (dfm.assigned-binding);
  format-out (" :== ");
  pprint-val (dfm.computation-value);
  format-out ("\n")
end;

define method pprint-dfm (dfm :: <definition>, depth :: <integer>) => ()
  indent (depth);
  pprint-val (dfm.assigned-binding);
  format-out ("  DEFINED-AS  ");
  pprint-val (dfm.computation-value);
  format-out ("\n")
end;

define method pprint-dfm (dfm :: <redefinition>, depth :: <integer>) => ()
  indent (depth);
  pprint-val (dfm.assigned-binding);
  format-out ("  REDEFINED-AS  ");
  pprint-val (dfm.computation-value);
  format-out ("\n")
end;

define method pprint-dfm (dfm :: <exit>, depth :: <integer>) => ()
  let  es = dfm.entry-state;
  indent (depth);
  maybe-assign-temp (dfm);
  format-out ("EXIT (");
  let maybe-merge = es.me-block.next-computation;
  if (instance? (maybe-merge, <bind-exit-merge>))
    format-out ("%s := ", maybe-merge.temporary)
  end;
  pprint-val (dfm.computation-value);
  format-out (") FROM ");
  pprint-val (es);
  format-out (" %s\n", element (*loop-namer*, es, default: out-of-scope:))
end;

define method pprint-dfm (dfm :: <loop>, depth :: <integer>) => ()
  let  loop-tag = size (*loop-namer*);
  *loop-namer* [dfm] := loop-tag;
  indent (depth);
  format-out ("LOOP %s\n", loop-tag);
  for (merge in dfm.loop-merges)
    indent (depth);
    pprint-val (merge.temporary);
    format-out (" := ");
    pprint-val (merge.loop-merge-parameter);
    format-out ("\n")
  end;
  indent (depth);
  format-out ("AGAIN %s\n", loop-tag);
  pprint-dfms (dfm.loop-body, #f, depth + 1);
  indent (depth);
  format-out ("END LOOP %s\n", loop-tag);
  remove-key! (*loop-namer*, dfm);
  #f
end;

define method pprint-dfm (dfm :: <loop-call>, depth :: <integer>) => ()
  let loop = dfm.loop-call-loop;

  for (merge in loop.loop-merges)
    indent (depth);
    pprint-val (merge.temporary);
    format-out (" := ");
    pprint-val (merge.loop-merge-argument);
    format-out ("\n")
  end;

  indent (depth);
  format-out ("GOTO %s\n", element (*loop-namer*, loop, default: out-of-scope:))
end;

define method pprint-dfm (dfm :: <end-loop>, depth :: <integer>) => ()
  let loop = dfm.ending-loop;
  indent (depth);
  format-out ("ESCAPE %s\n", element (*loop-namer*, loop, default: out-of-scope:))
end;

define method pprint-dfm (dfm :: <make-cell>, depth :: <integer>) => ()
  indent (depth);
  maybe-assign-temp (dfm);
  format-out ("make-cell (");
  pprint-val (dfm.computation-value);
  format-out (")\n")
end;

define method pprint-dfm (dfm :: <get-cell-value>, depth :: <integer>) => ()
  indent (depth);
  maybe-assign-temp (dfm);
  format-out ("[ %s ]\n", dfm.computation-cell)
end;

define method pprint-dfm (dfm :: <set-cell-value!>, depth :: <integer>) => ()
  indent (depth);
//  maybe-assign-temp (dfm);  // think this is never used
  format-out ("[ %s ] := ", dfm.computation-cell);
  pprint-val (dfm.computation-value);
  format-out ("\n")
end;

define method pprint-dfm (dfm :: <primitive-call>, depth :: <integer>) => ()
  indent (depth);
  maybe-assign-temp (dfm);
  pprint-val (dfm.primitive);
  format-out (" (");
  pprint-seq (dfm.arguments);
  format-out (")\n")
end;

define method pprint-dfm (dfm :: <values>, depth :: <integer>) => ()
  indent (depth);
  maybe-assign-temp (dfm);
  format-out ("VALUES (");
  pprint-seq (dfm.fixed-values);
  if (dfm.rest-value)
    format-out (", #rest ");
    pprint-val (dfm.rest-value)
  end;
  format-out (")\n")
end;

define method pprint-dfm (dfm :: <simple-call>, depth :: <integer>) => ()
  indent (depth);
  maybe-assign-temp (dfm);
  format-out ("CALL ");
  pprint-val (dfm.function);
  format-out (" (");
  pprint-seq (dfm.arguments);
  format-out (")\n")
end;

define method pprint-dfm (dfm :: <stack-vector>, depth :: <integer>) => ()
  indent (depth);
  maybe-assign-temp (dfm);
  format-out ("VECTOR ");
  format-out (" (");
  pprint-seq (dfm.arguments);
  format-out (")\n")
end;

define method pprint-dfm (dfm :: <return>, depth :: <integer>) => ()
  indent (depth);
  if (dfm.temporary)
    error ("<return> with a temporary")
  end;
  format-out ("RETURN ");
  pprint-val (dfm.computation-value);
  format-out ("\n")
end;

define method pprint-dfm (dfm :: <slot-value>, depth :: <integer>) => ()
  indent (depth);
  maybe-assign-temp (dfm);
  format-out ("SLOT ");
  pprint-val (dfm.computation-slot-descriptor);
  format-out (" (");
  pprint-val (dfm.computation-instance);
  format-out (")\n")
end;

define method pprint-dfm (dfm :: <slot-value-setter>, depth :: <integer>) => ()
  indent (depth);
  maybe-assign-temp (dfm);
  format-out ("SLOT ");
  pprint-val (dfm.computation-slot-descriptor);
  format-out (" (");
  pprint-val (dfm.computation-instance);
  format-out (") := ");
  pprint-val (dfm.computation-new-value);
  format-out ("\n")
end;

define method pprint-dfm (dfm :: <repeated-slot-value>, depth :: <integer>) => ()
  indent (depth);
  maybe-assign-temp (dfm);
  format-out ("SLOT ");
  pprint-val (dfm.computation-slot-descriptor);
  format-out (" (");
  pprint-val (dfm.computation-instance);
  format-out (", %s)\n", dfm.computation-index)
end;

define method pprint-dfm (dfm :: <repeated-slot-value-setter>, depth :: <integer>) => ()
  indent (depth);
  maybe-assign-temp (dfm);
  format-out ("SLOT ");
  pprint-val (dfm.computation-slot-descriptor);
  format-out (" (");
  pprint-val (dfm.computation-instance);
  format-out (", %s) := ", dfm.computation-index);
  pprint-val (dfm.computation-new-value);
  format-out ("\n")
end;

define method pprint-dfm (dfm :: <make-closure>, depth :: <integer>) => ()
  indent (depth);
  maybe-assign-temp (dfm);
  format-out ("MAKE-CLOSURE (");
  pprint-val (dfm.computation-closure-method);
  format-out (")\n")
end;

define method pprint-dfm (dfm :: <initialize-closure>, depth :: <integer>) => ()
  indent (depth);
  if (dfm.temporary)
    error ("<initialize-closure> unexpectedly has a temporary")
  end;
  format-out ("INIT-CLOSURE (");
  pprint-val (dfm.computation-closure-method);
  format-out (", ");
  pprint-val (dfm.computation-closure);
  format-out (")\n")
end;

define method pprint-dfm (dfm :: <extract-single-value>, depth :: <integer>) => ()
  indent (depth);
  maybe-assign-temp (dfm);
  format-out ("EXTRACT %s (", dfm.index);
  pprint-val (dfm.computation-value);
  format-out (")\n")
end;

define method pprint-dfm (dfm :: <extract-rest-value>, depth :: <integer>) => ()
  indent (depth);
  maybe-assign-temp (dfm);
  format-out ("EXTRACT-REST %s (", dfm.index);
  pprint-val (dfm.computation-value);
  format-out (")\n")
end;

define method pprint-dfm (dfm :: <multiple-value-spill>, depth :: <integer>) => ()
  indent (depth);
  format-out ("%s := MULTIPLE-SPILL (", dfm.temporary);
  pprint-val (dfm.computation-value);
  format-out (")\n")
end;
define method pprint-dfm (dfm :: <multiple-value-unspill>, depth :: <integer>) => ()
  indent (depth);
  format-out ("%s := MULTIPLE-UNSPILL (", dfm.temporary);
  pprint-val (dfm.computation-value);
  format-out (")\n")
end;

define method pprint-dfm (dfm :: <check-type>, depth :: <integer>) => ()
  indent (depth);
  format-out ("%s := CHECK-TYPE (", dfm.temporary);
  pprint-val (dfm.type);
  format-out (") ");
  pprint-val (dfm.computation-value);
  format-out ("\n")
end;

define method pprint-dfm (dfm :: <multiple-value-check-type>, depth :: <integer>) => ()
  indent (depth);
  format-out ("%s := MV-CHECK-TYPE (", dfm.temporary);
  pprint-seq (dfm.types);
  format-out (") ");
  pprint-val (dfm.computation-value);
  format-out ("\n")
end;

define method pprint-dfm (dfm :: <multiple-value-check-type-rest>, depth :: <integer>) => ()
  indent (depth);
  format-out ("%s := MV-CHECK-TYPE-REST (", dfm.temporary);
  pprint-seq (dfm.types);
  format-out (") ");
  pprint-val (dfm.computation-value);
  format-out ("\n")
end;



define function pprint-seq (seq :: <sequence>)
  let  n = size (seq);
  if (n > 0)
    pprint-val (seq [0]);
    for (i from 1 below n)
      format-out (", ");
      pprint-val (seq [i]);
    end
  end
end;


define sealed generic pprint-val (obj :: <object>) => ();

define method pprint-val (obj :: <object>) => ()
  format-out ("%s", obj);
  #f
end;

define method pprint-val (obj :: <module-binding>) => ()
  format-out ("{%s}", obj.name);
  #f
end;

define method pprint-val (obj :: <lexical-variable>) => ()
  format-out ("%s", obj.name.fragment-name);
  #f
end;

define method pprint-val (obj :: <&primitive>) => ()
  format-out ("%s", obj.model-definition.form-variable-name-or-names.fragment-name);
  #f
end;

define method pprint-val (obj :: <variable-reference>) => ()
  format-out ("<varref %s>", obj.referenced-binding);
  #f
end;

define method pprint-val (obj :: <entry-state>) => ()
  let  nm = obj.name;
  format-out ("%s", (nm & (nm.fragment-name)));
  #f
end;

define method pprint-val (obj :: <binding-reference>) => ()
  format-out ("%s", obj.referenced-binding.name);
  #f
end;

define method pprint-val (obj :: <&generic-function>) => ()
  format-out ("``%s''", obj.^debug-name);
  #f
end;

define method pprint-val (obj :: <&method>) => ()
  format-out ("`%s'", obj.^debug-name);
  #f
end;

define method pprint-val (obj :: <object-reference>) => ()
  format-out ("^");
  pprint-val (obj.reference-value)
end;

define method pprint-val (obj :: <symbol>) => ()
  if (obj)
    format-out ("%s:", obj)
  else   // emulator hack
    format-out ("#()")
  end;
  #f
end;

define method pprint-val (obj :: <&class>) => ()
  format-out ("%s", obj.^debug-name);
  #f
end;

