Module: dfmc-java-linker
Author: Mark Tillotson
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


//// TOP-LEVEL

define variable *ld-hash* = make (<object-table>);

define method emit-mainfile
  (back-end :: <java-back-end>, library, #rest flags, #key)
  format-out ("// emit-mainfile for %s, doing nothing...\n", library)
end;

define method emit-gluefile
  (back-end :: <java-back-end>, library, cr-names, #rest flags, #key)
  *ld-hash* := make (<object-table>);
  format-out ("// emit-gluefile for %s, clearing *ld-hash*...\n", library)
end;


//define function java-output-basename (basename :: <string>)
//  basename
//end;

/*
define method emit-record (back-end :: <java-back-end>, cr :: <compilation-record>)
  with-build-area-output (stream = compilation-record-library (cr),
                        base: java-output-basename (compilation-record-name (cr)),
                        type: "class")
    // this should really spurge all the classes into a zip archive!
    link-all (back-end, stream, cr)
  end
end;
*/

// DRIVER PROTOCOL FUNCTIONS
define method emit-library-records
    (back-end :: <java-back-end>, ld :: <library-description>, #key)
format-out ("### entering emit-library-records\n");
  with-simple-abort-retry-restart ("Abort the emission phase", "Restart the emission phase")
    format-out ("//// emit-library-records on %S, mode=%s\n",
                ld,
                ld.library-description-compilation-mode);
    dynamic-bind (*current-be-library* = ld.language-definition)
      let  a-base = #f;
      for (cr in ld.library-description-compilation-records)
        if (compilation-record-needs-linking?(cr))
          a-base := cr.compilation-record-name;
          compilation-record-needs-linking?(cr) := #f;
        end
      end;

      with-build-area-output (stream = ld, base: a-base, type: "java")
        progress-line("Java Linking %s", a-base);
        if (*current-module-java-class*)
          emit-java-class-for-library (*current-module-java-class*);

          flush-java-classes ();
          *current-module-java-class* := #f;
          finish-with-unique-strings ();
        else
          format-out ("// WHOOPS, no module-class to output in link phase\n")
        end
      end
    end dynamic-bind
  end;
format-out ("### leaving emit-library-records\n");
end;


// DRIVER PROTOCOL FUNCTIONS
define method emit-library-record
    (back-end :: <java-back-end>, cr :: <compilation-record>, ld :: <library-description>,  #rest flags, #key)
  if (element (*ld-hash*, ld, default: #f) == #f)
    *ld-hash* [ld] := ld;
    format-out ("//// emit-library-record on %s, %s, mode=%s\n",
                cr,
                ld,
                ld.library-description-compilation-mode);
    apply (emit-library-records, back-end, ld, flags)
  end
end;

/*
define method OLD-emit-library-records
    (back-end :: <java-back-end>, ld :: <library-description>, #rest flags)
  for (cr in library-description-compilation-records(ld))
    if (compilation-record-needs-linking?(cr))
      with-dependent ($compilation of cr)
        begin
          with-build-area-output (stream = ld, base: compilation-record-name(cr), type: "java")
            let name = cr.compilation-record-source-record.source-record-name;
            progress-line("Java Linking %s.dylan", name);
            link-all (back-end, stream, cr);
          end
        end
      end;
      compilation-record-needs-linking?(cr) := #f;
    end
  end
end method;


define function OLDlink-all (back-end :: <java-back-end>, stream :: <stream>, cr :: <compilation-record>)
  with-simple-abort-retry-restart
      ("Abort the emission phase", "Restart the emission phase")
    let heap = cr.compilation-record-model-heap;
    dynamic-bind (*current-be-library* = cr.compilation-record-library.language-definition)
      format-out ("// Java linking library %s\n", *current-be-library*.debug-name);
      emit-java-class-for-library (cr, heap.heap-defined-bindings, heap.heap-root-init-code);
    end dynamic-bind
  end
end;
*/


// eof

/*

define sealed method foo! (l :: <list>) => l :: <list>;
  let result    = #();
  let remaining = l;

  until ( remaining == #() )
    let t = tail (remaining);
    tail (remaining) := result;
    result           := remaining;
    remaining        := t;
  end until;

  result;
end;

define variable foo-list = make (<list>, size: 1000, fill: 42);

define function testy-reverse ()
  for (n from 0 to 1000)
    foo-list := foo-list.reverse!
  end
end;

define function testy-my-reverse ()
  for (n from 0 to 1000)
    foo-list := foo-list.foo!
  end
end;



define class <veclet-queue> (<stretchy-sequence>, <mutable-sequence>)
  slot start-vec :: <simple-object-vector>;
  slot end-vec   :: <simple-object-vector>;
  slot start  :: <integer>, init-value: 0;
  slot endy   :: <integer>, init-value: 0;
  slot spare-vec :: <simple-object-vector>;
end;

define constant $veclet-power-of-two$ = 4;
define constant $veclet-prev$ = 16;
define constant $veclet-next$ = 17;
define constant $veclet-size$ = 18;

define method initialize (this :: <veclet-queue>, #key size = 0, fill)
  next-method();
  let  init-vec = make(<simple-object-vector>, size: $veclet-size$);
  this.start-vec := init-vec;
  this.end-vec   := init-vec;
  this.spare-vec := init-vec;    // if (spare == start), its not spare!
  if (size > 0)
    for (n from 0 to size)
      push-last (this, fill)
    end
  end
end;


define sealed method size (this :: <veclet-queue>) => (n :: <integer>)
  let  s = this.start;
  let  e = this.endy;
  e - s
end;

define sealed method push (this :: <veclet-queue>, new-element) => (this :: <deque>)
  let  s = this.start - 1;
  let  e = this.endy;
  let  ind = logand (s, 15);
  if ((ind == 15) & (1 ~== (e - s)))
    let  startv  = this.start-vec;
    let  new-vec = this.spare-vec;
    if (startv == new-vec)
      new-vec := make (<simple-object-vector>, size: $veclet-size$);
      this.spare-vec := new-vec
    end;
    startv[$veclet-prev$] := new-vec;
    new-vec[$veclet-next$] := startv;
    this.start-vec := new-vec;
    new-vec[ind] := new-element
  else
    this.start-vec[ind] := new-element
  end;
  this.start := s;
  this
end;


define sealed method push-last (this :: <veclet-queue>, new-element) => (this :: <deque>)
  let  s = this.start;
  let  e = this.endy;
  let  ind = logand (e, 15);
  if ((ind == 0) & (s ~== e))
    let  endv  = this.end-vec;
    let  new-vec = this.spare-vec;
    let  startv = this.start-vec;
    if (startv == new-vec)
      new-vec := make (<simple-object-vector>, size: $veclet-size$)
    else
      this.spare-vec := startv  // mark spare-vec as not spare
    end;
    endv   [$veclet-next$] := new-vec;
    new-vec[$veclet-prev$] := endv;
    this.end-vec := new-vec;
    new-vec[ind] := new-element
  else
    this.end-vec[ind] := new-element
  end;
  this.endy := e + 1;
  this
end;


define sealed method pop (this :: <veclet-queue>) => (object)
  let  s = this.start;
  let  e = this.endy;
  if (s == e)
    error ("POP empty deque %=", this)
  end;
  let  ind = logand (s, 15);
  let  startv = this.start-vec;
  let  val = startv[ind];
  startv[ind] := #f;
  s := s + 1;
  if ((ind == 15) & (e ~== s))
    let  new-start = startv[$veclet-next$];
    new-start[$veclet-prev$] := #f;
    startv[$veclet-next$] := #f;
    this.spare-vec := startv;  // recycle it
    this.start-vec := new-start
  end;
  this.start := s;
  val
end;

define sealed method pop-last (this :: <veclet-queue>) => (object)
  let  s = this.start;
  let  e = this.endy;
  if (s == e)
    error ("POP empty deque %=", this)
  end;
  e := e - 1;
  let  ind = logand (e, 15);
  let  endv = this.end-vec;
  let  val = endv[ind];
  endv[ind] := #f;
  if ((ind == 0) & (e ~== s))
    let  new-end = endv[$veclet-prev$];
    new-end[$veclet-next$] := #f;
    endv[$veclet-prev$] := #f;
    this.spare-vec := endv;  // recycle it
    this.end-vec := new-end
  end;
  this.endy := e;
  val
end;


define function deq-test ()
  let  d = make (<veclet-queue>);
  push-last (d, 0);
  push-last (d, 1);
  push-last (d, 2);
  push-last (d, 3);
  push-last (d, 4);
  push-last (d, 5);
  push-last (d, 6);
  push-last (d, 7);
  push-last (d, 8);
  push-last (d, 9);
  push-last (d, 10);
  push-last (d, 11);
  push-last (d, 12);
  push-last (d, 13);
  push-last (d, 14);
  push-last (d, 15);
  push-last (d, 16);
  push-last (d, 17);
  push-last (d, 18);
  push-last (d, 19);
  push-last (d, 20);
  push-last (d, 21);

  for (n from 0 to 100000)
    pop-last (d);
    pop-last (d);
    pop-last (d);
    pop-last (d);
    pop-last (d);

    push (d, 111);
    push (d, 222);
    push (d, 333);
    push (d, 444);
    push (d, 555);
  end;
  d
end;


define function old-deq-test ()
  let  d = make (<deque>);
  push-last (d, 0);
  push-last (d, 1);
  push-last (d, 2);
  push-last (d, 3);
  push-last (d, 4);
  push-last (d, 5);
  push-last (d, 6);
  push-last (d, 7);
  push-last (d, 8);
  push-last (d, 9);
  push-last (d, 10);
  push-last (d, 11);
  push-last (d, 12);
  push-last (d, 13);
  push-last (d, 14);
  push-last (d, 15);
  push-last (d, 16);
  push-last (d, 17);
  push-last (d, 18);
  push-last (d, 19);
  push-last (d, 20);
  push-last (d, 21);

  for (n from 0 to 100000)
    pop-last (d);
    pop-last (d);
    pop-last (d);
    pop-last (d);
    pop-last (d);

    push (d, 111);
    push (d, 222);
    push (d, 333);
    push (d, 444);
    push (d, 555);
  end;
  d
end;



define method zippy (l :: <list>) => (<list>)
  let  res :: <list> = #();
  for (el in l)
    if (el)
      res := pair (el, pair (el, res))
    end
  end;
  res
end;
*/
