Module: dfmc-java-back-end
Author: Mark Tillotson
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



define class <java-backend-concrete-class-mixin> (<object>)
  sealed slot symbol-slots-list :: <list> = #();  // pairs of string/slot-spec
  sealed slot been-inited? :: <boolean> = #f;
  sealed constant slot library, required-init-keyword: library:;
  sealed slot ep-seqnum :: <integer> = 0;
  sealed slot iep-emitted? = #f;
  sealed slot mep-emitted? = #f;
  sealed slot xep-emitted? = #f;
end;

define class <java-backend-concrete-class-info> (<java-concrete-class-info>, <java-backend-concrete-class-mixin>)
end;

define class <java-backend-concrete-interface-info> (<java-concrete-interface-info>, <java-backend-concrete-class-mixin>)
end;

define generic backend-concrete-class-for (jcls :: <java-class-or-interface>) => (cls :: <class>);

define method backend-concrete-class-for (jcls :: <java-class>) => (cls :: <class>)
  <java-backend-concrete-class-info>
end;
define method backend-concrete-class-for (jcls :: <java-interface>) => (cls :: <class>)
  <java-backend-concrete-interface-info>
end;

define function ensure-backend-class-concrete (cls) => (cls)
  ensure-class-concrete (cls, class-for: backend-concrete-class-for)
end;


define variable *current-library-java-class* = #f;
define variable *current-library-jar* = #f;

/*
define method compute-operating-system-name
    (back-end :: <java-back-end>) => (name :: <symbol>)
  #"Java"
end method;

define method compute-processor-name
    (back-end :: <java-back-end>) => (name :: <symbol>)
  #"JVM"
end method;
*/

define method back-end-word-size (be :: <java-back-end>) => (ws :: <integer>)
  4   // rather arbitrary really, but related to Java's int size (and hence array index)
end;


//// TOP-LEVEL



define thread variable *current-be-library* :: false-or (<full-library>) = #f;
define thread variable *environment-bindings* :: false-or (<object-table>) = #f;
define thread variable *java-class-cache*   :: false-or (<object-table>) = #f;
define thread variable *gensym*             :: false-or (<integer>)      = #f;
define thread variable *closure-env-lookup* :: false-or (<object-table>) = #f;
define variable *recorded-generalized*      :: false-or (<object-table>) = #f;
define variable *recorded-refs*             :: false-or (<object-table>) = #f;

// this one is persistent so that link-phase can see it -
// the control-flow is hacked because we only get a compilation-record
// at a time and don't know when we're supposed to finish the library.

define variable *current-module-java-class* :: false-or (<java-concrete-class>) = #f;


define variable *anonymous-methods* :: <list> = #();
define variable *seen-methods* :: <object-table> = make (<object-table>);

define function maybe-queue-method (meth :: <&method>)
  if (~ element (*seen-methods*, meth, default: #f))
format-out ("maybe-queue-method queueing %s\n", meth);
    *anonymous-methods* := pair (meth, *anonymous-methods*);
    *seen-methods* [meth] := #t
  end
end;


define variable *debug-comp-record* = #f;

define method emit-all (back-end :: <java-back-end>, cr :: <compilation-record>, #rest flags)
  unless (*recorded-generalized*)
    *recorded-generalized* := make (<object-table>)
  end;
  unless (*recorded-refs*)
    *recorded-refs* := make (<object-table>)
  end;
  if (*debug-comp-record*)
    my-break (cr)
  end;

  with-simple-abort-retry-restart ("Abort the emission phase", "Restart the emission phase")
    let heap = cr.compilation-record-model-heap;
    clear-pending-java-classes ();

    dynamic-bind (*current-be-library* = cr.compilation-record-library.language-definition)
      format-out ("// current be library is a %s\n", *current-be-library*.object-class);
      // need to mangle name
      let  name = as-lowercase (as (<byte-string>, *current-be-library*.debug-name));
      unless (*current-library-jar* &
              *current-library-jar*.jar-library == *current-be-library*)

        if (*current-library-jar*)
          force-out-java-classes ()
        end;

        *current-library-jar* :=
          make (<jar-file-rep>,
                jar-library: *current-be-library*,
                jar-name: name,
                jar-stream:  open-output-stream (*java-back-end*, concatenate (name, ".jar")),
                jar-comment: concatenate ("Functional Developer generated JAR file for ", name)
                );
      end;

      unless (*java-class-cache*)
        *java-class-cache* := make (<object-table>)
      end;

      dynamic-bind (*gensym* = 0)
        if (*current-module-java-class*)
          *java-class-cache* [*current-be-library*] := *current-module-java-class*
        else
          start-with-unique-strings ();
          *current-module-java-class* := java-class-for-thing (*current-be-library*);
          // prevent it being emitted too early
          java-unemit-class (*current-module-java-class*)
        end;

        dump-static-objects (cr);

        // ensure all definitions have their slots created in the library
        for (foo in heap.heap-defined-bindings)
          format-out ("\n// heap-defined-binding %s\n", foo);
          if (instance? (foo, <module-binding> /*<canonical-module-binding>*/ ))
            // problem, this next line only works for tight mode.
            let  val = foo.binding-value-slot;

            // loose mode is not very consistent with use of binding-level modeling,
            // something called private-shadowable-binding seems to be involved

            if (val)
              if (instance? (val, <&generic-function>))
                format-out ("// Java CG <&generic-function>: %s\n\n", val);
                dynamic-bind (*environment-bindings* = make (<object-table>),
                              *closure-env-lookup* = make (<object-table>))
                  // also want to output initialization for GF
                  // format-out ("*** need to initialize GF %s\n", val);
                  emit-java-code-for (back-end, val);

                  // need to step *anonymous-methods* as we go, for-loop not enough
                  while (~ empty? (*anonymous-methods*))
                    let  anon :: <&method> = *anonymous-methods*.head;
                    format-out ("** processing an anon method, %s\n", anon);
                    *anonymous-methods* := *anonymous-methods*.tail;
                    emit-java-code-for (back-end, anon)
                  end
                end dynamic-bind
              end;

              if (instance? (val, <&method>))
                format-out ("// Java CG <&method>: %s\n\n", val);
                dynamic-bind (*environment-bindings* = make (<object-table>),
                              *closure-env-lookup* = make (<object-table>))
                  // also want to output initialization for GF
                  // format-out ("*** need to initialize bare method %s\n", val);
                  *seen-methods* [val] := #t;
                  emit-java-code-for (back-end, val);

                  format-out ("** about to scan %d anons\n", size (*anonymous-methods*));
                  while (~ empty? (*anonymous-methods*))
                    let  anon :: <&method> = *anonymous-methods*.head;
                    format-out ("** processing an anon method, %s\n", anon);
                    *anonymous-methods* := *anonymous-methods*.tail;
                    emit-java-code-for (back-end, anon)
                  end
                end dynamic-bind
              end;

              if (instance? (val, <&class>))
                format-out ("// processing <&class>: %s\n", val);
                emit-java-code-for (back-end, val)
              end
            end
          end;
          format-out ("// processing java-rep of %s\n", foo);
          java-rep (foo)
        end;

        dynamic-bind (*environment-bindings* = make (<object-table>),
                      *closure-env-lookup* = make (<object-table>))
          emit-class-init (*current-module-java-class*,
                           heap.heap-defined-bindings,
                           heap.heap-root-init-code);

          let anon-count = *anonymous-methods*.size;
          if (anon-count > 0)
            format-out ("// Java CG %d anonymous methods\n", anon-count);
            while (~ empty? (*anonymous-methods*))
              let  anon :: <&method> = *anonymous-methods*.head;
              format-out ("** processing an anon method, %s\n", anon);
              *anonymous-methods* := *anonymous-methods*.tail;
              emit-java-code-for (back-end, anon)
            end
          end
        end dynamic-bind;

        format-out ("### about to flush in emit-all\n");
        flush-java-classes ();
        format-out ("### flushed in emit-all\n");

        if (*current-module-java-class*)
//            emit-java-class-for-library (*current-module-java-class*);
//            *current-module-java-class* := #f;
          flush-java-classes ()
        end
      end dynamic-bind
    end dynamic-bind
  end
end;



// called from linker.

define function emit-java-class-for-library (jc :: <java-concrete-class>)
  let  concrete = jc.concrete-implementation;
  if (*recorded-generalized*)
    format-out ("// external methods generalized:\n");
    for (name in *recorded-generalized*.key-sequence)
      format-out ("  %s", name)
    end;
    format-out ("\n");
    *recorded-generalized* := #f
  end;
  if (*recorded-refs*)
    format-out ("// external GFs referenced:\n");
    for (name in *recorded-refs*.key-sequence)
      format-out ("  %s", name)
    end;
    format-out ("\n");
    *recorded-refs* := #f
  end;
  format-out ("// java emitting %s\n", jc.class-name);
  unless (*java-class-cache*)
    *java-class-cache* := make (<object-table>)
  end;
  *java-class-cache*[*current-be-library*] := jc;
  format-out ("emit-java-class-for-library, current library is a %s\n", *current-be-library*.object-class);
  end-emit-class-init (jc);
  if (*add-a-main*)  // purely for bytecode verification debugging!
    add-a-main-method (jc, "(library)")
  end;

  format-out ("*** Number of slots & methods in library class is %d %d\n",
              concrete.slots.size, concrete.methods.size);
  java-emit-class (jc);  // this ensures the library class gets registered for output
end;


define function record-generalized-external-reference (name)
  if (*recorded-generalized*)
    *recorded-generalized*[name] := #t
  end
end;

define function record-external-reference (name)
  if (*recorded-refs*)
    *recorded-refs*[name] := #t
  end
end;

define function finalize-java-linking ()
  format-out ("finalize-java-linking...\n");
  *current-be-library*         := #f;
  *current-library-java-class* := #f;
  *current-library-jar*        := #f;
  *environment-bindings*       := #f;
  *java-class-cache*           := #f;
  *gensym*                     := #f;
  *closure-env-lookup*         := #f;
  *current-module-java-class*  := #f;
  *anonymous-methods*          := #();
  remove-all-keys! (*model-cache*);
  remove-all-keys! (*seen-methods*);
end;


define method open-output-stream
  (back-end :: <java-back-end>, file-name :: <byte-string>) => (stream :: <stream>)
  make (<file-stream>,
        locator:   as (<file-locator>, file-name),
        direction: #"output");
end;

// UI hack to clean up
define function force-out-java-classes () => ()
  if (*current-library-jar*)
    if (*current-module-java-class*)
      end-emit-class-init (*current-module-java-class*);
      java-emit-class (*current-module-java-class*);
      *current-module-java-class* := #f
    end;
    flush-java-classes ();
    jar-close (*current-library-jar*);
    *current-library-jar* := #f
  end;
  *java-class-cache* := #f
end;
