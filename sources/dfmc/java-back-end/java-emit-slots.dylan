Module: dfmc-java-back-end
Author: Mark Tillotson
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define function examine (obj)
  break();
  if (obj == 4)
    break ()
  end;
end;



define function java-getter-and-setter
     (sd :: <&slot-descriptor>,
      #key
      must-have-writer? = #f,
      repeated? = #f) =>
     (kind :: <symbol>,
      slot-type-class :: <java-class>,
      reader-spec,
      writer-spec)
  let  class = sd.^slot-owner;        // owning Dylan class
  let  target-class = class.java-class-for-thing;  // owning Java class
  let  slot-type-class = sd.^slot-type.java-class-for-thing;   // Java class for slot's type
  if (repeated?)
    slot-type-class := array-type (slot-type-class)
  end;
  let  slot-name = sd.^debug-name.java-name-mangle;            // a java name for slot
  if (class.^class-primary?)
    values (#"direct",
            slot-type-class,
            slot-spec (target-class, slot-name, slot-type-class, #f))
  elseif (must-have-writer?)
    let  java-interface = find-slot-java-interface (target-class);  // creates one if necessary
    values (#"interface",
            slot-type-class,
            meth-spec (java-interface, slot-name, meth-type (slot-type-class), j-invokeinterface),
            meth-spec (java-interface, slot-name, meth-type ($java-void-type$, slot-type-class), j-invokeinterface))
  else
    let  java-interface = find-slot-java-interface (target-class);  // creates one if necessary
    values (#"interface",
            slot-type-class,
            sd.^slot-getter & meth-spec (java-interface, slot-name, meth-type (slot-type-class), j-invokeinterface),
            sd.^slot-setter & meth-spec (java-interface, slot-name, meth-type ($java-void-type$, slot-type-class), j-invokeinterface))
  end
end;


// this cache needs flushing per library, NOTE
define variable *slot-interface-map* = make (<object-table>);

define function find-slot-java-interface (jc :: <java-class>)
  element (*slot-interface-map*, jc, default: #f) |
    begin
      if (instance? (jc, <java-concrete-class>) |
          instance? (jc, <java-stub-class>))
        let  cls = jc.represents;
        let  model-library = cls.model-library.language-definition;
        let  clsname = jc.java-class-name.the-string;
        let  libname = model-library.java-class-for-thing.java-class-name.the-string;
        let  libpackage = java-package (copy-sequence (libname, end: (libname.size - "-library".size)));
        let  cls =
          if (instance? (jc, <java-concrete-class>))
            make (<java-concrete-interface>,
                  library:     model-library,
                  class-name:  new-invented-name ("JifJ", clsname),
                  package:     libpackage)
          else
            make (<java-stub-interface>,
                  library:    model-library,
                  class-name: new-invented-name ("JifJ", clsname),
                  package:    libpackage)
          end;
        *slot-interface-map* [jc] := cls;
        java-emit-class (cls);
        cls
      else
        error ("funny arg to find-slot-java-interface")
      end
    end
end;


define function create-getter-method-body (meth :: <java-method>, sd :: <&slot-descriptor>)
  let  (slot-kind, slot-type-class, reader-spec) = java-getter-and-setter (sd);
  let  jbb = make-jbb (meth);
  begin
    if (slot-kind == #"direct")
      emit-push-local (jbb, 1, j-ref-code);
      java-op2 (jbb, j-checkcast, slot-type-class);  // should be dylan's instance?, MI
      java-read (jbb, reader-spec)
    elseif (slot-kind == #"interface")
      emit-push-local (jbb, 1, j-ref-code);
      java-op2 (jbb, j-checkcast, slot-type-class);  // should be dylan's instance?, MI
      java-call (jbb, reader-spec)
    else
      error ("problem with slot getter method spec")
    end;
    emit-return (jbb, j-ref-code);
  end;
  finish-with-jbb (jbb, meth)
end;


define function create-setter-method-body (meth :: <java-method>, sd :: <&slot-descriptor>)
  let  (slot-kind, slot-type-class, reader-spec, writer-spec) = java-getter-and-setter (sd);
  let  jbb = make-jbb (meth);
  begin
    if (slot-kind == #"direct")
      emit-push-local (jbb, 2, j-ref-code);
      java-op2 (jbb, j-checkcast, slot-type-class);  // should be dylan's instance?, MI

      emit-push-local (jbb, 1, j-ref-code);  // the new value
      java-write (jbb, reader-spec) // for direct, reader-spec is the slot-spec itself
    elseif (slot-kind == #"interface")
      emit-push-local (jbb, 2, j-ref-code);
      java-op2 (jbb, j-checkcast, slot-type-class);  // should be dylan's instance?, MI

      emit-push-local (jbb, 1, j-ref-code);  // the new value
      java-call (jbb, writer-spec)
    else
      error ("problem with slot getter method spec")
    end;
    emit-push-local (jbb, 1, j-ref-code);
    emit-return (jbb, j-ref-code);
  end;
  finish-with-jbb (jbb, meth)
end;






define function java-deal-with-inheritance (jc :: <java-concrete-class>)
  let  cls = jc.represents;

  let  supers = cls.^direct-superclasses;

  add-class-protocol-init (jc, map (java-class-for-thing, supers), #f);

  let  java-inherited-classes = #();
  while (supers.size > 0)
    let  sup0 = supers[0];
    java-inherited-classes := pair (sup0, java-inherited-classes);
    supers := sup0.^direct-superclasses;
  end;

  supers := cls.^direct-superclasses;

  let  other-inherited-classes = #();
  local method find-others (a-class)
          unless (member? (a-class, java-inherited-classes))
            unless (member? (a-class, other-inherited-classes))
              other-inherited-classes := pair (a-class, other-inherited-classes);
              for (a-super in a-class.^direct-superclasses)
                find-others (a-super)
              end
            end
          end
        end;
  for (a-super in supers)
    find-others (a-super)
  end;

  // now have a list of the classes we inherit from in the java world,
  // and a list of the other superclasses.
  format-out ("$$$ for class %s, have %d java-primaries and %d other supers\n",
              cls, java-inherited-classes.size, other-inherited-classes.size);


//  for (a-class in other-inherited-classes)
  if (supers.size > 0)
    for (a-class in cls.^all-superclasses)
      unless (a-class == cls | member? (a-class, supers[0].^all-superclasses))
        let  slots = a-class.^direct-slot-descriptors;
        if (slots.size > 0)
          /*
          let  needs-interface? :: <boolean> = #t;
          for (p-class in java-inherited-classes)
            if (^subtype? (p-class, a-class))
              needs-interface? := #f
            end
          end;
          */
          let  inter = #f;
          for (slot in slots)
            let  (slot-kind, slot-type-class, reader-spec, writer-spec) = java-getter-and-setter (slot);
            if (slot-kind == #"interface")
              inter :=  reader-spec.java-class;
              // generate the implementations
              generate-interface-slot-implementation (jc, slot-type-class, reader-spec, writer-spec)
            end
          end;
          if (inter)
            mark-as-implementing (jc, inter)
          end;
        end
      end
    end
  end;

  let  slots = cls.^direct-slot-descriptors;
  if (slots.size > 0)
    // have to implement our own slots
    if (cls.^class-primary?)
    format-out ("$$$ dealing with %d direct slots\n", slots.size);

      // just add java slots
      for (slot in slots)
        let (slot-kind, slot-type-class, slot-spec) = java-getter-and-setter (slot);
        unless (slot-kind == #"direct")
          error ("jbe slot access mismatch")
        end;
        java-field (slot-spec)
      end
    else
      format-out ("$$$ dealing with %d interface slots\n", slots.size);

      let  the-if = find-slot-java-interface (jc);
      mark-as-implementing (jc, the-if);
      for (slot in slots)
        let  (slot-kind, slot-type-class, reader-spec, writer-spec) = java-getter-and-setter (slot);
        unless (slot-kind == #"interface")
          error ("jbe slot access mismatch")
        end;
        java-interface-method (reader-spec);
        if (writer-spec)
          java-interface-method (writer-spec)
        end;

        // generate the implementations
        generate-interface-slot-implementation (jc, slot-type-class, reader-spec, writer-spec)
      end
    end
  end
end;


define function generate-interface-slot-implementation
  (jc :: <java-concrete-class>,
   slot-type-class :: <java-class>,
   reader-spec :: <java-method-spec>,
   writer-spec :: false-or (<java-method-spec>)) => ()

  let  name = reader-spec.slot-name;
  let  impl-slot = slot-spec (jc, name, slot-type-class, #f);

  // force the slot to exist
  java-field (impl-slot);

  begin
    let  meth = java-method (meth-spec (jc, reader-spec.slot-name, reader-spec.slot-type, j-invokevirtual));
    let  jbb = make-jbb (meth);
    emit-push-this (jbb);
    java-read (jbb, impl-slot);
    emit-return (jbb, j-ref-code);
    finish-with-jbb (jbb, meth)
  end;

  if (writer-spec)
    let  meth = java-method (meth-spec (jc, writer-spec.slot-name, writer-spec.slot-type, j-invokevirtual));
    let  jbb = make-jbb (meth);
    emit-push-this (jbb);
    emit-push-local (jbb, 1, j-ref-code);
    java-write (jbb, impl-slot);
    emit-return (jbb, j-void-code);
    finish-with-jbb (jbb, meth)
  end
end;



define function bare-read-model-slot (jbb :: <java-basic-block>, slotd :: <&slot-descriptor>, repeated? :: <boolean>) => ()
  let (slot-kind, slot-type-class, reader-spec) =
    java-getter-and-setter (slotd, must-have-writer?: #t, repeated?: repeated?);
  if (slot-kind == #"interface")
    java-if-call (jbb, reader-spec);
  else
    java-read (jbb, reader-spec);
  end
end;

define function bare-write-model-slot (jbb :: <java-basic-block>, slotd :: <&slot-descriptor>, repeated? :: <boolean>) => ()
  let (slot-kind, slot-type-class, reader-spec, writer-spec) =
    java-getter-and-setter (slotd, must-have-writer?: #t, repeated?: repeated?);
  if (slot-kind == #"interface")
    java-if-call (jbb, writer-spec);
  else
    java-write (jbb, reader-spec);
  end
end;


define function dylan-read-model-slot (jbb :: <java-basic-block>, slotd :: <&slot-descriptor>, rep? :: <boolean>) => ()
  let (slot-kind, slot-type-class, reader-spec) =
    java-getter-and-setter (slotd, repeated?: rep?);
  if (slot-kind == #"interface")
    java-if-call (jbb, reader-spec);
  else
    java-read (jbb, reader-spec);
  end
end;

define function dylan-write-model-slot (jbb :: <java-basic-block>, slotd :: <&slot-descriptor>, rep? :: <boolean>) => ()
  let (slot-kind, slot-type-class, reader-spec, writer-spec) =
    java-getter-and-setter (slotd, repeated?: rep?);
  if (slot-kind == #"interface")
    java-if-call (jbb, writer-spec);
  else
    java-write (jbb, reader-spec);
  end
end;
