Module: dfmc-c-ffi
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// !@#$ todo: pointer-value, and pointer-value-setter methods
//


/*

  define c-pointer-type <foo*> => <foo>;

==>  // if <foo> was not defined in the current compilation context

  // the superclass below may be a subclass of <C-pointer-to-pointer>
  // depending on the class of <foo>
  define abstract open class temp-class-for-<foo*>
      (<C-statically-typed-pointer>)
    metaclass <define-C-pointer-metaclass>, referenced-type: <foo>;
  end;

  define sealed concrete class temp-class-for-concrete-<foo*>
      (temp-class-for-<foo*>)
    metaclass <define-C-pointer-metaclass>, referenced-type: <foo>;
  end;

  define constant temp-make-method-for-<foo*>
    = method (x == temp-class-for-<foo*>, #next n, #rest keys, #key)
        apply(make, temp-class-for-concrete-<foo*>, keys);
      end;

  // more for pointer-value, pointer-value-setter if necessary

  define constant <foo*>
    = %abstract-pointer-type(<foo>)
        | temp-class-for-<foo*>;

==> // if <foo> was defined in the current compilation context, but there
    // was no pointer type for it known at compile time

  // the superclass below may be a subclass of <C-pointer-to-pointer>
  // depending on the class of <foo>
  define abstract open class <foo*> (<C-statically-typed-pointer>)
    metaclass <C-automatic-pointer-definition>, referenced-type: <foo>;
  end class;

  define concrete sealed class instantiation-of-<foo*> (<foo*>)
    metaclass <C-automatic-pointer-definition>, referenced-type: <foo>;
  end class;

  define method pointer-value (p :: <foo*>, #key index = 0)
   => (v :: <mapped-class-for-<foo>>)
      // if <foo> has no low level type then



// most general make method for C pointer classes
define method make (c :: subclass(<c-pointer>), #rest make-keys, #key address)
  if (abstract?(c))
    // delegate abstract types
    apply(make, instantiation-class(c), make-keys)
  else
    // allocate an initialized pointer
    let p = allocate-instance(c);
    if (address)
      p.%address := address;
    else
      p.%address := apply(default-allocator(c), make-keys);
    end if;
    // call initialize;
    apply(initialize, p, make-keys);
    p
  end if;
end method;

//// define if-needed model

I need to be able to define a method in the compiler, but only actually
produce a model for it if the compiler cannot prove it doesn't exist,
which for my cases will probably never exists,

If the compiler can prove the thing is already defined, or my predicate
(that can be run at model building time) returns false:
  Do not define anything new, perhaps bind a name to the existing model.



*/




//// weak definitions
// We produce 2 separate definitions for each weak definition.
// The temporary definition defines the object strongly so that there is an
// object to refer to when initializing a weakly defined variable.
// The second definition is for the variable that is actually weakly
// defined.

///  The generalized runtime initialization code for a weak definition looks
///  something like this:
//     begin
//       let existing-def = look-for-existing-definition()
//       if (existing-def)
//         set-weak-var-from(existing-def)
//       else
//         set-weak-var-from(temp-def)
//       end if
//    end


///  The runtime initialization code for a weak pointer class definition looks
///  something like this:
//     begin
//       let existing-def = %pointer-type(ref-type)
//       if (existing-def)
//         <pointer-class> := existing-def;
//       else
//         %pointer-type(ref-type) := <pointer-class>;
//         <pointer-class> := temp-class-for-<pointer-class>;
//       end if
//    end

///  The runtime initialization code for a method definition looks
///  something like this:
//       if (find-method(make, list(singleton(<pointer-class>))))
//         // do nothing
//       else
//         add-method(make, temp-name-for-make-method)
//       end if

/// so for each weakly defined object we have 2 top-level definitions:
// The temp definition, and the weak definition.
// When expanding a temp definition in the compiler we first check to see if
// a definition already exists in the compilation context.  If not
// then we generate an ordinary definition for the temp name.  If there is
// a definition already then we can just use that directly for the temp name.

// When expanding the definition for the weak variable itself we
// look for a strong definition in the compilation context, and use it if
// it's available.  If not then if there is another weak definition we
// arrange for the initialization code to use the other weak definition
// if applicable, or (as in the case of methods), do nothing.  If there is
// no definition then we must use initialization code as illustrated above.



/*

weak definition model:
   May be available without any work on some platforms.

   Variable acquires extra indirection property.
     Every reference to the variable goes through an extra indirection so
     that it can live in a different library

   The definition expansion must provide initialization code for the
     variable, plus create the model for the object.
   The initialization code must get a handle on the object for the model
   and be able to set the global variable to the correct value.  In the
   code above It is in the form of an anonymous method that gets called
   with the runtime object created from the model, and

*/


define &macro c-pointer-type-definer
  { define c-pointer-type ?:name => ?referenced-type:expression }
  => unless-ffi-definition-dynamic (form)
       do-define-c-pointer-type(form, name, referenced-type);
     end;
end;

define method do-define-c-pointer-type
    (fragment :: <fragment>,
     variable-name :: <variable-name-fragment>,
     referenced-type :: <fragment>)
 => (forms :: <template>);
  let temp-abstract-name = gensym(variable-name, " t");
  let implicit-exports
    = generate-implicit-exports(temp-abstract-name);
  let frag
        // Why does this need to be open (dynamic)?  Who subclasses it?
    = #{ define dynamic primary class ?temp-abstract-name
                  (<C-pointer-to-pointer>)
           metaclass <temp-pointer-type-class>,
             temporary: #t,
             referenced-type: ?referenced-type,
             low-level-type: ?temp-abstract-name,
             self: ?temp-abstract-name;
         end;

         define sealed domain make(singleton(?temp-abstract-name));
         define sealed domain pointer-value-address(?temp-abstract-name);

         define constant ?variable-name
                = install-pointer-type(?variable-name,
                                       ?temp-abstract-name,
                                       ?referenced-type,
                                       ?temp-abstract-name);
         ?implicit-exports };
  frag
end;

/*
define method do-define-c-pointer-type
    (fragment :: <fragment>,
     variable-name :: <variable-name-fragment>,
     referenced-type :: <fragment>)
 => (forms :: <template>);
  let temp-abstract-name = gensym("temp-class-for-", variable-name);
  let temp-concrete-name = gensym("temp-concrete-class-for-", variable-name);
  let implicit-exports
    = generate-implicit-exports(temp-abstract-name, temp-concrete-name);
  let frag
    = #{ define abstract open class ?temp-abstract-name
                  (<C-pointer-to-pointer>)
           metaclass <temp-pointer-type-class>,
             temporary: #t,
             referenced-type: ?referenced-type,
             low-level-type: ?temp-abstract-name,
             self: ?temp-abstract-name;
         end;
         define concrete sealed class ?temp-concrete-name
                  (?temp-abstract-name)
           metaclass <temp-pointer-type-class>,
             temporary: #t,
             abstract-super: ?temp-abstract-name,
             self: ?temp-concrete-name,
             low-level-type: ?temp-concrete-name;
              end;

//         define sealed domain make(singleton(?temp-abstract-name));
         define sealed domain make(singleton(?temp-concrete-name));
         define sealed domain initialize(?temp-concrete-name);
//         define sealed domain pointer-value(?temp-concrete-name);
//         define sealed domain pointer-value-setter
//           (<object>, ?temp-concrete-name);
         define sealed domain pointer-value-address(?temp-concrete-name);
//         define sealed domain element(?temp-concrete-name, <integer>);
//         define sealed domain element-setter
//           (<object>, ?temp-concrete-name, <integer>);

         define constant ?variable-name
                = install-pointer-type(?variable-name,
                                       ?temp-abstract-name,
                                       ?referenced-type,
                                       ?temp-concrete-name);
         ?implicit-exports };
  frag
end;
*/



define &class <c-pointer-type-class> (<designator-class>)
end;

define-compiler-metaclass(#"<c-pointer-type-class>",
                           <&c-pointer-type-class>);

define &class <temp-pointer-type-class> (<designator-class>)
end;

define-compiler-metaclass(#"<temp-pointer-type-class>",
                           <&temp-pointer-type-class>);

define method ^initialize-class
    (designator :: <&C-pointer-type-class>,
     #rest keys,
     #key
       referenced-type,
       abstract-super)
  next-method();
  if (abstract-super)
    // reach out and update the links to myself
    let ref-type = designator.^referenced-type;
    ref-type.^concrete-pointer-type := designator;
  end if;
end method;


define method ^initialize-class
    (designator :: <&temp-pointer-type-class>,
     #rest keys,
     #key
       temp-referenced-type,
       abstract-super)
  next-method();
  if (temp-referenced-type)
    let ref-type = ^eval-designator(temp-referenced-type);
    designator.^referenced-type := ref-type;
  end if;
  if (abstract-super)
    // reach out and make links to myself
    let super-model = ^eval-designator(abstract-super);
    super-model.^concrete-class := designator;
  end if;

end method;


/* todo:
 * make install-pointer-type a top level initialization.
 * If a proper pointer type already exists  then
 * install-pointer-type arranges that both of the names point to the same
 * bindings. (Can I do that??)
 * If the existing pointer type is form this library then we can don't need
 * to do any initialization at run time, but if it is in a separate library
 * we may have to install the pointer type.
 */



define &macro install-pointer-type
  { install-pointer-type (?variable-name:name,
                          ?temp-pointer-type-name:name,
                          ?referenced-type-name:expression,
                          ?temp-concrete-pointer-type-name:name) }
  => begin
       let ref-type = ^eval-designator(referenced-type-name);
       if (designator-class?(ref-type))
         let referenced-type :: <&designator-class> = ref-type;
         let temp-pointer-type :: <&designator-class>
           = ^eval-designator(temp-pointer-type-name);
         ^ensure-pointer-types-initialized(referenced-type);
/*
         unless (temp-pointer-type)
           // generate an error
           // real error here since this was defined automatically
           error("unresolved temp-pointer-type-name in "
                   "install-pointer-type(%=, %=, %=, %=)",
                 variable-name,
                 temp-pointer-type-name,
                 referenced-type-name,
                 temp-concrete-pointer-type-name);
           // can't really continue here
         end;
*/
         let found-class = ^abstract-pointer-type(referenced-type);
         let found-lib = found-class & model-library(found-class);
         let ref-type-lib = model-library(referenced-type);
         let this-lib = model-library(temp-pointer-type);
         // !@#$ this junk is to get around the fact that we may
         // have defined the pointer type in a previous compile in this session
         if (found-lib == this-lib & found-class & found-class.proxy?)
           let new-found-class
             = ^eval-designator(as(<symbol>, found-class.^debug-name));
           if (new-found-class & new-found-class ~== found-class)
             // this is old crap so arrange to fix it up
             found-lib := #f;
           end;
         end;

         if (found-class &
            // either the already defined class came from this exact library,
            // or it was defined in the same library as the referenced type.
            // !@#$ could also use the found-class if it came from a
            // !@#$ library that this library uses, directly, or indirectly
              (found-lib == this-lib
                 | (found-lib == ref-type-lib & ~referenced-type.proxy?)))
           // !@#$ need to mark temp-pointer-type and its concrete subclass
           //      as redundant since it is not getting used.
           #{ ?found-class }
         elseif (found-class | this-lib ~== ref-type-lib
                   | referenced-type.proxy?)
           // found the class, but it is defined in a library that is too
           // far away.
           // We must arrange to get the right class, if it exists, or
           // install the temp pointer type at run time.
           //  All we really have to do here is ensure that the name generated
           // for the temp-class is uniquely derived from the ultimate name
           // for the referenced type, and return the temp-class.
           // At this point it is very difficult to see how to do that.
           //  If we could just put it off until run time then it would
           // be something like this:
           // #{ get-or-install-pointer-type
           //      (?temp-pointer-type-name, ?referenced-type-name) }
           // but the whole ffi depends on having the models available
           // at compile time, and this would prevent that, so we pretend
           // we are defining the real pointer type here.
           referenced-type.^abstract-pointer-type := temp-pointer-type;
           referenced-type.^concrete-pointer-type
             := ^eval-designator(temp-concrete-pointer-type-name);
           // indicate that this is (maybe) not the real thing.
           temp-pointer-type.proxy? := #t;
           #{ ?temp-pointer-type-name }
         else
           // no class already defined, but the referenced type was in our
           // library so we can install the pointer type at compile time
           // and return the temp pointer type
           referenced-type.^abstract-pointer-type := temp-pointer-type;
           referenced-type.^concrete-pointer-type
             := ^eval-designator(temp-concrete-pointer-type-name);
           #{ ?temp-pointer-type-name }
         end if;

       else
         generate-unresolved-designator-error
           (referenced-type-name, variable-name, #{ C-pointer-type }, #());
         #{ <C-void*> };
       end if;
     end;
end &macro;


/*
// if it could be defined as a function here's how it would be done
define function make-c-pointer (class :: <designator-class>,
                                raw-address :: <raw-machine-word>,
                                init-args :: <simple-object-vector>)
 => (v :: <C-pointer>);
  let address :: <machine-word> = primitive-wrap-machine-word(raw-address);
  let instance :: <C-pointer> = allocate-instance(class, init-args);
  apply(shared-initialize,instance, class.slot-descriptors,
        cooked-pointer-address: address, init-args);
  instance
end;
*/

define &macro make-c-pointer
  { make-c-pointer(?class:expression,
                   ?raw-address:expression,
                   ?init-args:expression) }
  => #{ begin
         let addr :: <machine-word>
           = primitive-wrap-machine-word(?raw-address);
         apply(make, ?class, address: addr, ?init-args);
        end }
end;

//// Utilities.

// TODO: CORRECTNESS: Currently this is just a hack to get the compiler
// to shut up by inserting some dummy references. It can be done without
// inventing new fragments.

define function generate-implicit-exports
   (#rest implicitly-exported-names) => (implicit-export-fragment)
  #{ begin ??implicitly-exported-names; ...; #f end }
end function;
