Module: dfmc-c-ffi
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define inline-only function designator-class? (class)
 => (well? :: <boolean>)
  instance?(class, <&designator-class>)
end function;


define method ^initialize-class 
    (designator :: <&designator-class>,
     #rest keys,
     #key low-level-type,
          raw-type-name,
          raw-dereferencer,
          bitfield-dereferencer,
	  //pointer-type-name,
	  referenced-type,
	  import-function,
	  export-function,
	  mapped-import-type,
	  mapped-export-type,
	  boxer-function-name,
	  unboxer-function-name,
	  temporary,
	  self,
	  // TODO: CORRECTNESS: debug-name is a string.
	  // Was that expected?
	  debug-name) 
  // this fills in the normal class information
  next-method();
  // if a pointer-type-name was specified then we need to generate a
  // definition for it.
  /*
  if (pointer-type-name)

    add-derived-top-level-fragment
      (form,
       make-automatic-pointer-definition-form(form));
    designator.^abstract-pointer-type := pointer-type-name;
  end if;
  */ 
  let superclass-models = designator.^direct-superclasses;
  // !@#$ error if superclass-models is not a sequence of class models
  let designator-super = block (return)
			   for (c in superclass-models)
			     if (instance?(c, <&designator-class>))
			       return(c);
			       
			     elseif
			       // !@#$ should be a model.  This is a hack
			       (instance?(c, <&static-values>)
				  & instance?(head (&values-model-objects(c)),
						<&designator-class>))
			       return(head(&values-model-objects(c)))
			     end if;
			   end for;
			 end block;
  // TODO: unless we are in the boot it is an error if there is no
  // designator superclass
/*
  unless (designator-class?(designator-super))
    note(<no-designator-superclass>,
	 designator-name: debug-name);
    designator-super := ^eval-designator(#{ <C-void*>});
  end unless;
*/
  local
    method set-or-inherit(val, class-field, class-field-setter,
			  #key fixup = identity,
			       unresolved-condition = <program-error>,
			       eval = #t)
      // !@#$ should really pass a default value to use if there is no 
      // designator-super, and it was not set by the form.
      if (val &
	    ~(instance?(val, <literal-constant-fragment>) & fragment-value(val) == #f))
	// !@#$ hack to deal with references to self ...
	if ((object-class(val) == object-class(debug-name))
	      & (as(<symbol>, val)
		   == as(<symbol>, debug-name)))
	  designator.class-field := fixup(designator);
	  // !@#$ end hack to deal with references to self
	else
	  let true-value
	    = if(eval) ^top-level-eval(val) else val end;
	  unless (true-value)
	    raise(unresolved-condition,
		  source-location: model-source-location(designator));
	  end unless;
	  designator.class-field := fixup(true-value);
	end if; 
	// !@#$ if this field was not compile time evaluable then
	// signal an error 
      elseif (designator-super)
	designator.class-field := designator-super.class-field;
      end if;
    end method,
    method as-variable (thing)
      as(<variable-name-fragment>, thing)
    end method,
    method compute-raw-type-info(raw-type-name)
     => (t :: <abstract-raw-type>);
      let raw-info = make(<basic-raw-type>,
			  raw-name: as-variable(raw-type-name));
      initialize-raw-type(raw-info);
      raw-info
    end;
  set-or-inherit(low-level-type, ^low-level-type, ^low-level-type-setter,
		 eval: #f);
  set-or-inherit(raw-type-name, ^raw-type-info, ^raw-type-info-setter,
		 fixup: compute-raw-type-info);
  set-or-inherit(raw-dereferencer, ^raw-dereferencer-name,
		 ^raw-dereferencer-name-setter, fixup: as-variable);
  set-or-inherit(bitfield-dereferencer, ^bitfield-dereferencer-name,
		 ^bitfield-dereferencer-name-setter, fixup: as-variable);
  set-or-inherit(boxer-function-name, ^boxer-function-name,
		 ^boxer-function-name-setter, fixup: as-variable);
  set-or-inherit(unboxer-function-name, ^unboxer-function-name,
		 ^unboxer-function-name-setter, fixup: as-variable);
  set-or-inherit
    (import-function, ^import-function, ^import-function-setter, eval: #f);
  set-or-inherit
    (export-function, ^export-function, ^export-function-setter, eval: #f);
  let raw-info = designator.^raw-type-info;
  if (raw-info)
    designator.^alignment-of := raw-info.raw-type-info-alignment;
    designator.^size-of := raw-info.raw-type-info-size;
  end;
  // we might want to use a separate metaclass for pointers since they
  // may also have a different implementation.
  // in that case all of the following will need to be moved to the
  // ^initialize method for that metaclass
  if (referenced-type)
    // !@#$ designator better be abstract ...
    let ref-type = ^eval-designator(referenced-type);
    // !@#$ but if ref-type isn't here, we're in big trouble
    if (ref-type)
      unless (temporary)
        ref-type.^abstract-pointer-type := designator;
      end;
      designator.^referenced-type := ref-type;
      // !@#$ assure that designator is abstract.
    end;
  elseif (designator-super & designator-super.^referenced-type)
    designator.^referenced-type := designator-super.^referenced-type
  end if;
  local method fragment-false? (f)
	  macro-case (f)
	    { #f } => #t;
	    { } => #f;
	    { ?any:* } => #f;
	  end;
        end;
  designator.^mapped-import-type
    := if (mapped-import-type & ~fragment-false?(mapped-import-type))
	 mapped-import-type
       else
	 (designator-super & designator-super.^mapped-import-type)
	   | designator.^low-level-type;
       end if;

  designator.^mapped-export-type
    := if (mapped-export-type & ~fragment-false?(mapped-export-type))
	 mapped-export-type
       else
	 (designator-super & designator-super.^mapped-export-type)
	   | designator.^low-level-type;
       end if;
  if (designator.^referenced-type)
    // any designator with a referenced-type is a pointer and has itself as
    // a low level type.
    designator.^low-level-type := self;
    if(~designator.^mapped-import-type
	 | designator.^mapped-import-type == designator-super)
      designator.^mapped-import-type := designator;
    end if;
    if(~designator.^mapped-export-type
	 | designator.^mapped-export-type == designator-super)
      designator.^mapped-export-type := designator;
    end if;
  end if;
  // !@#$ deal with size-of, alignment-of?
end method ^initialize-class;	 



/*
//!@#$ is this needed????
// Each foreign function has a signature
// There only needs to be one "caller" function for each signature
define class <C-function-signature> (<object>)
  slot arg-specs :: <sequence>;
  slot result-designator :: <class>;
end;

define class <C-arg-spec> (<object>)
  slot designator-class :: <class>;  // or <&class>
  slot discipline :: <symbol>; // input output or in-out
end;
*/



define method ^initialize-class
    (designator :: <&C-automatic-pointer-designator-class>,
     #rest keys,
     #key
       referenced-type,
       concrete-class-name,
       abstract-super,
       temporary,
       // TODO: CORRECTNESS: debug-name is a string.
       // Was that expected?
       debug-name)

  next-method();
  if (abstract-super)
    // designator is a concrete pointer type
    let super-model = ^eval-designator(abstract-super);
    super-model.^concrete-class := designator;
//    ^referenced-type(designator) := ^referenced-type(super-model);
  end if;
  if (referenced-type & ~temporary)
    let ref-type = designator.^referenced-type;
    ref-type.^abstract-pointer-type := designator;
//    ref-type.^concrete-pointer-type := designator.^concrete-class;
  end if;
end method;

/// This is modeled on ^ensure-slots-initialized in define-class-mop.dylan
define method ^ensure-pointer-types-initialized (class :: <&designator-class>)
 => ();
  // Note that this has to fire if either abstract or concrete is 
  // missing since one can be computed without the other by other
  // means (typically, the abstract pointer type is referred to by
  // name somewhere).
  unless ((class.^abstract-pointer-type & class.^concrete-class)
	    | (~class.pointer-type-name & ~class.concrete-class-name))
    let class-definition = model-definition(class);
    with-dependent-context ($compilation of class-definition)
      if (class.pointer-type-name)
	// should set ^abstract-pointer-type
	let pointer = ^eval-designator(class.pointer-type-name);
	if (pointer.concrete-class-name)
	  ^eval-designator(pointer.concrete-class-name);
	end;
      end if;
      if (class.concrete-class-name)
	// should set ^concrete-class
	^eval-designator(class.concrete-class-name);
      end if;
    end; // with-dependent-context
  end unless;
end;


/*
define method pointer-superclass (referenced-type) => (m :: <&object>);
  let ref-type = ^eval-designator(referenced-type);
  if (ref-type.^referenced-type)
    // a pointer so use pointer-to-pointer
    ^eval-designator(#{<c-pointer-to-pointer>});
  else
    // not apointer so use plain <c-pointer>.
    ^eval-designator(#{<c-statically-typed-pointer>});
  end if
end method;
*/

define method ^raw-type-name (c :: <&designator-class>) => (type-name)
  raw-type-name(^raw-type-info(c));
end;

define method ^raw-type-name (c :: <&c-struct/union-designator-class>)
 => (type-name)
  raw-struct-name(c);
end;



/*
///!@#$ this is wrong, but it should be compilable.
/// We have to fix this when we actually have a form of raw type that works
/// for structs
define method ^raw-type-name (c :: <&C-struct-designator-class>)
 => (type-name);
  // this should construct the syntax for the raw type based on the
  // ^raw-type-info in the struct
  #{ <raw-c-pointer> }
end;
*/

define sideways method compute-raw-aggregate-member (member-list :: <sequence>)
 => (member :: <raw-aggregate-member>)
  let kind = head(member-list);
  let member-type = second(member-list);
  let designator = ^eval-designator(member-type);
  if (instance?(designator, <&designator-class>))
    let raw-type-name = ^raw-type-name(designator);
    let raw-type-model = ^top-level-eval(raw-type-name);
    select (kind)
      #"member" =>
	make(<raw-aggregate-ordinary-member>, raw-type: raw-type-model);
      #"array-member"=>
	begin
	  let length = ^top-level-eval(third(member-list));
	  unless (instance?(length, <integer>))
	    length := 1;
	  end unless;
	  make(<raw-aggregate-array-member>,
	       raw-type: raw-type-model,
	       array-length: length);
	end;
      #"bitfield-member" =>
	begin
	  let width = ^top-level-eval(third(member-list));
	  unless (instance?(width, <integer>))
	    width := 1;
	  end unless;
	  make(<raw-struct-bitfield-member>,
	       raw-type: raw-type-model,
	       bitfield-width: width);
	end;
    end;
  else
    make(<raw-aggregate-ordinary-member>,
	 raw-type: ^top-level-eval(#{ <raw-c-pointer> }));
  end if;
end;

// eof

