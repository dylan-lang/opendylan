module: dfmc-c-ffi
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Copyright 1996 Functional Objects, Inc.  All rights reserved.

/* 

define c-mapped-suptype <new-type> (<super-type>)
  import-map <some-type>, import-function: import-fun;
  export-map <some-other-type>, export-function: export-fun;
  pointer-type <new-type*>, pointer-value-setter: #t;
end

 ==>


define class <new-type> (<super-type)
  metaclass <c-mapped-designator-class>,
    partial-import-function: import-fun,
    partial-export-function: export-fun,
    import-map: <some-type>,
    export-map: <some-other-type>,
    pointer-type-name: <new-type*>,
    pointer-value-setter: #t;
end;

define abstract class <new-type*> (<cant-tell-yet>)
  metaclass <C-automatic-pointer-designator-class>,
  referenced-type: <new-type>;
end;

define sealed concrete class
    concrete-instantiation-of-<new-type*> (<new-type*>)
  metaclass <C-automatic-pointer-designator-class>,
    abstract-super: <new-type*>;
end;

// maybe a make method ...

// definition for:
define method pointer-value (p :: <new-type*>, #key index = 0)
  combined-import-function(boxer-function(dereferencer(unbox-pointer(p), 0, index * size-of(<new-type>))));
end;

// definition for:
define method pointer-value-setter (new-value :: , p :: <new-type*>, #key index = 0)
  combined-import-function(boxer-function(dereferencer(unbox-pointer(p), 0, index * size-of(<new-type>))));
end;

// definition for
define function export-function-for-<new-type> (dylan-obj :: <some-type>)
 => (low-obj :: <low-level-type-for-<super-type>);
  export-fun(export-function-for-<super-type>(dylan-obj))
end;

// definition for
define function import-function-for-<new-type>
  (low-obj :: <low-level-type-for-<super-type>)
 => (dylan-obj :: <some-type>);
  import-fun(import-function-for-<super-type>(low-obj))
end;


/// this gets defined as a result of the concrete class definition

define method make (c == <new-type*>, #rest initargs, #key)
 => (o :: concrete-class-for-<new-type*>)
  apply(make, concrete-class-for-<new-type*>, initargs)
end;

*/



//
// C-MAPPED-SUBTYPE
//
define &macro c-mapped-subtype-definer
  { define ?mods:* c-mapped-subtype ?:name (?supers:*)
      ?specs:*
    end }
  => begin
       unless-ffi-definition-dynamic (form)
	 let keys = parse-c-mapped-subtype-specs(form, name, specs);
         apply(do-define-c-mapped-subtype, form, name, supers,
	       modifiers: mods, keys)
       end;
     end;

  supers:
    { } => #()
    { ?e:*, ... } => pair(e, ...)

  specs:
    { } => #();
    { ?spec:*; ... } => pair(spec, ...)
end &macro;


define option <c-mapped-subtype-import-function-option>
  => import-function: :: expression
end;

define option <c-mapped-subtype-export-function-option>
  => export-function: :: expression
end;

define option <c-mapped-subtype-pointer-value-setter-option>
  => pointer-value-setter: :: expression
end;

define option <c-mapped-subtype-c-name-option>
  => c-name: :: expression
end;

define option <c-mapped-subtype-pointer-type-name-option>
  => pointer-type-name: :: name
end;

define constant $c-mapped-subtype-map-options =
  list(<c-mapped-subtype-import-function-option>,
       <c-mapped-subtype-export-function-option>);

define constant $c-mapped-subtype-import-map-options =
  list(<c-mapped-subtype-import-function-option>);

define constant $c-mapped-subtype-export-map-options =
  list(<c-mapped-subtype-export-function-option>);

define constant $c-mapped-subtype-pointer-type-options =
  list(<c-mapped-subtype-pointer-value-setter-option>);

define constant $c-mapped-subtype-options =
  list(<c-mapped-subtype-c-name-option>,
       <c-mapped-subtype-pointer-type-name-option>);

define function process-map-options (map-type,
     #key import-function = #{ identity }, export-function = #{ identity })
 => (options :: <list>)
    list(partial-import-function: import-function,
	 partial-export-function: export-function,
	 import-type: map-type,
	 export-type: map-type )
end function;

define function process-import-map-options (clause, name, map-type,
     #key import-function = unsupplied())
 => (options :: <list>)
  if (supplied?(import-function))
    list(import-type: map-type, partial-import-function: import-function);
  else
    note(<missing-import-function>,
	 source-location: fragment-source-location(clause),
	 definition-name: name);
    #();
  end if;
end function;

define function process-export-map-options (clause, name, map-type,
     #key export-function = unsupplied())
 => (options :: <list>)
  if (supplied?(export-function))
    list(export-type: map-type, partial-export-function: export-function);
  else
    note(<missing-export-function>,
	 source-location: fragment-source-location(clause),
	 definition-name: name);
    #();
  end if;
end function;

define function process-pointer-type-options (pointer-type-name,
     #key pointer-value-setter = #{ #"not-given" })
 => (options :: <list>)
  list(pointer-type-name: pointer-type-name,
       define-pointer-value-setter: pointer-value-setter);
end function;

define method parse-c-mapped-subtype-specs (form, name, specs :: <sequence>)
 => (keys :: <sequence>)

  let import-mapping? :: <boolean> = #f;
  let export-mapping? :: <boolean> = #f;
  let keys :: <list> = #();

  for (clause in specs)
    let new-keys
      = macro-case (clause)
      { map ?map-type:expression, ?options:* }
      => begin
	   if (import-mapping?)
	     note(<multiple-import-mappings>,
		  source-location: fragment-source-location(form),
		  definition-name: name);
	     #();
	   elseif (export-mapping?)
	     note(<multiple-export-mappings>,
		  source-location: fragment-source-location(form),
		  definition-name: name);
	     #();
	   else
	     import-mapping? := #t;
	     export-mapping? := #t;
	     let keys = parse-options($c-mapped-subtype-map-options,
				      options, name);
	     apply(process-map-options, map-type, keys);
	   end if;
	 end;

    { import-map ?import-type:expression, ?options:* }
      => begin
	   if (import-mapping?)
	     note(<multiple-import-mappings>,
		  source-location: fragment-source-location(form),
		  definition-name: name);
	     #();
	   else
	     import-mapping? := #t;
	     let keys = parse-options($c-mapped-subtype-import-map-options,
				      options, name);
	     apply(process-import-map-options, form, name, import-type, keys);
	   end if;
	 end;

    { export-map ?export-type:expression, ?options:* }
      => begin
	   if (export-mapping?)
	     note(<multiple-export-mappings>,
		  source-location: fragment-source-location(form),
		  definition-name: name);
	     #();
	   else
	     export-mapping? := #t;
	     let keys = parse-options($c-mapped-subtype-export-map-options,
				      options, name);
	     apply(process-export-map-options, form, name, export-type, keys);
	   end if;
	 end;

    { pointer-type ?pointer-type-name:name, ?options:* }
      => begin
	   let keys = parse-options($c-mapped-subtype-pointer-type-options,
				    options, name);
	   apply(process-pointer-type-options, pointer-type-name, keys);
	 end;

    { ?options:* }
      => as(<list>,parse-options($c-mapped-subtype-options, options, name));
/*
    { ?other:* }
      => begin
	   note(<unrecognized-clause>,
		source-location: fragment-source-location(other),
		definition-name: name);
	   #();
	 end;
*/
    end macro-case;
    
    keys := concatenate(new-keys, keys);
  end for;
  keys;
end method;



//
// SIMPLE-C-MAPPED-SUBTYPE
//
define &macro simple-c-mapped-subtype-definer
  { define ?mods:* simple-c-mapped-subtype ?:name (?supers:*)
      ?specs:*
  end }
  =>
  unless-ffi-definition-dynamic (form)
    apply(method
	      (#key import-function, export-function,import-type,
	       export-type, pointer-type-name) => (template);
	    unless(pointer-type-name)
	      pointer-type-name := gensym("mapped-pointer-to-", name);
	    end;
	    let concrete-pointer-type
	      = gensym("instantiation-of-", pointer-type-name);
	    do-simple-c-mapped-subtype
	      (name, mods, supers, import-function,
	       export-function, import-type, export-type,
	       pointer-type-name, concrete-pointer-type, #{})
	  end,
	  specs);
  end;
supers:
  { } => #()
  { ?e:*, ... } => pair(e, ...)
specs:
  { } => #();
  { ?spec:*; ... } => concatenate(spec, ...)
spec:
  { map ?map-type:expression, #key ?import-function:expression = identity,
                                   ?export-function:expression = identity }
  => list(import-function: import-function,
	  export-function: export-function,
	  import-type: map-type,
	  export-type: map-type )
  { import-map ?import-type:expression,
                 import-function: ?import-function:expression }
  => list(import-function: import-function,
	  import-type: import-type)
  { export-map ?export-type:expression,
                 export-function: ?export-function:expression }
  => list(export-function: export-function,
	  export-type: export-type)
  { pointer-type ?pointer-type-name:name }
  => list(pointer-type-name: pointer-type-name)
end &macro;  


define method do-simple-c-mapped-subtype
    (name, modifiers, supers, import-function-name,
     export-function-name, import-type, export-type,
     abstract-pointer-type, concrete-pointer-type, slots)
 => (template);
  let pointer-type-defns
    = create-automatic-c-pointer-definition-fragment
        (abstract-pointer-type, name,
	 concrete-class-name: concrete-pointer-type);
  /*
  let implicit-exports 
    = generate-implicit-exports(concrete-pointer-type);
  */
  let implicit-exports = #();
  let pointer-value-def
    = if (import-function-name | import-type)
        #{ define method pointer-value 
               (p :: ?abstract-pointer-type, #key index :: <integer> = 0)
            => (v :: import-type-for(?name));
             pointer-value-body(p, index, ?abstract-pointer-type);
           end method pointer-value }
      else
        #{ }
      end;
  let pointer-value-setter-def
    = if (export-function-name | export-type)
        #{ define method pointer-value-setter 
               (new :: export-type-for(?name), p :: ?abstract-pointer-type,
		  #key index :: <integer> = 0)
            => (v :: export-type-for(?name));
	      pointer-value-setter-body
                (new, p, index, ?abstract-pointer-type);
           end method pointer-value-setter }
      else
        #{ }
      end;
    #{ define ?modifiers class ?name ( ??supers, ... )
	 metaclass <c-mapped-designator-class>,
	   import-function: ?import-function-name,
	   export-function: ?export-function-name,
	   mapped-import-type: ?import-type,
	   mapped-export-type: ?export-type,
	   pointer-type-name: ?abstract-pointer-type,
	   self: ?name;
	 ?slots
       end;
       ?pointer-type-defns;
       ?pointer-value-def;
       ?pointer-value-setter-def;
       ?implicit-exports
     }
end method;

define method do-define-c-mapped-subtype
    (form :: <fragment>,
     name :: <variable-name-fragment>,
     supers :: <sequence>,
     #key
       partial-import-function = #f,
       partial-export-function = #f,
       import-type = #f,
       export-type = #f,
       pointer-type-name = #f,
       define-pointer-value-setter = #f,
       c-name = #f,
       slots = #{ },
       modifiers = #{ })

 => (s :: <template>);
  // first do some syntactical consistency checking
  verify-mapped-subtype-options(form, name, supers,
				partial-import-function,
				partial-export-function,
				import-type,
				export-type,
				pointer-type-name,
				define-pointer-value-setter);
  let abstract-pointer-type
    = pointer-type-name | gensym(name, " *");
  let concrete-pointer-type
    = gensym("concrete-pointer-type-for-", name);
  let import-function-name
    = if (partial-import-function)
	gensym("import-function-for-", name);
      else
	#f
      end;
  let export-function-name
    = if (partial-export-function)
	gensym("export-function-for-", name);
      else
	#f
      end;
  let definitions
    = list(do-simple-c-mapped-subtype
	     (name, modifiers, supers, import-function-name,
	      export-function-name, import-type, export-type,
	      abstract-pointer-type, concrete-pointer-type, slots));

  if (partial-import-function)
    definitions
      := pair(#{ define function ?import-function-name
                     (arg :: low-level-type-for(?name))
//                  => (value :: check-import-type-defined(?import-type, ?name))
                  => (value :: ?import-type)
                   import-function-body(arg,
					?partial-import-function,
					(??supers, ...))
                 end },
	      definitions);
  end if;
  if (partial-export-function)
    definitions
      := pair(#{ define function ?export-function-name
//                     (arg :: check-export-type-defined(?export-type, ?name))
                     (arg :: ?export-type)
                  => (value :: low-level-type-for(?name))
                   export-function-body(arg,
					?partial-export-function,
					(??supers, ...))
                 end },
	      definitions);

  end if;
  #{ ??definitions; ...}
end method;

define &macro pointer-value-setter-body
  { pointer-value-setter-body(?new-value-var:variable,
			      ?pointer-var:variable,
			      ?index-var:variable,
			      ?pointer-type-expr:expression) }
  =>
  begin
    let pointer-type = ^eval-designator(pointer-type-expr);
    let ref-type = pointer-type.^referenced-type;
    let unboxer = ref-type.^unboxer-function-name;
    let export = ref-type.^export-function | #{ identity };
    let dereferencer = ref-type.^raw-dereferencer-name;
    if (dereferencer)
      #{ ?dereferencer(primitive-unwrap-c-pointer(?pointer-var),
		       integer-as-raw(?index-var),
                       integer-as-raw(0))
	  := ?unboxer(?export(?new-value-var));
	?new-value-var }
    else
      // !@#$ ideally the pointer-value-setter won't even be defined
      #{ error("trying to call pointer-value-setter with a bad class") }
    end if;
  end;
end &macro;

define &macro pointer-value-body
  { pointer-value-body(?pointer-var:variable,
			      ?index-var:variable,
			      ?pointer-type-expr:expression) }
  =>
  begin
    let pointer-type = ^eval-designator(pointer-type-expr);
    let ref-type = pointer-type.^referenced-type;
    let low-type = ref-type.^low-level-type;
    let boxer = ref-type.^boxer-function-name;
    let import = ref-type.^import-function | #{ identity };
    let dereferencer = ref-type.^raw-dereferencer-name;
    if (dereferencer)
      #{ ?import
	  (boxer-for-designator
	     (?low-type,
	      ?dereferencer
		(primitive-unwrap-c-pointer(?pointer-var),
		 integer-as-raw(?index-var),
                 integer-as-raw(0)),
	      ?boxer))
	  }
    else
      // !@#$ this is controversial Maybe we shouldn't even
      // !@#$ define the method in this case?
      let import = pointer-type.^import-function | #{ identity };
      #{ ?import( ?pointer-var + ?index-var) }
    end if
  end;
end &macro;


define &macro export-function-body
  { export-function-body(?arg-var:variable,
			 ?partial-export-function:*,
			 (?supers)) }
  =>
  begin
    let (designator-super, super-expr)
      = block (gotit)
	  for(c-expr in supers)
	    let c = ^eval-designator(c-expr);
	    if (designator-class?(c))
	      gotit(c, c-expr)
	    end if;
	  end for;
	  values(#f, #f);
	end block;
    // !@#$ need to error out if no designator super
    let superclass-export = designator-super.^export-function;
    if (superclass-export)
      #{ ?superclass-export(?partial-export-function(?arg-var)) }      
    else
      #{ ?partial-export-function(?arg-var) }
    end if
  end;
supers:
  { ?super:expression, ... } => pair(super, ...)
  { ?super:* } => list(super);
end;      

define &macro import-function-body
  { import-function-body(?arg-var:variable,
			 ?partial-import-function:*,
			 (?supers)) }
  =>
  begin
    let (designator-super, super-expr)
      = block (gotit)
	  for(c-expr in supers)
	    let c = ^eval-designator(c-expr);
	    if (designator-class?(c))
	      gotit(c, c-expr)
	    end if;
	  end for;
	  values(#f, #f);
	end block;
    // !@#$ need to error out if no designator-super
    let superclass-import = designator-super.^import-function;
    if (superclass-import)
      #{ ?partial-import-function
	  (?superclass-import(?arg-var)) }
    else
      #{ ?partial-import-function(?arg-var) }
    end if;
  end;
supers:
  { ?super:expression, ... } => pair(super, ...)
  { ?super:* } => list(super);
end;      

define &macro check-import-type-defined
  { check-import-type-defined (?import-type:expression, ?:name) }
  =>
  begin
    if (^eval-designator(import-type))
      #{ ?import-type };
    else
      note(<unresolved-import-type>,
	   source-location: fragment-source-location(import-type),
	   definition-name: name);
      #{ <object> };
    end if;
  end;
end &macro;

define &macro check-export-type-defined
  { check-export-type-defined (?export-type:expression, ?:name) }
  =>
  begin
    if (^eval-designator(export-type))
      #{ ?export-type };
    else
      note(<unresolved-export-type>,
	   source-location: fragment-source-location(export-type),
	   definition-name: name);
      #{ <object> };
    end if;
  end;
end &macro;


define method verify-mapped-subtype-options (fragment, name, supers,
					     partial-import-function,
					     partial-export-function,
					     import-type,
					     export-type,
					     pointer-type-name,
					     define-pointer-value-setter)
 => ();
  // !@#$ do this error checking
  values();
end;



//
// C-SUBTYPE-DEFINER
//

define option <c-subtype-pointer-type-name-option>
  => pointer-type-name: :: expression
end option;

define option <c-subtype-c-name-option>
  => c-name: :: expression
end option;

define constant $c-subtype-options =
  list(<c-subtype-pointer-type-name-option>,
       <c-subtype-c-name-option>);

define function process-c-subtype-options (name, clause,
    #key pointer-type-name = unsupplied())
 => (options :: <list>)
  if (supplied?(pointer-type-name))
    if (fragment-name?(pointer-type-name))
      list(pointer-type-name: pointer-type-name);
    else
      note(<invalid-pointer-type-name-value>,
	   source-location: fragment-source-location(pointer-type-name),
	   definition-name: name,
	   pointer-type-name-expression: pointer-type-name);
      #();
    end if;
  else
    #();
  end if;
end function;

define &macro c-subtype-definer
  { define ?mods:* c-subtype ?:name (?supers:*)
      ?keys-and-slots:*
    end }
  =>
  unless-ffi-definition-dynamic (form)
    let (keys, slots) = parse-subtype-keys-and-slots(name, keys-and-slots);
    apply(do-define-c-mapped-subtype, form, name, supers,
	  modifiers: mods,
	  slots: slots,
	  keys);
  end;
supers:
{ } => #();
{ ?super:expression, ... } => pair(super, ...);
end;

define method parse-subtype-keys-and-slots (name :: <fragment>,
					    keys-and-slots :: <fragment>)
 => (keys :: <sequence>, slots :: <template>)

  local method parse-item (item) => (key :: <list>, value :: <list>)
	  macro-case (item)
	  { ?modifiers:* slot ?stuff:* }
	  => values(#(), list(item))
	  { ?options:* }
	  => values(apply(process-c-subtype-options, name, options,
			  parse-options($c-subtype-options, options, name)),
		    #())
	end macro-case;
  end method parse-item;

  macro-case (keys-and-slots)
    { ?items:* }
    => begin
         let keys = head(items);
	 let slots = tail(items);
	 values(keys, #{ ??slots; ...});
       end;
  items:
    { } => pair(#(), #());
    { ?item:*; ...}
      => begin
           let (key, slot) = parse-item(item);
           let other-keys = head(...);
           let other-slots = tail(...);
           pair(concatenate(key, other-keys), concatenate(slot, other-slots))
         end;
  end macro-case;
end;


//
// BOXER-FOR-DESIGNATOR
//

define &macro boxer-for-designator
  { boxer-for-designator(?type-expr:expression, ?raw-value:expression,
			 ?default-boxer:expression) }
  => begin
       let low-type = ^eval-designator(type-expr);
       boxer-expression(low-type, type-expr, raw-value, default-boxer)
     end;
end;


define method boxer-expression
    (type :: <&designator-class>, type-expr :: <fragment>,
     raw-expr :: <fragment>, default-boxer :: <fragment>)
 => (exp);
  #{ make-c-pointer(?type-expr,
		    primitive-cast-pointer-as-raw(?raw-expr),
		    #[]); }
end;
  
define method boxer-expression
    (type :: <object>, type-expr :: <fragment>,
     raw-expr :: <fragment>, default-boxer :: <fragment>)
 => (exp);
  #{ ?default-boxer(?raw-expr) }
end;

