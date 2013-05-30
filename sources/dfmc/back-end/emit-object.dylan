module: dfmc-back-end
author: keith playford and jonathan bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// GENERIC OBJECT EMISSION

define method emit-object
    (back-end :: <back-end>, stream :: <stream>, o :: <object>) => (object)
  print-message(o, stream); // hack
end method;

/// REFERENCES

// Direct objects are always emitted in-line. References to indirect objects
// are genuine references to previously dumped structures.

define method emit-reference 
   (back-end :: <back-end>, stream :: <stream>, o) => (reference)
 signal("Don't know how to emit a reference to %=", o);
 format(stream, "%=", o);
end method;

// VIRTUAL OBJECTS

define method emit-reference
    (back-end :: <back-end>, stream :: <stream>, o :: <virtual-object>) 
 => (reference)
  break("Illegal to reference a virtual object %=\n", o);
end method;

define method emit-object
    (back-end :: <back-end>, stream :: <stream>, o :: <virtual-object>) => (object)
  // nothing to emit -- all in run-time
end method;


/// EMIT-NAME

define function emit-name
  (back-end :: <back-end>, stream, o) => (name :: <byte-string>)
  string-emitter(back-end, stream, emit-namestring(back-end, stream, o));
end function;

define function emit-namestring
  (back-end :: <back-end>, stream, o) => (name)
  let name = o.emitted-name;
  if (instance?(name, <byte-string>)) name
  else
    o.emitted-name := emit-name-internal(back-end, stream, o)
  end if
end function;

define method string-emitter (back-end :: <back-end>, stream, name :: <byte-string>)
  write(stream, name);
  name
end method;

/*
define method emitted-name? (name) => (emitted? :: <boolean>)
  #f
end method;

define method emitted-name? (name :: <byte-string>) => (emitted? :: <boolean>)
  #t
end method;
*/

define macro name-emitter
  { name-emitter (?args:*) }
    => { method (?=back-end) format-to-string(?args) end }
end macro;

define method emit-name-internal
    (back-end :: <back-end>, stream, o :: <virtual-object>) => (name)
  break("Illegal to emit the name of a virtual object %=\n", o);
end method;

/// NAMES

define method emit-name-internal 
    (back-end :: <back-end>, stream, o :: <symbol>) 
 => (name)
  emit-symbol-name(back-end, stream, o, as(<string>, o));
end method;

define method emit-symbol-name 
    (back-end :: <back-end>, stream, o, name :: <string>) 
 => (name :: <string>)
  mangle-symbol(local-mangle(back-end, as-lowercase(name)));
end method;

define method emit-name-internal 
    (back-end :: <back-end>, stream, o :: <uninterned-symbol>) 
 => (name)
  emit-symbol-name(back-end, stream, o, symbol-name(o));
end method;

define constant $anonymous-name-cache = make(<object-table>);

define method emit-anonymous-name
    (back-end :: <back-end>, stream, o) => (name :: <string>)
  let number = o.emitted-name;
  debug-assert(number, "Missing emitted-name for %s", o);
  element($anonymous-name-cache, number, default: #f)
    | (element($anonymous-name-cache, number)
         := mangle-constant(mangle-integer(number)));
end method;

define generic emit-method-name 
    (back-end :: <back-end>, stream, 
     o :: <&method>, defn) => (name :: <string>);

// !@#$ temporary method until incr compilation method numbering is in place

define method emit-method-name
    (back-end :: <back-end>, stream, 
     o :: <&method>, defn :: <method-definition>) => (name :: <string>)
  let generic-library-name
    = debug-name(home-library(binding-home
				(form-variable-binding(defn))));
  let method-library-name
    = debug-name(language-definition(form-library(defn)));
  mangle-generic-method(mangler(back-end),
			global-mangle(back-end, defn),
			method-number(defn),
			method-library-name,
			generic-library-name);
end method;

define method emit-method-name
    (back-end :: <back-end>, stream, 
     o :: <&method>, defn :: <constant-definition>) => (name :: <string>)
  mangle-constant(global-mangle(back-end, defn));
end method;

define method find-anonymous-method-parents-name (o :: <&method>) => (res)
  #f
end method;

define method find-anonymous-method-parents-name (o :: <&lambda>) => (res)
  local method method-debug-string (object :: <&method>) => (res)
	  let debug-name = object.debug-name;
	  if (instance?(debug-name, <variable-name-fragment>))
	    debug-name.fragment-identifier
	  elseif (debug-name)
	    as(<symbol>, debug-name)
	  else 
	    #f
	  end
	end method;
  let env = environment(o);
  when (env)
    let next-env = lambda-environment(outer(env));
    iterate find-top 
        (next-env :: <lambda-lexical-environment> = next-env,
	 env      :: <lambda-lexical-environment> = env) 
      case
	top-level-environment?(next-env) 
	  => let lambda = lambda(env);
	     lambda-top-level?(lambda) & method-debug-string(lambda);
	lambda-initializer?(lambda(next-env))
	  => method-debug-string(lambda(env));
	otherwise
	  => find-top(lambda-environment(outer(next-env)), next-env);
      end case
    end iterate;
  end when;
end method;

define method emit-method-name
    (back-end :: <back-end>, stream, o :: <&method>, defn) => (name :: <string>)
  if (o.debug-name)
    debug-assert(o.emitted-name, "Missing emitted-name for %s", o);
    mangle-local-method(raw-mangle(back-end, as-lowercase(as(<string>, o.debug-name))), 
			o.emitted-name);
  else
    let name = find-anonymous-method-parents-name(o);
    if (name)
      debug-assert(o.emitted-name, "Missing emitted-name for %s", o);
      mangle-local-method(concatenate(raw-mangle(back-end, "anonymous-of-"), 
				      raw-mangle(back-end, as-lowercase(as(<string>, name)))), 
			  o.emitted-name);
    else 
      emit-anonymous-name(back-end, stream, o);
    end if
  end if;
end method;

define method emit-name-internal 
    (back-end :: <back-end>, stream, o :: <&method>) => (name)
  emit-method-name(back-end, stream, o, o.model-definition);
end method;


define method emit-name-internal 
    (back-end :: <back-end>, stream, o :: <&singular-terminal-engine-node>)
 => (name :: <string>);
  concatenate($singular-terminal-engine-node-prefix,
	      raw-mangle(back-end, o.^object-class.^debug-name))
end method;

define method emit-name-internal
    (back-end :: <back-end>, stream, o :: <&domain>) => (name)
  let defn = o.model-definition;
  if (defn)
    mangle-domain(global-mangle(back-end, defn),
		  domain-number(defn),
		  raw-mangle(back-end, library-description-emit-name(form-library(defn))));
  else
    // This can happen e.g. when we copy a sealed inline-only method due to
    // a non-inlineable access -- corresponding domain gets copied along with
    // the method, resulting in an anonymous domain.
    emit-anonymous-name(back-end, stream, o)
  end;
end method;


/// !@#$ THIS SHOULD BE DONE WHEN NAMING THE OBJECT -- 
/// !@#$ NORMAL MANGLING WILL TAKE CARE OF THIS

define method emit-slot-descriptor-name 
    (back-end :: <back-end>, stream, o :: <&slot-initial-value-descriptor>, name :: false-or(<string>))
 => (name :: <string>)
  if (name)
    emit-slot-descriptor-name-internal(back-end, stream, o, name, #f)
  else
    emit-anonymous-name(back-end, stream, o);
  end if;
end method;

define method emit-slot-descriptor-name
    (back-end :: <back-end>, stream, o :: <&slot-initial-value-descriptor>, defn :: <variable-defining-form>)
 => (name :: <string>)
  emit-slot-descriptor-name-internal(back-end, stream, o,
				     global-mangle(back-end, defn),
				     debug-name(language-definition(form-library(defn))))
end method;

define method emit-slot-descriptor-name-internal
    (back-end :: <back-end>, stream, o :: <&slot-initial-value-descriptor>, slot-name :: <string>, slot-library)
 => (name :: <string>)
  let owner = o.^slot-owner;
  let owner-library = owner.model-library;
  let owner-library-name = owner-library.library-description-emit-name;
  let owner-defn-binding = owner.model-variable-binding;
  let owner-name = owner-defn-binding.binding-identifier;

  mangle-slot-descriptor(mangler(back-end),
			 slot-name,
			 slot-library,
			 raw-mangle(back-end, owner-name),
			 owner-defn-binding.binding-home.debug-name,
			 owner-library-name);

end method;


define method emit-name-internal 
    (back-end :: <back-end>, stream, o :: <&slot-descriptor>)
 => (name)
  let getter-defn = o.^slot-getter.model-definition;
  emit-slot-descriptor-name(back-end, stream, o, getter-defn);
end method;

define method emit-name-internal 
    (back-end :: <back-end>, stream, 
     o :: <&inherited-slot-descriptor>)
 => (name)
  let getter-defn = o.^inherited-slot-getter.model-definition;
  emit-slot-descriptor-name(back-end, stream, o, getter-defn);
end method;

define method emit-name-internal 
    (back-end :: <back-end>, stream, o :: <&init-arg-descriptor>)
 => (name)
  emit-slot-descriptor-name
    (back-end, stream, o, 
     raw-mangle(back-end, as-lowercase(as(<string>, o.^init-keyword))));
end method;

define method emit-name-internal 
    (back-end :: <back-end>, stream, o :: <&mm-wrapper>) 
 => (name)
  mangle-wrapper(global-mangle(back-end, ^iclass-class(^mm-wrapper-implementation-class(o))));
end method;

define method emit-name-internal
    (back-end :: <back-end>, stream, o :: <&implementation-class>)
 => (name)
  emit-anonymous-name(back-end, stream, o)
end method;


define method emit-name-internal 
    (back-end :: <back-end>, stream, o) => (name)
  let defn = o.model-definition;
  if (defn) // !@#$ handle case with multiple variables / definition
    debug-assert(~instance?(defn, <domain-definition>) & ~instance?(o, <&domain>),
		 "emit-name-internal for domain %=", o);
    mangle-constant(global-mangle(back-end, defn));
  else
    emit-anonymous-name(back-end, stream, o);
  end if;
end method;
 
define method emit-name-internal
    (back-end :: <back-end>, stream, o :: <&primitive>) => (name)
  raw-mangle(back-end, o.binding-name)
end method;

define method emit-iep-name
    (back-end :: <back-end>, stream, o :: <&function>) => (name)
  concatenate(emit-namestring(back-end, stream, o), $iep-suffix);
end method;

define method emit-name-internal
    (back-end :: <back-end>, stream, o :: <&iep>) => (name)
  emit-iep-name(back-end, stream, o.function);
end method;

define method emit-name-internal
    (back-end :: <back-end>, stream, o :: <&c-function>) => (name)
  if (o.binding-name)
    o.binding-name;
  else
    emit-anonymous-name(back-end, stream, o);
  end if;
end method;

define method emit-name-internal
    (back-end :: <back-end>, stream, o :: <&c-callable-function>)
 => (name)
  if (o.binding-name)
    o.binding-name;
  else // use alternate-name
    concatenate("c_callable_", local-mangle(back-end, o.alternate-name));
  end;
end method;

define method emit-iep-name
    (back-end :: <back-end>, stream, o :: <&c-callable-function>) 
 => (name)
  emit-namestring(back-end, stream, o)
end method;

define method emit-name-internal
    (back-end :: <back-end>, stream, o :: <&raw-type>)
 => (name)
  o.raw-type-c-name;
end method;
