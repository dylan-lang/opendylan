module: dfmc-harp-browser-support
Synopsis: HARP back end for the debug info browsing routines
Author: Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// *permit-leaf-frames?* controls whether the debug-info interface permits 
/// local variable information for leaf-case function frames to be 
/// made available.

define variable *permit-leaf-frames?* = #t;



define sideways method compiled-lambda-symbolic-name 
    (context :: dfmc-<library-description>, compiled-lambda :: <harp-compiled-lambda>) 
    => (name :: false-or(<byte-string>))
  compiled-lambda.lambda-name;
end method;


define sideways method back-end-symbolic-name-compiled-lambda
    (back-end :: <harp-back-end>, 
     context :: dfmc-<library-description>, 
     name :: <byte-string>,
     #key file-name) 
    => (compiled-lambda :: false-or(<compiled-lambda>))
  let leaf-name = file-name.file-name-leaf-stem;
  block (found)
    local method check (debug-data)
	    if (instance?(debug-data, <string-table>))
	      let lookup = element(debug-data, name, default: #f);
	      if (lookup) found(lookup) end;
	    end if;
	  end;
    check(context.dfmc-library-combined-back-end-data);
    for (cr in context.dfmc-library-description-compilation-records)
      let cr-name = cr.dfmc-compilation-record-name.as-lowercase;
      matching-file-name?(cr-name, leaf-name)
	& check(cr.dfmc-compilation-record-back-end-data);
    end for;
    #f;
  end block;
end method;



/// source mapping



define method nearest-mapping-for-code-offset
    (locs, code-offset :: <integer>)
     => (nearest :: false-or(<relative-source-position>),
         difference :: <integer>);
  let nearest = #f;
  let difference :: <integer> = $maximum-integer;
  for (loc :: <relative-source-position> in locs)
    let loc-offset = loc.function-relative-code-position;
    if (loc-offset <= code-offset)
      let this-diff = code-offset - loc-offset;
      if (this-diff <= difference)
        nearest := loc;
        difference := this-diff;
      end if;
    end if;
  end for;
  values(nearest, difference);
end method;



define method nearest-mapping-for-line 
    (locs, wanted-line :: <integer>)
     => (nearest :: false-or(<relative-source-position>),
         difference :: <integer>);
  let nearest = #f;
  let difference :: <integer> = $maximum-integer;
  local method record(i :: <integer>)
	  let loc :: <relative-source-position> = locs[i];
	  let line = loc.function-relative-line-number;
	  let this-diff = abs(line - wanted-line);
	  if (this-diff < difference)
	    nearest := loc;
	    difference := this-diff;
	  end if;
	end method;
  for (i :: <integer> from 0 below locs.size)
    record(i)
  end for;
  values(nearest, difference);
end method;

define primary class <candidate-lambda>(<object>)
  slot candidate-lambda = #f;
  slot candidate-offset = #f;
  slot candidate-diff :: <integer> = $maximum-integer;
end class;

define inline function update-candidate-lambda
    (candidate :: <candidate-lambda>, lambda, offset, diff :: <integer>)
  if (diff < candidate.candidate-diff)
    candidate.candidate-lambda := lambda;
    candidate.candidate-offset := offset;
    candidate.candidate-diff := diff;
  end if;
end function;

define sideways method back-end-source-position-compiled-lambda
    (back-end :: <harp-back-end>, 
     context :: dfmc-<library-description>, 
     sr :: dfmc-<source-record>,
     line-no :: <integer>,
     #key interactive-only? = #t) 
    => (compiled-lambda :: false-or(<compiled-lambda>),
        code-offset :: false-or(<integer>))

  // Make initalization-code separate from internal-code
  let lambda :: <candidate-lambda> = make(<candidate-lambda>);
  let init-lambda :: <candidate-lambda> = make(<candidate-lambda>);

  let debug-data
    = context.dfmc-library-combined-back-end-data
        | begin
	    let cr = dfmc-source-record-compilation-record
	               (context, sr, default: #f);
	    cr & cr.dfmc-compilation-record-back-end-data
	  end;
  when (instance?(debug-data, <string-table>))
    for (cl :: <compiled-lambda> in debug-data)
      let cl-pos = cl.lambda-location;
      if (cl-pos)
	let cl-start = cl-pos.source-record-start-line;
	let cl-end   = cl-pos.source-record-end-line;
	if ((line-no >= cl-start) & (line-no <= cl-end))
	  let rel-pos = line-no - cl-start;
	  let locs = compiled-lambda-locators(cl, #t, interactive-only?);
	  let (loc, diff) = nearest-mapping-for-line(locs, rel-pos);
	  if (loc)
	    let offset =  loc.function-relative-code-position;
	    let cl-name = cl.lambda-name;
	    let init-lambda? = cl-name & cl-name.init-lambda?;
	    update-candidate-lambda
	      (if (init-lambda?) init-lambda else lambda end, cl, offset, diff)
	  end if;
	end if;
      end if;
    end for;
  end when;
  // internal-code breakpoints override initialization-code breakpoints
  let compiled-lambda = lambda.candidate-lambda;
  if (compiled-lambda)
    values(compiled-lambda, lambda.candidate-offset);
  else
    let compiled-lambda = init-lambda.candidate-lambda;
    if (compiled-lambda)
      values(compiled-lambda, init-lambda.candidate-offset);
    else
      values(#f, #f)
    end;
  end;
end method;

// Hack for determining initialization-code

define constant $init-code-marker = "_Init_";

define method init-lambda?(name :: <byte-string>) => (init-lambda? :: <boolean>)
  subsequence-position(name, $init-code-marker) = 0
end method;

define sideways method compiled-lambda-source-location
    (compiled-lambda :: <harp-compiled-lambda>, code-offset :: <integer>,
     #key exact? = #f,
          line-only? = #t,
          interactive-only? = #t) 
    => (source-location :: false-or(<source-location>),
        exact? :: <boolean>)
  let abs-loc = compiled-lambda.lambda-location;
  if (abs-loc)
    if (code-offset == 0)
      values(absolute-locator-source-location(abs-loc), #t)
    else
      let locs = compiled-lambda-locators(compiled-lambda, line-only?, interactive-only?);
      let (nearest, difference) = nearest-mapping-for-code-offset(locs, code-offset);
      let found-exact? = difference == 0;
      if (nearest & (found-exact? | ~ exact?))
        values(relative-locator-source-location(abs-loc, nearest), found-exact?);
      else values(#f, #f);
      end if;
    end if;
  else
    values(#f, #f);
  end if;
end method;



define sideways method compiled-lambda-code-offset
    (compiled-lambda :: <harp-compiled-lambda>, source-location :: <source-location>,
     #key exact? = #f,
          line-only? = #t,
          interactive-only? = #t) 
    => (code-offset :: false-or(<integer>))
  let abs-loc = compiled-lambda.lambda-location;
  if (abs-loc & (source-location.source-location-source-record
                 = abs-loc.source-position-source-record))
    let abs-line = source-location.source-location-start-line;
    let base-line = abs-loc.start-offset-into-source-record;
    let wanted-line = abs-line - base-line;
    let locs = compiled-lambda-locators(compiled-lambda, line-only?, interactive-only?);
    let (nearest, difference) = nearest-mapping-for-line(locs, wanted-line);
    let found-exact? = difference == 0;
    if (nearest & (found-exact? | ~ exact?))
      nearest.function-relative-code-position;
    else #f
    end if;
  else #f
  end if;
end method;



define sideways method compiled-lambda-mapped-source-locations
    (compiled-lambda :: <harp-compiled-lambda>, 
     #key line-only? = #t,
          interactive-only? = #t) 
    => (locations :: <sequence>)
  let abs-loc = compiled-lambda.lambda-location;
  if (abs-loc)
    let locs = compiled-lambda-locators(compiled-lambda, line-only?, interactive-only?);
    map(curry(relative-locator-source-location, abs-loc), locs);
  else #[];
  end if;
end method;


define sideways method compiled-lambda-mapped-code-offsets
    (compiled-lambda :: <harp-compiled-lambda>, 
     #key line-only? = #t,
          interactive-only? = #t) 
    => (code-offsets :: <sequence>)
  let abs-loc = compiled-lambda.lambda-location;
  if (abs-loc)
    let locs = compiled-lambda-locators(compiled-lambda, line-only?, interactive-only?);
    map(function-relative-code-position, locs);
  else #[];
  end if;
end method;

define function absolute-locator-source-location
    (abs-loc :: <absolute-source-position>) => (loc :: <source-location>)
  let line = abs-loc.start-offset-into-source-record;
  make-line-location(abs-loc.source-position-source-record, line);
end function;

define function relative-locator-source-location 
    (abs-loc :: <absolute-source-position>, 
     rel-loc :: <relative-source-position>) 
    => (location :: <source-location>);
  let line = abs-loc.start-offset-into-source-record + rel-loc.function-relative-line-number;
  make-line-location(abs-loc.source-position-source-record, line);
end function;

define function compiled-lambda-locators 
    (compiled-lambda :: <harp-compiled-lambda>, 
     line-only? :: <boolean>, 
     interactive-only? :: <boolean>) => (locs :: <simple-object-vector>)
  ignore(interactive-only?);
  if (line-only?) 
    compiled-lambda.lambda-selected-locators;
  else compiled-lambda.lambda-all-locators;
  end if;
end function;


/// local variables



define sideways method local-variable-argument?
    (context :: dfmc-<library-description>, var :: <named-variable>)
    => (variable? :: <boolean>)
  #f;  // TO DO: improve this
end method;


define constant $demangler = make(dfmc-<demangler>);

define sideways method back-end-local-variable-debug-name-dylan-name
    (back-end :: <harp-back-end>, 
     context :: dfmc-<library-description>, 
     symbolic-name :: <byte-string>) 
    => (dylan-name :: <byte-string>)
  dfmc-demangle-name-locally($demangler, symbolic-name);
end method;




define sideways method local-variable-debug-name 
    (context :: dfmc-<library-description>, var :: <named-variable>)
    => (name :: <byte-string>)
  var.unique-variable-name;
end method;

define sideways method local-variable-debug-name 
    (context :: dfmc-<library-description>, var :: <variable-indirections>)
    => (name :: <byte-string>)
  var.unique-variable-name;
end method;


define sideways method local-variable-location
    (context :: dfmc-<library-description>, var :: <variable-in-register>)
    => (base-reg :: <integer>, indirections :: <simple-object-vector>)
  values(var.variable-register-enumeration, #[]);
end method;


define sideways method local-variable-location
    (context :: dfmc-<library-description>, var :: <variable-in-frame-spill>)
    => (base-reg :: <integer>, indirections :: <simple-object-vector>)
  let back-end = context.context-back-end;
  values(real-register-debug-info-enumeration(back-end, back-end.registers.reg-frame),
         vector(var.variable-frame-pointer-offset));
end method;


define sideways method local-variable-location
    (context :: dfmc-<library-description>, var :: <variable-in-leaf-spill>)
    => (base-reg :: <integer>, indirections :: <simple-object-vector>)
  let back-end = context.context-back-end;
  values(real-register-debug-info-enumeration(back-end, back-end.registers.reg-stack),
         vector(var.variable-frame-pointer-offset));
end method;


define sideways method local-variable-location
    (context :: dfmc-<library-description>, var :: <variable-indirections>)
    => (base-reg :: <integer>, indirections :: <sequence>)
  let (base :: <integer>, ind1 :: <simple-object-vector>) = next-method();
  let indirs = var.variable-indirections;
  unless (indirs.size == 1)
    error("Harp browser support: Failed singularization of indirect variables");
  end unless;
  values(base, concatenate(ind1, local-variable-sub-indirections(indirs[0])));
end method;



define method local-variable-sub-indirections 
    (var) => (offsets :: <simple-object-vector>)
  #[];
end method;


define method local-variable-sub-indirections 
    (var :: <named-indirection>) => (offsets :: <simple-object-vector>)
  vector(var.variable-indirection-offset);
end method;


define method local-variable-sub-indirections 
    (var :: <indirections-variable-in-indirection>) => (offsets :: <simple-object-vector>)
  let indirs = var.variable-indirections;
  let ind1 = var.variable-indirection-offset;
  unless (indirs.size == 1)
    error("Harp browser support: Failed singularization of indirect variables");
  end unless;
  apply(vector, ind1, local-variable-sub-indirections(indirs[0]));
end method;


define sideways method compiled-lambda-local-variables
    (compiled-lambda :: <harp-compiled-lambda>, code-offset :: <integer>)
    => (local-variables :: <sequence>)
  block (return)
    let all-scopes :: <simple-object-vector> = compiled-lambda.lambda-all-variable-scopes;
    let all-vars :: <simple-object-vector> = compiled-lambda.lambda-all-variable-names;
    local method vars-in-scope-at-offset
	      (scopes :: <debug-scopes>, vars :: <vector-32bit>) => (vars :: <list>)
	    for-debug-scope(scope in scopes of all-scopes)
	      if (code-offset >= scope.start-code-offset)
		if (code-offset <= scope.end-code-offset)
		  if (scope.acceptable-scope?)
		    vars-in-scope-at-offset(scope.nested-scopes,
					    concatenate-variables(vars, scope.named-variables));
		  else
		    return(process-variables-to-singularize(debug-vars-as-list(vars, all-vars)))
		  end if;
		end if
	      else 
		return(process-variables-to-singularize(debug-vars-as-list(vars, all-vars)))
	      end if;
	    end for-debug-scope;
            return(process-variables-to-singularize(debug-vars-as-list(vars, all-vars)))
    end method;

    vars-in-scope-at-offset
      (compiled-lambda.lambda-variable-scopes-internal, make(<vector-32bit>));
  end block;
end method;


define function process-variables-to-singularize 
    (vars :: <list>) => (processed-vars :: <list>)
  // Instances of <variable-indirections> might correspond to multiple
  // variables. But clients of this interface expect a flattened sequence
  // of variables rather than a hierarchy. Perform the flattening here.
  if (vars.empty?)
    vars
  else
    let new-tail = vars.tail.process-variables-to-singularize;
    let this = vars.head;
    if (instance?(this, <variable-indirections>) & (this.variable-indirections.size ~= 1))
      let seen = new-tail;
      for (sub in this.variable-indirections)
	let var-for-sub = copy-var-with-indirections(this, sub);
	seen := pair(var-for-sub, seen);
      end for;
      seen;
    elseif (vars.tail == new-tail)
      vars;
    else pair(this, new-tail);
    end if;
  end if;
end function;


define method copy-var-with-indirections
    (this :: <indirections-variable-in-spill>, sub) => (new :: <variable-indirections>)
  make(<indirections-variable-in-spill>, 
       offset: this.variable-frame-pointer-offset,
       name: this.harp-variable-name,
       indirections: vector(sub));
end method;

define method copy-var-with-indirections
    (this :: <indirections-variable-in-leaf-spill>, sub) => (new :: <variable-indirections>)
  make(<indirections-variable-in-leaf-spill>, 
       offset: this.variable-frame-pointer-offset,
       name: this.harp-variable-name,
       indirections: vector(sub));
end method;

define method copy-var-with-indirections
    (this :: <indirections-variable-in-register>, sub) => (new :: <variable-indirections>)
  make(<indirections-variable-in-register>, 
       enumeration: this.variable-register-enumeration,
       name: this.harp-variable-name,
       indirections: vector(sub));
end method;

define method copy-var-with-indirections
    (this :: <indirections-variable-in-indirection>, sub) => (new :: <variable-indirections>)
  make(<indirections-variable-in-indirection>, 
       indirection-offset: this.variable-indirection-offset,
       indirections: vector(sub));
end method;


define method unique-variable-name 
    (var :: <named-variable>) => (name :: <byte-string>)
  var.harp-variable-name;
end method;

define method unique-variable-name 
    (var :: <variable-indirections>) => (name :: <byte-string>)
  unique-variable-name(var.variable-indirections[0]);
end method;


define sideways method compiled-lambda-frame-boundaries
    (compiled-lambda :: <harp-compiled-lambda>)
    => (start-offsets :: <sequence>, stop-offsets :: <sequence>)
  frame-boundaries-for-scopes
  (compiled-lambda.lambda-variable-scopes-internal,
   compiled-lambda.lambda-all-variable-scopes);
end method;

define function frame-boundaries-for-scopes
    (scopes :: <debug-scopes>, all-scopes :: <simple-object-vector>)
    => (start-offsets :: <list>, stop-offsets :: <list>)
  let (remaining-starts, remaining-stops) = values(#(), #());

  for-reversed-debug-scope(scope in scopes of all-scopes)
    if (scope.debug-scope-with-frame?)
      remaining-starts := pair(scope.start-code-offset, remaining-starts);
      remaining-stops := pair(scope.end-code-offset, remaining-stops);
    end if;
  end for-reversed-debug-scope;

  values(remaining-starts, remaining-stops);

end function;

define method acceptable-scope? 
    (scope :: <debug-scope>) => (acceptable? :: <boolean>)
  if (scope.debug-scope-with-frame?)
    #t;
  else
    *permit-leaf-frames?*;
  end if;
end method;




/// components



define sideways method back-end-compilation-context-initializer-symbolic-name
    (back-end :: <harp-back-end>, 
     context :: dfmc-<library-description>)
    => (symbolic-name :: <byte-string>, component-name :: <byte-string>)
  let plain-name = context.dfmc-library-description-emit-name;
  let emit-name = as-lowercase(as(<byte-string>, plain-name));
  let component-name = context.compilation-context-component-name;
  values(concatenate($init-code-marker, dfmc-raw-mangle(back-end, emit-name), "_"),
         component-name);
end method;



/// Some file matching utilities



define generic file-name-leaf-stem 
    (file-name) => (leaf-name :: false-or(<byte-string>));

define method file-name-leaf-stem 
    (file-name :: <byte-string>) => (leaf-name :: <byte-string>)
  // Not sure whether there's a utility for doing this short of 
  // using the locators library. Here's a home grown version with
  // only limited portability
  let name-size = file-name.size;
  let (leaf-start, ext-start) = file-name.file-name-stem-limits;
  if ((leaf-start == 0) & (ext-start == name-size))
    file-name;
  else
    let leaf-end = if (ext-start > leaf-start) 
                     ext-start
                   else name-size;
                   end if;
    copy-sequence(file-name, start: leaf-start, end: leaf-end);
  end if;
end method;

define method file-name-leaf-stem 
    (file-name :: <object>) => (leaf-name :: singleton(#f))
  #f;
end method;


define function file-name-stem-limits
    (file-name :: <byte-string>) 
    => (leaf-start :: <integer>,
        ext-start :: <integer>)
  let name-size = file-name.size;
  let leaf-start :: <integer> = 0;
  let ext-start :: <integer> = name-size;
  for (i from 0 below name-size)
    select (file-name[i])

      // Assume that "." is the only possible extension delimiter
      '.' 
        => ext-start := i;

      // But allow for Unix & MS-DOS pathname & drivename separators
      '/', '\\', ':'
        => leaf-start := i + 1;

      otherwise => #f;
    end select;
  end for;
  values(leaf-start, ext-start);
end function;



define generic matching-file-name?
    (name1, name2) => (matching? :: <boolean>);

define method matching-file-name?
    (name1 :: <byte-string>, name2 :: <byte-string>) => (matching? :: <boolean>)
  case-insensitive-equal(name1, name2);
end method;

define method matching-file-name?
    (name1, name2) => (matching? :: <boolean>)
  if (name1 & name2) #f else #t end;
end method;


