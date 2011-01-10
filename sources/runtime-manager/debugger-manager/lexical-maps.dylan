module:      dm-internals
synopsis:    Generating lists of live lexical variables from stack frames
author:      Paul "if it moves, debug it" Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// NUMBER-OF-LEXICAL-VARIABLES
//    Returns the number of live lexical variables at the current position
//    in the <call-frame>.
//    TODO: Add a method on <dylan-call-frame>, because this method
//          will return a (potentially) inaccurate value in that case.
//          The method on <dylan-call-frame> should ask the _compiler_
//          how many live variables there are.

define method number-of-lexical-variables
    (application :: <debug-target>, dm-frame :: <call-frame>,
     #key arguments-only? = #f)
      => (i :: <integer>)
  let count = 0;
  local method one-more (x :: <lexical-variable>)
          count := count + 1
  end method;
  if (arguments-only?)
    do-frame-arguments(one-more,
                       application.debug-target-access-path,
                       call-frame-description(application, dm-frame));
  else
    do-frame-lexicals(one-more,
                      application.debug-target-access-path,
                      call-frame-description(application, dm-frame));
  end if;
  count;
end method;


///// LIVE-FRAME-LEXICAL-VARIABLES
//    Returns the variables and their values that are live at the current
//    position in the given <call-frame>.

define method live-frame-lexical-variables
    (application :: <debug-target>, dm-frame :: <call-frame>,
     #key arguments-only? = #f)
       => (vars :: <sequence>, vals :: <sequence>)

  // Find out how many variables we're talking about, and build
  // the vectors to hold the return values.

  let count = number-of-lexical-variables(application, dm-frame);
  let vars = make(<vector>, size: count);
  let vals = make(<vector>, size: count);
  let i = 0;
  let path = application.debug-target-access-path;
  let ap-frame = call-frame-description(application, dm-frame);

  // A method to run for each lexical variable
  local method add-this-one (x :: <lexical-variable>) => ()
          vars[i] := x;
          vals[i] := read-value(path, x.lexical-variable-address);
          i := i + 1;
        end method;

  // Obtain the list.
  if (arguments-only?)
    do-frame-arguments(add-this-one, path, ap-frame);
  else
    do-frame-lexicals(add-this-one, path, ap-frame);
  end if;

  // And return it.
  values(vars, vals);

end method;


///// SEPERATE-ARGUMENTS-AND-LEXICALS
//    A utility function to return separate sequences of call frame
//    arguments, and other lexical variables.

define method seperate-arguments-and-lexicals
    (application :: <debug-target>, call-frame :: <call-frame>)
 => (args :: <sequence>, lex :: <sequence>)

  let ap-frame = call-frame-description(application, call-frame);
  let args = make(<stretchy-vector>);
  let lex = make(<stretchy-vector>);
  let path = application.debug-target-access-path;

  local method add-this-arg (l :: <lexical-variable>) => ()
          add!(args, l)
	end method;

  local method add-this-lex (l :: <lexical-variable>) => ()
          unless(member?(l, args))
            add!(lex, l)
	  end unless
	end method;

  do-frame-arguments(add-this-arg, path, ap-frame);
  do-frame-lexicals(add-this-lex, path, ap-frame);

  values(args, lex);
end method;


///// ACTIVE-DYLAN-LEXICAL-VARIABLES
//    Returns the active dylan variables that are live at the current
//    position in the given <call-frame>.
//    The strategy for this is to obtain the nearest source locator to
//    our current position, and pass this on to the compiler. The compiler
//    will give us a list of model objects for the variables that are
//    in scope (according to the source code). We match the mangled names
//    against the live lexical variables.

define method active-dylan-lexical-variables
    (application :: <debug-target>, dm-frame :: <call-frame>,
     #key arguments-only? = #f)
      => (names :: <sequence>, 
          types :: <sequence>,
          models :: <sequence>, 
          vals :: <sequence>,
          locations :: <sequence>)

  let names = #[];
  let models = #[];
  let vals = #[];
  let types = #[];
  let locs = #[];

  let path = application.debug-target-access-path;
  let ap-frame = call-frame-description(application, dm-frame);
  let thread = dm-frame.frame-associated-thread;

  local method read-value-from-local-location
                  (base-register :: <integer>, indirections :: <sequence>)
	 => (val :: <remote-value>, location :: <remote-location>)
           let val = as-remote-value(0);
           let location = #f;
           block ()
             let r-inactive = 
               enumeration-code-to-register(path, base-register);
             let r-active =
               active-register(path, thread, r-inactive);
             location := r-active;
             val := read-value(path, r-active, stack-frame: ap-frame);
             for (offset in indirections)
               location := byte-indexed-remote-value(val, offset);
               val := read-value(path, location);
	     end for;
	   exception (<remote-access-violation-error>)
             val := as-remote-value(0);
             location := as-remote-value(0);
	   end block;
           values(val, location);
	end method;

  local method defer-to-runtime-information () => ()
          let (arg-vector, lex-vector) =
            seperate-arguments-and-lexicals(application, dm-frame);
          let count =
            if (arguments-only?) 
              size(arg-vector) 
	    else
              size(arg-vector) + size(lex-vector)
	    end if;
          let i = 0;
          names := make(<vector>, size: count);
          models := make(<vector>, size: count);
          vals := make(<vector>, size: count);
          types := make(<vector>, size: count);
          locs := make(<vector>, size: count);
          for (argument in arg-vector)
            names[i] := demangle-local-dylan-name
                          (argument.lexical-variable-name);
            models[i] := #f;
            vals[i] := read-value(path, argument.lexical-variable-address);
            types[i] := #"function-argument";
            locs[i] := argument.lexical-variable-address;
            i := i + 1;
	  end for;
          unless (arguments-only?)
            for (lexical in lex-vector)
              names[i] := demangle-local-dylan-name
                            (lexical.lexical-variable-name);
              models[i] := #f;
              vals[i] := read-value(path, lexical.lexical-variable-address);
              types[i] := #"local-variable";
              locs[i] := lexical.lexical-variable-address;
              i := i + 1;
	    end for
	  end unless;
	end method;

  if (dylan-call-frame?(application, dm-frame))
    let (sym, func, gen) = call-frame-function(application, dm-frame);
    let offset = call-frame-code-offset(application, dm-frame);
    if (sym)
      let (context, lambda) =
        compiled-lambda-in-context-from-symbol(application, sym);
      if (lambda & context)
        let local-variables-from-compiler
          = compiled-lambda-local-variables(lambda, offset);
        let count = size(local-variables-from-compiler);
        let i = 0;
        names := make(<vector>, size: count);
        models := make(<vector>, size: count);
        vals := make(<vector>, size: count);
        types := make(<vector>, size: count);
        locs := make(<vector>, size: count);
        for (local-variable in local-variables-from-compiler)
          let debug-name = local-variable-debug-name(context, local-variable);
          let real-name = 
            local-variable-debug-name-dylan-name(context, debug-name);
          let (base-register, indirections) =
            local-variable-location(context, local-variable);
          names[i] := real-name;
          models[i] := local-variable;
          types[i] := #"local-variable";
          let (val, loc) =
            read-value-from-local-location (base-register, indirections);
          vals[i] := val; locs[i] := loc;
          i := i + 1;
	end for;
      else
        defer-to-runtime-information();
      end if
    end if
  end if;

  values(names, types, models, vals, locs);
end method;


define method number-of-active-dylan-variables
    (application :: <debug-target>, dm-frame :: <call-frame>,
     #key arguments-only? = #f)
      => (count :: <integer>)

  let path = application.debug-target-access-path;
  let ap-frame = call-frame-description(application, dm-frame);
  let count = 0;

  local method defer-to-runtime-information () => ()
          let (arg-vector, lex-vector) =
            seperate-arguments-and-lexicals(application, dm-frame);
          count :=
            if (arguments-only?) 
              size(arg-vector) 
	    else
              size(arg-vector) + size(lex-vector)
	    end if;
	end method;

  if (dylan-call-frame?(application, dm-frame))
    let (sym, func, gen) = call-frame-function(application, dm-frame);
    let offset = call-frame-code-offset(application, dm-frame);
    if (sym)
      let (context, lambda) =
        compiled-lambda-in-context-from-symbol(application, sym);
      if (lambda & context)
        let local-variables-from-compiler
          = compiled-lambda-local-variables(lambda, offset);
        count := size(local-variables-from-compiler);
      else
        defer-to-runtime-information();
      end if
    end if
  end if;

  count
end method;

