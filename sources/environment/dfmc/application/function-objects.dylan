Module:    dfmc-application
Synopsis:  <function-object> environment protocols from the application.
Author:    Paul Howard.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

///// FUNCTION-CALLED-FUNCTIONS (Internal Utility)
//    Given a <dylan-function-object>, returns a sequence of all <dylan-function-object>s
//    that might be called by it. This is done using the
//    exported protocol DO-USED-DEFINITIONS, and filtering all
//    non-functions out of the results. This also divides generic
//    functions into their constituent methods.

define method function-called-functions
    (application :: <dfmc-application>, func :: <dylan-function-object>)
 => (callee-sequence :: <sequence>)

  let sequences = make(<stretchy-vector>, size: 0);
  let project = application.server-project;

  local method collect-if-function (def :: <environment-object>) => ()
          select (def by instance?)
            <method-object> =>
              add!(sequences, vector(def));
            <generic-function-object> =>
              add!(sequences, 
                   generic-function-object-methods(project, def) | #[]);
            otherwise =>
              #f;
          end select;
        end method;

  do-used-definitions(collect-if-function, project, func);
  apply(concatenate, #[], sequences);
end method;


///// FUNCTION-AT-TOP-OF-STACK (Internal Utility)
//    Tries to find a <function-object> for the function being called
//    at the top of the stack for a particular thread.

define method function-at-top-of-stack
    (application :: <dfmc-application>, thread :: <thread-object>)
 => (func :: false-or(<function-object>))
  let project = application.server-project;
  let trace = thread-complete-stack-trace(application, thread);
  block (return)
    for (frame in trace)
      let frame-type = stack-frame-type(application, frame);
      select (frame-type)
        #"dylan-call" => return(stack-frame-function(project, frame));
        #"foreign-call" => return(#f);
        otherwise => #f;
      end select;
    end for;
    #f
  end block;
end method;


///// FUNCTION-PARAMETERS (Environment Protocol Method)
//    Returns descriptions of the parameters for a function.
//    The method for GENERIC-FUNCTION-OBJECT

define method function-parameters
    (application :: <dfmc-application>, func :: <generic-function-object>)
 => (required :: <parameters>,
     rest :: false-or(<parameter>),
     keys :: <optional-parameters>,
     all-keys? :: <boolean>,
     next :: false-or(<parameter>),
     values :: <parameters>,
     rest-value :: false-or(<parameter>))

  let target = application.application-target-app;

  // Initialize all the return values to #f, in the hope that we'll update
  // at least some of them during the execution of this function.

  let required = #[];
  let rest = #f;
  let keys = #[];
  let next = #f;
  let vals = #[];
  let rest-value = #f;
  let all-keys? = #f;	//---*** need to get the real value for this!

  // Within a debugger transaction, inspect the generic function for its
  // signature, and its list of methods. Construct all of the return
  // values while still within the debugger transaction (because it might
  // involve the interning of proxies and construction of environment
  // objects).

  perform-debugger-transaction
    (target,
     method ()

       let function-proxy =
         ensure-application-value-proxy(application, func);

       // Get the actual value of the function object from its interned
       // proxy.

       let function-value =
         runtime-proxy-to-remote-value(application, function-proxy);

       // Call the DM's inspector for generic functions,

       let (sig, methods) = 
         remote-generic-function-inspect(target, function-value);

       // Call the DM's inspector for the signature. We are not interested
       // in the method list for the time being, although there may come
       // a time when we have to look at the signatures for all the methods,
       // in order to get a union of all accepted keywords. Nasty.

       let (reqtypes, valtypes, rtype, kwds, ktypes) =
         remote-signature-inspect(target, sig);

       // We now know the size of the returned sequences.

       required := make(<vector>, size: size(reqtypes));
       vals := make(<vector>, size: size(valtypes));

       // Iterate over the required arguments, generating <parameter> objects
       // with invented names (based on the counter, since we don't know
       // their real names from the runtime), and environment objects for the
       // types.

       for (i from 0 below size(reqtypes))
         let name-i = format-to-string("r%d", i);
         let type-i = 
           make-environment-object-for-runtime-value
             (application, reqtypes[i]);
         required[i] := make(<parameter>, name: name-i, type: type-i);
       end for;

       // Iterate over the return value types in the same way.

       for (i from 0 below size(valtypes))
         let name-i = format-to-string("v%d", i);
         let type-i = 
           make-environment-object-for-runtime-value
             (application, valtypes[i]);
         vals[i] := make(<parameter>, name: name-i, type: type-i);
       end for;

       // If there's a typed #rest argument, build the parameter for it.

       if (rtype)
         let name-r = "args";
         let type-r = 
           make-environment-object-for-runtime-value
             (application, rtype);
         rest := make(<parameter>, name: name-r, type: type-r);
       end if;

       // If there are keyword arguments present, construct the optional
       // parameters for them.

       if (kwds)
         keys := make(<vector>, size: size(kwds));
         for (k from 0 below size(kwds))
           let name-k = dylan-keyword-name(target, kwds[k]);
           let type-k =
             make-environment-object-for-runtime-value
                (application, ktypes[k]);
           let default-k = #f;  // GF's can't have defaulted keyword args!!
                                // Only methods can.
	   //---*** need to get the real keyword
	   let key-k = name-k;
           keys[k] := make(<optional-parameter>,
                           name: name-k, 
			   keyword: key-k,
                           type: type-k, 
                           default-value: default-k);
         end for
       end if;

     end method);

  // Assume that we've "legalized" the return values as far as possible.
  values(required, rest, keys, all-keys?, next, vals, rest-value);
end method;


///// FUNCTION-PARAMETERS (Environment Protocol Method)
//    Returns descriptions of the parameters for a function.
//    The method for METHOD-OBJECT

define method function-parameters
    (application :: <dfmc-application>, func :: <method-object>)
 => (required :: <parameters>,
     rest :: false-or(<parameter>),
     keys :: <optional-parameters>,
     all-keys? :: <boolean>,
     next :: false-or(<parameter>),
     values :: <parameters>,
     rest-value :: false-or(<parameter>))

  let target = application.application-target-app;

  // Initialize all the return values to #f, in the hope that we'll update
  // at least some of them during the execution of this function.

  let required = #[];
  let rest = #f;
  let keys = #[];
  let next = #f;
  let vals = #[];
  let rest-value = #f;
  let all-keys? = #f;	//---*** need to get the real value for this!

  // GET-KEYWORD-DEFAULT
  // Given a keyword, and a sequence of alternating keywords and values,
  // return the value for the keyword, if it is a member of the sequence.

  local method get-keyword-default (kw :: <remote-value>, kspec :: <sequence>)
                       => (default :: false-or(<remote-value>))
    let limit = size(kspec) - 1;
    let i = 0;
    let found = #f;
    while ((~found) & (i < limit))
      if (kw = kspec[i])
        found := kspec[i + 1];
      else
        i := i + 2;
      end if;
    end while;
    found;
  end method;

  // Within a debugger transaction, inspect the method for its
  // signature, and its keyword specifiers. Construct all of the return
  // values while still within the debugger transaction (because it might
  // involve the interning of proxies and construction of environment
  // objects).

  perform-debugger-transaction
    (target,
     method ()

       let function-proxy =
         ensure-application-value-proxy(application, func);

       // Get the actual value of the function object from its interned
       // proxy.

       let function-value =
         runtime-proxy-to-remote-value(application, function-proxy);

       // Call the DM's inspector for methods.

       let (sig, iep, keyword-specifiers) = 
         remote-method-inspect(target, function-value);

       // Call the DM to inspect the signature. From there, we follow most
       // of the same procedure as for generic functions.

       let (reqtypes, valtypes, rtype, kwds, ktypes) =
         remote-signature-inspect(target, sig);

       // We now know the size of the returned sequences.

       required := make(<vector>, size: size(reqtypes));
       vals := make(<vector>, size: size(valtypes));

       // Iterate over the required arguments, generating <parameter> objects
       // with invented names (based on the counter, since we don't know
       // their real names from the runtime), and environment objects for the
       // types.

       for (i from 0 below size(reqtypes))
         let name-i = format-to-string("r%d", i);
         let type-i = 
           make-environment-object-for-runtime-value
             (application, reqtypes[i]);
         required[i] := make(<parameter>, name: name-i, type: type-i);
       end for;

       // Iterate over the return value types in the same way.

       for (i from 0 below size(valtypes))
         let name-i = format-to-string("v%d", i);
         let type-i = 
           make-environment-object-for-runtime-value
             (application, valtypes[i]);
         vals[i] := make(<parameter>, name: name-i, type: type-i);
       end for;

       // If there's a typed #rest argument, build the parameter for it.

       if (rtype)
         let name-r = "args";
         let type-r = 
           make-environment-object-for-runtime-value
             (application, rtype);
         rest := make(<parameter>, name: name-r, type: type-r);
       end if;

       // If there are keyword arguments present, construct the optional
       // parameters for them. Since this is a method, we may also have a
       // default value in the list of keyword-specifiers from the method
       // object.

       if (kwds)
         keys := make(<vector>, size: size(kwds));
         for (k from 0 below size(kwds))
           let name-k = dylan-keyword-name(target, kwds[k]);
           let type-k =
             make-environment-object-for-runtime-value
                (application, ktypes[k]);
           let default-k = get-keyword-default(kwds[k], keyword-specifiers);
	   //---*** Need to get the real value for this!
	   let key-k = name-k;
           keys[k] := make(<optional-parameter>,
                           name: name-k, 
			   keyword: key-k,
                           type: type-k, 
                           default-value: default-k);
         end for
       end if;

     end method);

  // Assume that we've "legalized" the return values as far as possible.
  values(required, rest, keys, all-keys?, next, vals, rest-value);
end method;


///// DO-GENERIC-FUNCTION-METHODS (Environment Protocol Method)
//    Iterates a function over the <method-objects>s belonging to a
//    <generic-function-object>.

define method do-generic-function-methods
    (do-this-one :: <function>, application :: <dfmc-application>,
     gf :: <generic-function-object>,
     #key client)
 => ()

  let target = application.application-target-app;

  // Within a debugger transaction, call the DM to inspect the generic
  // function object, thus obtaining a list of <remote-value>s for the
  // methods. From each of these, intern a proxy and construct the
  // environment object. Apply the supplied function to the environment
  // object.

  perform-debugger-transaction
     (target,
      method ()
        let generic-proxy =
          ensure-application-value-proxy(application, gf);
        let generic-value = 
          runtime-proxy-to-remote-value(application, generic-proxy);

        // Inspect the generic function.
        let (sig, method-values) = 
          remote-generic-function-inspect(target, generic-value);

	debug-message("Generic function: signature %=, methods %=",
		      sig, method-values);

	//---*** andrewa: this won't match up with the same methods
	//---*** from the compiler database. :-(
	do-environment-objects-for-runtime-values
	  (do-this-one, application, method-values)
      end method);

end method;


///// METHOD-SPECIALIZERS (Environment Protocol Method)
//    Returns a sequence of the specializers for a method.

define method method-specializers
    (application :: <dfmc-application>, meth :: <method-object>)
 => (specializers :: <sequence>)

  let specializers = #[];
  let target = application.application-target-app;

  // Within a debugger transaction, inspect the method object for its
  // signature. Inspect the signature for its required argument types,
  // these being the specializers for the method concerned. Go through the
  // usual trauma of interning a runtime proxy for each one, and building
  // the appropriate environment object.

  perform-debugger-transaction
     (target,
      method ()

        let method-proxy =
          ensure-application-value-proxy(application, meth);

        let method-value =
          runtime-proxy-to-remote-value(application, method-proxy);

        // Call the DM's special inspector for methods. This function
        // returns three values, but the signature is the first value, and
        // that's the only one we want.

        let sig = remote-method-inspect(target, method-value);

        // Call the DM's special inspector for signatures. This function
        // returns several values, bu we are only interested in the first,
        // so that is the only one we bind.

        let reqtypes = remote-signature-inspect(target, sig);

        // Now we know how big the return sequence is.

        specializers := make(<vector>, size: size(reqtypes));

        // The usual story. Iterate over the sequence of specializers, and
        // turn each one into a kosher environment object.

        for (i from 0 below size(reqtypes))
           specializers[i] :=
             make-environment-object-for-runtime-value
                (application, reqtypes[i]);
        end for;
      end method);

  // Return the sequence of specializers.
  specializers;  
end method;


///// METHOD-GENERIC-FUNCTION (Environment Protocol Method)
//    Returns the generic function that a method belongs to.
//    TODO: This method returns #f, because the application server cannot
//          implement it. Method objects in the runtime do not contain
//          back-pointers to their parent GF objects. It will be necessary
//          for the environment to link methods to a generic function
//          at a higher level, or use the compiler-database backend, which
//          will work in most cases
//---*** andrewa: surely we could do something by demangling the function
//---*** name? What else can we do when debugging an exe project?

define method method-generic-function
    (application :: <dfmc-application>, meth :: <method-object>)
 => (parent-gf :: false-or(<generic-function-object>))
  debug-message("Ignoring method-generic-function in application server");
  #f
end method;


///// DO-USED-DEFINITIONS (Environment Protocol Method)
//    The application server cannot serve this operation, but defines a
//    dummy method anyway.

define method do-used-definitions
    (operation :: <function>, application :: <dfmc-application>,
     func :: <dylan-function-object>,
     #key client = #f, modules = #f, libraries = #f)
 => ()
  // Ef off!
end method;


///// DO-CLIENT-SOURCE-FORMS (Environment Protocol Method)
//    The application server cannot serve this operation, but defines a
//    dummy method anyway.

define method do-client-source-forms
    (operation :: <function>, application :: <dfmc-application>,
     func :: <dylan-function-object>,
     #key client = #f, modules = #f, libraries = #f)
 => ()
  // Ef off!
end method;
