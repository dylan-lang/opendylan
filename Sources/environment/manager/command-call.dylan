Module:    environment-manager
Author:    Hugh Greene
Synopsis:  Controlling the Environment from external sources.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// -=- ERROR CLASSES -=-

// Superclass for all <condition>s here, to facilitate handling.
define abstract class <command-call-condition> (<condition>)
end class;

// This indicates an error in either this file or one of the
// registered functions.  It can also be caught and ignored -- the
// command won't be called but the system will be left stable.
// One of
//   1. No function has been registered for the requested name.
//   2. The function was supplied with too many or too few arguments.
//   3. No method on "coerce-for-command-call" could be found to
//      coerce a given argument to the required parameter type of
//      the looked-up function;
//   4. No method on the function could be found to apply to the
//      coerced arguments (this probably indicates ambiguity in
//      method applicability, which lies outside this file).
define class <command-call-error> (<simple-error>, <command-call-condition>)
end class;



/// -=- CALLING INTO THE ENVIRONMENT VIA LOOKUP -=-

/// -=- Table of commands -=-

/// Table for the functions corresponding to commands.

define constant $environment-command-functions = make(<table>);


/// -=- Registering functions by name -=-

// This function allows users of this module to register functions
// other than those with GFs in this library, in case that's handy.
define function register-command-function
    (name :: <symbol>, func :: <function>)
 => ()
  $environment-command-functions[name]
    := pair(func, #"Returning values is not implemented yet!");
end function;


/// -=- Looking up functions and their arg-info by name -=-

// This function allows servers to know which function to call, based
// on a command sent to them.
// It takes a <symbol> in the hope that most data formats can easily
// be converted to that; other methods could be added if not.
define function lookup-command-function
    (name :: <symbol>)
 => (command-function :: <function>)
  let command-pair
    = element($environment-command-functions, name, default: #f);
  if ( command-pair )
    head(command-pair)
  else
    error(make(<command-call-error>,
      format-string: "lookup-command-function: Unknown command %=",
      format-arguments: name));
  end
end function;


/// Combining lookup with function application, also coercing
/// arguments to the right type (to help, e.g., string-based
/// parsers).

define function lookup-and-coerced-apply
    (fun-name :: <symbol>, args :: <sequence>)
// ---*** Unspecified return value.
  let func = lookup-command-function(fun-name);
  let (req-number, rest?) = function-arguments(func);
  let types = function-specializers(func);
  let args-size = args.size;

  if ( args-size < req-number )
    error(make(<command-call-error>,
      format-string:
        "lookup-and-coerced-apply: too few args for command %=\n"
        "(got %=, need %=)\n%=",
      format-arguments:
        vector(fun-name, args-size, req-number, as(<simple-vector>, args))));
  end if;
  if ( args-size > req-number & ~ rest? )
    error(make(<command-call-error>,
      format-string:
        "lookup-and-coerced-apply: too many args for command %=\n"
        "(got %=, need %=, rest = %=)\n%=",
      format-arguments: 
        vector(fun-name, args-size, req-number, rest?,
               as(<simple-vector>, args))));
  end if;

  let coerced-args = copy-sequence(args); // As args may be immutable.

  debug-message
    ("lookup-and-coerced-apply: calling %=\n"
     "  requiring %=\n  with args %=",
     func, types, coerced-args);

  block(return)
    // Convert the required args and leave any #rest args alone.
    for ( i from 0 below req-number )
      if ( applicable-method?
	     (coerce-for-command-call, types[i], coerced-args[i]) )
        coerced-args[i] := coerce-for-command-call(types[i], coerced-args[i]);
      else
        error(make(<command-call-error>,
          format-string:
            "lookup-and-coerced-apply: could not coerce "
            "argument %d for command %= (%=)\n  from class %= to type %=",
          format-arguments:
            vector
              (i, fun-name, coerced-args[i], coerced-args[i].object-class,
               types[i])));
      end if;
    end for;
    debug-message
      ("lookup-and-coerced-apply: coerced args to %=", coerced-args);

    if ( apply(applicable-method?, func, coerced-args) )
      // There should always be an applicable method here, since we used
      // function introspection to convert the arguments to the correct
      // type.
      apply(func, coerced-args)
    else
      error(make(<command-call-error>,
        format-string:
          "lookup-and-coerced-apply: no applicable method after "
          "coercion for %= with %=",
        format-arguments:
          vector(func, coerced-args)));
    end if;
  end;
end function;



/* ---*** Not used yet

// -=- RETURNING CALLS -=-

// This type is used to indicate what the environment should do with
// output information from the command, if any.
define constant <environment-command-return-action>
  = one-of(
      #"call-editor", // Send a separate command to the editor, if needed.
      #"return-call" // Use environment-return-call() to call-back the client.
    );

// Results from environment commands.
//
// Remove this when environment-commands moves into environment-manager.

define abstract class <environment-result> (<object>)
  // The emulator doesn't handle each-subclass properly :-(
  /* each-subclass */ slot command-name :: <symbol>;
  // We have nothing else to put here yet, nor in subclasses, but we will
  // later on.  Other slots' names should begin with "result-".
end class;


// This function allows callers of environment-return-call to know
// which subclass of <environment-result> to use.
// It takes a symbol since that will already be available from the
// original <environment-command> object.
define function lookup-result-class
    (symbol :: <symbol>)
 => (command-class :: subclass(<environment-result>))
  tail($environment-command-classes[symbol]);
end function;


// The methods on environment-call provided by the environment should
// call environment-return-call on instances of <environment-result>,
// if they wish to return information to the client which called them
// in the first place.  The various listeners using this module define
// methods on this for each class of result, which they use to encode
// and return the information in whatever way is applicable.
//
// These return calls should be asynchronous, on *separate* threads,
// rather than within the extent of the call to environment-call.  This
// is because some IPC mechanisms used might be of the blocking kind,
// and so deadlock.  If the client wants to return the results to *its*
// (external) client synchronously, it must wait for some observable
// effect of the environment-return-call. 
//
// Use lookup-result-class to find out which subclass to make().

define open generic environment-return-call
    (result :: <environment-result>, client-id :: <object>)
 => ();

---*** end of "Not used yet" */
