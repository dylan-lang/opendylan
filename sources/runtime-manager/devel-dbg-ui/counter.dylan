module:    devel-dbg-ui
synopsis:  A class to keep track of seen and top counts for a function
author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <function-descriptor> (<object>)
//slot function-counters-name :: false-or(<string>); // Not used?
end class;

define class <single-function-descriptor> (<function-descriptor>)
  constant slot function-descriptor-address :: <remote-value>,
    required-init-keyword: address:;
  slot function-descriptor-index :: false-or(<integer>) = #f;
  slot function-descriptor-name :: false-or(<symbol>);
  slot function-descriptor-raw-name :: false-or(<byte-string>);
  slot function-descriptor-binding :: false-or(<symbol>);
  slot function-descriptor-module :: false-or(<symbol>);
  slot function-descriptor-library :: false-or(<symbol>);
  slot function-descriptor-dll :: false-or(<symbol>);
  slot function-descriptor-filename :: false-or(<string>);
  slot function-descriptor-linenumber :: false-or(<integer>);
  slot function-descriptor-aggregates-1 :: <simple-object-vector> = #[];
  slot function-descriptor-aggregates-2 :: <simple-object-vector> = #[];
end class;

define method function-descriptor-index-name (function :: <single-function-descriptor>)
  let index-name
    = if (function-descriptor-index(function))
	format-to-string("[%d]", function-descriptor-index(function))
      else
	""
      end if;
  index-name
end method;

define method function-descriptor-display-name (function :: <single-function-descriptor>)
  let binding = function.function-descriptor-binding;
  let module  = function.function-descriptor-module;
  let library = function.function-descriptor-library;
  let name    = function.function-descriptor-raw-name;
  let dll     = function.function-descriptor-dll;
  let display-name
    = if (binding & module)
	as(<string>, binding)
      elseif (name & dll)
	format-to-string
	  ("%s!%s", as-uppercase(as(<string>, dll)), name)
      else
	"?????"
      end;
  display-name
end method;

define method function-descriptor-locator-name (function :: <single-function-descriptor>)
  let filename = function-descriptor-filename(function);
  if (filename)
    format-to-string("in %s line %d", filename, function-descriptor-linenumber(function))
  else
    ""
  end if
end method;

define method compute-function-descriptor-aggregates
    (function :: <single-function-descriptor>, 
     filter :: <profile-set>, aggregates :: <collection>)
 => (res :: <simple-object-vector>)
  if (profile-set-member?(function, filter))
    #[]
  else
    let aggregates =
      collecting ()
        for (set in aggregates)
	  if (profile-set-member?(function, set))
	    collect(profile-set-function-descriptor(set))
	  end if
	end for
      end collecting;
    if (empty?(aggregates))
      vector(function)
    else
      as(<vector>, aggregates)
    end if;
  end if
end method;

define method initialize
    (descriptor :: <single-function-descriptor>, 
     #key debug-target, filter, aggregates, #all-keys) => ()
  next-method();
  local method false-or-as-symbol (name :: false-or(<string>))
	  name & ~empty?(name) & as(<symbol>, name)
	end method;
  let (name, dll, filename, linenumber)
    = ip-to-name(debug-target, descriptor.function-descriptor-address);
  function-descriptor-name(descriptor)           := false-or-as-symbol(name);
  function-descriptor-raw-name(descriptor)       := name;
  let (binding-name, module-name, library-name) =
    name & demangle-dylan-name(name);
  function-descriptor-binding(descriptor)    := false-or-as-symbol(binding-name);
  function-descriptor-module(descriptor)     := false-or-as-symbol(module-name);
  function-descriptor-library(descriptor)    := false-or-as-symbol(library-name);
  function-descriptor-dll(descriptor)        := dll;
  function-descriptor-filename(descriptor)   := filename & filename-strip-to-leaf(filename);
  function-descriptor-linenumber(descriptor) := linenumber;
  let filter-1     = application-profile-filter-1(debug-target);
  let aggregates-1 = application-profile-aggregates-1(debug-target);
  function-descriptor-aggregates-1(descriptor)
    := compute-function-descriptor-aggregates(descriptor, filter-1, aggregates-1);
  let filter-2     = application-profile-filter-2(debug-target);
  let aggregates-2 = application-profile-aggregates-2(debug-target);
  function-descriptor-aggregates-2(descriptor)
    := compute-function-descriptor-aggregates(descriptor, filter-2, aggregates-2);
end method initialize;

define class <aggregate-function-descriptor> (<function-descriptor>)
  constant slot function-descriptor-set :: <profile-set>,
    required-init-keyword: set:;
end class;

define method function-descriptor-index-name (function :: <aggregate-function-descriptor>)
  ""
end method;

define method function-descriptor-display-name (function :: <aggregate-function-descriptor>)
  profile-set-name(function-descriptor-set(function)) | "?????"
end method;

define method function-descriptor-locator-name (function :: <aggregate-function-descriptor>)
  ""
end method;

//////
// The class <function-counters> associates two counters with a function.
// The function is identified by a <remote-value> which is an address
// that falls inside the address range of the function. The three counters
// kept by the class are:
//
//   seen count:
//     number of times the function appears on the stack.
//   top count:
//     number of times the function appears on the top of the stack.
//   user top count:
//     number of times the function appears above any other user
//     functions on the stack (i.e. only system calls are above it).
//
define abstract primary class <function-counters> (<object>)
  constant slot function-counters-descriptor :: <function-descriptor>,
    required-init-keyword: function:;
  slot function-counters-called :: <integer>    = 0;
  slot function-counters-seen :: <single-float> = 0.0;
  slot function-counters-top :: <single-float>  = 0.0;
end class;

/// FIRST ORDER COUNTERS

define class <first-function-counters> (<function-counters>)
  constant slot function-counters-height :: <integer>       = 0;
  slot function-counters-caller-normalizer :: <integer>     = 0;
  slot function-counters-caller-top-normalizer :: <integer> = 0;
  slot function-counters-callers :: <table>                 = make(<table>);
  slot function-counters-callee-normalizer :: <integer>     = 0;
  slot function-counters-callee-top-normalizer :: <integer> = 0;
  slot function-counters-callees :: <table>                 = make(<table>);
end class;

/// SECOND ORDER COUNTERS

define class <second-function-counters> (<function-counters>)
  slot function-counters-subtotal :: <integer>     = 0;
  slot function-counters-top-subtotal :: <integer> = 0;
end class;

define macro incf 
  { incf(?lhs:expression) }
    => { ?lhs := ?lhs + 1 }
  { incf(?lhs:expression, ?value:expression) }
    => { ?lhs := ?lhs + ?value }
end macro;

