Module:    dfmc-macro-expander
Synopsis:  A packaged representation of a compiled macro for use by
           the compiler.
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Macro descriptors encapsulate everything the compiler needs to know
// about a macro in order to be able to call it, a pretty lightweight
// protocol. It needs to know its word given the name of the binding
// under which its visible, and that word's grammar classification 
// (the latter being invariant w.r.t the binding). This is enough
// to construct a call. Finally, an expander function is provided.

define abstract class <macro-descriptor> (<object>)
  constant slot macro-word-class :: <integer>,
    required-init-keyword: word-class:;
  constant slot macro-expander-function :: <function>,
    required-init-keyword: expander-function:;
  // Procedural macros don't need to supply this keyword.
  constant slot macro-referenced-names :: <sequence> = #(),
    init-keyword: referenced-names:;
end class;

define generic macro-word-in-variable-name 
    (desc :: <macro-descriptor>, name :: <symbol>)
 => (word :: <symbol>, word-class :: <integer>);

define generic macro-expander-function
    (desc :: <macro-descriptor>) => (expander :: <function>);

define class <simple-macro-descriptor> (<macro-descriptor>) end;

define method macro-word-in-variable-name 
    (desc :: <macro-descriptor>, name :: <symbol>)
 => (word :: <symbol>, word-class :: <integer>)
  values(name, macro-word-class(desc));
end method;

define class <suffixed-macro-descriptor> (<macro-descriptor>) 
  constant slot macro-suffix :: <string>,
    required-init-keyword: suffix:;
end class;

define method macro-word-in-variable-name 
    (desc :: <suffixed-macro-descriptor>, name :: <symbol>)
 => (word :: <symbol>, word-class :: <integer>)
  let suffix = macro-suffix(desc);
  let main-part = suffixed-name?(name, suffix);
  if (main-part)
    values(main-part, macro-word-class(desc));
  else
    values(name, $unreserved-name-token);
  end;
end method;

define method suffixed-name? 
    (name :: <symbol>, suffix :: <string>) 
 => (main-name-or-false :: false-or(<symbol>))
  let name = as(<string>, name);
  let name-size = size(name);
  let suffix-size = size(suffix);
  // Strict comparison because the suffix on its own isn't valid.
  if (name-size > suffix-size)
    let size-diff = name-size - suffix-size;
    if (compare-suffix-insensitively(suffix, name, size-diff))
      as(<symbol>, copy-sequence(name, end: size-diff));
    else
      #f
    end
  else
    #f
  end
end method;

define function compare-suffix-insensitively 
    (string :: <string>, container :: <string>, suffix-start :: <integer>)
  for (i from suffix-start, c in string, 
       until: as-lowercase(c) ~== as-lowercase(container[i]))
  finally
    if (i = size(container))
      #t
    else
      #f
    end
  end;
end function;

define function expand-macro-call 
    (desc :: <macro-descriptor>, f :: <fragment>) => (#rest results)
  block ()
    macro-expander-function(desc)(#f, f);
  exception (<stack-overflow-error>)
    note(<infinite-aux-rule-recursion-match-error>,
         source-location: fragment-source-location(f),
         macro-name:      fragment-macro(f));
    #f
  end;
end function;

// eof
