module:        dm-internals
synopsis:      Unpicking and inspection of Dylan objects.
author:        Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// <object-type-description> refers to the "type" of a dylan object
// instance (ie whether it is an integer, simple-object-vector, string,
// some user-defined class etc...)

define constant <object-type-description> = <integer>;

define constant $unbound-type                          = 0;
define constant $double-integer-type                   = 1;
define constant $machine-integer-type                  = 2;
define constant $unsigned-machine-integer-type         = 3;
define constant $single-float-type                     = 4;
define constant $string-type                           = 5;
define constant $vector-type                           = 6;
define constant $pair-type                             = 7;
define constant $empty-list-type                       = 8;
define constant $symbol-type                           = 9;
define constant $class-type                            = 10;
define constant $instance-slot-descriptor-type         = 11;
define constant $repeated-slot-descriptor-type         = 12;
define constant $method-type                           = 13;
define constant $generic-function-type                 = 14;
define constant $boolean-false                         = 15;
define constant $boolean-true                          = 20;
define constant $user-defined-type                     = 16;
define constant $integer-type                          = 17;
define constant $character-type                        = 18;
define constant $unknown-type                          = 19;

define constant $simple-warning-type                   = 21;
define constant $simple-error-type                     = 22;
define constant $simple-restart-type                   = 23;

define constant $singleton-type                        = 25;
define constant $stretchy-vector-type                  = 26;
define constant $thread-type                           = 27;
define constant $table-type                            = 28;
define constant $keyword-signature-type                = 30;
define constant $infinite-range-type                   = 31;
define constant $finite-range-type                     = 32;
define constant $constant-range-type                   = 33;
define constant $empty-range-type                      = 34;
define constant $virtual-slot-descriptor-type          = 35;
define constant $class-slot-descriptor-type            = 36;
define constant $each-subclass-slot-descriptor-type    = 37;

define constant $simple-method-type                    = 38;
define constant $keyword-method-type                   = 39;
define constant $accessor-method-type                  = 40;

define constant $signature+values-type                     = 41;
define constant $signature+values+rest-value-type          = 42;
define constant $signature+rest-value-type                 = 43; 
define constant $keyword-signature+values-type             = 44;
define constant $keyword-signature+values+rest-value-type  = 45;
define constant $keyword-signature+rest-value-type         = 46;
define constant $signature-required-only-type              = 47;

define constant $union-type                           = 48;
define constant $subclass-type                        = 49;
define constant $bottom-type                          = 50;

define constant $string-table-type                    = 51;
define constant $deque-type                           = 52;
define constant $double-float-type                    = 53;
define constant $array-type                           = 54;
define constant $limited-array-type                   = 55;
define constant $limited-vector-type                  = 56;
define constant $limited-simple-vector-type           = $limited-vector-type;
define constant $limited-stretchy-vector-type         = 57;

define constant $maximum-print-length                  = 1;
define constant $maximum-print-level                   = 0;


///// TRUNCATED-DYLAN-STRING-DATA
//    A utility wrapper of the accessor DYLAN-STRING-DATA that cuts
//    the length of the string to some "sane" maximum size.

define constant $string-clip-limit = 500;

define method truncated-dylan-string-data
    (application :: <debug-target>, str :: <remote-value>)
      => (str :: <string>)
  let (str, clipped?) = 
    dylan-string-data(application, str, clip-at: $string-clip-limit);
  if (clipped?)
    concatenate(str, "...")
  else
    str
  end if;
end method;


///// FIND-LIBRARY-CALLED
//    Attempts to find a <remote-library> whose name matches the
//    supplied string. Returns #f if no matching library is found.

define method find-library-called
    (application :: <debug-target>, core-name :: <string>)
       => (maybe-lib :: <remote-library>)
  let foundit = #f;
  let first = #f;
  block (exit)
    do-libraries
     (method (l :: <remote-library>)
        unless (first)
          first := l
        end unless;
        if ((as-uppercase(core-name) = as-uppercase(l.library-core-name)) |
            (as-uppercase(concatenate("hqn-", core-name)) =
                                       as-uppercase(l.library-core-name)))
          foundit := l;
          exit();
        end if;
      end method,
      application.debug-target-access-path);
  end block;
  foundit | first;
end method;


///// CLASSIFY-DYLAN-OBJECT
//    Slightly more general than the above function, returns the type of
//    an instance, whether it be a tagged immediate or a pointer.

define method classify-dylan-object 
    (application :: <debug-target>, instance :: <remote-value>,
     #key address? :: <boolean>)
 => (type-descr :: <object-type-description>)
  case
    ~application.dylan-application? =>
      $unknown-type;
    ~address? & instance = head(application.last-classified-object) =>
      tail(application.last-classified-object);
    otherwise =>
      let tag
	= if (address?)
	    $dylan-tag-pointer
	  else
	    inspect-instance-tag(application, instance)
	  end;
      let answer
	= select (tag)
	    $dylan-tag-integer =>
	      $integer-type;
	    $dylan-tag-character =>
	      $character-type;
	    $dylan-tag-boolean =>
	      let sod = application.static-object-directory;
	      if (sod.booleans-tagged?)
		select (instance by \=)
		  sod.canonical-false-object =>
		    $boolean-false;
		  sod.canonical-true-object =>
		    $boolean-true;
		  otherwise =>
		    $unknown-type;
		end select
	      else
		$unknown-type;
	      end if;
	    $dylan-tag-pointer =>
	      let sod = application.static-object-directory;
	      if (instance = sod.canonical-false-object)
		$boolean-false;
	      elseif (instance = sod.canonical-true-object)
		$boolean-true;
	      elseif (instance = sod.canonical-empty-list-object)
		$empty-list-type;
	      elseif (instance = sod.canonical-unbound-object)
		$unbound-type;
	      elseif (dylan-object?(application, instance, address?: address?))
		let (wrapper, read-ok) 
		  = read-instance-header(application, instance);
		get-type-from-wrapper(application, wrapper)
	      else
		$unknown-type
	      end if;
	  end select;
      unless (address?)
	application.last-classified-object := pair(instance, answer)
      end;
      answer;
  end case
end method;


///// MOST-APPROPRIATE-NAME-FOR-METHOD
//    Given a method, return a string that should be used to name it.
//    If the method has a precise symbolic name, then that will be
//    used.

define method most-appropriate-name-for-method
    (application :: <debug-target>, method-instance :: <remote-value>)
  => (name :: <string>)
  let (lib, mod, object-name, exact?) =
    dylan-instance-symbolic-name(application, method-instance);
  if (exact?)
    object-name
  else
    let (signature, entry-point, key-specifiers) =
      remote-method-inspect(application, method-instance);
    let (lib, mod, entry-point-name, exact?) =
      dylan-instance-symbolic-name(application, entry-point);
    if (exact?)
      entry-point-name
    else
      "anonymous"
    end if
  end if
end method;


///// FLATTEN-TYPE-UNION-MEMBERS
//    Given a type union, returns all of the members of the union by
//    canonicalizing it. This allows the union to be printed naturally,
//    without an unpleasant recursive representation.

define method flatten-type-union-members
     (application :: <debug-target>, union :: <remote-value>)
  => (seq :: <sequence>)
  let seq = make(<stretchy-vector>, size: 1);
  let union-first :: <remote-value> = 
    dylan-union-type-first-member(application, union);
  let union-second :: <remote-value> = 
    dylan-union-type-second-member(application, union);
  seq[0] := union-second;
  while (classify-dylan-object(application, union-first) == $union-type)
    let new-union = union-first;
    add!(seq, dylan-union-type-second-member(application, new-union));
    union-first := dylan-union-type-first-member(application, new-union);
  end while;
  add!(seq, union-first);
  seq;
end method;


///// CLASSIFY-TYPE-UNION-FROM-MEMBERS
//    Given a flattened type-union, classify it as being either a normal
//    type-union, or a special case such as FALSE-OR or ONE-OF.

define method classify-type-union-from-members
    (application :: <debug-target>, members :: <sequence>)
  => (classification :: one-of(#"normal", #"one-of", #"false-or"))
  let classification = #"normal";

  // If every member of the union is a singleton type, classify it
  // as a #"one-of".
  block (exit)
    for (m in members)
      unless (classify-dylan-object(application, m) == $singleton-type)
        exit()
      end unless
    end for;
    classification := #"one-of"
  end block;

  // If we didn't re-classify this as a #"one-of", then check to see if
  // it's a FALSE-OR.
  if ((classification == #"normal") & (members.size == 2))
    if (singleton-false-type?(application, members[0]) |
        singleton-false-type?(application, members[1]))
      classification := #"false-or";
    end if;
  end if;
  classification;
end method;


///// SINGLETON-FALSE-TYPE?
//    Convenience function.

define method singleton-false-type?
    (application :: <debug-target>, instance :: <remote-value>)
  => (well? :: <boolean>)
  (classify-dylan-object(application, instance) == $singleton-type) &
  (classify-dylan-object
     (application, 
        dylan-singleton-object(application, instance)) == $boolean-false)
end method;


///// DYLAN-PRINTABLE-REPRESENTATION
//    Generic Function.
//    Given an instance, and a description of its "general" type (such as
//    simple-object-vector, integer or user-defined type), produces a
//    printable representation of the object.


define generic dylan-printable-representation
    (type :: <object-type-description>, application :: <debug-target>,
     instance :: <remote-value>, length :: <integer>, level :: <integer>,
     level-now :: <integer>, 
     #key decorate? :: <boolean> = #t, 
          format :: false-or(<symbol>))
 => (rep :: <string>);

///// Catch-all method for unimplemented cirumstances.....
//    (Print as a user-defined type).

define method dylan-printable-representation
    (type :: <object-type-description>, application :: <debug-target>,
     instance :: <remote-value>, length :: <integer>, level :: <integer>,
     level-now :: <integer>, 
     #key decorate? :: <boolean> = #t,
          format :: false-or(<symbol>) = #f) 
 => (rep :: <string>)
  dylan-printable-representation
      ($user-defined-type, application, instance, length, level, level-now,
       decorate?: decorate?, format: format);
end method;


///// Specific methods

define method dylan-printable-representation
    (type == $simple-warning-type, application :: <debug-target>,
     instance :: <remote-value>, length :: <integer>, level :: <integer>,
     level-now :: <integer>, 
     #key decorate? :: <boolean> = #t, 
          format :: false-or(<symbol>) = #f)
 => (rep :: <string>)
  let format-string =
    dylan-simple-condition-format-string(application, instance);
  let remote-format-args =
    dylan-simple-condition-format-arguments(application, instance);
  if (classify-dylan-object(application, format-string) == $string-type)
    let format-arg-seq = 
      canonicalize-sequence(application, remote-format-args);
    let uploaded-format-string =
      truncated-dylan-string-data(application, format-string);
    let formatted-message =
      apply(remote-format-to-string, application, uploaded-format-string,
            format-arg-seq);
    concatenate("{<simple-warning>: ", formatted-message, "}");
  else
    "{<simple-warning>: <No legal format string>}"
  end if
end method;

define method dylan-printable-representation
    (type == $simple-error-type, application :: <debug-target>,
     instance :: <remote-value>, length :: <integer>, level :: <integer>,
     level-now :: <integer>, 
     #key decorate? :: <boolean> = #t,
          format :: false-or(<symbol>) = #f)
 => (rep :: <string>)
  let format-string =
    dylan-simple-condition-format-string(application, instance);
  let remote-format-args =
    dylan-simple-condition-format-arguments(application, instance);
  if (classify-dylan-object(application, format-string) == $string-type)
    let format-arg-seq = 
      canonicalize-sequence(application, remote-format-args);
    let uploaded-format-string =
      truncated-dylan-string-data(application, format-string);
    let formatted-message =
      apply(remote-format-to-string, application, uploaded-format-string,
            format-arg-seq);
    concatenate("{<simple-error>: ", formatted-message, "}");
  else
    "{<simple-error>: <No legal format string>}"
  end if
end method;

define method dylan-printable-representation
    (type == $simple-restart-type, application :: <debug-target>,
     instance :: <remote-value>, length :: <integer>, level :: <integer>,
     level-now :: <integer>, 
     #key decorate? :: <boolean> = #t,
          format :: false-or(<symbol>) = #f)
 => (rep :: <string>)
  let format-string =
    dylan-simple-condition-format-string(application, instance);
  let remote-format-args =
    dylan-simple-condition-format-arguments(application, instance);
  if (classify-dylan-object(application, format-string) == $string-type)
    let format-arg-seq = 
      canonicalize-sequence(application, remote-format-args);
    let uploaded-format-string =
      truncated-dylan-string-data(application, format-string);
    let formatted-message =
      apply(remote-format-to-string, application, uploaded-format-string,
            format-arg-seq);
    concatenate("{<simple-restart>: ", formatted-message, "}");
  else
    "{<simple-restart>: <No legal format string>}"
  end if
end method;

define method dylan-printable-representation
    (type == $union-type, application :: <debug-target>,
     instance :: <remote-value>, length :: <integer>, level :: <integer>,
     level-now :: <integer>, 
     #key decorate? :: <boolean> = #t,
          format :: false-or(<symbol>) = #f) 
 => (rep :: <string>)

  local method print-members (members :: <sequence>, dec? :: <boolean>) 
              => (printed :: <string>)
          let initial = 
            dude-print-dylan-object(application, members[0],
                                    length, level, level-now + 1,
                                    decorate?: dec?);
          let others = make(<vector>, size: members.size - 1);
          for (i from 1 below members.size)
            others[i - 1] := 
              format-to-string(", %s",
                 dude-print-dylan-object
                   (application, members[i], length, level, level-now + 1,
                    decorate?: dec?));
          end for;
          apply(concatenate, initial, others);
        end method;

  local method print-the-other-one (members :: <sequence>) => (s :: <string>)
          let the-other-one =
            if (singleton-false-type?(application, members[0]))
              members[1]
            else
              members[0]
            end if;
          dude-print-dylan-object
            (application, the-other-one, length, level, level-now + 1,
             decorate?: decorate?);
        end method;

  let seq = flatten-type-union-members(application, instance);
  if (decorate?)
    let opening = "{<union>: ";
    let middle = print-members(seq, decorate?);
    let closing = "}";
    format-to-string("%s%s%s", opening, middle, closing);
  else
    let classification = classify-type-union-from-members(application, seq);
    select (classification)
      #"false-or" =>
        format-to-string("false-or(%s)", print-the-other-one(seq));
      #"one-of" =>
        let new-seq = 
          map(curry(dylan-singleton-object, application), seq);
        format-to-string("one-of(%s)", print-members(new-seq, #t));
      #"normal" =>
        format-to-string("type-union(%s)", print-members(seq, #f));
    end select;
  end if;
end method;

define method dylan-printable-representation
    (type == $subclass-type, application :: <debug-target>,
     instance :: <remote-value>, length :: <integer>, level :: <integer>,
     level-now :: <integer>, 
     #key decorate? :: <boolean> = #t,
          format :: false-or(<symbol>) = #f) 
 => (rep :: <string>)
  let c = dylan-subclass-type-class(application, instance);
  let c-printed =
    dude-print-dylan-object(application, c, length, level, level-now + 1,
                            decorate?: decorate?);
  if (decorate?)
    format-to-string("{<subclass>: of %s}", c-printed);
  else
    format-to-string("(subclass %s)", c-printed);
  end if;
end method;

define method dylan-printable-representation
    (type == $bottom-type, application :: <debug-target>,
     instance :: <remote-value>, length :: <integer>, level :: <integer>,
     level-now :: <integer>, 
     #key decorate? :: <boolean> = #t,
          format :: false-or(<symbol>) = #f) 
 => (rep :: <string>)
  if (decorate?)
    "{<bottom-type>}"
  else
    "_|_"
  end if
end method;

define method dylan-printable-representation
    (type == $symbol-type, application :: <debug-target>,
     instance :: <remote-value>, length :: <integer>, level :: <integer>,
     level-now :: <integer>, 
     #key decorate? :: <boolean> = #t,
          format :: false-or(<symbol>) = #f) 
 => (rep :: <string>)

  let str = dylan-symbol-name (application, instance);
  let strpr = dylan-printable-representation
                    ($string-type, application, str,
                     length, level, level-now, decorate?: decorate?);
  if (decorate?)
    format-to-string ("#%s", strpr);
  else
    strpr
  end if
end method;

define method dylan-printable-representation
    (type == $boolean-true, application :: <debug-target>,
     instance :: <remote-value>, length :: <integer>, level :: <integer>,
     level-now :: <integer>, 
     #key decorate? :: <boolean> = #t,
          format :: false-or(<symbol>) = #f) 
 => (rep :: <string>)

  "#t"
end method;

define method dylan-printable-representation
    (type == $boolean-false, application :: <debug-target>,
     instance :: <remote-value>, length :: <integer>, level :: <integer>,
     level-now :: <integer>, 
     #key decorate? :: <boolean> = #t,
          format :: false-or(<symbol>) = #f) 
 => (rep :: <string>)
  "#f"
end method;

define method dylan-printable-representation 
    (type == $integer-type, application :: <debug-target>,
     instance :: <remote-value>, length :: <integer>, level :: <integer>,
     level-now :: <integer>, 
     #key decorate? :: <boolean> = #t,
          format :: false-or(<symbol>) = #f) 
 => (rep :: <string>)
  integer-to-string(dylan-integer-data(application, instance),
		    base: select (format)
			    #"binary" =>  2;
			    #"octal"  =>  8;
			    #"hex"    => 16;
			    otherwise => 10;
			  end)
end method;

define method dylan-printable-representation 
    (type == $character-type, application :: <debug-target>,
     instance :: <remote-value>, length :: <integer>, level :: <integer>,
     level-now :: <integer>, 
     #key decorate? :: <boolean> = #t,
          format :: false-or(<symbol>) = #f) 
 => (rep :: <string>)

  local method char-code-as-string (i :: <integer>) => (s :: <string>)
          let hex = format-to-string("%x", i);
          // Hack because %2x doesn't work as a format directive in
          // functional-extensions, but I _have_ to use functional-extensions
          // because the format library can't print double floats or
          // big integers.
          if (hex.size == 1)
            hex := concatenate("0", hex);
          end if;
          format-to-string("\\<%s>", hex)
        end method;

  let char = dylan-character-data(application, instance);
  let char-as-string = add!("", char);
  if (char < ' ')
    select (char)
      '\a' => char-as-string := "\\a";
      '\b' => char-as-string := "\\b";
      '\e' => char-as-string := "\\e";
      '\f' => char-as-string := "\\f";
      '\n' => char-as-string := "\\n";
      '\r' => char-as-string := "\\r";
      '\t' => char-as-string := "\\t";
      '\0' => char-as-string := "\\0";
      otherwise =>
        let char-code = as(<integer>, char);
        char-as-string := char-code-as-string(char-code);
        if (char-as-string[2] == ' ')
          char-as-string[2] := '0';  // Pad with a zero, not a space!
        end if;
    end select;
  end if;
  if (decorate?)
    format-to-string ("'%s'", char-as-string)
  else
    char-as-string
  end if
end method;

define method dylan-printable-representation 
    (type == $string-type, application :: <debug-target>,
     instance :: <remote-value>,
     length :: <integer>, level :: <integer>,
     level-now :: <integer>, 
     #key decorate? :: <boolean> = #t,
	  format :: false-or(<symbol>) = #f)
 => (rep :: <string>)
  if (decorate?)
    format-to-string("\"%s\"", 
		     truncated-dylan-string-data(application, instance));
  else
    truncated-dylan-string-data(application, instance);
  end if;
end method;

define method dylan-printable-representation
    (type == $empty-list-type, application :: <debug-target>,
     instance :: <remote-value>,
     length :: <integer>, level :: <integer>,
     level-now :: <integer>, 
     #key decorate? :: <boolean> = #t,
	  format :: false-or(<symbol>) = #f)
 => (rep :: <string>)
  if (decorate?)
    "#()";
  else
    ""
  end if;
end method;

define method dylan-printable-representation
    (type == $unbound-type, application :: <debug-target>,
     instance :: <remote-value>,
     length :: <integer>, level :: <integer>,
     level-now :: <integer>, 
     #key decorate? :: <boolean> = #t,
	  format :: false-or(<symbol>) = #f)
 => (rep :: <string>)
  "{value unbound}";
end method;

define method dylan-printable-representation
    (type == $singleton-type, application :: <debug-target>,
     instance :: <remote-value>,
     length :: <integer>, level :: <integer>,
     level-now :: <integer>, 
     #key decorate? :: <boolean> = #t,
	  format :: false-or(<symbol>) = #f)
 => (rep :: <string>)
  
  let (singleton-instance, ok?) = 
    dylan-singleton-object(application, instance);
  let singleton-printed =
    dude-print-dylan-object(application, singleton-instance,
                            length, level, level-now + 1, 
                            decorate?: decorate?);
  if (decorate?)
    format-to-string("{<singleton>: %s}", singleton-printed);
  else
    format-to-string("== %s", singleton-printed);
  end if;
end method;

define method dylan-printable-representation
    (type == $simple-method-type, application :: <debug-target>,
     instance :: <remote-value>,
     length :: <integer>, level :: <integer>,
     level-now :: <integer>, 
     #key decorate? :: <boolean> = #t,
          format :: false-or(<symbol>) = #f)
 => (rep :: <string>)
  let name
    = most-appropriate-name-for-method(application, instance);

  let sig = dylan-lambda-signature(application, instance);

  if (decorate?)
    format-to-string("{<method>: %s %s}",
                     name,
                     dude-print-dylan-object
                       (application, 
                        sig,
                        length,
                        level,
                        level-now + 1,
                        decorate?: decorate?))
  else
    name
  end if;
end method;

define method dylan-printable-representation
    (type == $keyword-method-type, application :: <debug-target>,
     instance :: <remote-value>,
     length :: <integer>, level :: <integer>,
     level-now :: <integer>, 
     #key decorate? :: <boolean> = #t,
          format :: false-or(<symbol>) = #f)
 => (rep :: <string>)
  let name
    = most-appropriate-name-for-method(application, instance);

  let sig = dylan-lambda-signature(application, instance);

  if (decorate?)
    format-to-string("{<method>: %s %s}",
                     name,
                     dude-print-dylan-object
                       (application, 
                        sig,
                        length,
                        level,
                        level-now + 1,
                        decorate?: decorate?))
  else
    name
  end if;
end method;

define method signature-printable-representation
    (application :: <debug-target>, instance :: <remote-value>,
     length :: <integer>, level :: <integer>, 
     level-now :: <integer>, 
     #key decorate? :: <boolean> = #t,
          format :: false-or(<symbol>) = #f)
 => (rep :: <string>)
  let required-bit = "";
  let values-bit = "";
  let keys-bit = "";
  let rest-bit = "";
  let maybe-space = "";
  let first? = #t;

  local method this-one (x :: <remote-value>) => (str :: <string>)
    let str = 
      if (first?)
        first? := #f;
        ""
      else
        ", "
      end if;
    str := concatenate(str,
                       dude-print-dylan-object
                         (application, x, length, level, level-now + 1,
                          decorate?: #f));
    str;
  end method;

  let (sig-required-types,
       sig-value-types,
       sig-rest-type,
       sig-keys,
       sig-key-types)
    = remote-signature-inspect(application, instance);

  first? := #t;
  for (x in sig-required-types)
    required-bit := concatenate(required-bit, this-one(x))
  end for;

  first? := #t;
  for (x in sig-value-types)
    values-bit := concatenate(values-bit, this-one(x));
    maybe-space := " ";
  end for;

  first? := #t;
  if (sig-rest-type)
    rest-bit := concatenate(maybe-space, "#rest ", this-one(sig-rest-type))
  end if;  

  first? := #t;
  if (sig-keys)
    keys-bit := " #key ";
    for (x in sig-keys)
      keys-bit := concatenate(keys-bit, this-one(x))
    end for
  end if;

  format-to-string("(%s%s) => (%s%s)", required-bit, keys-bit,
                   values-bit, rest-bit)
end method;

define method dylan-printable-representation
    (type == $signature-required-only-type, 
     application :: <debug-target>,
     instance :: <remote-value>, 
     length :: <integer>, level :: <integer>,
     level-now :: <integer>, 
     #key decorate? :: <boolean> = #t,
          format :: false-or(<symbol>) = #f)
 => (rep :: <string>)
  signature-printable-representation
     (application, instance, length, level, level-now, decorate?: decorate?)
end method;

define method dylan-printable-representation
    (type == $keyword-signature-type, 
     application :: <debug-target>,
     instance :: <remote-value>, 
     length :: <integer>, level :: <integer>,
     level-now :: <integer>, 
     #key decorate? :: <boolean> = #t,
          format :: false-or(<symbol>) = #f)
 => (rep :: <string>)
  signature-printable-representation
     (application, instance, length, level, level-now, decorate?: decorate?)
end method;

define method dylan-printable-representation
    (type == $signature+values-type, 
     application :: <debug-target>,
     instance :: <remote-value>, 
     length :: <integer>, level :: <integer>,
     level-now :: <integer>, 
     #key decorate? :: <boolean> = #t,
          format :: false-or(<symbol>) = #f)
 => (rep :: <string>)
  signature-printable-representation
     (application, instance, length, level, level-now, decorate?: decorate?)
end method;

define method dylan-printable-representation
    (type == $signature+values+rest-value-type, 
     application :: <debug-target>,
     instance :: <remote-value>, 
     length :: <integer>, level :: <integer>,
     level-now :: <integer>, 
     #key decorate? :: <boolean> = #t,
          format :: false-or(<symbol>) = #f)
 => (rep :: <string>)
  signature-printable-representation
     (application, instance, length, level, level-now, decorate?: decorate?)
end method;

define method dylan-printable-representation
    (type == $signature+rest-value-type, 
     application :: <debug-target>,
     instance :: <remote-value>, 
     length :: <integer>, level :: <integer>,
     level-now :: <integer>, 
     #key decorate? :: <boolean> = #t,
          format :: false-or(<symbol>) = #f)
 => (rep :: <string>)
  signature-printable-representation
     (application, instance, length, level, level-now, decorate?: decorate?)
end method;

define method dylan-printable-representation
    (type == $keyword-signature+values-type, 
     application :: <debug-target>,
     instance :: <remote-value>, 
     length :: <integer>, level :: <integer>,
     level-now :: <integer>, 
     #key decorate? :: <boolean> = #t,
          format :: false-or(<symbol>) = #f)
 => (rep :: <string>)
  signature-printable-representation
     (application, instance, length, level, level-now, decorate?: decorate?)
end method;

define method dylan-printable-representation
    (type == $keyword-signature+values+rest-value-type, 
     application :: <debug-target>,
     instance :: <remote-value>, 
     length :: <integer>, level :: <integer>,
     level-now :: <integer>, 
     #key decorate? :: <boolean> = #t,
          format :: false-or(<symbol>) = #f)
 => (rep :: <string>)
  signature-printable-representation
     (application, instance, length, level, level-now, decorate?: decorate?)
end method;

define method dylan-printable-representation
    (type == $keyword-signature+rest-value-type, 
     application :: <debug-target>,
     instance :: <remote-value>, 
     length :: <integer>, level :: <integer>,
     level-now :: <integer>, 
     #key decorate? :: <boolean> = #t,
          format :: false-or(<symbol>) = #f)
 => (rep :: <string>)
  signature-printable-representation
     (application, instance, length, level, level-now, decorate?: decorate?)
end method;

define method dylan-printable-representation
    (type == $generic-function-type, application :: <debug-target>,
     instance :: <remote-value>,
     length :: <integer>, level :: <integer>,
     level-now :: <integer>, 
     #key decorate? :: <boolean> = #t,
          format :: false-or(<symbol>) = #f)
 => (rep :: <string>)
  
  let (lib, mod, name, exact?) 
    = dylan-instance-symbolic-name (application, instance);

  unless (exact?)
    name := "anonymous"
  end unless;

  if (decorate?)
    let sig = dylan-lambda-signature(application, instance);
    let printed-sig = 
      dude-print-dylan-object(application, sig, length, level, level-now + 1);
    format-to-string ("{<generic-function>: %s %s}", name, printed-sig);
  else
    name
  end if
end method;

define method dylan-printable-representation
    (type == $single-float-type, application :: <debug-target>,
     instance :: <remote-value>,
     length :: <integer>, level :: <integer>,
     level-now :: <integer>, 
     #key decorate? :: <boolean> = #t,
          format :: false-or(<symbol>) = #f)
 => (rep :: <string>)
  format-to-string("%=", dylan-single-float-data(application, instance))
end method;

define method dylan-printable-representation
    (type == $double-float-type, application :: <debug-target>,
     instance :: <remote-value>,
     length :: <integer>, level :: <integer>,
     level-now :: <integer>, 
     #key decorate? :: <boolean> = #t,
	  format :: false-or(<symbol>) = #f)
 => (rep :: <string>)
  format-to-string("%=", dylan-double-float-data(application, instance))
end method;

define method dylan-printable-representation
    (type == $machine-integer-type, application :: <debug-target>,
     instance :: <remote-value>,
     length :: <integer>, level :: <integer>,
     level-now :: <integer>, 
     #key decorate? :: <boolean> = #t,
	  format :: false-or(<symbol>) = #f)
 => (rep :: <string>)
  let (mw, ok?) =
    dylan-machine-word-data(application, instance);
  let printed-mw =
    remote-value-as-string(application.debug-target-access-path, mw, 16);
  if (decorate?)
    concatenate("#x", printed-mw);
  else
    concatenate("#x", printed-mw);
  end if;
end method;

define method dylan-printable-representation
    (type == $double-integer-type, application :: <debug-target>,
     instance :: <remote-value>,
     length :: <integer>, level :: <integer>,
     level-now :: <integer>, 
     #key decorate? :: <boolean> = #t,
	  format :: false-or(<symbol>) = #f)
 => (rep :: <string>)
  let double-int
    = dylan-double-integer-data(application, instance);
  format-to-string("%d", double-int);
end method;

define method dylan-printable-representation
    (type == $class-type, application :: <debug-target>,
     instance :: <remote-value>,
     length :: <integer>, level :: <integer>,
     level-now :: <integer>, 
     #key decorate? :: <boolean> = #t,
          format :: false-or(<symbol>) = #f)
 => (rep :: <string>)

  local method create-comma-separated-class-list (classes :: <sequence>)
                 => (result :: <string>)
          if (classes.size == 0)
            ""
          elseif (classes.size == 1)
            dylan-printable-representation
              ($class-type, application, classes[0], length, level,
               level-now, decorate?: #f)
          else
            let initial = 
              dylan-printable-representation
                ($class-type, application, classes[0], length, level,
                 level-now, decorate?: #f);
            let others = make(<vector>, size: classes.size - 1);
            for (i from 1 below classes.size)
              others[i - 1] :=
                format-to-string(", %s",
                  dylan-printable-representation
                    ($class-type, application, classes[0], length, level,
                     level-now, decorate?: #f));
            end for;
            apply(concatenate, initial, others);
          end if;
        end method;

  let (lib, mode, name, exact?)
    = dylan-instance-symbolic-name(application, instance);
  let undecorated =
    if (exact?)
      name
    else
      let (dirsubs, dirsups, allsups, dirslots, allslots, dirmeths)
        = remote-class-inspect(application, instance);
      format-to-string("anonymous subclass of (%s)",
                       create-comma-separated-class-list(dirsups));
    end if;
  if (decorate?)
    format-to-string ("{<class>: %s}", undecorated);
  else
    undecorated
  end if
end method;

define method dylan-printable-representation
    (type == $user-defined-type, application :: <debug-target>,
     instance :: <remote-value>,
     length :: <integer>, level :: <integer>,
     level-now :: <integer>, 
     #key decorate? :: <boolean> = #t,
	  format :: false-or(<symbol>) = #f)
 => (rep :: <string>)
  
  let (wrapper, ok) = read-instance-header (application, instance);
  if (ok)
    let (cl, ok) = wrapper-to-class (application, wrapper);
    if (ok)
      format-to-string("{%s}",
                       dylan-printable-representation
                         ($class-type, application, cl, length, level,
                          level-now, decorate?: #f));
    else
      "{could not print this object}"
    end if
  else
    "{could not print this object}"
  end if
end method;

define method dylan-table-printable-representation
    (type :: <object-type-description>, application :: <debug-target>,
     instance :: <remote-value>,
     length :: <integer>, level :: <integer>,
     level-now :: <integer>, 
     #key decorate? :: <boolean> = #t,
	  format :: false-or(<symbol>) = #f) 
 => (rep :: <string>)
  let opener = 
    if (type == $string-table-type)
      "{<string-table>: "
    else
      "{<object-table>: "
    end if;
  let maps-to = " => ";
  let seperator = ", ";
  let ellipsis = ", ...";
  let closer = "}";
  let (key-seq, val-seq) = remote-table-inspect(application, instance);
  let (max, truncating?)
     = if (level-now > level)
         values(0, #t)
       elseif(key-seq.size > length)
         values(length, #t)
       else
         values(key-seq.size, #f)
       end if;

  let first? = #t;

  local method this-one(x :: <remote-value>, y :: <remote-value>)
                         => (rep :: <string>)
    let maybe-sep =
      if (first?) first? := #f; "" else seperator end if;
    let printed-x
      = dude-print-dylan-object(application, x, length, level,
                                level-now + 1, decorate?: decorate?);
    let printed-y
      = dude-print-dylan-object(application, y, length, level,
                                level-now + 1, decorate?: decorate?);
    format-to-string("%s%s%s%s",
                     maybe-sep, printed-x, maps-to, printed-y);
  end method;

  if ((max == 0) & (~truncating?))
    format-to-string("%s%s%s", opener, "size 0", closer)
  else
    let elements = "";
    for (i from 0 below max)
      elements := concatenate(elements, this-one(key-seq[i], val-seq[i]))
    end for;
    if (truncating?)
      format-to-string("%s%s%s%s", opener, elements, ellipsis, closer)
    else
      format-to-string("%s%s%s", opener, elements, closer)
    end if
  end if
end method;

define method dylan-printable-representation
    (type == $table-type, application :: <debug-target>,
     instance :: <remote-value>,
     length :: <integer>, level :: <integer>,
     level-now :: <integer>, 
     #key decorate? :: <boolean> = #t,
	  format :: false-or(<symbol>) = #f) 
 => (rep :: <string>)
  dylan-table-printable-representation
    (type, application, instance, length, level, level-now,
     decorate?: decorate?)
end method;

define method dylan-printable-representation
    (type == $string-table-type, application :: <debug-target>,
     instance :: <remote-value>,
     length :: <integer>, level :: <integer>,
     level-now :: <integer>, 
     #key decorate? :: <boolean> = #t,
	  format :: false-or(<symbol>) = #f) 
 => (rep :: <string>)
  dylan-table-printable-representation
    (type, application, instance, length, level, level-now,
     decorate?: decorate?)
end method;

define method dylan-printable-representation
    (type == $unknown-type, application :: <debug-target>,
     instance :: <remote-value>,
     length :: <integer>, level :: <integer>,
     level-now :: <integer>, 
     #key decorate? :: <boolean> = #t,
          format :: false-or(<symbol>) = #f)
 => (rep :: <string>)

  if (decorate?)
    format-to-string ("{? 0x%s}", 
      remote-value-as-string(application.debug-target-access-path,
                             instance, 
                             16));
  else
    format-to-string("?0x%s",
      remote-value-as-string(application.debug-target-access-path,
                             instance,
                             16));
  end if
end method;

define method dylan-printable-representation
   (type == $thread-type, application :: <debug-target>,
    instance :: <remote-value>,
    length :: <integer>, level :: <integer>,
    level-now :: <integer>, 
    #key decorate? :: <boolean> = #t,
         format :: false-or(<symbol>) = #f)
 => (rep :: <string>)
  let name-object = dylan-thread-name(application, instance);
  let name-object-type = classify-dylan-object(application, name-object);
  if (name-object-type == $string-type)
    let name-string = truncated-dylan-string-data(application, name-object);
    if (decorate?)
      format-to-string("{<thread>: \"%s\"}", name-string);
    else
      name-string
    end if
  else
    if (decorate?) "{<thread>: anonymous}" else "un-named" end if;
  end if;
end method;

define method dylan-printable-representation 
    (type == $vector-type, application :: <debug-target>,
     instance :: <remote-value>,
     length :: <integer>, level :: <integer>,
     level-now :: <integer>, 
     #key decorate? :: <boolean> = #t,
	  format :: false-or(<symbol>) = #f)
 => (rep :: <string>)
  let opener = if (decorate?) "#[" else "(" end if;
  let closer = if (decorate?) "]" else ")" end if;
  let seperator = if (decorate?) ", " else " " end if;
  let ellipsis = "...";
  let elstring = "";
  if (level-now > level)
    format-to-string("%s%s%s", opener, ellipsis, closer);
  else
    let elements = 
      canonicalize-sequence
        (application, instance, max-size-override: length + 1);
    let num-elements = size(elements);
    let (max, truncating?) =
      if (num-elements > length)
        values(length, #t)
      else
        values(num-elements, #f)
      end if;
    let first-element? = #t;
    for (i from 0 below max)
      if (first-element?)
        first-element? := #f
      else
        elstring := concatenate(elstring, seperator)
      end if;
      let element-printed = 
        dude-print-dylan-object(application, elements[i], length, level,
                                level-now + 1, decorate?: decorate?);
      elstring := concatenate(elstring, element-printed);
    end for;
    if (truncating?)
      format-to-string("%s%s%s%s%s",
                       opener, elstring, seperator, ellipsis, closer)
    else
      format-to-string("%s%s%s",
                       opener, elstring, closer)
    end if;
  end if;
end method;

define method dylan-printable-representation 
    (type == $limited-vector-type, application :: <debug-target>,
     instance :: <remote-value>,
     length :: <integer>, level :: <integer>,
     level-now :: <integer>, 
     #key decorate? :: <boolean> = #t,
	  format :: false-or(<symbol>) = #f)
 => (rep :: <string>)
  let element-type-name
    = dude-print-dylan-object
        (application, dylan-limited-vector-element-type(application, instance), 
	 length, level, level-now + 1, decorate?: decorate?);
  let type-name = if (decorate?) format-to-string("limited(<vector>, of: %s)", element-type-name) end;
  let opener = if (decorate?) format-to-string("{%s: ", type-name) else "(" end if;
  let empty = if (decorate?) format-to-string("{%s: size 0}", type-name) else "()" end if;
  let closer = if (decorate?) "}" else ")" end if;
  let seperator = if (decorate?) ", " else " " end if;
  let ellipsis = "...";
  let elstring = "";
  if (level-now > level)
    format-to-string("%s%s%s", opener, ellipsis, closer);
  else
    let elements = 
      canonicalize-sequence
        (application, instance, max-size-override: length + 1);
    let num-elements = size(elements);
    if (num-elements == 0)
      empty
    else
      let (max, truncating?) =
	if (num-elements > length)
	  values(length, #t)
	else
	  values(num-elements, #f)
	end if;
      let first-element? = #t;
      for (i from 0 below max)
	if (first-element?)
	  first-element? := #f
	else
	  elstring := concatenate(elstring, seperator)
	end if;
	let element-printed = 
	  dude-print-dylan-object(application, elements[i], length, level,
				  level-now + 1, decorate?: decorate?);
	elstring := concatenate(elstring, element-printed);
      end for;
      if (truncating?)
	format-to-string("%s%s%s%s%s",
			 opener, elstring, seperator, ellipsis, closer)
      else
	format-to-string("%s%s%s",
			 opener, elstring, closer)
      end if;
    end if
  end if;
end method;

define method dylan-printable-representation 
    (type == $stretchy-vector-type, application :: <debug-target>,
     instance :: <remote-value>,
     length :: <integer>, level :: <integer>,
     level-now :: <integer>, 
     #key decorate? :: <boolean> = #t,
          format :: false-or(<symbol>) = #f)
 => (rep :: <string>)
  let opener = if (decorate?) "{<stretchy-vector>: " else "(" end if;
  let empty = if (decorate?) "{<stretchy-vector>: size 0}" else "()" end if;
  let closer = if (decorate?) "}" else ")" end if;
  let seperator = if (decorate?) ", " else " " end if;
  let ellipsis = "...";
  let elstring = "";
  if (level-now > level)
    format-to-string("%s%s%s", opener, ellipsis, closer);
  else
    let elements = 
      canonicalize-sequence
        (application, instance, max-size-override: length + 1);
    let num-elements = size(elements);
    if (num-elements == 0)
      empty
    else
      let (max, truncating?) =
        if (num-elements > length)
          values(length, #t)
        else
          values(num-elements, #f)
        end if;
      let first-element? = #t;
      for (i from 0 below max)
        if (first-element?)
          first-element? := #f
        else
          elstring := concatenate(elstring, seperator)
        end if;
        let element-printed = 
          dude-print-dylan-object(application, elements[i], length, level,
                                  level-now + 1, decorate?: decorate?);
        elstring := concatenate(elstring, element-printed);
      end for;
      if (truncating?)
        format-to-string("%s%s%s%s%s",
                         opener, elstring, seperator, ellipsis, closer)
      else
        format-to-string("%s%s%s",
                         opener, elstring, closer)
      end if
    end if
  end if
end method;

define method dylan-printable-representation 
    (type == $limited-stretchy-vector-type, application :: <debug-target>,
     instance :: <remote-value>,
     length :: <integer>, level :: <integer>,
     level-now :: <integer>, 
     #key decorate? :: <boolean> = #t,
          format :: false-or(<symbol>) = #f)
 => (rep :: <string>)
  let element-type-name
    = dude-print-dylan-object
        (application, dylan-limited-stretchy-vector-element-type(application, instance), 
	 length, level, level-now + 1, decorate?: decorate?);
  let type-name = if (decorate?) format-to-string("limited(<stretchy-vector>, of: %s)", element-type-name) end;
  let opener = if (decorate?) format-to-string("{%s: ", type-name) else "(" end if;
  let empty = if (decorate?) format-to-string("{%s: size 0}", type-name) else "()" end if;
  let closer = if (decorate?) "}" else ")" end if;
  let seperator = if (decorate?) ", " else " " end if;
  let ellipsis = "...";
  let elstring = "";
  if (level-now > level)
    format-to-string("%s%s%s", opener, ellipsis, closer);
  else
    let elements = 
      canonicalize-sequence
        (application, instance, max-size-override: length + 1);
    let num-elements = size(elements);
    if (num-elements == 0)
      empty
    else
      let (max, truncating?) =
        if (num-elements > length)
          values(length, #t)
        else
          values(num-elements, #f)
        end if;
      let first-element? = #t;
      for (i from 0 below max)
        if (first-element?)
          first-element? := #f
        else
          elstring := concatenate(elstring, seperator)
        end if;
        let element-printed = 
          dude-print-dylan-object(application, elements[i], length, level,
                                  level-now + 1, decorate?: decorate?);
        elstring := concatenate(elstring, element-printed);
      end for;
      if (truncating?)
        format-to-string("%s%s%s%s%s",
                         opener, elstring, seperator, ellipsis, closer)
      else
        format-to-string("%s%s%s",
                         opener, elstring, closer)
      end if
    end if
  end if
end method;

define method dylan-printable-representation 
    (type == $array-type, application :: <debug-target>,
     instance :: <remote-value>,
     length :: <integer>, level :: <integer>,
     level-now :: <integer>, 
     #key decorate? :: <boolean> = #t,
	  format :: false-or(<symbol>) = #f)
 => (rep :: <string>)
  let opener = if (decorate?) "{<array>: " else "(" end if;
  let empty = if (decorate?) 
                "{<array>: size 0}" 
              else 
                "()" 
              end if;
  let closer = if (decorate?) "}" else ")" end if;
  let seperator = if (decorate?) ", " else " " end if;
  let ellipsis = "...";
  let dims = dylan-multidimensional-array-dimensions(application, instance);
  let printed-dims = print-dylan-object(application, dims, length: 5);
  let elstring = 
    format-to-string("dimensions: %s, elements: ", printed-dims);
  if (level-now > level)
    format-to-string("%s%s%s", opener, ellipsis, closer);
  else
    let elements = 
      canonicalize-sequence
        (application, instance, max-size-override: length + 1);
    let num-elements = size(elements);
    if (num-elements == 0)
      empty
    else
      let (max, truncating?) =
        if (num-elements > length)
          values(length, #t)
        else
          values(num-elements, #f)
        end if;
      let first-element? = #t;
      for (i from 0 below max)
        if (first-element?)
          first-element? := #f
        else
          elstring := concatenate(elstring, seperator)
        end if;
        let element-printed = 
          dude-print-dylan-object(application, elements[i], length, level,
                                  level-now + 1, decorate?: decorate?);
        elstring := concatenate(elstring, element-printed);
      end for;
      if (truncating?)
        format-to-string("%s%s%s%s%s",
                         opener, elstring, seperator, ellipsis, closer)
      else
        format-to-string("%s%s%s",
                         opener, elstring, closer)
      end if
    end if
  end if
end method;

define method dylan-printable-representation 
    (type == $limited-array-type, application :: <debug-target>,
     instance :: <remote-value>,
     length :: <integer>, level :: <integer>,
     level-now :: <integer>, 
     #key decorate? :: <boolean> = #t,
	  format :: false-or(<symbol>) = #f)
 => (rep :: <string>)
  let element-type-name
    = dude-print-dylan-object
        (application, dylan-limited-array-element-type(application, instance), 
	 length, level, level-now + 1, decorate?: decorate?);
  let type-name = if (decorate?) format-to-string("limited(<array>, of: %s)", element-type-name) end;
  let opener = if (decorate?) format-to-string("{%s: ", type-name) else "(" end if;
  let empty = if (decorate?) format-to-string("{%s: size 0}", type-name) else "()" end if;
  let closer = if (decorate?) "}" else ")" end if;
  let seperator = if (decorate?) ", " else " " end if;
  let ellipsis = "...";
  let dims = dylan-limited-array-dimensions(application, instance);
  let printed-dims = print-dylan-object(application, dims, length: 5);
  let elstring = 
    format-to-string("dimensions: %s, elements: ", printed-dims);
  if (level-now > level)
    format-to-string("%s%s%s", opener, ellipsis, closer);
  else
    let elements = 
      canonicalize-sequence
        (application, instance, max-size-override: length + 1);
    let num-elements = size(elements);
    if (num-elements == 0)
      empty
    else
      let (max, truncating?) =
        if (num-elements > length)
          values(length, #t)
        else
          values(num-elements, #f)
        end if;
      let first-element? = #t;
      for (i from 0 below max)
        if (first-element?)
          first-element? := #f
        else
          elstring := concatenate(elstring, seperator)
        end if;
        let element-printed = 
          dude-print-dylan-object(application, elements[i], length, level,
                                  level-now + 1, decorate?: decorate?);
        elstring := concatenate(elstring, element-printed);
      end for;
      if (truncating?)
        format-to-string("%s%s%s%s%s",
                         opener, elstring, seperator, ellipsis, closer)
      else
        format-to-string("%s%s%s",
                         opener, elstring, closer)
      end if
    end if
  end if
end method;

define method dylan-printable-representation 
    (type == $deque-type, application :: <debug-target>,
     instance :: <remote-value>,
     length :: <integer>, level :: <integer>,
     level-now :: <integer>, 
     #key decorate? :: <boolean> = #t,
	  format :: false-or(<symbol>) = #f)
 => (rep :: <string>)
  let opener = if (decorate?) "{<deque>: " else "(" end if;
  let empty = if (decorate?) "{<deque>: size 0}" else "()" end if;
  let closer = if (decorate?) "}" else ")" end if;
  let seperator = if (decorate?) ", " else " " end if;
  let ellipsis = "...";
  let elstring = "";
  if (level-now > level)
    format-to-string("%s%s%s", opener, ellipsis, closer);
  else
    let elements = 
      canonicalize-sequence
        (application, instance, max-size-override: length + 1);
    let num-elements = size(elements);
    if (num-elements == 0)
      empty
    else
      let (max, truncating?) =
        if (num-elements > length)
          values(length, #t)
        else
          values(num-elements, #f)
        end if;
      let first-element? = #t;
      for (i from 0 below max)
        if (first-element?)
          first-element? := #f
        else
          elstring := concatenate(elstring, seperator)
        end if;
        let element-printed = 
          dude-print-dylan-object(application, elements[i], length, level,
                                  level-now + 1, decorate?: decorate?);
        elstring := concatenate(elstring, element-printed);
      end for;
      if (truncating?)
        format-to-string("%s%s%s%s%s",
                         opener, elstring, seperator, ellipsis, closer)
      else
        format-to-string("%s%s%s",
                         opener, elstring, closer)
      end if
    end if
  end if
end method;

define method dylan-printable-representation
    (type == $pair-type, application :: <debug-target>,
     instance :: <remote-value>,
     length :: <integer>, level :: <integer>,
     level-now :: <integer>, 
     #key decorate? :: <boolean> = #t,
	  format :: false-or(<symbol>) = #f)
 => (rep :: <string>)
  let top-head = dylan-head(application, instance);
  let top-tail = dylan-tail(application, instance);
  let tail-type = classify-dylan-object(application, top-tail);
  if ((tail-type == $pair-type) | (tail-type == $empty-list-type))
    let opener = if (decorate?) "#(" else "(" end if;
    let closer = if (decorate?) ")" else ")" end if;
    let seperator = if (decorate?) ", " else " " end if;
    let ellipsis = "...";
    let elstring = "";
    if (level-now > level)
      format-to-string("%s%s%s", opener, ellipsis, closer);
    else
      let elements = 
        canonicalize-sequence
          (application, instance, max-size-override: length + 1);
      let num-elements = size(elements);
      let (max, truncating?) =
        if (num-elements > length)
          values(length, #t)
        else
          values(num-elements, #f)
        end if;
      let first-element? = #t;
      for (i from 0 below max)
        if (first-element?)
          first-element? := #f
        else
          elstring := concatenate(elstring, seperator)
        end if;
        let element-printed = 
          dude-print-dylan-object(application, elements[i], length, level,
                                  level-now + 1, decorate?: decorate?);
        elstring := concatenate(elstring, element-printed);
      end for;
      if (truncating?)
        format-to-string("%s%s%s%s%s",
                         opener, elstring, seperator, ellipsis, closer)
      else
        format-to-string("%s%s%s",
                         opener, elstring, closer)
      end if
    end if;
  else
    let opener = if (decorate?) "#(" else "(" end if;
    let closer = ")";
    let seperator = " . ";
    let printed-head =
      dude-print-dylan-object
        (application, top-head, length, level, level-now + 1,
         decorate?: decorate?);
    let printed-tail =
      dude-print-dylan-object
        (application, top-tail, length, level, level-now + 1,
         decorate?: decorate?);
    format-to-string("%s%s%s%s%s",
                     opener, printed-head, seperator, printed-tail, closer);
  end if
end method;

       
///// PRINT-DYLAN-OBJECT
//    Given a dylan instance, returns its printable representation as a string.

// dude-print-dylan-object
// is delegated to by print-dylan-object

define method dude-print-dylan-object 
    (application :: <debug-target>, object :: <remote-value>,
     length :: <integer>, level :: <integer>, level-now :: <integer>, 
     #key decorate? :: <boolean> = #t,
          format :: false-or(<symbol>) = #f) 
 => (rep :: <string>)

  let type-description :: <object-type-description>
    = classify-dylan-object(application, object);

  dylan-printable-representation(type-description, application, object,
				 length, level, level-now,
				 decorate?: decorate?, format: format);
end method;

define method print-dylan-object 
    (application :: <debug-target>, instance :: <remote-value>,
     #key length = $maximum-print-length, level = $maximum-print-level,
          decorate? :: <boolean> = #t,
          format :: false-or(<symbol>) = #f)
  => (rep :: <string>)
  dude-print-dylan-object(application, instance, length, level, 0,
			  decorate?: decorate?, format: format);
end method;


// The new implementation...

///// GET-INSPECTOR-VALUES

define method get-inspector-values
    (application :: <debug-target>, instance :: <remote-value>)
      => (instance-class :: <remote-value>,
          instance-slots :: <sequence>,
          slot-getters :: <sequence>,
          slot-setters :: <sequence>,
          repeats :: false-or(<integer>),
          rept-slot :: false-or(<remote-value>),
          rept-getter :: false-or(<function>),
          rept-setter :: false-or(<function>),
          nonword-repeats :: false-or(<integer>),
          nonword-repeat-vector :: false-or(<vector>))

  let path = application.debug-target-access-path;

  // instance-slot-getter returns a closure which, when called, gets the
  // i'th slot value.

  local method instance-slot-getter (i :: <integer>)
    method ()
      let (v, ok)
        = read-instance-slot-element (application, instance, i);
      if (ok)
        v;
      else
        as-remote-value(0);
      end if
    end method
  end method;

  // instance-slot-setter returns a closure which, when called with a
  // <remote-value>, sets the i'th slot to that value.

  local method instance-slot-setter (i :: <integer>)
    method (v :: <remote-value>)
      // This is not implemented yet
    end method
  end method;

  // repeated-element-getter returns a closure which, when called with an
  // integer value i, returns the <remote-value> that residing in the i'th
  // repeat in the instance.
  // The argument b stands for 'base', and is the offset of the first
  // repeated element.

  local method repeated-element-getter (b :: <integer>)
    method (i :: <integer>)
      let (v, ok)
        = read-instance-slot-element (application, instance, b + i);
      if (ok)
        v
      else
        as-remote-value(0)
      end if
    end method
  end method;

  // repeated-element-setter returns a closure which, when called with an
  // integer value i, and a <remote-value> (v), sets the i'th repeated
  // element to be that value.
  // Again, the argument b is the offset of the first repeated element.

  local method repeated-element-setter (b :: <integer>)
    method (i :: <integer>, v :: <remote-value>)
      // This is not implemented yet.
    end method
  end method;

  // Initialize some potential return values...

  let class-object :: <remote-value> = as-remote-value(0);
  let slot-vector = #[];     // A vector of slot descriptors
  let getter-vector = #[];   // A vector of slot getters
  let setter-vector = #[];   // A vector of slot setters
  let repeat-count = #f;     // The count of repeated elements (if any)
  let nonword-repeats = #f;
  let nonword-vector = #[];
  let rgetter = #f;          // Repeated element getter
  let rsetter = #f;          // Repeated element setter
  let rdescriptor = #f;      // Repeated element slot descriptor
  let base = 0;              // Offset of the first repeated element.

  // Get a classification of the instance. Tagged immediates do not yield
  // any sensible inspection data.

  let type-description :: <object-type-description>
    = classify-dylan-object (application, instance);

  // We're not interested in tagged instances!

  if ((type-description ~= $integer-type) &
        (type-description ~= $character-type) &
          (type-description ~= $unknown-type))

    let (wrapper :: <remote-value>, valid-wrapper?) =
      read-instance-header (application, instance);
    class-object 
      := wrapper-to-iclass (application, wrapper);
    let (fixed-slot-count, vector-scaling) =
      dylan-wrapper-properties(application, wrapper);
    let has-byte-repeats? = (vector-scaling == 1);
    let slot-descriptors 
      = dylan-iclass-instance-slot-descriptors (application, class-object);
    let slot-count
      = dylan-vector-size (application, slot-descriptors);
    rdescriptor
      := dylan-iclass-repeated-slot-descriptor (application, class-object);
 
    // Get details of repeated slots if their are any.

    if ((classify-dylan-object (application, rdescriptor) ~= 
             $boolean-false) &
        (type-description ~= $string-type))

        let rsize = read-instance-slot-element
                    (application, instance, slot-count - 1);
        repeat-count := dylan-integer-data (application, rsize);
        base := slot-count;
    end if;

    slot-vector := make (<vector>, size: slot-count);
    getter-vector := make (<vector>, size: slot-count);
    setter-vector := make (<vector>, size: slot-count);

    // Collect the slots and their data.

    for (i from 0 below slot-count)
      slot-vector[i]
        := dylan-vector-element (application, slot-descriptors, i);
      getter-vector[i]
        := instance-slot-getter (i);
      setter-vector[i]
        := instance-slot-setter (i);
    end for;

    // Fill in the repeated slot data if it is relevant.

    if (repeat-count)
      if (has-byte-repeats?)
        nonword-repeats := repeat-count;
        let base-address = indexed-remote-value(instance, base);
        let nonword-bytestring =
          read-byte-string(path, base-address, repeat-count);
        nonword-vector := make(<vector>, size: repeat-count);
        for (i from 0 below repeat-count)
          nonword-vector[i] := as(<integer>, nonword-bytestring[i])
	end for;
        rgetter := #f;
        rsetter := #f;
        repeat-count := #f;
      else
        rgetter := repeated-element-getter(base);
        rsetter := repeated-element-setter(base);
      end if;
    end if;
   end if;

   values (dylan-iclass-class(application, class-object),
           slot-vector, 
           getter-vector, 
           setter-vector,
           repeat-count, 
           rdescriptor, 
           rgetter, 
           rsetter,
           nonword-repeats,
           nonword-vector);
    
end method;


///// DESCRIBE-DYLAN-OBJECT
//    The version 4 implementation...

define method describe-dylan-object
              (application :: <debug-target>, instance :: <remote-value>)
               => (class-name :: <string>,
                   slots :: <sequence>,
                   slot-values :: <sequence>,
                   repeats :: false-or(<integer>),
                   repeated-slot-name :: false-or(<string>),
                   repeated-slot-values :: false-or(<sequence>))

  let type-desc =
    classify-dylan-object (application, instance);

  if (type-desc == $integer-type)
    values ("<INTEGER>", #[], #[], #f, #f, #f);
  elseif (type-desc == $character-type)
    values ("<CHARACTER>", #[], #[], #f, #f, #f);
  elseif (type-desc == $unknown-type)
    values ("<???>", #[], #[], #f, #f, #f);
  else

    // Get the inspector values, and turn them into nice convenient
    // return values.

    let (class-object, slot-descriptors, slot-getters, slot-setters,
         repeats, rslot, rgetter, rsetter)
      = get-inspector-values (application, instance);
    let slot-count = size(slot-descriptors);
    let slot-names = make (<vector>, size: slot-count);
    let slot-vals = make (<vector>, size: slot-count);
    let rslot-name = #f;
    let rslot-vals = #f;
    let (lib, mod, class-name)
      = dylan-instance-symbolic-name (application, class-object);

    // Apply the slot getter to each slot.

    for (i from 0 below slot-count)
      let slot-getter
        = dylan-slot-getter (application, slot-descriptors[i]);
      let (lib, mod, slot-name)
        = dylan-instance-symbolic-name (application, slot-getter);
      slot-names[i] := slot-name;
      slot-vals[i] := slot-getters[i]();
    end for;

    // If there are repeats, apply the repeat-getter to each repeated
    // element

    if (repeats)
      let rslot-getter
        = dylan-slot-getter (application, rslot);
      let (lib, mod, nm)
        = dylan-instance-symbolic-name (application, rslot-getter);
      rslot-name := nm;
      rslot-vals := make (<vector>, size: repeats);
      for (i from 0 below repeats)
        rslot-vals[i] := rgetter(i)
      end for
    end if;

    values (class-name, slot-names, slot-vals, repeats,
            rslot-name, rslot-vals);
  end if 
            
end method;


///// RESOLVE-DYLAN-NAME

define method resolve-dylan-name
                (application :: <debug-target>, name :: <string>,
                 context :: <dylan-name-context>, #key indirect? = #t,
                 library = #f)
                 => (val :: false-or(<remote-value>),
                     address :: false-or(<remote-value>));

  let lib = library
            | element(application.library-component-names,
                      context.context-library,
                      default: #f)
            | find-library-called
                (application, 
                 obtain-component-name(application, context.context-library));

  let sym = 
    if (indirect?)
      symbol-table-find-symbol (application.debug-target-symbol-table,
                                mangle-in-context 
                                  (name, context, as-static-object?: #f),
                                library: lib);
    else
      symbol-table-find-symbol (application.debug-target-symbol-table,
                                mangle-in-context 
                                  (name, context, as-static-object?: #t),
                                library: lib);
    end if;

  if (sym)
    let address = sym.remote-symbol-address;
    if (indirect?)
      let val = 
        read-dylan-value(application, address);
      values(val, address);
    else
      values(address, address);
    end if
  else
    values(#f, #f);
  end if
end method;


///// FIND-DYLAN-NAME
//    TODO: The same (in reverse) has for RESOLVE-DYLAN-NAME

define method find-dylan-name 
    (application :: <debug-target>, address :: <remote-value>,
     #key disambiguate-methods? = #t)
  => (name :: <string>, context :: <dylan-name-context>,
      precise? :: <boolean>, constant? :: <boolean>)

  // Find the closest symbol to the given address. Note that we don't
  // need to specify a DLL this time, because the address will implicitly
  // restrict the search.

  let (closest, offset)
    = symbol-table-symbol-relative-address
          (application.debug-target-symbol-table,
           address);

  if (closest)
    let (nm, mod, lib,
         is-method?, is-iep?, defining-library, method-number) 
      = demangle-dylan-name (closest.remote-symbol-name);
    if (is-method? & disambiguate-methods?)
      nm := format-to-string("%s#%s", nm, method-number)
    end if;
    values (nm, 
            make (<dylan-name-context>, library: lib, module: mod),
            address = closest.remote-symbol-address,
            closest.remote-symbol-name[0] == 'K');
  else
    values ("", make (<dylan-name-context>), #f, #f)
  end if;
end method;


///// DYLAN-METHOD-IEP

define method dylan-method-iep
    (application :: <debug-target>, method-object :: <remote-value>)
       => (meth-iep :: <remote-value>)

  let dylan-type = classify-dylan-object(application, method-object);

  let (entry-point, ok?) =
    if (dylan-type == $method-type)
      method-iep (application, method-object);
    else
      values(as-remote-value(0), #f)
    end if;

  if (ok?)
     entry-point
  else
     as-remote-value(0)
  end if
end method;


///// DYLAN-GENERIC-FUNCTION-METHODS

define method dylan-generic-function-methods
    (application :: <debug-target>, gf-object :: <remote-value>)
       => (methods :: <sequence>)

  let dylan-type = classify-dylan-object(application, gf-object);

  let (method-vector, ok?) =
    if (dylan-type == $generic-function-type)
      gf-methods (application, gf-object)
    else
      values(as-remote-value(0), #f)
    end if;

  if (ok?)
     canonicalize-sequence(application, method-vector)
  else
     #[]
  end if
end method;


///// DYLAN-METHOD-SPECIALIZERS

define method dylan-method-specializers
    (application :: <debug-target>, method-object :: <remote-value>)
       => (specializers :: <sequence>)

  let (spec-vector, ok?) =
    get-method-specializers (application, method-object);

  if (ok?)
     let spec-count = dylan-vector-size (application, spec-vector);
     let ss = make (<vector>, size: spec-count);
     for (i from 0 below spec-count)
         ss[i] := dylan-vector-element (application, spec-vector, i);
     end for;
     ss;
  else
     #[]
  end if
end method;


///// DYLAN-SLOT-DESCRIPTOR-GETTER

define method dylan-slot-descriptor-getter
  (application :: <debug-target>, descriptor :: <remote-value>)
    => (getter :: <remote-value>)
  dylan-slot-getter (application, descriptor);
end method;


///// REMOTE-FORMAT-TO-STRING
//    Just like format-to-string, except that the format args are actually
//    <remote-value> instances to be formatted out of the runtime. This
//    is used to allow the environment to generate formatted strings for
//    printing conditions and debug messages.

define method printable-representation-of-single-argument
    (application :: <debug-target>, format-code :: <character>,
     val :: <remote-value>)
 => (rep :: <string>, consumed? :: <boolean>)

  select (as-uppercase(format-code))
    'D' =>
      let int = dylan-integer-data(application, val);
      values(format-to-string("%d", int), #t);
    'B' =>
      let int = dylan-integer-data(application, val);
      values(format-to-string("%b", int), #t);
    'O' =>
      let int = dylan-integer-data(application, val);
      values(format-to-string("%o", int), #t);
    'X' =>
      let int = dylan-integer-data(application, val);
      values(format-to-string("%x", int), #t);
    'C' =>
      let char = dylan-character-data(application, val);
      values(format-to-string("%c", char), #t);
    'S' =>
      let desc = classify-dylan-object(application, val);
      if (desc == $string-type)
	let string = truncated-dylan-string-data(application, val);
	values(format-to-string("%s", string), #t);
      else
	values(print-dylan-object(application, val, length: 10, level: 3), #t);
      end if;
    'A' =>
      let desc = classify-dylan-object(application, val);
      if (desc == $string-type)
	let string = truncated-dylan-string-data(application, val);
	values(format-to-string("%s", string), #t);
      else
	values(print-dylan-object(application, val, length: 10, level: 3), #t);
      end if;
    '=' =>
      values(print-dylan-object(application, val, length: 10, level: 3), #t);
    '%' =>
      values("%", #f);
    otherwise =>
      values("{Illegal directive in format string}", #f);
  end select
end method;

define method remote-format-to-string
    (application :: <debug-target>, format-string :: <string>,
     #rest remote-format-args)
       => (rep :: <string>)
  let build-string = "";
  let i = 0;
  let next-argument = 0;
  let max-arguments = size(remote-format-args);
  let limit = size(format-string);
  while (i < limit)
    let char = element(format-string, i);
    if (char == '%')
      let control-char = element(format-string, i + 1, default: '%');
      i := i + 2;
      if (next-argument < max-arguments)
	let (printed-rep, consume?) = 
	  printable-representation-of-single-argument
	    (application, control-char, remote-format-args[next-argument]);
	build-string := concatenate(build-string, printed-rep);
	if (consume?)
	  next-argument := next-argument + 1
	end if;
      else
        build-string := concatenate(build-string, "#f");
      end if;
    else
      build-string := concatenate(build-string, add!("", char));
      i := i + 1;
    end if;
  end while;
  build-string;
end method;


///// CANONICALIZE-SEQUENCE
//    A new innovation for DM5. Inpsects the given runtime value, which
//    is assumed to be some kind of sequence. No matter what the precise
//    kind of sequence is (be it a list or a simple-object-vector), this
//    generates a LOCAL sequence of <remote-value> instances. The 
//    remote values are the runtime elements of the sequence. This is
//    very useful for interpreting runtime sequences when there is no
//    precise protocol dictating what kind of sequence it is.

//    New:
//      This function now also accepts the max-size-override keyword
//      argument. If this is supplied, and the number of elements in
//      the "real" sequence exceeds this limit, the "proxy" sequence
//      will be truncated.

define method canonicalize-sequence
    (application :: <debug-target>, sequence-instance :: <remote-value>,
     #key max-size-override = #f)
      => (seq :: <sequence>)
  let sequence-type = classify-dylan-object(application, sequence-instance);
  let build-sequence = #[];
  select (sequence-type)
    $vector-type =>
      let vsize = dylan-vector-size(application, sequence-instance);
      if (max-size-override & (max-size-override < vsize))
        vsize := max-size-override
      end if;
      build-sequence := make(<vector>, size: vsize);
      for (i from 0 below vsize)
        build-sequence[i] := 
          dylan-vector-element(application, sequence-instance, i);
      end for;

    $limited-vector-type =>
      let vsize = dylan-limited-vector-size(application, sequence-instance);
      if (max-size-override & (max-size-override < vsize))
        vsize := max-size-override
      end if;
      build-sequence := make(<vector>, size: vsize);
      for (i from 0 below vsize)
        build-sequence[i] := 
          dylan-limited-vector-element(application, sequence-instance, i);
      end for;

    $array-type =>
      let asize = dylan-multidimensional-array-size
                     (application, sequence-instance);
      if (max-size-override & (max-size-override < asize))
        asize := max-size-override
      end if;
      build-sequence := make(<vector>, size: asize);
      for (i from 0 below asize)
        build-sequence[i] := 
          dylan-multidimensional-array-row-major-element
            (application, sequence-instance, i);
      end for;

    $limited-array-type =>
      let asize = dylan-limited-array-size
                     (application, sequence-instance);
      if (max-size-override & (max-size-override < asize))
        asize := max-size-override
      end if;
      build-sequence := make(<vector>, size: asize);
      for (i from 0 below asize)
        build-sequence[i] := 
          dylan-limited-array-row-major-element
            (application, sequence-instance, i);
      end for;

    $deque-type =>
      let dsize = dylan-deque-size(application, sequence-instance);
      if (max-size-override & (max-size-override < dsize))
        dsize := max-size-override;
      end if;
      build-sequence := make(<vector>, size: dsize);
      for (i from 0 below dsize)
        build-sequence[i] :=
          dylan-deque-element(application, sequence-instance, i);
      end for;

    $stretchy-vector-type =>
      let svrep = 
         dylan-stretchy-vector-representation(application, sequence-instance);
      let svsize =
         dylan-stretchy-vector-size(application, svrep);
      if (max-size-override & (max-size-override < svsize))
        svsize := max-size-override
      end if;
      build-sequence := make(<vector>, size: svsize);
      for (i from 0 below svsize)
        build-sequence[i] :=
          dylan-stretchy-vector-element(application, svrep, i);
      end for;

    $limited-stretchy-vector-type =>
      let svrep = 
         dylan-limited-stretchy-vector-representation(application, sequence-instance);
      let svsize =
         dylan-limited-stretchy-vector-size(application, svrep);
      if (max-size-override & (max-size-override < svsize))
        svsize := max-size-override
      end if;
      build-sequence := make(<vector>, size: svsize);
      for (i from 0 below svsize)
        build-sequence[i] :=
          dylan-limited-stretchy-vector-element(application, svrep, i);
      end for;

    $pair-type =>
      let list-so-far :: <remote-value> = sequence-instance;
      let pair-wrapper :: <remote-value>
          = read-instance-header(application, sequence-instance);
      let keep-going = #t;
      build-sequence := make(<stretchy-vector>, size: 0);
      while (keep-going &
             (read-instance-header(application, list-so-far) = pair-wrapper))
        if (max-size-override & (max-size-override == build-sequence.size))
          keep-going := #f
	else
          add!(build-sequence, 
               dylan-head(application, list-so-far));
          list-so-far := dylan-tail(application, list-so-far);
	end if;
      end while;

    otherwise =>
      build-sequence := #[];

  end select;
  build-sequence; 
end method;


///// GET-KEYWORD-VALUE-FROM-VECTOR
//    Given a remote <vector> of keyword/value pairs, and a keyword to
//    match on (as a symbol), return the <remote-value> associated with
//    that keyword.
//    Note: This is an internal function, and will not work for
//          arbitrary keywords like #"splunge". Only a pre-defined
//          group of keywords will be detected.

define method get-keyword-value-from-vector
    (application :: <debug-target>, vector-instance :: <remote-value>,
     keyword-item :: <symbol>) 
       => (maybe-value :: false-or(<remote-value>))
  if (classify-dylan-object(application, vector-instance) == $vector-type)
    let sz = dylan-vector-size(application, vector-instance);
    let i = 0;
    let found = #f;
    let kwd-address = lookup-static-keyword(application, keyword-item);
    if (kwd-address)
      while ((~found) & (i < (sz - 1)))
        if (dylan-vector-element
               (application, vector-instance, i) = kwd-address)
          found := dylan-vector-element(application, vector-instance, i + 1);
        else
          i := i + 1;
        end if;
      end while;
      found;
    else
      #f
    end if
  else
    #f
  end if
end method;


///// DYLAN-KEYWORD-NAME
//    From the address of an interned keyword, return a string that is the
//    name of the keyword.

define method dylan-keyword-name
    (application :: <debug-target>, sym :: <remote-value>)
       => (str :: <string>)
  truncated-dylan-string-data
    (application, dylan-symbol-name(application, sym))
end method;


///// RESOLVE-DYLAN-KEYWORD
//    Generates an address from a string denoting an interned keyword in
//    the runtime.

define method resolve-dylan-keyword
    (application :: <debug-target>, sym :: <string>)
       => (addr :: false-or(<remote-value>))

  // Run the spy on the primary thread.
  let spy-thread = select-thread-for-spy(application);

  // We need to construct a byte-string object to pass to our spy function.

  let string-wrapper =
    lookup-static-wrapper(application, "<byte-string>", "dylan");

  if (application.temporary-download-block)

    // Make this call to recycle-static-block. This is so we can keep
    // re-using the same memory again and again, and not gradually
    // allocate shitloads of it as more calls are made to this API.

    recycle-static-block(application.temporary-download-block);
    block-align-n(application.temporary-download-block, 4);

    // DOWNLOAD:
    // The string wrapper, the size field, and the byte string itself.

    let header = vector(string-wrapper,
                        integer-as-tagged-remote-value(size(sym)));
    let headerval = download-remote-value-vector-into
                       (application.debug-target-access-path,
                        application.temporary-download-block,
                        header);
    let stringval = download-byte-string-into
                       (application.debug-target-access-path,
                        application.temporary-download-block,
                        sym);
    if (stringval & headerval)
      run-spy-on-thread
         (application, spy-thread, 
          application.dylan-spy.resolve-string-to-symbol,
          headerval) 
    else
      #f
    end if;
  else
    #f;
  end if;
end method;


///// FIND-CLOSEST-SYMBOLIC-NAME
//    A new (Post-DM5) interface for obtaining the lowlevel (mangled)
//    symbolic name for a dylan object.

define method find-closest-symbolic-name
    (application :: <debug-target>, instance :: <remote-value>)
       => (maybe-name :: false-or(<remote-symbol>), precise? :: <boolean>)
  let (closest, offset)
    = symbol-table-symbol-relative-address
         (application.debug-target-symbol-table, instance);
  if (closest)
    values(closest, offset == 0);
  else
    values(#f, #f);
  end if;
end method;


///// RESOLVE-SYMBOLIC-NAME
//    Performs the opposite work on FIND-CLOSEST-SYMBOLIC-NAME. Given
//    a lowlevel (mangled) name, attempts to find its definition, and
//    returns it if possible.

define method resolve-symbolic-name
    (application :: <debug-target>, symbolic-name :: <string>,
     #key library = #f)
       => (definition-address :: false-or(<remote-value>))
  let sym =
    if (library)
      symbol-table-find-symbol(application.debug-target-symbol-table,
                               symbolic-name,
                               library: library);
    else
      symbol-table-find-symbol(application.debug-target-symbol-table, 
                               symbolic-name)
    end if;
  if (sym)
    sym.remote-symbol-address
  else
    #f
  end if
end method;


///// DYLAN-OBJECT-CLASS
//    Given a general dylan instance, try to return a <remote-value>
//    for its class.

define method dylan-object-class
    (application :: <debug-target>, instance :: <remote-value>,
     #key browsable-only? = #f)
      => (class-instance :: false-or(<remote-value>),
          incarnation :: false-or(<remote-value>),
          current-incarnation :: false-or(<remote-value>),
          immediate? :: <boolean>)
  let tag = inspect-instance-tag(application, instance);
  let class-instance = #f;
  let immediate? = #f;
  let incarnation = #f;
  let current-incarnation = #f;

  // If our instance is tagged, then there is no wrapper-->class chain
  // to follow, so we have to explicitly search for the correct class
  // objects. Fortunately, we know what those are, and that they will
  // be present in all dylan applications.

  select (tag)
    $dylan-tag-integer =>
      unless (browsable-only?)
        class-instance :=
          lookup-static-object(application, "<integer>", "dylan");
      end unless;
      immediate? := #t;

    $dylan-tag-character =>
      unless (browsable-only?)
        class-instance :=
          lookup-static-object(application, "<character>", "dylan");
      end unless;
      immediate? := #t;

    $dylan-tag-boolean =>
      let sod = application.static-object-directory;
      if (sod.booleans-tagged?)
        unless (browsable-only?)
          class-instance :=
            lookup-static-object(application, "<boolean>", "dylan");
        end unless;
        immediate? := #t;
      end if;
      
    $dylan-tag-pointer =>
      let (wrapper-instance, ok?) = 
        read-instance-header(application, instance);
      if (ok?)
         let (class-val, ok?) = 
           wrapper-to-class(application, wrapper-instance);
         let (iclass-val, ok2?) = 
           wrapper-to-iclass(application, wrapper-instance);
        if (ok? & ok2?)
          class-instance := class-val;
          incarnation := iclass-val;
          current-incarnation := dylan-class-iclass(application, class-val);
        end if;
      end if;

  end select;
  values(class-instance, incarnation, current-incarnation, immediate?);
end method;


///// FOREIGN-OBJECT-TYPE
//    Dummy implementation. Just returns FALSE.

define method foreign-object-type
    (application :: <debug-target>, foreign-instance :: <remote-value>)
      => (remote-type-description)
  #f
end method;


///// DYLAN-OBJECT-IMMEDIATE-VALUE
//    Uploads a tagged value from the runtime, returning an environment-side
//    replica.

define method dylan-object-immediate-value
    (application :: <debug-target>, instance :: <remote-value>)
       => (replica :: <object>, success? :: <boolean>)
  let tag = inspect-instance-tag(application, instance);
  let replica = #f;
  let success? = #f;
  select (tag)
    $dylan-tag-integer =>
      success? := #t;
      replica := tagged-remote-value-as-integer(instance);

    $dylan-tag-character =>
      success? := #t;
      replica := tagged-remote-value-as-character(instance);

  end select;
  values(replica, success?);
end method;
