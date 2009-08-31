Module:    print-internals
Author:    Gwydion Project
Synopsis:  This file implements object printing.
Copyright: See below.


///======================================================================
///
/// Copyright (c) 1994  Carnegie Mellon University
/// All rights reserved.
/// 
/// Use and copying of this software and preparation of derivative
/// works based on this software are permitted, including commercial
/// use, provided that the following conditions are observed:
/// 
/// 1. This copyright notice must be retained in full on any copies
///    and on appropriate parts of any derivative works.
/// 2. Documentation (paper or online) accompanying any system that
///    incorporates this software, or any part of it, must acknowledge
///    the contribution of the Gwydion Project at Carnegie Mellon
///    University.
/// 
/// This software is made available "as is".  Neither the authors nor
/// Carnegie Mellon University make any warranty about the software,
/// its performance, or its conformity to any specification.
/// 
/// Bug reports, questions, comments, and suggestions should be sent by
/// E-mail to the Internet address "gwydion-bugs@cs.cmu.edu".
///
///======================================================================
///

/// This code was modified at Functional Objects, Inc. to work with the new Streams
/// Library designed by Functional Objects and CMU.
///


// Print-length holds the maximum number of elements the user wants a
// sequence to be printed.  This does not apply to some sequences, such as
// strings.
define variable *default-length* :: false-or(<integer>) = #f;
define thread variable *print-length* :: false-or(<integer>) = *default-length*;

// Print-level holds the maximum depth to which the user wants recursive
// printing to go.
define variable *default-level* :: false-or(<integer>) = #f;
define thread variable *print-level* :: false-or(<integer>) = *default-level*;

// Print-circle? holds whether the user wants circular printing.
define variable *default-circle?* :: <boolean> = #f;
define thread variable *print-circle?* :: <boolean> = *default-circle?*;

// Print-pretty? holds whether the user wants pretty printing.
define variable *default-pretty?* :: <boolean> = #f;
define thread variable *print-pretty?* :: <boolean> = *default-pretty?*;

// Print-escape? holds whether the user wants to print as strings or objects
// #t => print as structure, #f => print as string
define variable *default-escape?* :: <boolean> = #t;
define thread variable *print-escape?* :: <boolean> = *default-escape?*;

// Print-depth holds the current level of printing.  When incremeting this
// slot causes the depth to exceed print-level, then the print function
// only outputs $print-level-exceeded-string.
define thread variable *print-depth* :: <integer> = -1; 

/// <circular-print-stream> Class -- Internal.
///
/// These streams hold print state so that the print function can do most
/// of the work maintaining circular printing state.
///
define sealed class <circular-print-stream> (<wrapper-stream>)
  // Circular-first-pass? indicates to the print function whether it is on
  // the first pass of printing, in which it just builds a table of objects
  // referenced during the printing.  On the second pass of printing, print
  // actually generates output.
  slot circular-first-pass? :: <boolean> = #t;
  //
  // Circular-references is a table of objects referenced during printing
  // when print-circle? is #t.
  slot circular-references :: false-or(<object-table>) = #f;
  //
  // Circular-next-id holds the next ID to use when printing circularly.
  // Each time print sees an object for a second time during the first
  // printing pass, print assigns as the object's ID the current value of
  // this slot.
  slot circular-next-id :: <integer> = 0;
end class;

define method circular-first-pass? (stream :: <stream>) => (first? :: <boolean>)
  #f
end method;

/*
define sealed domain make (singleton(<circular-print-stream>));
define sealed domain initialize (<circular-print-stream>);
*/


/// <print-reference> Class.
///

/// <print-reference> Class -- Internal.
///
/// These objects hold information about object references encountered when
/// print-circle? is #t.  The print function creates these objects in a fake
/// first printing pass, and then it uses these objects during a real second
/// printing pass to determine whether the object needs to be tagged,
/// printed normally, or printed by reference to the objects circular ID to
/// avoid infinite recursive printing.
///
define sealed class <print-reference> (<object>)
  //
  // This slot holds the object referenced during printing.
  constant slot print-reference-object, required-init-keyword: object:;
  //
  // This slot holds the object's ID for circular references.  The object
  // prints as its ID after the first time.  Before the first time the object
  // is printed, this slot is #f.
  slot print-reference-id :: false-or(<byte-string>) = #f;
  //
  // This slot counts the number of references to the object.
  slot print-reference-count :: <integer> = 0;
end class;

/*
define sealed domain make (singleton(<print-reference>));
define sealed domain initialize (<print-reference>);
*/


/// Print-reference routines.
///

/// print-reference -- Internal Interface.
///
/// This function returns the print-reference object associated with object.
/// If none exists, then this creates a print-reference and installs it in
/// the circular-references table.
///
define method print-reference
    (object, stream :: <circular-print-stream>)
 => (ref :: <print-reference>)
  let table = stream.circular-references;
  let ref = element(table, object, default: #f);
  if (ref)
    ref;
  else
    let ref = make(<print-reference>, object: object);
    element(table, object) := ref;
  end;
end method;

/// new-print-reference-id -- Internal Interface.
///
/// This function gets the next circular print reference ID, assigns it to ref,
/// and updates the stream so that it doesn't return the same ID again.
///
define method new-print-reference-id
    (stream :: <circular-print-stream>, ref :: <print-reference>)
 => (ID :: <byte-string>)
  let id = stream.circular-next-id;
  stream.circular-next-id := id + 1;
  ref.print-reference-id := integer-to-string(id);
end method;




/// Print and global defaults.
///

/// What to print when the current depth exceeds the users requested print
/// level limit.
///
define constant $print-level-exceeded-string :: <byte-string> = "#";

/// What to print before a circular print ID.
///
define constant $circular-id-prestring :: <byte-string> = "#";

/// What to print after a circular print ID.
///
define constant $circular-id-poststring :: <byte-string> = "#";


/// Print -- Exported.
///
define generic print (object, stream :: <stream>,
		      #key level, length, circle?, pretty?, escape?) => ();


define constant <boolean-or-unsupplied> = <object>;  // !@#$ HACK can't deal
//  = type-union(<boolean>, singleton($unsupplied));
define constant <integer-or-false-or-unsupplied> = <object>; // !@#$ HACK ditto
//  = type-union(<integer>, one-of(#f, $unsupplied)); 

/// Print -- Method for Exported Interface.
///
/// This method must regard the values of the keywords and construct a
/// <print-stream> to hold the values for the requested print operation.
///
define method print (object, stream :: <stream>,
		     #key level :: <integer-or-false-or-unsupplied> = $unsupplied,
		          length :: <integer-or-false-or-unsupplied> = $unsupplied,
		          circle? :: <boolean-or-unsupplied> = $unsupplied,
		          pretty? :: <boolean-or-unsupplied> = $unsupplied,
		          escape? :: <boolean-or-unsupplied> = $unsupplied) => ()
  block ()
    //
    // Lock the stream so that all the calls to print-object build output
    // contiguously, without intervening threads screwing up the print
    // request.
    lock-stream(stream);
    //
    // Set slots with those values supplied by the user.
    dynamic-bind (*print-length*  = if (supplied?(length))  length  else *print-length* end,
		  *print-level*   = if (supplied?(level))   level   else *print-level* end,
		  *print-circle?* = if (supplied?(circle?)) circle? else *print-circle?* end,
		  *print-pretty?* = if (supplied?(pretty?)) pretty? else *print-pretty?* end,
		  *print-escape?* = if (supplied?(escape?)) escape? else *print-escape?* end)
      //
      // Make the stream defaulting the slots to the global default values for
      // the keyword arguments.  No need to lock this stream because only this
      // thread should have any references to it ... barring extreme user
      // silliness.
      let p-stream = if (*print-circle?*)
		       make(<circular-print-stream>, inner-stream: stream)
		     else
		       stream
		     end;
      //
      // When printing circularly, we first print to a "null stream" so that we
      // can find the circular references.
      if (*print-circle?*)
	start-circle-printing(object, p-stream);
      end;
      //
      // Determine whether, and how, to print object.
      maybe-print-object(object, p-stream);
    end
  cleanup
    unlock-stream(stream);
  end;
end method;

define method print (object, stream :: <circular-print-stream>,
		     #key level :: <integer-or-false-or-unsupplied> = $unsupplied,
		          length :: <integer-or-false-or-unsupplied> = $unsupplied,
		          circle? :: <boolean-or-unsupplied> = $unsupplied,
		          pretty? :: <boolean-or-unsupplied> = $unsupplied,
		          escape? :: <boolean-or-unsupplied> = $unsupplied) => ()
  dynamic-bind (*print-length*  = if (supplied?(length))  length  else *print-length* end,
		*print-level*   = if (supplied?(level))   level   else *print-level* end,
		*print-pretty?* = if (supplied?(pretty?)) pretty? else *print-pretty?* end,
		*print-escape?* = if (supplied?(escape?)) escape? else *print-escape?* end)
    maybe-print-object(object, stream);
  end
end method;

/// start-circle-printing -- Internal.
///
/// This function makes sure the stream has a circular-references table,
/// makes sure object has a print-reference, checks for circular references
/// within object, and considers what sort of output may be necessary to
/// define a tag for object or print object's tag.
///
/// This function is called both from the very first call to print and
/// recursive calls to print.  The calls to start-circle-printing within
/// recursive calls to print occur when the original call to print had
/// circular printing turned off, and the recursive calls to print turn
/// circular printing on.  Because of this function's use within recursive
/// calls to print, it cannot make certain assumptions:
///    Whether stream already has a circular-references table.
///    Whether there already is a print-reference for object.
///    What print-reference-count is for object.
///    Whether to do a first pass on object looking for circular references.
///    Whether object already has a print-reference-id.
///
/// Recursive calls to print cannot turn off circular printing, so we don't
/// have to account for that.
///
define method start-circle-printing (object, stream :: <circular-print-stream>) => ()
  let table = stream.circular-references;
  if (~ table)
    table := make(<object-table>);
    stream.circular-references := table;
  end;
  let ref = print-reference(object, stream);
  let count :: <integer> = (ref.print-reference-count + 1);
  ref.print-reference-count := count;
  if (count = 1)
    // If this is the first time we've seen this object, then dive into it
    // looking for circular references.
    stream.circular-first-pass? := #t;
    print-object(object, stream);
    stream.circular-first-pass? := #f;
  end;
end method;

/// maybe-print-object -- Internal.
///
/// This function increments print-depth and regards print-level to see
/// whether it should print object.  If it should print object, then it
/// regards print-circle? and does the right thing.
///
define method maybe-print-object (object, stream :: <stream>)
  let depth :: <integer> = (*print-depth* + 1);
  dynamic-bind (*print-depth* = depth)
    let requested-level :: false-or(<integer>) = *print-level*;
    case
      (requested-level & (depth > requested-level)) =>
	write(stream, $print-level-exceeded-string);
      (~ *print-circle?*) =>
	print-object(object, stream);
      (stream.circular-first-pass?) =>
	// When printing circularly, we first print to a "null stream" so
	// that we can find the circular references.
	let ref = print-reference(object, stream);
	let ref-count = (ref.print-reference-count + 1);
	ref.print-reference-count := ref-count;
	if (ref-count = 1)
	  // If ref-count is already greater than one, then there's
	  // no reason to go further into the object gathering references.
	  print-object(object, stream);
	end;
      otherwise
	output-print-reference(print-reference(object, stream), stream);
    end case;
  end;
end method;

/// output-print-reference -- Internal.
///
/// This function determines how to output a print-reference for circular
/// printing.
///
define method output-print-reference (ref :: <print-reference>, stream :: <stream>) => ()
  let ref-id = ref.print-reference-id;
  case
    (ref.print-reference-count = 1) =>
      print-object(ref.print-reference-object, stream);
    (~ ref-id) =>
      write(stream, $circular-id-prestring);
      write(stream, new-print-reference-id(stream, ref));
      write(stream, $circular-id-poststring);
      write(stream, "=");
      print-object(ref.print-reference-object, stream);
    otherwise =>
      write(stream, $circular-id-prestring);
      write(stream, ref-id);
      write(stream, $circular-id-poststring);
  end;
end method;


/// Print-object generic function and its default method.
///

/// print-object -- Exported.
///
define open generic print-object (object, stream :: <stream>)
    => ();

/// Any object.
///
define method print-object (object :: <object>, stream :: <stream>) => ()
  printing-logical-block (stream, prefix: "{", suffix: "}")
    let obj-class = object.object-class;
    let name = obj-class.debug-name;
    if (name)
      write(stream, as-lowercase(as(<byte-string>, name)));
    else
      print(obj-class, stream);
    end if;
    write(stream, " ");
    write(stream, machine-word-to-string(address-of(object)));
  end;
end method;

/*---*** This doesn't seem to be used anymore... anyone care to flush it?
define method print-object-slots 
    (object :: <object>, stream :: <stream>)
 => ()
  printing-logical-block (stream, prefix: "{", suffix: "}")
    let obj-class = object.object-class;
    write-class-name(obj-class, stream);
    write(stream, " instance");
    let descriptors = obj-class.slot-descriptors;
    if (~ (descriptors = #()))
      write(stream, ", ");
      pprint-indent(#"block", 2, stream);
      pprint-newline(#"linear", stream);
      // Print slot names and values.
      printing-logical-block (stream, prefix: #f)
	block (exit)
	  let length :: false-or(<integer>) = *print-length*;
	  for (desc in descriptors,
	       // Count each slot name and value as two
	       // for considerations of print-length.
	       count = 0 then (count + 2))
	    if (count ~= 0)
	      write(stream, ", ");
	      pprint-newline(#"linear", stream);
	    end if;
	    if (length & (count >= length))
	      write(stream, "...");
	      exit();
	    end if;
	    write(stream,
		  as(<byte-string>, desc.debug-name));
	    write(stream, ": ");
	    // pprint-tab(#"section-relative", 0, 0, stream);
	    pprint-newline(#"fill", stream);
	    let (value, win?) = slot-value(desc, object);
	    if (win?)
	      print(value, stream);
	    else
	      write(stream, "{UNINITIALIZED}");
	    end if;
	  end for;
	end block;
      end;
    end if;
  end
end method print-object;
*/


/// Print-object <byte-string>, <unicode-string> and <character> methods.
///

/// Characters.
///
define sealed method print-object (char :: <character>, stream :: <stream>) => ()
  if (*print-escape?*)
    write-element(stream, '\'');
    write-char-maybe-escape(stream, char, '\'');
    write-element(stream, '\'')
  else
    write-element(stream, char)
  end
end method print-object;

/// write-char-maybe-escape -- Internal.
///
/// Utility routine used for printing characters appropriately escaped.
///
define method write-char-maybe-escape
    (stream :: <stream>, char :: <character>, _quote :: one-of('\'', '"')) 
 => ()
  case
    char < ' ' =>
      select (char)
	'\0' => write(stream, "\\0");
	'\a' => write(stream, "\\a");
	'\b' => write(stream, "\\b");
	'\t' => write(stream, "\\t");
	'\f' => write(stream, "\\f");
	'\r' => write(stream, "\\r");
	'\n' => write(stream, "\\n");
	'\e' => write(stream, "\\e");
	otherwise =>
	  write(stream, "\\<");
	  write(stream, integer-to-string(as(<integer>, char), base: 16));
	  write-element(stream, '>');
      end select;
    char == _quote =>
      write-element(stream, '\\');
      write-element(stream, char);
    char == '\\' =>
      write(stream, "\\\\");
    char > '~' =>
      write(stream, "\\<");
      write(stream, integer-to-string(as(<integer>, char), base: 16));
      write-element(stream, '>');
    otherwise =>
      write-element(stream, char);
  end case;
end method write-char-maybe-escape;


/// strings.
///
define method print-object (object :: <string>, stream :: <stream>) => ()
  if (*print-escape?*)
    write-string-escaped(stream, object)
  else
    write-text(stream, object)
  end
end method print-object;


/// write-string-escaped -- Internal Interface.
///
/// Utility used by <byte-string>, <unicode-string>, and <symbol> print-object
/// methods to print the string with appropriate characters escaped.
///
define generic write-string-escaped (stream :: <stream>, object :: <string>)
    => ();

/// write-string-escaped -- Method for Internal Interface.
///
/// We try to write as much of the string as possible at once in order to
/// keep from having to make lots of extra calls to write.  We scan the
/// string for the next character that required special processing and then
/// write all the skipped over characters in one chunk.
///
define method write-string-escaped
    (stream :: <stream>, object :: <byte-string>) => ()
  let len :: <integer> = object.size;
  local

    method find-next-break (index :: <integer>)
	=> (next-break :: <integer>, char :: <byte-character>)
      if (index == len)
	// It doesn't matter what character we return, we just need to
	// return some character so the type matches.
	values(index, 'x');
      else
	let char = object[index];
	if (char < ' ' | char == '"' | char == '\\' | char > '~')
	  values(index, char);
	else
	  find-next-break(index + 1);
	end if;
      end if;
    end method find-next-break,

    method write-guts (from :: <integer>) => ()
      let (next-break, char) = find-next-break(from);
      unless (from == next-break)
	write(stream, object, start: from, end: next-break);
      end unless;
      unless (next-break == len)
	write-char-maybe-escape(stream, char, '"');
	write-guts(next-break + 1);
      end unless;
    end method write-guts;

  write-element(stream, '"');
  write-guts(0);
  write-element(stream, '"');
end method write-string-escaped;

/// write-string-escaped -- Method for Internal Interface.
///
/// We can't write chunks of the string at once, so just pay the cost and
/// write each character individually.
///
define method write-string-escaped
    (stream :: <stream>, object :: <string>) => ()
  write-element(stream, '"');
  for (char in object)
    write-char-maybe-escape(stream, char, '"');
  end for;
  write-element(stream, '"');
end method write-string-escaped;



/// Print-object <list> method.
///

/// For circular printing to be correct, we need to count references to the
/// tail pointers as well as the head pointers.  Because we do not print lists
/// by calling print on the tail of each pair, we need to specially handle
/// the tail pointers in this method.  The object passed in and all head
/// pointers are handled naturally via calls to print.
///
define sealed method print-object (object :: <list>, stream :: <stream>) => ()
  printing-logical-block (stream, prefix: "#(", suffix: ")")
    if (~ (object == #()))
      print-list(object, stream);
    end
  end
end method;

define method print-list (object :: <list>, stream :: <stream>) => ()
  block(exit)
    let length :: false-or(<integer>) = *print-length*;
    if (length & (length <= 0))
      write(stream, "...");
    else
      print(object.head, stream);
      let circle? = *print-circle?*;
      let first-pass? = stream.circular-first-pass?;
      for (remaining = object.tail then remaining.tail,
	   count = 1 then (count + 1),
	   until: (remaining == #()))
	if (~ instance?(remaining, <list>))
	    // Object was not a proper list, so print dot notation.
	    write(stream, " . ");
	    pprint-newline(#"fill", stream);
	    print(remaining, stream);
	    exit();
	end if;
	write(stream, ", ");
	pprint-newline(#"fill", stream);
	case
	  (length & (count >= length)) =>
	    // We've exceeded print-length for this print request.
	    write(stream, "...");
	    exit();
	  (~ circle?) =>
	    // No circular printing, so this is the simple and normal case.
	    print(remaining.head, stream);
	  (first-pass?) =>
	    // Get or create the print-reference for the remaining pointer.
	    let ref = print-reference(remaining, stream);
	    let ref-count = (ref.print-reference-count + 1);
	    ref.print-reference-count := ref-count;
	    if (ref-count = 1)
	      // First time through, so keep gathering references.
	      print(remaining.head, stream);
	    else
	      // If ref-count is already greater than one, then we've seen
	      // everything once.  Stop iterating.
	      exit();
	    end;
	  otherwise =>
	    // Circular printing on the second pass.
	    let ref = print-reference(remaining, stream);
	    let ref-id = ref.print-reference-id;
	    case
	      (ref.print-reference-count = 1) =>
		// Only one reference to the rest of the list, so print the
		// remaining elements normally.
		print(remaining.head, stream);
	      (~ ref-id) =>
		// Print the tag and its value with dot notation so that
		// the rest of the list does not appear to be a single
		// element of the list (that is, a nested list).
		write(stream, ". ");
		pprint-newline(#"fill", stream);
		write(stream, $circular-id-prestring);
		write(stream, new-print-reference-id(stream, ref));
		write(stream, $circular-id-poststring);
		write(stream, "=");
		print(remaining, stream);
	      otherwise =>
		// Print the tag with dot notation.  See previous cases's
		// comment.
		write(stream, ". ");
		pprint-newline(#"fill", stream);
		write(stream, $circular-id-prestring);
		write(stream, ref-id);
		write(stream, $circular-id-poststring);
		exit();
	    end case;
	end case;
      end for;
    end if;
  end block;
end method;


/// Print-object <simple-object-vector> method.
///

/// Vectors.
///
define sealed method print-object
    (object :: <simple-object-vector>, stream :: <stream>) => ()
  printing-logical-block (stream, prefix: "#[", suffix: "]")
    print-items(object, print, stream)
  end
end method;


/// Print-object <function> method.
///

/// Functions.
///
define sealed method print-object
    (object :: <generic-function>, stream :: <stream>) => ()
  printing-logical-block (stream, prefix: "{", suffix: "}")
    write(stream, "generic function");
    let name = debug-name(object);
    if (name)
      write-element(stream, ' ');
      pprint-newline(#"fill", stream);
      write(stream, as-lowercase(as(<byte-string>, name)));
    end;
  end
end method;

define sealed method print-object (object :: <method>, stream :: <stream>) => ()
  printing-logical-block (stream, prefix: "{", suffix: "}")
    write(stream, "method");
    let name = debug-name(object);
    if (name)
      write-element(stream, ' ');
      pprint-newline(#"fill", stream);
      write(stream, as(<byte-string>, name));
    end;
    let specializers = function-specializers(object);
    write-element(stream, ' ');
    pprint-newline(#"fill", stream);
    printing-logical-block (stream, prefix: "(", suffix: ")")
      print-function-specializers(object, stream)
    end
  end
end method;

define method print-function-specializers
    (object :: <function>, stream :: <stream>) => ();
  let specializers = function-specializers(object);
  if (~ (specializers = #()))
    write-element(stream, ' ');
    pprint-newline(#"fill", stream);
    printing-logical-block (stream, prefix: "(", suffix: ")")
      print-items(specializers, print-specializer, stream)
    end
  end if;
end method print-function-specializers;

/// print-items -- Internal Interface.
///
/// This function prints each element of items, separated by commas, using
/// print-fun.  This function also regards print-length.  Stream must be a
/// pretty printing stream or a <print-stream> whose target is a pretty
/// printing stream, so this function is basically good for use in body:
/// methods passed to pprint-logical-block.
///
/// DO NOT use this function for collections that may be tail-circular; it
/// will not terminate.
///
define method print-items
    (items :: <collection>, print-fun :: <function>, stream :: <stream>) => ()
  block (exit)
    let length :: false-or(<integer>) = *print-length*;
    for (x in items,
	 count = 0 then (count + 1))
      if (count ~= 0)
	write(stream, ", ");
	pprint-newline(#"fill", stream);
      end;
      if (length & (count = length))
	write(stream, "...");
	exit();
      end;
      print-fun(x, stream);
    end for;
  end block;
end method;



/// Print-specializer generic function and methods.
///

/// This function is used in printing methods.
///

define sealed generic print-specializer (type :: <type>, stream :: <stream>)
    => ();

define method print-specializer (type :: <type>, stream :: <stream>) => ()
  // write(stream, "{unknown type}")
  print(type, stream)
end method;

define method print-specializer (type :: <class>, stream :: <stream>) => ()
  let name = type.debug-name;
  if (name)
    write(stream, as-lowercase(as(<byte-string>, name)));
  else
    print(type, stream);
  end if;
end method;

define method print-specializer (type :: <singleton>, stream :: <stream>) => ()
  write(stream, "singleton(");
  print(type.singleton-object, stream);
  write(stream, ")");
end method;

define method print-specializer (type :: <subclass>, stream :: <stream>) => ()
  write(stream, "subclass(");
  print-specializer(type.subclass-class, stream);
  write(stream, ")");
end method;

define method print-specializer (type :: <limited-integer>, stream :: <stream>)
    => ();
  write(stream, "{Limited integer ");
  // write-class-name(type.limited-integer-class, stream);
  // write-element(stream, ' ');
  print(type.limited-integer-min, stream);
  write(stream, "..");
  print(type.limited-integer-max, stream);
  write(stream, "}");
end method print-specializer;

define method print-specializer (type :: <union>, stream :: <stream>)
    => ();
  printing-logical-block (stream, prefix: "{", suffix: "}")
    write(stream, "Union ");
    pprint-newline(#"fill", stream);
    // print(type.union-members, stream);
    print(union-type1(type), stream);
    write(stream, ", ");
    pprint-newline(#"linear", stream);
    print(union-type2(type), stream);
  end
end method print-specializer;


/// Print-object methods for <type> and its subclasses.
///

// General method for lots of different kinds of types.  We don't
// specialize on <type>, because if print-specializer doesn't know how to
// print the type, it will call print, and we would just end up back here.
// Instead, we carefully enumerate the types that print-specializer can
// deal with.
// 

/*
// Kludge: unions don't work in the emulator, so split this up
define sealed method print-object
    (object :: type-union(<singleton>, <limited-integer>, <union>), stream :: <stream>) => ()
  pprint-logical-block
    (stream,
     prefix: "{Type ",
     body: method (stream :: <stream>) => ()
	     print-specializer(object, stream);
	   end method,
     suffix: "}");
end method print-object;
*/

define function print-type-object (object, stream :: <stream>) => ()
  pprint-logical-block
    (stream,
     prefix: "{Type ",
     body: method (stream :: <stream>) => ()
	     print-specializer(object, stream);
	   end method,
     suffix: "}");
end;

define sealed method print-object
    (object :: <singleton>, stream :: <stream>) => ()
  print-type-object(object, stream);
end method print-object;

define sealed method print-object
    (object :: <limited-integer>, stream :: <stream>) => ()
  print-type-object(object, stream);
end method print-object;

define sealed method print-object
    (object :: <union>, stream :: <stream>) => ()
  print-type-object(object, stream);
end method print-object;

/// For classes, we just print the class name if there is one.
/// 
define sealed method print-object (object :: <class>, stream :: <stream>) => ();
  write(stream, "{class ");
  write-class-name(object, stream);
  write(stream, "}");
end method print-object;

/// write-class-name -- Internal Interface.
///
/// This function writes the name of the class or "<UNNAMED-CLASS>" to stream.
/// It does not output any curly braces, the word "class", or anything else.

define method write-class-name (object :: <class>, stream :: <stream>)
    => ();
  let name = debug-name(object);
  if (name)
    write(stream, as-lowercase(as(<byte-string>, name)));
  else
    write(stream, "<unnamed-class>");
  end if;
end method write-class-name;


/// Print-object miscellaneous methods.
///

define method print-object
    (deque :: <deque>, stream :: <stream>) => ()
  if (empty?(deque))
    write(stream, "{empty deque}")
  else
    printing-logical-block (stream, prefix: "{deque of ", suffix: "}")
      print-list(as(<list>, deque), stream)
    end
  end
end method;

define method print-object
    (r :: <range>, stream :: <stream>) => ()
  let s = r.size;
  if (s & zero?(s))
    write(stream, "{empty range}")
  else
    let f = range-from(r);
    let b = range-by(r);
    printing-logical-block (stream, prefix: "{range ", suffix: "}")
      if (s)
	print-object(f, stream);
	write(stream, " through ");
	print-object(last(r), stream);
	write(stream, " by ");
	print-object(b, stream);
      else
	print-object(f, stream);
	write(stream, " by ");
	print-object(b, stream);
      end
    end
  end
end method;

define method print-object
    (object :: <stretchy-vector>, stream :: <stream>) => ()
  printing-logical-block (stream, prefix: "{stretchy vector ", suffix: "}")
    print-items(object, print, stream)
  end
end method;

/// #t.
///
define sealed method print-object
    (object :: singleton(#t), stream :: <stream>) => ()
  write(stream, "#t");
end method;

/// #f.
///
define sealed method print-object
    (object :: singleton(#f), stream :: <stream>) => ()
  write(stream, "#f");
end method;

/// Symbols.
///
define sealed method print-object
    (object :: <symbol>, stream :: <stream>) => ()
  if (*print-escape?*)
    write-element(stream, '#');
    write-string-escaped(stream, as-lowercase(as(<byte-string>, object)))
  else
    write(stream, as-lowercase(as(<byte-string>, object)))
  end
end method;

/// Integers.
///
///---*** NOTE: When division is implemented for <big-integer>s, change
///---*** the specializer here to <abstract-integer> and rely on
///---*** integer-to-string being defined for <big-integer> as well as <integer>
define sealed method print-object
    (object :: <integer>, stream :: <stream>) => ()
  write(stream, integer-to-string(object));
end method;

/// Ratios.
///
//define sealed method print-object
//    (object :: <ratio>, stream :: <stream>) => ()
//  write(stream, integer-to-string(object.numerator));
//  write-element(stream, '/');
//  write(stream, integer-to-string(object.denominator));
//end;

/// Machine-Words
///
define sealed method print-object
    (object :: <machine-word>, stream :: <stream>) => ()
  write(stream, machine-word-to-string(object))
end method;


/// Float printing.
///

define sealed method print-object
    (float :: <float>, stream :: <stream>) => ()
  write(stream, float-to-string(float))
end;


/// Locator printing.
///

define sealed method print-object
    (locator :: <locator>, stream :: <stream>) => ()
  if (*print-escape?*)
    next-method()
  else
    write(stream, as(<string>, locator))
  end
end method print-object;


/// print-to-string -- Exported.
///
define generic print-to-string (object, #rest args,
				#key level, length, circle?, pretty?, escape?)
    => (result :: <string>);

define method print-to-string
    (object, #rest args,
     #key level :: false-or(<integer>), length :: false-or(<integer>),
          circle? :: <boolean>, pretty? :: <boolean>, escape? :: <boolean>)
 => (result :: <byte-string>);
  // Assume it is a small amount of printing.
  let s = make(<byte-string-stream>, contents: "", direction: #"output");
  apply(print, object, s, args);
  s.stream-contents
end method;



/// Output methods for <circular-print-stream>s.
///

define method write-element
    (stream :: <circular-print-stream>, ele :: <object>) => ()
  if (~ (*print-circle?* & (stream.circular-first-pass?)))
    write-element(stream.inner-stream, ele);
  end;
end method;

define method write
    (stream :: <circular-print-stream>, seq :: <sequence>,
     #key start :: <integer> = 0, end: stop :: <integer> = seq.size) => ()
  if (~ (*print-circle?* & (stream.circular-first-pass?)))
    write(stream.inner-stream, seq, start: start, end: stop);
  end;
end method;

define method force-output
    (stream :: <circular-print-stream>, #key synchronize? :: <boolean>) => ()
  ignore(synchronize?);
  if (~ (*print-circle?* & (stream.circular-first-pass?)))
    force-output(stream.inner-stream);
  end;
end method;

define method synchronize-output
    (stream :: <circular-print-stream>) => ()
  if (~ (*print-circle?* & (stream.circular-first-pass?)))
    synchronize-output(stream.inner-stream);
  end;
end method;

define method
    discard-output (stream :: <circular-print-stream>) => ()
  if (~ (*print-circle?* & (stream.circular-first-pass?)))
    discard-output(stream.inner-stream);
  end;
end method;

define method write-line
    (stream :: <circular-print-stream>, string :: <string>,
     #key start :: <integer> = 0, end: stop :: <integer> = string.size) => ()
  if (~ (*print-circle?* & (stream.circular-first-pass?)))
    write-line(stream.inner-stream, string, start: start, end: stop);
  end;
end method;

define method new-line
    (stream :: <circular-print-stream>) => ()
  if (~ (*print-circle?* & (stream.circular-first-pass?)))
    new-line(stream.inner-stream);
  end;
end method;

define method stream-open?
    (stream :: <circular-print-stream>) => (open? :: <boolean>)
  if (~ (*print-circle?* & (stream.circular-first-pass?)))
    stream-open?(stream.inner-stream);
  end;
end method;

/*--- andrewa: This doesn't type check, and seems bogus
define method stream-element-type
    (stream :: <circular-print-stream>) => (type :: <type>)
  if (~ (*print-circle?* & (stream.circular-first-pass?)))
    stream-element-type(stream.inner-stream);
  end;
end method;
*/

define method stream-at-end?
    (stream :: <circular-print-stream>) => (at-end? :: <boolean>)
  if (~ (*print-circle?* & (stream.circular-first-pass?)))
    stream-at-end?(stream.inner-stream);
  end;
end method;


/// Pretty-printer support.

/// The methods on this page extend the pprint interface to <print-stream>s.
/// Doing this allows users to write print-object methods that attempt to do
/// pretty printing, but when print is called with pretty?: #f, all the
/// pretty printing directions in the print-object method become no-ops.
///

/// pprint-logical-block -- Method for Exported Interface.
///
/// When we first enter this method, we pass the print-target of the
/// <print-stream> to the recursive call to pprint-logical-block.  This
/// causes pprint-logical-block to wrap a pretty printing stream around the
/// target.  Then, when pprint-logical-block calls the body: method defined
/// here, the body: method wraps the <print-stream> around the newly created
/// pretty printing stream, nesting the ultimate target stream twice.  This
/// allows printing to continue with the print function handling all the
/// stuff for the user as it is supposed to do, but as output is passed on
/// to the print-stream's target, it gets pretty print processed before
/// going onto the ultimate target.
///
/// Since the <print-stream> passed into this method is wrapped around the
/// outside of the pretty printing stream, should some print-object method
/// call ppring-logical-block, this method executes again.  However, during
/// this recursive execution, this method invokes pprint-logical-block on
/// the print-target of <print-stream>, which the pretty printing stream.
/// That means the value of pretty-stream in our body: method below is == to
/// print-target of our <print-stream>.  When this is true, we do not need
/// to do any extra wrapping because we already have the three streams
/// (print stream, pretty stream, and the target) nested just the way we
/// want them.
///
define sealed method pprint-logical-block
    (stream :: <circular-print-stream>,
     #key column :: <integer> = 0, prefix :: false-or(<byte-string>),
          per-line-prefix :: false-or(<byte-string>),
	  body :: <function>, suffix :: false-or(<byte-string>)) => ()
  if (prefix & per-line-prefix)
    error("Can't specify both a prefix: and a per-line-prefix:");
  end;
  case
    (*print-circle?* & (stream.circular-first-pass?)) => #f;
    (*print-pretty?*) =>
      let target = stream.inner-stream;
      pprint-logical-block(target,
			   column: column,
			   prefix: prefix,
			   per-line-prefix: per-line-prefix,
			   body: method (pretty-stream)
				   if (pretty-stream == target)
				     body(stream);
				   else
				     let orig-target = stream.inner-stream;
				     stream.inner-stream := pretty-stream;
				     body(stream);
				     stream.inner-stream := orig-target;
				   end;
				 end,
			   suffix: suffix);
    otherwise =>
      if (prefix | per-line-prefix)
	write(stream, (prefix | per-line-prefix));
      end;
      body(stream);
      if (suffix) write(stream, suffix) end;
  end case;
end method;

/// pprint-newline -- Method for Exported Interface.
///
define sealed method pprint-newline
    (kind :: <pretty-newline-kind>, stream :: <stream>)
 => ()
  case
    ((~ (*print-circle?* & (stream.circular-first-pass?)))
       & *print-pretty?*) =>
      pprint-newline(kind, stream.inner-stream);
    (kind == #"mandatory") =>
      new-line(stream);
  end;
end;

define sealed method pprint-indent
    (relative-to :: <indentation-kind>, n :: <integer>, stream :: <stream>)
 => ()
  if ((~ (*print-circle?* & (stream.circular-first-pass?)))
	& *print-pretty?*)
    pprint-indent(relative-to, n, stream.inner-stream);
  end;
end;

define sealed method pprint-tab
    (kind :: <tab-kind>, colnum :: <integer>, colinc :: <integer>,
     stream :: <stream>)
 => ()
  if ((~ (*print-circle?* & (stream.circular-first-pass?)))
	& *print-pretty?*)
    pprint-tab(kind, colnum, colinc, stream.inner-stream);
  end;
end;


/// 'printing-object'

define macro printing-object
  { printing-object (?object:expression, ?stream:name, #rest ?options:expression)
      ?:body
    end }
    => { begin
           let print-object-body = method (?stream) ?body end;
           do-printing-object(?object, ?stream, print-object-body, ?options)
         end }
end macro printing-object;

define method do-printing-object 
    (object, stream :: <stream>, continuation :: <function>,
     #key type? = #t, identity? = #t) => ()
  let class = object.object-class;
  printing-logical-block (stream, prefix: "{", suffix: "}")
    case
      type? & identity? =>
        write(stream, as-lowercase(as(<byte-string>, class.debug-name)));
        write(stream, " ");
        continuation(stream);
        //--- write(stream, " ");
        //--- write(stream, integer-to-string(object-address(object), base: 16));
      type? =>
        write(stream, as-lowercase(as(<byte-string>, class.debug-name)));
        write(stream, " ");
        continuation(stream);
      identity? =>
        continuation(stream);
        //--- write(stream, " ");
        //--- write(stream, integer-to-string(object-address(object), base: 16));
      otherwise =>
        continuation(stream);
    end
  end
end method do-printing-object;
