Module:       duim-utilities
Synopsis:     DUIM utilities
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// String and character functions
/// Note that this assumes <byte-string> implements 7-bit ASCII encoding

/// Utilities

define inline function upper-case-code?
    (code :: <integer>) => (true? :: <boolean>)
  as(<integer>, 'A') <= code & code <= as(<integer>, 'Z')
end function upper-case-code?;

define inline function lower-case-code?
    (code :: <integer>) => (true? :: <boolean>)
  as(<integer>, 'a') <= code & code <= as(<integer>, 'z')
end function lower-case-code?;


/// Case-insensitive character comparisons

define sealed method char-equal?
    (char1 :: <byte-character>, char2 :: <byte-character>)
 => (true? :: <boolean>)
  let code1 = as(<integer>, char1);
  let code2 = as(<integer>, char2);
  code1 == code2
  | (zero?(logand(#o337, logxor(code1, code2)))
     & (upper-case-code?(code1) | lower-case-code?(code1))
     & (upper-case-code?(code2) | lower-case-code?(code2)))
end method char-equal?;

define sealed method char-less?
    (char1 :: <byte-character>, char2 :: <byte-character>)
 => (true? :: <boolean>)
  let code1 = as(<integer>, char1);
  let code2 = as(<integer>, char2);
  when (lower-case-code?(code1))
    code1 := logxor(code1, #o40)
  end;
  when (lower-case-code?(code2))
    code2 := logxor(code2, #o40)
  end;
  code1 < code2
end method char-less?;

define sealed method char-greater? 
    (char1 :: <byte-character>, char2 :: <byte-character>)
 => (true? :: <boolean>)
  let code1 = as(<integer>, char1);
  let code2 = as(<integer>, char2);
  when (lower-case-code?(code1))
    code1 := logxor(code1, #o40)
  end;
  when (lower-case-code?(code2))
    code2 := logxor(code2, #o40)
  end;
  code1 > code2
end method char-greater?;


/// Case-insensitive string comparisons

define sealed method string-equal? 
    (string1 :: <byte-string>, string2 :: <byte-string>,
     #key start1 :: <integer> = 0, end1 :: <integer> = size(string1),
	  start2 :: <integer> = 0, end2 :: <integer> = size(string2))
 => (true? :: <boolean>)
  range-check(string1, size(string1), start1, end1);
  range-check(string2, size(string2), start2, end2);
  block (return)
    end1 - start1 = end2 - start2
    & without-bounds-checks
	for (i :: <integer> from start1 below end1,
	     j :: <integer> from start2 below end2)
	  let char1 :: <byte-character> = string1[i];
	  let char2 :: <byte-character> = string2[j];
	  unless (char-equal?(char1, char2))
	    return(#f)
	  end;
	finally
	  return(#t);
	end
      end
  end
end method string-equal?;

define sealed method string-less?
    (string1 :: <byte-string>, string2 :: <byte-string>,
     #key start1 :: <integer> = 0, end1 :: <integer> = size(string1),
	  start2 :: <integer> = 0, end2 :: <integer> = size(string2))
 => (true? :: <boolean>)
  range-check(string1, size(string1), start1, end1);
  range-check(string2, size(string2), start2, end2);
  let length1 = end1 - start1;
  let length2 = end2 - start2;
  let result = string-compare(string1, start1, 
			      string2, start2, min(length1, length2));
  if (result = 0)
    length1 < length2
  else
    result < 0
  end
end method string-less?;

define sealed method string-greater?
    (string1 :: <byte-string>, string2 :: <byte-string>,
     #key start1 :: <integer> = 0, end1 :: <integer> = size(string1),
	  start2 :: <integer> = 0, end2 :: <integer> = size(string2))
 => (true? :: <boolean>)
  range-check(string1, size(string1), start1, end1);
  range-check(string2, size(string2), start2, end2);
  let length1 = end1 - start1;
  let length2 = end2 - start2;
  let result = string-compare(string1, start1,
			      string2, start2, min(length1, length2));
  if (result = 0)
    length1 > length2
  else
    result > 0
  end
end method string-greater?;

define sealed method string-compare
    (string1 :: <byte-string>, start1 :: <integer>, 
     string2 :: <byte-string>, start2 :: <integer>, count :: <integer>)
 => (result :: <integer>)
  let subrange1 = size(string1) - start1;
  let subrange2 = size(string2) - start2;
  let state = 0;
  case
    count > subrange1 =>
      case
	count > subrange2 =>
	  count := min(subrange1, subrange2);
	  state := 1;
	otherwise =>
	  count := subrange1;
	  state := 2
      end;
    count > subrange2 =>
      count := subrange2;
      state := 3
  end;
  block (return)
    without-bounds-checks
      for (i1 :: <integer> = start1 then i1 + 1,
	   i2 :: <integer> = start2 then i2 + 1,
	   until: count = 0)
	let char1 :: <byte-character> = string1[i1];
	let char2 :: <byte-character> = string2[i2];
	unless (char-equal?(char1, char2))
	  return(if (char-less?(char1, char2))
		   (start1 - i1) - 1
		 else
		   (i1 + 1) - start1
		 end)
	end;
	count := count - 1;
      finally
	select (state)
	  0 => 0;
	  1 => case
		 subrange1 = subrange2 => 0;
		 subrange1 < subrange2 => -1 - i1;
		 otherwise => i1 + 1
	       end;
	  2 => (start1 - i1) - 1;
	  otherwise => (i1 - start1) + 1
	end
      end
    end
  end
end method string-compare;


/// Predicates

define sealed method alpha-char?
    (char :: <byte-character>) => (true? :: <boolean>)
  let code = as(<integer>, char);
  upper-case-code?(code) | lower-case-code?(code)
end method alpha-char?;

define sealed method digit-char?
    (char :: <byte-character>, #key radix :: <integer> = 10) => (true? :: <boolean>)
  let code = as(<integer>, char);
  (as(<integer>, '0') <= code & code <= as(<integer>, '9'))
  | (radix > 10 & radix < 36
     & ((code   >= as(<integer>, 'A') & code - as(<integer>, 'A') < radix - 10)
	| (code >= as(<integer>, 'a') & code - as(<integer>, 'a') < radix - 10)))
end method digit-char?;

define sealed inline method alphanumeric-char?
    (char :: <byte-character>) => (true? :: <boolean>)
  alpha-char?(char) | digit-char?(char)
end method alphanumeric-char?;


define sealed method upper-case?
    (char :: <byte-character>) => (true? :: <boolean>)
  let code = as(<integer>, char);
  upper-case-code?(code)
end method upper-case?;

define sealed method lower-case?
    (char :: <byte-character>) => (true? :: <boolean>)
  let code = as(<integer>, char);
  lower-case-code?(code)
end method lower-case?;


// Returns #t iff the character is a "graphic" (printing) character
define sealed method graphic-char?
    (char :: <byte-character>) => (true? :: <boolean>)
  let code = as(<integer>, char);
  as(<integer>, ' ') <= code & code <= as(<integer>, '~')
end method graphic-char?;

// Returns #t iff the character is a "graphic" (printing) character
// or is one of the other "standard" characters
define sealed method standard-char?
    (char :: <byte-character>) => (true? :: <boolean>)
  let code = as(<integer>, char);
  (as(<integer>, ' ') <= code & code <= as(<integer>, '~'))
  | code == as(<integer>, '\n')
  | code == as(<integer>, '\r')
  | code == as(<integer>, '\t')
  | code == as(<integer>, '\b')
  | code == as(<integer>, '\f')
end method standard-char?;

define sealed inline method whitespace-char?
    (char :: <byte-character>) => (true? :: <boolean>)
  char == ' ' | char == '\t'
end method whitespace-char?;


/// Other string utilities

define inline function string-capitalize
    (string :: <byte-string>,
     #key start: _start :: <integer> = 0, end: _end :: <integer> = size(string))
 => (string :: <byte-string>)
  string-capitalize!(copy-sequence(string), start: _start, end: _end)
end function string-capitalize;

define sealed method string-capitalize!
    (string :: <byte-string>,
     #key start: _start :: <integer> = 0, end: _end :: <integer> = size(string))
 => (string :: <byte-string>)
  range-check(string, size(string), _start, _end);
  let state = #f;
  without-bounds-checks
    for (i :: <integer> from _start below _end)
      let char :: <byte-character> = string[i];
      case
	~state =>		// between words
	  case
	    alpha-char?(char) =>
	      string[i] := as-uppercase(char);
	      state := #t;
	    digit-char?(char) =>
	      state := #t;
	  end;
	otherwise =>
	  case
	    alpha-char?(char) =>
	      string[i] := as-lowercase(char);
	    ~digit-char?(char) =>
	      state := #f;
	  end;
      end
    end
  end;
  string
end method string-capitalize!;


/// Cool string hacks

// Pluralize the given string
define method string-pluralize
    (string :: <byte-string>, #key count) => (plural :: <byte-string>)
  let length :: <integer> = size(string);
  let pos    :: <integer> = (string-search-set(string, #[' ', '\t'], from-end?: #t) | -1) + 1;
  if (length = 0 | (count & count = 1))
    string
  else
    let flush  = #f;
    let suffix = #f;
    let last-char   :: <byte-character> = string[length - 1];
    let penult-char :: <byte-character>
      = if (length > 1) string[length - 2] else '*' end;
    local method find-char (chars :: <byte-string>, char :: <byte-character>)
	    member?(char, chars, test: char-equal?)
	  end method;
    case
      char-equal?(last-char, 'y')
      & ~find-char("aeiou", penult-char) =>
	flush  := 1;
	suffix := "ies";
      string-equal?(string, "ox", start1: pos)
      | string-equal?(string, "vax", start1: pos) =>
	suffix := "en";
      length >= 5
      & string-equal?(string, "index", start1: length - 5) =>
	flush  := 2;
	suffix := "ices";
      (char-equal?(last-char, 'h') & find-char("cs", penult-char))
      | find-char("szx", last-char) =>
	suffix := "es";
      length >= 3
      & string-equal?(string, "man", start1: length - 3)
      & ~string-equal?(string, "human", start1: pos) =>
	flush  := 2;
	suffix := "en";
      length >= 3
      & string-equal?(string, "ife", start1: length - 3) =>
	flush  := 2;
	suffix := "ves";
      length >= 5
      & string-equal?(string, "child", start1: length - 5) =>
	suffix := "ren";
      otherwise =>
	suffix := "s";
    end;
    concatenate-as(<byte-string>,
		   if (flush) copy-sequence(string, start: 0, end: length - flush)
		   else string end,
		   suffix)
  end
end method string-pluralize;

// Returns an article to be used with the specified string
// We admittedly heuristicate...
define method string-a-or-an
    (string :: <byte-string>) => (article :: <byte-string>)
  let length :: <integer> = size(string);
  if (length = 0)
    ""
  else
    let char :: <byte-character> = string[0];
    if (digit-char?(char))	// pronounce leading digits number
      string-a-or-an(pronounce-string(string))
    else
      local method find-char (chars :: <byte-string>, char :: <byte-character>)
	      member?(char, chars, test: char-equal?)
	    end method;
      case
	string-equal?(string, "one")
	| (length >= 4 & string-equal?(string, "one ", end1: 4)) =>
	  "a ";
	length = 1
	// "an x", but "a xylophone"
	// "an fff", but "a frog"
	| ~string-search-set(string, "aeiou")
	// "an xl400", but "a xylophone"
        | string-search-set(string, "0123456789") =>
	  if (find-char("aefhilmnorsx", char)) "an " else "a " end;
	otherwise =>
	  if (find-char("aio", char)
              // "an egg", but "a eunich"
              | (char-equal?(char, 'e')
                 & ~string-equal?(string, "eu", end1: 2))
              // "an umbrella", but "a unicorn"
              // "a uniform", but "an uninformed ..."
              // And of course, "a unix"
              | (char-equal?(char, 'u')
                 & ~(string-equal?(string, "uni", end1: 3)
                     & (length < 5
                        // Treat "y" as a vowel here, e.g., "unicycle"
                        | ~find-char("bcdfghjklmnpqrstvwxz", string[4])))))
	    "an "
	  else
            "a "
	  end
      end
    end
  end
end method string-a-or-an;

//--- Implement this sometime!
define method pronounce-string
    (string :: <byte-string>) => (new-string :: <byte-string>)
  string
end method pronounce-string;

// Find any of the given characters within a string
define method string-search-set
    (string :: <byte-string>, char-set :: <sequence>,
     #key start: _start :: <integer> = 0, end: _end :: <integer> = size(string),
	  from-end? :: <boolean> = #f, test = char-equal?)
 => (index :: false-or(<integer>))
  range-check(string, size(string), _start, _end);
  block (return)
    let set-length :: <integer> = size(char-set);
    without-bounds-checks
      if (from-end?)
	for (i :: <integer> = _end - 1 then i - 1,
	     until: i < _start)
	  let char :: <byte-character> = string[i];
	  for (j :: <integer> = 0 then j + 1,
	       until: j >= set-length)
	    when (test(char, char-set[j]))
	      return(i)
	    end;
	  finally #f;
	  end;
	finally #f;
	end
      else
	for (i :: <integer> = _start then i + 1,
	     until: i >= _end)
	  let char :: <byte-character> = string[i];
	  for (j :: <integer> = 0 then j + 1,
	       until: j >= set-length)
	    when (test(char, char-set[j]))
	      return(i)
	    end;
	  finally #f;
	  end;
	finally #f;
	end
      end
    end
  end
end method string-search-set;
