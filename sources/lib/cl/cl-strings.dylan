Module:    CL-internals
Author:    Scott McKay
Synopsis:  Implementation of useful Common Lisp string functions
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Utilities

define function upper-case-code? (code :: <integer>) => (true? :: <boolean>)
  as(<integer>, 'A') <= code & code <= as(<integer>, 'Z')
end function upper-case-code?;

define function lower-case-code? (code :: <integer>) => (true? :: <boolean>)
  as(<integer>, 'a') <= code & code <= as(<integer>, 'z')
end function lower-case-code?;


/// Case-insensitive character comparisons

define generic char-equal?
    (char1 :: <character>, char2 :: <character>, #key character-set)
 => (true? :: <boolean>);

define generic char-less?
    (char1 :: <character>, char2 :: <character>, #key character-set)
 => (true? :: <boolean>);

define generic char-greater?
    (char1 :: <character>, char2 :: <character>, #key character-set)
 => (true? :: <boolean>);

//--- The character set should default from the character itself.  For example,
//--- if there's a Unicode character, use Unicode.  "It is an error" if the
//--- two characters don't share the same character set.
define method char-equal?
    (char1 :: <character>, char2 :: <character>, #key character-set)
 => (true? :: <boolean>)
  let code1 = as(<integer>, char1);
  let code2 = as(<integer>, char2);
  code1 = code2
  | if (character-set)
      // Assume ASCII byte string encoding
      do-char-equal?(character-set, char1, char2)
    else
      zero?(logand(#o337, logxor(code1, code2)))
      & (upper-case-code?(code1) | lower-case-code?(code1))
      & (upper-case-code?(code2) | lower-case-code?(code2))
    end
end method char-equal?;

define method do-char-equal?
    (char-set, char1 :: <character>, char2 :: <character>)
 => (true? :: <boolean>)
  let code1 = as(<integer>, char1);
  let code2 = as(<integer>, char2);
  code1 = code2
  | zero?(logand(#o337, logxor(code1, code2)))
    & (upper-case-code?(code1) | lower-case-code?(code1))
    & (upper-case-code?(code2) | lower-case-code?(code2))
end method do-char-equal?;

define function char-not-equal?
    (char1 :: <character>, char2 :: <character>, #key character-set)
 => (true? :: <boolean>)
  ~char-equal?(char1, char2, character-set: character-set)
end function char-not-equal?;

define method char-less?
    (char1 :: <character>, char2 :: <character>, #key character-set)
 => (true? :: <boolean>)
  if (character-set)
    // Assume ASCII byte string encoding
    do-char-less?(character-set, char1, char2)
  else
    let code1 = as(<integer>, char1);
    let code2 = as(<integer>, char2);
    if (lower-case-code?(code1))
      code1 := logxor(code1, #o40)
    end;
    if (lower-case-code?(code2))
      code2 := logxor(code2, #o40)
    end;
    code1 < code2
  end
end method char-less?;

define method do-char-less?
    (char-set, char1 :: <character>, char2 :: <character>)
 => (true? :: <boolean>)
  let code1 = as(<integer>, char1);
  let code2 = as(<integer>, char2);
  if (lower-case-code?(code1))
    code1 := logxor(code1, #o40)
  end;
  if (lower-case-code?(code2))
    code2 := logxor(code2, #o40)
  end;
  code1 < code2
end method do-char-less?;

define function char-not-less? 
    (char1 :: <character>, char2 :: <character>, #key character-set)
 => (true? :: <boolean>)
  ~char-less?(char1, char2, character-set: character-set)
end function char-not-less?;

define method char-greater? 
    (char1 :: <character>, char2 :: <character>, #key character-set)
 => (true? :: <boolean>)
  if (character-set)
    // Assume ASCII byte string encoding
    do-char-greater?(character-set, char1, char2)
  else
    let code1 = as(<integer>, char1);
    let code2 = as(<integer>, char2);
    if (lower-case-code?(code1))
      code1 := logxor(code1, #o40)
    end;
    if (lower-case-code?(code2))
      code2 := logxor(code2, #o40)
    end;
    code1 > code2
  end
end method char-greater?;

define method do-char-greater?
    (char-set, char1 :: <character>, char2 :: <character>)
 => (true? :: <boolean>)
  let code1 = as(<integer>, char1);
  let code2 = as(<integer>, char2);
  if (lower-case-code?(code1))
    code1 := logxor(code1, #o40)
  end;
  if (lower-case-code?(code2))
    code2 := logxor(code2, #o40)
  end;
  code1 > code2
end method do-char-greater?;

define function char-not-greater? 
    (char1 :: <character>, char2 :: <character>, #key character-set)
 => (true? :: <boolean>)
  ~char-greater?(char1, char2, character-set: character-set)
end function char-not-greater?;


/// Case-insensitive string comparisons

define generic string-equal? 
    (string1 :: <string>, string2 :: <string>,
     #key start1, end1, start2, end2, character-set)
    
 => (true? :: <boolean>);

define generic string-less?
    (string1 :: <string>, string2 :: <string>,
     #key start1, end1, start2, end2, character-set)
 => (true? :: <boolean>);

define generic string-greater?
    (string1 :: <string>, string2 :: <string>,
     #key start1, end1, start2, end2, character-set)
 => (true? :: <boolean>);

define method string-equal? 
    (string1 :: <string>, string2 :: <string>,
     #key start1 = 0, end1, start2 = 0, end2, character-set)
 => (true? :: <boolean>)
  block (return)
    unless (end1)
      end1 := size(string1)
    end;
    unless (end2)
      end2 := size(string2)
    end;
    end1 - start1 = end2 - start2
    & for (i from start1 below end1,
           j from start2 below end2)
        let char1 = string1[i];
        let char2 = string2[j];
        if (char-not-equal?(char1, char2, character-set: character-set))
          return(#f)
        end;
      finally
        return(#t);
      end
  end
end method string-equal?;

define function string-not-equal?
    (string1 :: <string>, string2 :: <string>,
     #rest keys, #key start1 = 0, end1, start2 = 0, end2, character-set)
 => (true? :: <boolean>)
  // declare dynamic-extent keys;
  // declare ignore start1, end1, start2, end2, character-set;
  ~apply(string-equal?, string1, string2, keys)
end function string-not-equal?;

define method string-less?
    (string1 :: <string>, string2 :: <string>,
     #key start1 = 0, end1, start2 = 0, end2, character-set)
 => (true? :: <boolean>)
  unless (end1)
    end1 := size(string1)
  end;
  unless (end2)
    end2 := size(string2)
  end;
  let length1 = end1 - start1;
  let length2 = end2 - start2;
  let result = string-compare(string1, start1, 
			      string2, start2, min(length1, length2),
			      character-set: character-set);
  if (zero?(result))
    length1 < length2
  else
    negative?(result)
  end
end method string-less?;

define function string-not-less?
    (string1 :: <string>, string2 :: <string>,
     #rest keys, #key start1 = 0, end1, start2 = 0, end2, character-set)
 => (true? :: <boolean>)
  // declare dynamic-extent keys;
  // declare ignore start1, end1, start2, end2, character-set;
  ~apply(string-less?, string1, string2, keys)
end function string-not-less?;

define method string-greater?
    (string1 :: <string>, string2 :: <string>,
     #key start1 = 0, end1, start2 = 0, end2, character-set)
 => (true? :: <boolean>)
  unless (end1)
    end1 := size(string1)
  end;
  unless (end2)
    end2 := size(string2)
  end;
  let length1 = end1 - start1;
  let length2 = end2 - start2;
  let result = string-compare(string1, start1,
			      string2, start2, min(length1, length2),
			      character-set: character-set);
  if (zero?(result))
    length1 > length2
  else
    positive?(result)
  end
end method string-greater?;

define function string-not-greater?
    (string1 :: <string>, string2 :: <string>,
     #rest keys, #key start1 = 0, end1, start2 = 0, end2, character-set)
 => (true? :: <boolean>)
  // declare dynamic-extent keys;
  // declare ignore start1, end1, start2, end2, character-set;
  ~apply(string-greater?, string1, string2, keys)
end function string-not-greater?;


define method string-compare
    (string1 :: <string>, start1 :: <integer>, 
     string2 :: <string>, start2 :: <integer>,
     count :: <integer>, #key character-set)
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
    for (i1 = start1 then i1 + 1,
	 i2 = start2 then i2 + 1,
	 until: zero?(count))
      let char1 = string1[i1];
      let char2 = string2[i2];
      unless (char-equal?(char1, char2, character-set: character-set))
	return
	  (if (char-less?(char1, char2, character-set: character-set))
	     (start1 - i1) - 1
	   else
	     (i1 + 1) - start1
	   end)
      end;
      count := count - 1;
    finally
      select (state)
	0 =>
	  0;
	1 =>
	  case
	    subrange1 = subrange2 =>
	      0;
	    subrange1 < subrange2 =>
	      -1 - i1;
	    otherwise =>
	      i1 + 1
	  end;
	2 =>
	  (start1 - i1) - 1;
	otherwise =>
	  (i1 - start1) + 1
      end;
    end
  end
end method string-compare;


/// Predicates

define method alpha-char?
    (char :: <character>, #key character-set) => (true? :: <boolean>)
  if (character-set)
    // Assume ASCII byte string encoding
    do-alpha-char?(character-set, char)
  else
    let code = as(<integer>, char);
    upper-case-code?(code) | lower-case-code?(code)
  end
end method alpha-char?;

define method do-alpha-char?
    (char-set, char :: <character>) => (true? :: <boolean>)
  let code = as(<integer>, char);
  upper-case-code?(code) | lower-case-code?(code)
end method do-alpha-char?;

define method digit-char?
    (char :: <character>, #key character-set, radix = 10) => (true? :: <boolean>)
  if (character-set)
    // Assume ASCII byte string encoding
    do-digit-char?(character-set, char, radix: radix)
  else
    let code = as(<integer>, char);
    (as(<integer>, '0') <= code & code <= as(<integer>, '9'))
    | (radix > 10 & radix < 36
       & ((code >= as(<integer>, 'A') & code - as(<integer>, 'A') < radix - 10)
	  | (code >= as(<integer>, 'a') & code - as(<integer>, 'a') < radix - 10)))
  end
end method digit-char?;

define method do-digit-char?
    (char-set, char :: <character>, #key radix = 10) => (true? :: <boolean>)
  let code = as(<integer>, char);
  (as(<integer>, '0') <= code & code <= as(<integer>, '9'))
  | (radix > 10 & radix < 36
     & ((code >= as(<integer>, 'A') & code - as(<integer>, 'A') < radix - 10)
	| (code >= as(<integer>, 'a') & code - as(<integer>, 'a') < radix - 10)))
end method do-digit-char?;

define method alphanumeric-char?
    (char :: <character>, #key character-set) => (true? :: <boolean>)
  alpha-char?(char, character-set: character-set)
  | digit-char?(char, character-set: character-set)
end method alphanumeric-char?;


define method upper-case?
    (char :: <character>, #key character-set) => (true? :: <boolean>)
  if (character-set)
    // Assume ASCII byte string encoding
    do-upper-case?(character-set, char)
  else
    let code = as(<integer>, char);
    upper-case-code?(code)
  end
end method upper-case?;

define method do-upper-case?
    (char-set, char :: <character>) => (true? :: <boolean>)
  let code = as(<integer>, char);
  upper-case-code?(code)
end method do-upper-case?;

define method lower-case?
    (char :: <character>, #key character-set) => (true? :: <boolean>)
  if (character-set)
    // Assume ASCII byte string encoding
    do-lower-case?(character-set, char)
  else
    let code = as(<integer>, char);
    lower-case-code?(code)
  end
end method lower-case?;

define method do-lower-case?
    (char-set, char :: <character>) => (true? :: <boolean>)
  let code = as(<integer>, char);
  lower-case-code?(code)
end method do-lower-case?;


define method standard-char?
    (char :: <character>, #key character-set) => (true? :: <boolean>)
  if (character-set)
    // Assume ASCII byte string encoding
    do-standard-char?(character-set, char)
  else
    let code = as(<integer>, char);
    code = as(<integer>, '\r')
    | (as(<integer>, ' ') <= code & code <= as(<integer>, '~'))
  end
end method standard-char?;

define method do-standard-char?
    (char-set, char :: <character>) => (true? :: <boolean>)
  let code = as(<integer>, char);
  code = as(<integer>, '\r')
  | (as(<integer>, ' ') <= code & code <= as(<integer>, '~'))
end method do-standard-char?;

define method graphic-char?
    (char :: <character>, #key character-set) => (true? :: <boolean>)
  if (character-set)
    // Assume ASCII byte string encoding
    do-graphic-char?(character-set, char)
  else
    let code = as(<integer>, char);
    code < #o200
  end
end method graphic-char?;

define method do-graphic-char?
    (char-set, char :: <character>) => (true? :: <boolean>)
  let code = as(<integer>, char);
  code < #o200
end method do-graphic-char?;

define method ordinary-char?
    (char :: <character>, #key character-set) => (true? :: <boolean>)
  graphic-char?(char, character-set: character-set)
  | char == '\n'
  | char == '\r'
  | char == '\t'
end method ordinary-char?;

define function whitespace-char? (char :: <character>) => (true? :: <boolean>)
  char == ' ' | char == '\t'
end function whitespace-char?;


/// Miscellaneous

// Capitalize an entire string

define function string-capitalize
    (string :: <string>, #key start = 0, end: finish) => (string :: <string>)
  string-capitalize!(copy-sequence(string), start: start, end: finish)
end function string-capitalize;

define method string-capitalize!
    (string :: <string>, #key start = 0, end: finish) => (string :: <string>)
  unless (finish)
    finish := size(string)
  end;
  let state = #f;
  for (i from start below finish)
    let char = string[i];
    case
      ~state =>
        //between words
        case
          alpha-char?(char) =>
            string[i] := as-uppercase(char);
            state := #t;
          digit-char?(char) =>
            state := #t
        end;
      otherwise =>
        case
          alpha-char?(char) =>
            string[i] := as-lowercase(char);
          ~digit-char?(char) =>
            state := #f
        end
    end;
  end;
  string
end method string-capitalize!;


// Capitalize each word in a string, converting dashes to spaces
// along the way
define function string-capitalize-words
    (string :: <string>, #key start = 0, end: finish) => (string :: <string>)
  string-capitalize-words!(copy-sequence(string), start: start, end: finish)
end function string-capitalize-words;

define method string-capitalize-words!
    (string :: <string>, #key start = 0, end: finish) => (string :: <string>)
  unless (finish)
    finish := size(string)
  end;
  let state = #t;
  for (i from start below finish)
    let char = string[i];
    case
      char == '-' =>
        string[i] := ' ';
        state := #t;
      char == ' ' =>
        state := #t;
      state =>
        if (alpha-char?(char))
          string[i] := as-uppercase(char)
        end;
        state := #f;
      alpha-char?(char) =>
        string[i] := as-lowercase(char)
    end;
  end;
  string
end method string-capitalize-words!;


define method string-trim
    (string :: <string>, char-set) => (string :: <string>)
  let i = string-search-not-set(string, char-set);
  case
    ~i =>
      "";
    otherwise =>
      let j = string-search-not-set(string, char-set, from-end?: #t);
      copy-sequence(string, start: i, end: j + 1)
  end
end method string-trim;

define method string-left-trim
    (string :: <string>, char-set) => (string :: <string>)
  let i = string-search-not-set(string, char-set);
  case
    i =>
      copy-sequence(string, start: i, end: size(string));
    otherwise =>
      ""
  end
end method string-left-trim;

define method string-right-trim
    (string :: <string>, char-set) => (string :: <string>)
  let i = string-search-not-set(string, char-set, from-end?: #t);
  case
    i =>
      copy-sequence(string, start: 0, end: i + 1);
    otherwise =>
      ""
  end
end method string-right-trim;


// Find any of the given characters within a string
define method string-search-set
    (string :: <string>, char-set,
     #key start = 0, end: finish, from-end?, test = char-equal?)
 => (index :: false-or(<integer>))
  block (return)
    unless (finish)
      finish := size(string)
    end;
    let set-length = size(char-set);
    if (from-end?)
      for (i = finish - 1 then i - 1, until: i < start)
        let char = string[i];
        for (j = 0 then j + 1, until: j >= set-length)
          if (test(char, char-set[j]))
            return(i)
          end;
        finally
          #f;
        end;
      finally
        #f;
      end
    else
      for (i = start then i + 1, until: i >= finish)
        let char = string[i];
        for (j = 0 then j + 1, until: j >= set-length)
          if (test(char, char-set[j]))
            return(i)
          end;
        finally
          #f;
        end;
      finally
        #f;
      end
    end
  end
end method string-search-set;

define method string-search-not-set
    (string :: <string>, char-set,
     #key start = 0, end: finish, from-end?, test = char-equal?)
 => (index :: false-or(<integer>))
  block (return)
    unless (finish)
      finish := size(string)
    end;
    let set-length = size(char-set);
    if (from-end?)
      for (i = finish - 1 then i - 1, until: i < start)
        let char = string[i];
        block (break)
          for (j = 0 then j + 1, until: j >= set-length)
            if (test(char, char-set[j]))
              break()
            end;
          finally
            return(i);
          end
        end;
      finally
        #f;
      end
    else
      for (i = start then i + 1, until: i >= finish)
        let char = string[i];
        block (break)
          for (j = 0 then j + 1, until: j >= set-length)
            if (test(char, char-set[j]))
              break()
            end;
          finally
            return(i);
          end
        end;
      finally
        #f;
      end
    end
  end
end method string-search-not-set;


// Pluralize the given string
define method string-pluralize
    (string :: <string>, #key count) => (plural :: <string>)
  block (return)
    let length = size(string);
    let pos
      = (string-search-set(string, #(' ', '\t'), from-end?: #t) | -1) + 1;
    if (zero?(length) | (count & count = 1))
      return(string)
    end;
    let flush = #f;
    let suffix = #f;
    let last-char = string[length - 1];
    let penult-char
      = if (length > 1) string[length - 2] else '*' end;
    begin
      let find-char
        = method (char-set, char)
            member?(char, char-set, test: char-equal?)
          end;
      // declare dynamic-extent find-char;
      case
        char-equal?(last-char, 'y')
        & ~find-char(#('a', 'e', 'i', 'o', 'u'), penult-char) =>
          flush := 1;
          suffix := "ies";
        string-equal?(string, "ox", start1: pos)
        | string-equal?(string, "vax", start1: pos) =>
          suffix := "en";
        (char-equal?(last-char, 'h') & find-char(#('c', 's'), penult-char))
        | find-char(#('s', 'z', 'x'), last-char) =>
          suffix := "es";
        length >= 3
        & string-equal?(string, "man", start1: length - 3)
        & ~string-equal?(string, "human", start1: pos) =>
          flush := 2;
          suffix := "en";
        length >= 3 & string-equal?(string, "ife", start1: length - 3) =>
          flush := 2;
          suffix := "ves";
        length >= 5 & string-equal?(string, "child", start1: length - 5) =>
          suffix := "ren";
        otherwise =>
          suffix := "s"
      end
    end;
    concatenate-as
      (<string>,
       if (flush)
         copy-sequence(string, start: 0, end: length - flush)
       else
         string
       end,
       suffix)
  end
end method string-pluralize;

// Returns an article to be used with the specified string
define method string-a-or-an
    (string :: <string>) => (article :: <string>);
  block (return)
    let length = size(string);
    let char = ~zero?(length) & string[0];
    if (zero?(length))
      return("")
    end;
    if (digit-char?(char))  //--- Pronounce leading digits number!
      string-a-or-an(pronounce-string(string))
    else
      let find-char
        = method (char-set, char)
            member?(char, char-set, test: char-equal?)
          end;
      // declare dynamic-extent find-char;
      case
        string-equal?(string, "one")
        | (length >= 4 & string-equal?(string, "one ", end1: 4)) =>
          "a ";
        length = 1
        //"an x", but "a xylophone"
        // "an fff" but "a frog"
        | ~string-search-set(string, "aeiou")
        // "an xl400" but "a xylophone"
        | string-search-set(string, "0123456789") =>
          if (find-char("aefhilmnorsx", char)) "an " else "a " end;
        otherwise =>
          if (find-char("aio", char)
              // "an egg", but "a eunich"
              | (char-equal?(char, 'e')
                 & ~string-equal?(string, "eu", end1: 2))
              // "an umbrella", but "a unicorn"
              // "a uniform", but "an uninformed ..."
              // And of course, "a unix".  We admittedly heuristicate
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
    (string :: <string>) => (new-string :: <string>)
  string
end method pronounce-string;
