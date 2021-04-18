Module:    CL-internals
Author:    Scott McKay
Synopsis:  Implementation of useful Common Lisp string functions
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Most of the original code here has been deleted because it had become
// redundant with the "strings" library and/or was not used.

// Pluralize the given string
define method string-pluralize
    (string :: <string>, #key count) => (plural :: <string>)
  block (return)
    let length = size(string);
    let pos = (find-any(string, whitespace?, from-end?: #t) | -1) + 1;
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
            member?(char, char-set, test: char-equal-ic?)
          end;
      // declare dynamic-extent find-char;
      case
        char-equal-ic?(last-char, 'y')
        & ~find-char(#('a', 'e', 'i', 'o', 'u'), penult-char) =>
          flush := 1;
          suffix := "ies";
        string-equal?(string, "ox", start1: pos)
        | string-equal?(string, "vax", start1: pos) =>
          suffix := "en";
        (char-equal-ic?(last-char, 'h') & find-char(#('c', 's'), penult-char))
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
