Module:    format-internals
Author:    Gwydion Project
Synopsis:  This file implements a simple mechanism for formatting output.
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



/// format-to-string.
///

/// format-to-string -- Exported.
///
define generic format-to-string (control-string :: <string>, #rest args)
    => result :: <string>;

define method format-to-string (control-string :: <byte-string>, #rest args)
    => result :: <byte-string>;
  // Format-to-string is typically used for small amounts of output, so
  // use a smaller string to collect the contents.
  let s :: <byte-string-stream>
    = make(<byte-string-stream>,
           contents: make(<byte-string>, size: 32), direction: #"output");
  apply(format, s, control-string, args);
  s.stream-contents
end method;



/// Print-message.
///

/// print-message -- Exported.
///
define open generic print-message (object :: <object>, stream :: <stream>)
    => ();

define /* sealed */ method print-message (object :: <object>, stream :: <stream>)
    => ();
  dynamic-bind (*print-escape?* = #f)        // print as a string
    print-object(object, stream)
  end
end method;

define /* sealed */ method print-message (object :: <condition>, stream :: <stream>)
    => ();
  //---*** This method is broken right now.  It should just call report-condition.
  //---*** If this interim bit of code is not good enough, then we'll have to do
  //---*** something better (assuming we can't just fix report-condition).
  //---*** report-condition(object, stream);
  dynamic-bind (*print-escape?* = #f)        // print as a string
    print-object(object, stream)
  end
end method;

define sealed method print-message (object :: <string>, stream :: <stream>)
    => ();
  write-text(stream, object);
end method;

define sealed method print-message (object :: <character>, stream :: <stream>)
    => ();
  write-element(stream, object);
end method;

define sealed method print-message (object :: <symbol>, stream :: <stream>)
    => ();
  write(stream, as(<string>, object));
end method;



/// Format.
///

define constant $dispatch-char = '%';

define constant char-classes :: <simple-object-vector>
  = make(<vector>, size: 256, fill: #f);
///
for (i from as(<byte>, '0') below (as(<byte>, '9') + 1))
  char-classes[i] := #"digit";
end;
char-classes[as(<byte>, '-')] := #"digit";


define generic format (stream :: <stream>, control-string :: <string>,
                       #rest args)
    => ();

define method format (stream :: <stream>, control-string :: <byte-string>,
                      #rest args)
    => ();
  let control-len :: <integer> = control-string.size;
  block (exit)
    let start :: <integer> = 0;
    let arg-i :: <integer> = 0;
    while (start < control-len)
      // Skip to dispatch char.
      for (i :: <integer> = start then (i + 1),
           until: ((i == control-len)
                   | (control-string[i] == $dispatch-char)
                   | (control-string[i] == '\n')))
      finally
        if (i ~== start)
          write(stream, control-string, start: start, end: i);
        end;
        if (i == control-len)
          exit();
        else
          start := i + 1;
        end;
      end for;
      if (control-string[start - 1] == '\n')
        new-line(stream)
      else
        // Parse for field within which to pad output.
        let (field, field-spec-end)
          = if (char-classes[as(<byte>, control-string[start])] == #"digit")
              parse-integer(control-string, start);
            end;
        if (field)
          // Capture output in string and compute padding.
          // Assume the output is very small in length.
          let s :: <byte-string-stream>
            = make(<byte-string-stream>,
                   contents: make(<byte-string>, size: 80),
                   direction: #"output");
          if (do-dispatch(control-string[field-spec-end], s,
                          element(args, arg-i, default: #f)))
            arg-i := arg-i + 1;
          end;
          let output :: <byte-string> = s.stream-contents;
          let output-len :: <integer> = output.size;
          let padding :: <integer> = (abs(field) - output-len);
          case
            (padding < 0) =>
              write(stream, output);
            (field > 0) =>
              write(stream, make(<byte-string>, size: padding, fill: ' '));
              write(stream, output);
            otherwise =>
              write(stream, output);
              write(stream, make(<byte-string>, size: padding, fill: ' '));
          end;
          start := field-spec-end + 1;  // Add one to skip dispatch char.
        else
          if (do-dispatch(control-string[start], stream,
                          element(args, arg-i, default: #f)))
            arg-i := arg-i + 1;
          end;
          start := start + 1;  // Add one to skip dispatch char.
        end
      end
    end while;
  end block;
end method;

/// do-dispatch -- Internal.
///
/// This function dispatches on char, which should be a format directive.
/// The return value indicates whether to consume one format argument;
/// otherwise, consume none.
///
define method do-dispatch (char :: <byte-character>, stream :: <stream>, arg)
    => consumed-arg? :: <boolean>;
  select (char by \==)
    ('s'), ('S'), ('c'), ('C') =>
      print-message(arg, stream);
      #t;
    ('=') =>
      dynamic-bind (*print-escape?* = #t)        // print as an object
        print-object(arg, stream)
      end;
      #t;
    ('d'), ('D') =>
      format-integer(arg, 10, stream);
      #t;
    ('b'), ('B') =>
      format-integer(arg, 2, stream);
      #t;
    ('o'), ('O') =>
      format-integer(arg, 8, stream);
      #t;
    ('x'), ('X') =>
      format-integer(arg, 16, stream);
      #t;
    ('m'), ('M') =>
      apply(arg, list(stream));
      #t;
    ('%') =>
      write-element(stream, '%');
      #f;
    otherwise =>
      error("Unknown format dispatch character, %c", char);
  end;
end method;

/// parse-integer -- Internal.
///
/// This function reads an integer from input starting at index.  Index must
/// be at the first digit or a leading negative sign.  This function reads
/// decimal representation, and it stops at the first character that is not
/// a decimal digit.  It returns the integer parsed and the index
/// immediately following the last decimal digit.
///
define method parse-integer (input :: <byte-string>, index :: <integer>)
    => (result :: false-or(<integer>), index :: <integer>);
  let result :: <integer> = 0;
  let negative? = if (input[index] = '-')
                    index := index + 1;
                  end;
  for (i :: <integer> = index then (i + 1),
       len :: <integer> = input.size then len,
       ascii-zero :: <byte> = as(<byte>, '0') then ascii-zero,
       until: ((i = len) |
                 (~ (char-classes[as(<byte>, input[i])] == #"digit"))))
    result := ((result * 10) + (as(<byte>, input[i]) - ascii-zero));
  finally
    if (result = 0)
      values(#f, index);
    else
      values(if (negative?) (- result) else result end, i);
    end;
  end;
end method;


define constant $digits = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";

/// format-integer -- internal.
///

///---*** KLUDGE: Temporary method until division is implemented for <double-integer>
///---*** Once division is available, remove this method!
define method format-integer (arg :: <double-integer>,
                              radix :: limited(<integer>, min: 2, max: 36),
                              stream :: <stream>) => ()
  print(arg, stream)
end method;

define method format-integer (arg :: <integer>,
                              radix :: limited(<integer>, min: 2, max: 36),
                              stream :: <stream>) => ()
  // Define an iteration that collects the digits for the print
  // representation of arg.
  local method repeat (arg :: <integer>, digits :: <list>)
          let (quotient :: <integer>, remainder :: <integer>)
            = floor/(arg, radix);
          let digits = pair($digits[remainder], digits);
          if (zero?(quotient))
            for (digit in digits)
              write-element(stream, digit)
            end
          else
            repeat(quotient, digits)
          end
        end;
  // Set up for the iteration.
  if (negative?(arg))
    write-element(stream, '-');
    // Pick off one digit before beginning the iteration to ensure that we
    // don't need Generic-arithmetic.  If arg were the minimum signed
    // machine word, and we simply negated it and called repeat, then it
    // would turn into an integer that was one larger than the maximum
    // signed integer.
    let (quotient :: <integer>, remainder :: <integer>)
      = truncate/(arg, radix);
    if (~ zero?(quotient))
      repeat(- quotient, list($digits[- remainder]))
    else
      write-element(stream, $digits[- remainder])
    end
  else
    repeat(arg, #())
  end
end method;

define method format-integer (arg :: <float>,
                              radix :: limited(<integer>, min: 2, max: 36),
                              stream :: <stream>) => ()
  //--- Should we really be this compulsive?
  assert(radix = 10, "Can only print floats in base 10");
  print(arg, stream)
end method;
