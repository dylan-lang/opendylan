Module:       common-dylan-internals
Author:       Andy Armstrong
Synopsis:     Implementations of format-to-string and format-out
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// String buffers

//---*** Oh for a stretchy string...
define constant <string-buffer> = limited(<stretchy-vector>, of: <byte-character>);

//---*** Is there a more efficient way to do this?
define function print-string
    (buffer :: <string-buffer>, string :: <byte-string>) => ()
  for (character :: <byte-character> in string)
    add!(buffer, character)
  end
end function print-string;

define function buffer-contents
    (buffer :: <string-buffer>) => (contents :: <byte-string>)
  as(<byte-string>, buffer)
end function buffer-contents;


/// User assertions (maybe export this?)

define macro user-assert
 { user-assert(?value:expression, ?format-string:expression, ?format-arguments:*) }
    => { unless (?value)
           user-assertion-error(?format-string, ?format-arguments)
         end }
end macro user-assert;

define class <user-assertion-error> (<simple-error>)
end class <user-assertion-error>;

define function user-assertion-error
    (format-string :: <string>, #rest format-arguments)
  error(make(<user-assertion-error>,
             format-string: format-string,
             format-arguments: format-arguments))
end function user-assertion-error;


/// format-to-string

define function print-format
    (buffer :: <string-buffer>, format-string :: <string>,
     #rest format-arguments)
 => ()
  let found-percent? = #f;
  let argument-index :: <integer> = 0;
  let no-of-arguments = size(format-arguments);
  local method argument
            (char :: <character>, class :: <class>) => (argument)
          let current-index = argument-index;
          argument-index := argument-index + 1;
          user-assert(current-index < no-of-arguments,
                      "Too few arguments for format string %=: %=",
                      format-string, format-arguments);
          let argument = format-arguments[current-index];
          user-assert(class == <object> | instance?(argument, class),
                      "Format argument for directive '%%%c' not of class %s: %=",
                      char, object-name(class) | class, argument);
          argument
        end;
  local method collect (string :: <string>) => ()
          print-string(buffer, string)
        end method collect;
  local method collect-character (character :: <character>) => ()
          add!(buffer, character)
        end method collect-character;
  for (char :: <character> in format-string)
    if (found-percent?)
      select (as-uppercase(char))
        'D' => collect(number-to-string(argument(char, <number>)));
        'B' => collect(integer-to-string(argument(char, <integer>), base: 2));
        'O' => collect(integer-to-string(argument(char, <integer>), base: 8));
        'X' => collect(integer-to-string(argument(char, <integer>), base: 16));
        'C' => collect-character(argument(char, <character>));
        'S' => print-pretty-name(buffer, argument(char, <object>));
        '=' => print-unique-name(buffer, argument(char, <object>));
        '%' => collect-character('%');
        otherwise =>
          error("Invalid format directive '%s' in \"%s\"",
                char, format-string);
      end;
      found-percent? := #f;
    else
      if (char == '%')
        found-percent? := #t;
      else
        collect-character(char)
      end
    end
  end;
  user-assert(~found-percent?,
              "Incomplete format directive in \"%s\"", format-string);
end function print-format;

define function format-to-string
    (format-string :: <string>, #rest format-arguments)
 => (string :: <string>)
  let buffer :: <string-buffer> = make(<string-buffer>);
  apply(print-format, buffer, format-string, format-arguments);
  buffer-contents(buffer)
end function format-to-string;


/// Basic object printing

define function object-name
    (object :: <object>) => (name :: <string>)
  let maybe-name = debug-name(object);
  if (maybe-name)
    as(<byte-string>, maybe-name);
  else
    "???"
  end;
end function object-name;

define method print-pretty-name
    (buffer :: <string-buffer>, object :: <object>)
 => ()
  let name = primitive-name(object);
  case
    name      => print-string(buffer, name);
    otherwise => print-unique-name(buffer, object);
  end
end method print-pretty-name;

define function object-class-name
    (object :: <object>) => (name :: false-or(<string>))
  select (object by instance?)
    <class>   => "<class>";
    otherwise => object-name(object-class(object))
  end
end function object-class-name;

define function print-basic-name
    (buffer :: <string-buffer>,
     #key object :: <object>,
          name :: false-or(<string>) = primitive-name(object),
          class-name :: <string> = object-class-name(object))
 => ()
  if (name)
    print-format(buffer, "{%s: %s}", class-name, name)
  else
    print-format(buffer, "{%s}", class-name)
  end
end function print-basic-name;

define method print-unique-name
    (buffer :: <string-buffer>, object :: <object>) => ()
  local method symbol-name (symbol :: <symbol>) => (name :: <string>)
          as-lowercase(as(<string>, symbol))
        end method symbol-name;
  select (object by instance?)
    <byte-string>  => print-format(buffer, "\"%s\"", object);
    <symbol>       => print-format(buffer, "#\"%s\"", symbol-name(object));
    <character>    => print-format(buffer, "'%c'", object);
    <collection>   => print-collection(buffer, object);
    <boolean>      => print-string(buffer, if (object) "#t" else "#f" end);
    <integer>      => print-string(buffer, integer-to-string(object));
    <float>        => print-string(buffer, float-to-string(object));
    <machine-word> => print-string(buffer, machine-word-to-string(object));
    <method>       => print-method(buffer, object);
    otherwise      => print-basic-name(buffer, object: object);
  end
end method print-unique-name;

define function object-unique-name
    (object :: <object>) => (name :: <string>)
  let buffer :: <string-buffer> = make(<string-buffer>);
  print-unique-name(buffer, object);
  buffer-contents(buffer)
end function object-unique-name;

define function primitive-name
    (object :: <object>) => (name :: false-or(<string>))
  select (object by instance?)
    <byte-string>  => object;
    <character>    => make(<byte-string>, size: 1, fill: object);
    <condition>    => condition-to-string(object);
    <locator>      => as(<string>, object);
    <class>        => object-name(object);
    <function>     => object-name(object);
    otherwise      => #f;
  end
end function primitive-name;


/// Types

define method print-unique-name
    (buffer :: <string-buffer>, union :: <union>) => ()
  local method print-union-type
            (buffer :: <string-buffer>, type :: <type>)
         => (object :: <object>)
          select (type by instance?)
            <singleton> => print-unique-name(buffer, singleton-object(type));
            <class>     => print-pretty-name(buffer, type);
            otherwise   => print-unique-name(buffer, type);
          end
        end method print-union-type;
  print-format(buffer, "{%s: ", object-class-name(union));
  print-union-type(buffer, union-type1(union));
  print-string(buffer, ", ");
  print-union-type(buffer, union-type2(union));
  print-string(buffer, "}")
end method print-unique-name;

define method print-unique-name
    (buffer :: <string-buffer>, singleton :: <singleton>) => ()
  print-format(buffer, "{%s: ", object-class-name(singleton));
  print-unique-name(buffer, singleton-object(singleton));
  print-string(buffer, "}")
end method print-unique-name;


/// Number/string conversion

define constant $number-characters = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";

define inline function integer-to-character
    (integer :: <integer>) => (character :: <byte-character>)
  $number-characters[integer]
end function integer-to-character;

define inline function character-to-integer
    (character :: <byte-character>) => (integer :: false-or(<integer>))
  //---*** We might want to make this more efficient...
  position($number-characters, character)
end function character-to-integer;

define function integer-to-string
    (integer :: <integer>,
     #key base :: <integer> = 10,
          size: string-size :: <integer> = 0,
          fill :: <byte-character> = '0')
 => (string :: <byte-string>)
  user-assert(2 <= base & base <= 36,
              "Base %d is not between 2 and 36",
              base);
  let negative-integer? = negative?(integer);
  let buffer = make(<string-buffer>);
  if (zero?(integer))
    buffer := add!(buffer, '0');
  end;
  if (negative-integer?)
    // Do the first digit by hand to avoid overflows when printing
    // $minimum-integer, since -$minimum-integer > $maximum-integer.
    let (quotient, remainder :: <integer>) = truncate/(integer, base);
    buffer := add!(buffer, integer-to-character(-remainder));
    integer := -quotient
  end;
  until (zero?(integer))
    let (quotient, remainder :: <integer>) = truncate/(integer, base);
    buffer := add!(buffer, integer-to-character(remainder));
    integer := quotient
  end;
  let remaining = string-size - buffer.size;
  if (negative-integer?)
    remaining := remaining - 1;
  end;
  for (i from 0 below remaining)
    buffer := add!(buffer, fill);
  end;
  if (negative-integer?)
    buffer := add!(buffer, '-');
  end;
  as(<string>, reverse!(buffer));
end function integer-to-string;

// Given a string, parse an integer from it.  Skips left whitespace.
define function string-to-integer
    (string :: <string>,
     #key base          :: <integer> = 10,
          start         :: <integer> = 0,
          end: stop     :: false-or(<integer>),
          default = $unsupplied)
 => (n :: <integer>, next-key :: <integer>)
  let actual-start :: <integer> = start;
  let string-length :: <integer> = size(string);
  user-assert(start == 0 | (start > 0 & start < string-length),
              "start: %d is out of range [0, %d] for string %=",
              start, string-length, string);
  if (stop)
    user-assert(stop >= start & stop <= string-length,
                "end: %d is out of range [0, %d] for string %=",
                stop, string-length, string);
  else
    stop := string-length
  end;
  user-assert(2 <= base & base <= 36,
              "Base %d is not between 2 and 36",
              base);
  while (start < stop & member?(string[start], #(' ', '\n', '\r', '\f', '\t')))
    start := start + 1
  end;
  let sign :: <integer>
    = select (start < stop & string[start])
        '+'       => start := start + 1; 1;
        '-'       => start := start + 1; -1;
        otherwise => 1;
      end;
  let value :: <integer> = 0;      // Shifting digits into here
  let valid? :: <boolean> = #f;
  let next-key :: <integer>
    = block (return)
        for (i :: <integer> from start below stop)
          let ch :: <byte-character> = as-uppercase(string[i]);
          let digit = character-to-integer(ch);
          when (~digit | digit >= base)
            return(i)
          end;
          valid? := #t;
          value := value * base + digit * sign
        end;
        stop
      end;
  if (valid?)
    values(value, next-key)
  else
    when (unsupplied?(default))
      error("Reached end of %s without finding an integer", string)
    end;
    values(default, actual-start)
  end
end function string-to-integer;

//---*** NOTE: The following function is a kludge that's needed until
//---*** the necessary primitives are added and the Dylan library provides
//---*** an interface.  Unfortunately, this kludge relies on a code generation
//---*** bug which causes all NaNs to compare equal to any other value.
//---*** When that bug is fixed, we'll need a new way to check for NaNs.
define inline-only function nan? (float :: <float>) => (nan? :: <boolean>)
  float = 0.0 & float = 1.0
end function nan?;

//---*** NOTE: The following function is a kludge that's needed until
//---*** the necessary primitives are added and the Dylan library provides
//---*** an interface.
define inline-only function infinity? (float :: <float>) => (infinity? :: <boolean>)
  ~zero?(float) & (float / 2.0) = float
end function infinity?;

define function float-to-string
    (float :: <float>,
     #key decimal-points :: false-or(<integer>) = #f)
 => (string :: <string>)
  if (decimal-points & zero?(decimal-points))
    integer-to-string(round(float))
  else
    let class = object-class(float);
    //---*** Until this code can be rewritten to not integer overflow,
    //---*** keep the number of digits down to 7 for all types of float.
    let (max-digits, marker, force-marker?)
      = select (float by instance?)
          <single-float>   => values(7, 's', #f);
          <double-float>   => values(7, 'd', #t); // values(15, 'd', #t);
          <extended-float> => values(7, 'x', #t); // values(34, 'x', #t);
        end;
    let digits = min(decimal-points | max-digits, max-digits);
    case
      nan?(float) =>
        // The sign of a NaN is meaningless ...
        if (class == <single-float>)
          "{NaN}"
        else
          format-to-string("{NaN}%c0", marker)
        end;
      infinity?(float) =>
        let sign :: <byte-character> = if (negative?(float)) '-' else '+' end;
        if (class == <single-float>)
          format-to-string("%c{infinity}", sign)
        else
          format-to-string("%c{infinity}%c0", sign, marker)
        end;
      zero?(float) =>
        if (class == <single-float>)
          "0.0"
        else
          format-to-string("0.0%c0", marker)
        end;
      negative?(float) =>
        let positive-string = float-to-string(negative(float));
        format-to-string("-%s", positive-string);
      otherwise =>
        let buffer :: <string-buffer> = make(<string-buffer>);
        let ten = as(class, 10.0);
        let dec-point :: <integer> = digits - 1;
        local method sub-print (mantissa :: <integer>, count :: <integer>)
                let (quotient :: <integer>, remainder :: <integer>)
                  = truncate/(mantissa, 10);
                unless (count = digits)
                  // Recurse until you have all the digits pushed on stack
                  sub-print(quotient, count + 1)
                end unless;
                // Then as each recursive call unwinds, turn the digit (in
                // remainder) into a character and output the character.
                if (count = dec-point)
                  add!(buffer, '.')
                end if;
                add!(buffer, as(<character>, as(<integer>, '0') + remainder))
              end method;
        let scale :: <integer> = truncate/(log(float), log(ten));
        if (scale > 0 & scale <= dec-point)
          dec-point := dec-point - scale;
        end;
        let sub-float :: <float> = float;
        // let tens :: <integer> = 1;
        let tens :: <float> = ten / ten;
        for (i :: <integer> from 1 to abs(digits - scale))
          if (tens < 100000000)
            tens := ten * tens;
          else
            // make sure tens dont go bignum:
            sub-float
              := if (digits < scale) sub-float / tens else sub-float * tens end;
            tens := ten;
          end if;
        end for;
        if (digits < scale)
          sub-print(round/(sub-float, tens), 0);
        else
          sub-print(round(sub-float * tens), 0);
        end;
        if (force-marker? | (scale ~= 0 & dec-point = digits - 1))
          add!(buffer, marker);
          if (dec-point = digits - 1)
            for (character in integer-to-string(scale))
              add!(buffer, character)
            end
          else
            // If the scale was small enough, we moved the decimal point
            // instead so don't print a bogus exponent ...
            add!(buffer, '0')
          end
        end;
        buffer-contents(buffer);
    end
  end
end function float-to-string;

define open generic number-to-string
    (number :: <number>) => (string :: <string>);

define method number-to-string
    (number :: <number>) => (string :: <string>)
  object-unique-name(number)
end method number-to-string;

define method number-to-string
    (float :: <float>) => (string :: <string>)
  float-to-string(float)
end method number-to-string;

define method number-to-string
    (integer :: <integer>) => (string :: <string>)
  integer-to-string(integer, base: 10)
end method number-to-string;


/// Machine-word/string conversion

define function machine-word-to-string
    (mw :: <machine-word>, #key prefix :: false-or(<string>) = "#x")
 => (string :: <string>)
  let halfword-size = ash($machine-word-size, -1);
  let digits-per-halfword = ash(halfword-size, -2);
  let high
    = coerce-machine-word-to-integer
        (machine-word-unsigned-shift-right(mw, halfword-size));
  let low
    = coerce-machine-word-to-integer
        (machine-word-unsigned-shift-right
           (machine-word-unsigned-shift-left(mw, halfword-size),
            halfword-size));
  concatenate-as(<string>,
                 prefix | "",
                 integer-to-string(high, base: 16, size: digits-per-halfword),
                 integer-to-string(low, base: 16, size: digits-per-halfword))
end function machine-word-to-string;

define function string-to-machine-word
    (str :: <string>,
     #key start         :: <integer> = 0,
          default = $unsupplied,
          end: stop     :: false-or(<integer>))
 => (n :: <machine-word>, next-key :: <integer>)
  let string-length :: <integer> = size(str);
  user-assert(start >= 0 & start < string-length,
              "Start: %d is out of range [0, %d] for string %s",
              start, string-length, str);
  if (stop)
    user-assert(stop >= start & stop <= string-length,
                "Stop: %d is out of range [0, %d] for string %s.",
                stop, string-length, str);
  else
    stop := size(str)
  end;
  while (start < stop & member?(str[start], #(' ', '\n', '\r', '\f', '\t')))
    start := start + 1
  end;
  // Remove common prefixes (#x, 0x) ...
  if ((start < stop - 2)
        &((str[start] = '#' & str[start + 1] = 'x')
            | (str[start] = '0' & str[start + 1] = 'x')))
    start := start + 2
  end;
  let value :: <machine-word> = as(<machine-word>, 0);
  let next-key
    = block (return)
        for (i from start below stop)
          let ch :: <byte-character> = as-uppercase(str[i]);
          let digit = character-to-integer(ch);
          when (~digit | digit >= 16)
            return(i > start & i)
          end;
          //--- andrewa: trick the typist into knowing that this is
          //--- an integer, so inlining works. Hopefully the new
          //--- typist will be able to work this out for itself.
          let integer-digit :: <integer> = digit;
          value
            := machine-word-logior
                 (machine-word-unsigned-shift-left(value, 4),
                  coerce-integer-to-machine-word(integer-digit));
        end;
        stop
      end;
  unless (next-key)
    when (unsupplied?(default))
      error("Reached end of %s without finding an integer", str)
    end;
    value := default;
    next-key := start
  end;
  values(value, next-key)
end function string-to-machine-word;


/// Condition/string conversion

define open generic condition-to-string
    (condition :: <condition>) => (string :: <string>);

define method condition-to-string
    (condition :: <condition>) => (string :: <string>)
  concatenate("Condition of class ", condition.object-class-name, " occurred")
end method condition-to-string;

define method condition-to-string
    (condition :: <format-string-condition>) => (string :: <string>)
  apply(format-to-string,
        condition-format-string(condition),
        condition-format-arguments(condition))
end method condition-to-string;

define method condition-to-string
    (error :: <type-error>) => (string :: <string>)
  format-to-string("%= is not of type %=",
                   type-error-value(error),
                   type-error-expected-type(error))
end method condition-to-string;

define method print-pretty-name
    (buffer :: <string-buffer>, condition :: <condition>)
 => ()
  let message = condition-to-string(condition);
  print-string(buffer, message)
end method print-pretty-name;


/// Collection printing

define constant $collection-empty-text    = "size 0";
define variable *collection-print-length* = 10;

define method print-collection
    (buffer :: <string-buffer>, collection :: <collection>,
     #key print-length :: false-or(<integer>) = *collection-print-length*)
 => ()
  print-string(buffer, "{");
  print-string(buffer, object-class-name(collection));
  print-string(buffer, ": ");
  print-collection-contents(buffer, collection, print-length: print-length);
  print-string(buffer, "}");
end method print-collection;

define method print-collection-contents
    (buffer :: <string-buffer>, collection :: <collection>,
     #key print-length :: false-or(<integer>) = *collection-print-length*)
 => ()
  ignore(print-length);
  print-format(buffer, "size %d", size(collection))
end method print-collection-contents;

define method print-collection-contents
    (buffer :: <string-buffer>, array :: <array>,
     #key print-length :: false-or(<integer>) = *collection-print-length*)
 => ()
  let dimensions = dimensions(array);
  print-elements(buffer, dimensions,
                 print-length: print-length, separator: " x ")
end method print-collection-contents;

define method print-collection-contents
    (buffer :: <string-buffer>, range :: <range>,
     #key print-length :: false-or(<integer>) = *collection-print-length*)
 => ()
  ignore(print-length);
  local method print-range
            (buffer :: <string-buffer>, from :: <real>, to :: <real>,
             by :: <real>)
         => ()
          select (by)
            1         => print-format(buffer, "%d to %d", from, to);
            otherwise => print-format(buffer, "%d to %d by %d", from, to, by);
          end
        end method print-range;
  let range-size = size(range);
  if (range-size = 0)
    print-string(buffer, $collection-empty-text)
  else
    let from = range[0];
    let by   = if (~range-size | range-size > 1) range[1] - from else 1 end;
    select (range-size)
      1         => print-range(buffer, from, from, by);
      #f        => print-format(buffer, "%d by %d", from, by);
      otherwise => print-range(buffer, from, range[range-size - 1], by);
    end
  end
end method print-collection-contents;

define method print-collection-contents
    (buffer :: <string-buffer>, sequence :: <sequence>,
     #key print-length :: false-or(<integer>) = *collection-print-length*)
 => ()
  if (empty?(sequence))
    print-string(buffer, $collection-empty-text)
  else
    print-elements(buffer, sequence, print-length: print-length)
  end
end method print-collection-contents;

//---*** Needed to override the array method... can we avoid this?
define method print-collection-contents
    (buffer :: <string-buffer>, sequence :: <vector>,
     #key print-length :: false-or(<integer>) = *collection-print-length*)
 => ()
  if (empty?(sequence))
    print-string(buffer, $collection-empty-text)
  else
    print-elements(buffer, sequence, print-length: print-length)
  end
end method print-collection-contents;

define method print-collection-contents
    (buffer :: <string-buffer>, pair :: <pair>,
     #key print-length :: false-or(<integer>) = *collection-print-length*)
 => ()
  let tail-object = tail(pair);
  if (instance?(tail-object, <pair>))
    next-method()
  else
    print-format(buffer, "%=, %=", head(pair), tail-object)
  end
end method print-collection-contents;

define function print-elements
    (buffer :: <string-buffer>, sequence :: <sequence>,
     #key print-length = *collection-print-length*,
          separator = ", ",
          print-function)
 => ()
  let current-separator = "";
  let sequence-size = size(sequence);
  let print-length = print-length | sequence-size;
  for (element in sequence,
       count from 0 below print-length)
    print-string(buffer, current-separator);
    current-separator := separator;
    case
      print-function =>
        print-function(buffer, element);
      instance?(element, <collection>) & ~instance?(element, <string>) =>
        print-basic-name(buffer, object: element, name: #f);
      otherwise =>
        print-unique-name(buffer, element);
    end
  end;
  if (sequence-size > print-length)
    print-string(buffer, separator);
    print-string(buffer, "...")
  end
end function print-elements;


/// Method printing

define function print-method
    (buffer :: <string-buffer>, object :: <method>) => ()
  print-string(buffer, "{");
  print-string(buffer, object-class-name(object));
  print-string(buffer, ": ");
  print-string(buffer, primitive-name(object));
  let specializers = function-specializers(object);
  print-string(buffer, " (");
  unless (empty?(specializers))
    print-elements(buffer, specializers, print-function: print-specializer)
  end;
  print-string(buffer, ")}");
end function print-method;

define method print-specializer
    (buffer :: <string-buffer>, type :: <type>) => ()
  print-unique-name(buffer, type)
end method print-specializer;

define method print-specializer
    (buffer :: <string-buffer>, object :: <class>) => ()
  print-pretty-name(buffer, object)
end method print-specializer;

define method print-specializer
    (buffer :: <string-buffer>, type :: <singleton>) => ()
  print-string(buffer, "singleton(");
  print-unique-name(buffer, singleton-object(type));
  print-string(buffer, ")")
end method print-specializer;

define method print-specializer
    (buffer :: <string-buffer>, type :: <subclass>) => ()
  print-string(buffer, "subclass(");
  print-pretty-name(buffer, subclass-class(type));
  print-string(buffer, ")")
end method print-specializer;
