module: make-dylan-app

define constant $alphabetic-characters
  = #('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o',
      'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
      'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O',
      'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z');

define constant $numeric-characters
  = #('1', '2', '3', '4', '5', '6', '7', '8', '9', '0');

define constant $graphic-characters
  = #('!', '&', '*', '<', '=', '>', '|', '^', '$', '%', '@', '_');

define function is-alphabetic? (c :: <character>) => (alphabetic? :: <boolean>)
  any?(curry(\=, c), $alphabetic-characters)
end function is-alphabetic?;

define function is-numeric? (c :: <character>) => (numeric? :: <boolean>)
  any?(curry(\=, c), $numeric-characters)
end function is-numeric?;

define function is-graphic? (c :: <character>) => (graphic? :: <boolean>)
  any?(curry(\=, c), $graphic-characters)
end function is-graphic?;

define function is-name? (c :: <character>) => (name? :: <boolean>)
  is-alphabetic?(c) | is-numeric?(c) | is-graphic?(c)
  | any?(curry(\=, c), #('-', '+', '~', '?', '/'))
end function is-name?;

// see: http://opendylan.org/books/drm/Lexical_Syntax
define function is-valid-dylan-name? (word :: <string>) => (name? :: <boolean>)
  every?(is-name?, word) &
  case
    is-alphabetic?(word[0]) => #t;
    is-graphic?(word[0]) => (word.size > 1) & any?(is-alphabetic?, word);
    word[0] = '\\' => (word.size > 1);
    is-numeric?(word[0])
      => (word.size > 2) &
        block(return)
          for(i from 1 below word.size - 1)
            if(is-alphabetic?(word[i]) & is-alphabetic?(word[i + 1]))
              return(#t)
            end if;
          end for;
        end block;
  end case;
end function is-valid-dylan-name?;
