module: locator-internals
author: Tim McNerney
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method find-key-from-end 
    (collection :: <collection>, predicate :: <function>)
 => (key)
  block (return)
    let (final-state, limit, previous-state, finished-state?, current-key,
	 current-element) = backward-iteration-protocol(collection);
    for (state = final-state then previous-state(collection, state),
	   until: finished-state?(collection, state, limit))
      if (predicate(current-element(collection, state)))
	return (current-key(collection, state))
      end if;
    end for;
    #f;
  end block;
end method;

define method non-whitespace-char? 
    (char :: <character>) => (non-whitespace? :: <boolean>)
  ~member?(char, " \t\n\r\f");
end method;

define method trim-whitespace 
    (string :: <string>) => (trimmed-string :: <string>)
  let start = find-key(string, non-whitespace-char?);
  if (start == #f)
    ""
  else
    let finish = find-key-from-end(string, non-whitespace-char?) + 1;
    if (start = 0 & finish = size(string))
      string
    else
      copy-sequence(string, start: start, end: finish)
    end if;
  end if;
end method;

// This is incompatible with Jonathan's position in functional-extensions.dylan
define method position 
    (item, sequence :: <sequence>, #key start = 0)
 => (position :: false-or(<integer>))
  block (return)
    for (index :: <integer> from start below size(sequence))
      if (sequence[index] == item)
	return(index)
      end if;
    end for;
    #f;
  end block;
end method;

define method position-from-end 
    (item, sequence :: <sequence>)
 => (position :: false-or(<integer>))
  block (return)
    for (index :: <integer> from size(sequence) - 1 to 0 by -1)
      if (sequence[index] == item)
	return(index)
      end if;
    end for;
    #f;
  end block;
end method;


/// Case insensitive comparisons
//---*** andrewa: needed for comparison of Microsoft locators.
//---*** This really should be defined somewhere.

// Non-byte string version
define method case-insensitive=
    (string1 :: <string>, string2 :: <string>)
 => (equal? :: <boolean>)
  if (string1.size == string2.size)
    block (return)
      for (char1 :: <character> in string1,
	   char2 :: <character> in string2)
	unless (as-lowercase(char1) == as-lowercase(char2))
	  return(#f)
	end
      end;
      #t
    end
  end
end method case-insensitive=;

define method case-insensitive=
    (string1 :: <byte-string>, string2 :: <byte-string>)
 => (equal? :: <boolean>)
  if (string1.size == string2.size)
    block (return)
      for (char1 :: <byte-character> in string1,
	   char2 :: <byte-character> in string2)
	unless (as-lowercase(char1) == as-lowercase(char2))
	  return(#f)
	end
      end;
      #t
    end
  end
end method case-insensitive=;
