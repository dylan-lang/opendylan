module: fast-subclass
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// This is the code for managing the bits that implement some of
/// the sets used in the paper
/// Efficient Handling of Multiple Inheritance Hierarchies
/// by Yves Caseau form Oopsla 93
/// 

/// should really modularize this stuff out better, but that makes
/// debugging in the emulator painful

/// try subclass bits as a vector of 24 16bit integers first
/// <subclass-bits> is exported
define constant <subclass-bits> = <vector>;
define constant $subclass-bits-size = 24;
define constant $subclass-bits-word-size = 16;
define constant $gene-limit = $subclass-bits-size * $subclass-bits-word-size;

/// make-subclass-bits is exported
define method make-subclass-bits () => (s :: <subclass-bits>);
  make(<vector>, fill: 0, size: $subclass-bits-size);
end;

/// subclass-bit? is exported
// a useful default
define method subclass-bit? (bits :: <subclass-bits>, false == #f)
 => (false :: singleton(#f));
  #f
end;


define method subclass-bit? (bits :: <subclass-bits>, i :: <integer>)
 => (set? :: <boolean>);
  let (word-offset, bit-offset) = truncate/(i, $subclass-bits-word-size);
  if (word-offset >= $subclass-bits-size)
    error("not enough words in bit table to check bit %s", i);
  end;
  let word = bits[word-offset];
  logbit?(bit-offset, word)
end;

/// set-subclass-bit is exported
define method set-subclass-bit (bits :: <subclass-bits>, i :: <integer>)
 => (bits :: <subclass-bits>);
  let (word-offset, bit-offset) = truncate/(i, $subclass-bits-word-size);
  if (word-offset >= $subclass-bits-size)
    error("not enough words in bit table to allocate bit %s", i);
  end;
  let word = bits[word-offset];
  bits[word-offset] := logior(word, ash(1, bit-offset));
  bits
end;

define method set-subclass-bit (bits :: <subclass-bits>, f == #f)
 => (bits :: <subclass-bits>);
  bits
end;


/// unset-subclass-bit is exported
define method unset-subclass-bit
    (bits :: <subclass-bits>, i :: <integer>)
 => (bits :: <subclass-bits>);
  let (word-offset, bit-offset) = truncate/(i, $subclass-bits-word-size);
  if (word-offset >= $subclass-bits-size)
    error("not enough words in bit table to deallocate bit %s", i);
  end;
  let word = bits[word-offset];
  bits[word-offset] := logand(word, lognot(ash(1, bit-offset)));
  bits
end;


define method bit-word (bits :: <subclass-bits>, i :: <integer>)
 => (word :: <integer>);
  bits[i]
end;

define method bit-word-setter (word :: <integer>,
			       bits :: <subclass-bits>,
			       i :: <integer>)
 => (word :: <integer>);
  bits[i] := word
end;

/// subclass-bits-copy is exported
define method subclass-bits-copy (old-bits :: <subclass-bits>)
 => (bits :: <subclass-bits>);
  let new-bits = make-subclass-bits();
  for (i from 0 below $subclass-bits-size)  
    bit-word(new-bits, i) := bit-word(old-bits, i);
  end;
  new-bits
end;

/// subclass-bit-union is exported
define method subclass-bit-union (#rest more-bits)
 => (new-bits :: <subclass-bits>);
  let new-bits = make-subclass-bits();
  if (empty?(more-bits))
    new-bits
  else
    for (i from 0 below $subclass-bits-size)
      bit-word(new-bits, i)
        := reduce(method (accum, bits2)
  		    logior(accum,
			   bit-word(bits2, i))
		  end,
		  0,
		  more-bits)
    end for;
    new-bits
  end if;
end;


define method subclass-bit-= (bits1, bits2)
  bits1 == bits2
    | every?(\=, bits1, bits2)
end;


/// This function is right out of the paper
/// it's here because it really knows aobut how these bits are implemented.
/// next-gene is exported
define method next-gene (#rest more-bits)
  block (return)
    if (empty?(more-bits))
      0
    else
      for (i from 0 below $gene-limit)
        block (not-it)
	  for (bits in more-bits)
	    if (subclass-bit?(bits, i))
	      not-it();
	    end;
	  end for;
	  // fall through means this one wins
	  return(i);
        end block;
      end for;
      // fall through here means we couldn't find it so error
      error("gene bits are all used up");
    end if;
  end block;
end;


/// The unltimate subclass test
define method fast-subclass (class1 :: <class>, class2 :: <class>)
 => (yes? :: <boolean>);
  let bits1 = subclass-bits(class1);
  let bits2 = subclass-bits(class2);
  block (return)
    for (i from 0 below $subclass-bits-word-size)
      let word1 = bit-word(bits1);
      unless (word1 = logior(bit-word(bits2), word1))
	return(#f)
      end;
    end;
    // fall through
    return(#t);
  end;
end;
