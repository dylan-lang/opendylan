module:    x86-harp
Synopsis:  Pentium copying instructions
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// This is where the fun starts.



with-ops-in pentium-instructions (copy-words-down, copy-words-up, 
                                  copy-words-down-w, copy-words-up-w,
                                  copy-bytes-down, copy-bytes-up, 
                                  fill-words, fill-words-w)
  c-preserved-destroys-fn := all-c-preserved-fn;
end with-ops-in;


/// The MOVSD (#xa5) instruction moves (si) to (di) and then adjusts each by
/// appropriate stride - a CLD (#xfc) makes this direction increment, an STD
/// (#xfd) makes it decrement. If preceded by REP (#xf3), this is repeated for
/// ECX times. The STOS (#xab) instruction puts EAX at (di), then moves di:
/// this too maye be prefixed.

/// Note that how-many is a byte counter, so we shift it right two places.

/// COPY-WORDS-DOWN-W COPY-WORDS-UP-W are like COPY-WORDS-DOWN
/// COPY-WORDS-UP respectively, but take a size argument in words
/// rather than bytes.


// Pad the info field of the COPY-* instructions with symbolic
// lists saying whether the op is up / down and word sized.

with-ops-in pentium-instructions (copy-words-down)   info := #() end;
with-ops-in pentium-instructions (copy-words-up)     info := list(#"up") end;
with-ops-in pentium-instructions (copy-words-down-w) info := list(#"w") end;
with-ops-in pentium-instructions (copy-words-up-w)   info := list(#"w",#"up") end;

define method is-copy-up? (info :: <list>) 
  member?(#"up", info);
end method;

define method is-copy-w? (info :: <list>) 
  member?(#"w", info);
end method;


define pentium-template (copy-words-down, copy-words-up, 
                         copy-words-down-w, copy-words-up-w)
  options (self);

  // Rely on the clash function to stop registers occuring in the
  // wrong place.
  pattern (be, i :: <list> by op-info, to, from, how-many)
    harp-out (be)
      move(be, ecx, how-many);	// ecx guaranteed not to hold from or to
      move(be, esi, from);	// esi guaranteed not to hold to
      move(be, edi, to);
    end harp-out;
    do-appropriate-thing(be, i);

end pentium-template;


define pentium-template (copy-bytes-down)
  pattern (be, to, from, how-many)
    // first do all full words
    harp-out (be) copy-words-down(be, to, from, how-many) end;
    // now any left over bytes
    harp-out (be) and(be, ecx, how-many, #b11) end;
    emit(be, rep, movsb);
end pentium-template;

define pentium-template (copy-bytes-up)
  pattern (be, to, from, how-many)
    // first do all full words
    harp-out (be) copy-words-up(be, to, from, how-many) end;
    // now any left over bytes
    harp-out (be) and(be, ecx, how-many, #b11) end;
    emit(be, rep, movsb);
end pentium-template;


/// do-appropriate-thing generates the move code for up or down, depending
/// on the symbol its given. The arguments are all lined up in their registers.

define method do-appropriate-thing  (be :: <harp-x86-back-end>, info :: <list>)

  let up = is-copy-up?(info);
  let w  = is-copy-w?(info);
  let word = ~ w;
  if (up)
    if (w)
      for (i from 0 below 4)
        call-local(add2, be, edi, ecx);
        call-local(add2, be, esi, ecx);
      end for;
    else
      call-local(add2, be, edi, ecx);
      call-local(add2, be, esi, ecx);
    end if;
    call-local(sub2, be, edi, 4);
    call-local(sub2, be, esi, 4);
    emit(be, std);		        // pointers move down

  else
    emit(be, cld);			// pointers move up
  end if;

  if (word)
     emit(be, #xc1, #xf9, 2);		// ecx := ecx >> 2
  end if;
  emit(be, rep,				// REP while ecx /= 0
	   movsd);			// the magical move instruction

end method;

/// fillet up (groan)

with-ops-in pentium-instructions (fill-words)   info := #t end;
with-ops-in pentium-instructions (fill-words-w) info := #f end;


define method fried-fillet-of-plaice (be :: <harp-x86-back-end>, info)
  emit(be, cld);				// pointers move up
  if (info)  // i.e. this is a fill-words
    emit(be, #xc1, #xf9, 2)		// ecx := ecx >> 2
  end if;
  emit(be, rep,				// while ecx /= 0
       stosd);				// STOSD
end method;

// FILL-WORDS-W is like FILL-WORDS but takes a word number rather than
// a byte number.

define pentium-template (fill-words, fill-words-w)
  options (self);

  // Cases where registers are in the wrong place cannot occur because
  // of the clash function.

  // Hopefully some of these moves will be eliminated ...
  pattern (be, i :: <boolean> by op-info, at, how-many, with)
    harp-out (be)
      move(be, ecx, how-many);
      move(be, eax, with);
      move(be, edi, at);
    end harp-out;
    fried-fillet-of-plaice(be, i);

end pentium-template;


// Fill-bytes fills with bytes. Implement in terms of fill-words
// for efficiency

define pentium-template (fill-bytes)
  pattern (be, at, how-many, with)
    // first do all full words
    harp-out (be) 
      move(be, eax, with);
      and(be, eax, eax, #xff); // strip all but the low byte
      asl(be, ecx, eax, 8);    // get the second byte
      or(be, eax, eax, ecx);
      asl(be, ecx, eax, 16);   // get bytes 3 & 4
      or(be, eax, eax, ecx);
      fill-words(be, at, how-many, eax);
    end harp-out;
    // now any left over bytes
    harp-out (be) and(be, ecx, how-many, #b11) end;
    emit(be, rep, stosb);
end pentium-template;
