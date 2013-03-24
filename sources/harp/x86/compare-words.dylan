module:    harp-x86
Synopsis:  Pentium block memory comparison instructions
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


with-ops-in pentium-instructions (bne-words, bne-bytes)
  destroys-fn := constant-fn(vector(esi, edi, ecx));
  c-preserved-destroys-fn := all-c-preserved-fn;
end with-ops-in;


with-ops-in pentium-instructions (bne-bytes)
  clash-fn := pentium-method (uuu)
	        list(list(uuu-uze(1), esi, ecx),
                     list(edi, uuu-uze(2), ecx),
                     list(edi, esi, ecx, uuu-uze(3)));
              end pentium-method;

  prefer-fn := pentium-method (uuu)
		 prefer(uuu-uze(1), $vector-edi);
		 prefer(uuu-uze(2), $vector-esi);
               end pentium-method;
end with-ops-in;


with-ops-in pentium-instructions (bne-words)
  clash-fn := pentium-method (uuu)
	        list(list(uuu-uze(1), esi, ecx),
                     list(edi, uuu-uze(2), ecx),
                     list(edi, esi, uuu-uze(3)));
              end pentium-method;

  prefer-fn := pentium-method (uuu)
		 prefer(uuu-uze(1), $vector-edi);
		 prefer(uuu-uze(2), $vector-esi);
		 prefer(uuu-uze(3), $vector-ecx);
               end pentium-method;
end with-ops-in;


define pentium-template (bne-words)

  // Rely on the clash function to stop registers occurring in the
  // wrong place.
  pattern (be, tag, mem1, mem2, how-many)
    harp-out (be)
      move(be, ecx, how-many);	// ecx guaranteed not to hold mem1 or mem2
      move(be, esi, mem2);	// esi guaranteed not to hold mem1
      move(be, edi, mem1);
    end harp-out;
    emit(be, cld);		// pointers move up
    emit(be, repe,		// REP while ecx /= 0
	      cmpsd);		// the magical compare instruction
    emit-branch-sdi(be, bne-x, tag);
end pentium-template;


define pentium-template (bne-bytes)
  pattern (be, tag, mem1, mem2, how-many)
    // first do all full words
    harp-out (be) 
      asr(be, ecx, how-many, 2);
      bne-words(be, tag, mem1, mem2, ecx);
    end harp-out;
    // now any left over bytes
    harp-out (be) and(be, ecx, how-many, #b11) end;
    emit(be, repe, cmpsb);
    emit-branch-sdi(be, bne-x, tag);
end pentium-template;

