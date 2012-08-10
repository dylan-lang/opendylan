module:    x86-harp
Synopsis:  Pentium PC-specific instructions (multiple-values etc.)
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



with-ops-in pentium-instructions (push-last-arg)       
  c-preserved-destroys-fn := tmp1-fn;
end with-ops-in;

define pentium-template push-last-arg
  // It looks as though this should not really be a template function.
  // But it is done here rather than in CG so that leaf-case won't
  // spot the use of the PUSH as a stack op, and require a frame.
  pattern (be)
    harp-out (be)
      pop(be, reg--tmp1);      // pop the return address
      push(be, reg--arg0);     // push the last arg
      push(be, reg--tmp1);
    end harp-out;
end pentium-template;


with-ops-in pentium-instructions (reset-values, set-values, 
                                  clear-direction-flag)       
  c-preserved-destroys-fn := flags-fn;
end with-ops-in;

define pentium-template reset-values
  // Just set the direction flag
  pattern (be)
    emit(be, std);
end pentium-template;


define pentium-template (set-values, clear-direction-flag)
  // Just reset the direction flag.
  pattern (be)
    emit(be, cld);
end pentium-template;



define pentium-template halt
  // INT 3
  pattern (be)
    emit(be, #xcc);
end pentium-template;
