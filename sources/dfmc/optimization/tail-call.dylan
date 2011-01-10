module:   dfmc-optimization
synopsis: simple-minded tail call analysis
author:   Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method tail-position? (c :: <call>)
  returns-temporary?(c.next-computation, c.temporary)
end method tail-position?;

define method returns-temporary?(c :: <computation>, t)
  #f
end method returns-temporary?;

define method returns-temporary?(c :: <adjust-multiple-values-computation>,
				 t :: <temporary>)
  #f
end method returns-temporary?;

define method returns-temporary?(c :: <check-type-computation>,
				 t :: <temporary>)
  if (compiling-dylan-library?())
    next-method();
  end if;
end method returns-temporary?;

define method returns-temporary?(c :: <temporary-transfer-computation>, t :: <temporary>)
  let source = c.computation-value;

  if (instance?(source, <temporary>))
    (source == t) & 
        returns-temporary?(c.next-computation, c.temporary);
  else
    returns-temporary?(c.next-computation, c.temporary);
  end if;
end method returns-temporary?;

define method returns-temporary?(c :: <return>, t :: <temporary>)
  c.computation-value == t
end method returns-temporary?;

define method returns-temporary?(c :: <nop-computation>, t :: <temporary>)
  returns-temporary?(c.next-computation, c.temporary)
end method returns-temporary?;

define method returns-temporary?(c :: <binary-merge>, t :: <temporary>)
  returns-temporary?(c.next-computation, c.temporary)
end method returns-temporary?;

define method returns-temporary?(c :: <end-loop>, t :: <temporary>)
  // HACK: NEXT-COMPUTATION OF END-LOOP SHOULD BE THIS
  let loop-c = ending-loop(c); 
  returns-temporary?(loop-c.next-computation, t)
end method returns-temporary?;

