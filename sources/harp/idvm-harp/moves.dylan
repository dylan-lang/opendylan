module:    idvm-harp
Synopsis:  IDVM Move instructions
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Moves are essentially between locals, literals and result.  
// Environment variables are handled by harp instructions.

/*
done   res-gets-lit(literal)
done   loc-gets-lit(local-index,literal)
done   res-gets-loc(local-index)
done   loc-gets-res(local-index)
done   loc-gets-loc (descriptor :: hilo(dst-local-index,src-local-index))

The following by special insructions
       res-gets-ev  (index)
       res-gets-evc (index)
       res-gets-vc  (index)
       evc-gets-res (index)
       vc-gets-res  (index)
       loc-gets-evc (descriptor :: hilo(dst-index,src-index))
       loc-gets-ev  (descriptor :: hilo(dst-index,src-index))
       loc-gets-vc  (descriptor :: hilo(dst-index,src-index))
       evc-gets-loc (descriptor :: hilo(dst-index,src-index))
       ev-gets-loc  (descriptor :: hilo(dst-index,src-index))
       vc-gets-loc  (descriptor :: hilo(dst-index,src-index))
*/


define idvm-template move

  // first check for eliminable case
  pattern (be, d, d)
    #f;

  // Register destination and spill-ref source
  pattern (be, d by res-ref, s by env/spill-ref)
    emit-res-gets-loc(be, s);

  // Spill-ref destination and register source
  pattern (be, d by env/spill-ref, s by res-ref)
    emit-loc-gets-res(be, d);

  // Register destination and source of any type (literal)
  pattern (be, d by res-ref, s)
    emit-res-gets-lit(be, s);

  // Literal destination illegal

  // Spill-ref destination and spill-ref source
  pattern (be, d by env/spill-ref, s by env/spill-ref)
    emit-loc-gets-loc(be, d, s);

  // Spill-ref destination and source of any type (literal)
  pattern (be, d by env/spill-ref, s)
    emit-loc-gets-lit(be, d, s);

end idvm-template;

