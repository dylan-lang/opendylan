module:    idvm-harp
Synopsis:  Support for canonicalised IDVM operators
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



/// This file defines local templates which corresponds closely to the IDVM 
/// operator and branch instructions. The operands (S1 and S2)  must be in 
/// a pattern supported by the IDVM - i.e.:
///    res   OP local
///    res   OP lit
///    local OP local
///    local OP lit
///
/// The template also takes an EMITTERS argument. This is a vector
/// with indices used as follows:


define constant idvm-harp-res-loc = 0;
define constant idvm-harp-res-lit = 1;
define constant idvm-harp-loc-loc = 2;
define constant idvm-harp-loc-lit = 3;




/// canon-op is the local template which expects canonicalisation such that
/// it closely maps to the IDVM instruction format for operators

define local-template canon-op

  // result OP local case
  pattern (be, emitters, s1 by res-ref, s2 by env/spill-ref)
    let emitter :: <function> = emitters[idvm-harp-res-loc];
    emitter(be, s2);

  // result OP literal case
  pattern (be, emitters, s1 by res-ref, s2)
    let emitter :: <function> = emitters[idvm-harp-res-lit];
    emitter(be, s2);

  // local OP local case
  pattern (be, emitters, s1 by env/spill-ref, s2 by env/spill-ref)
    let emitter :: <function> = emitters[idvm-harp-loc-loc];
    emitter(be, s1, s2);

  // local OP literal case
  pattern (be, emitters, s1 by env/spill-ref, s2)
    let emitter :: <function> = emitters[idvm-harp-loc-lit];
    emitter(be, s1, s2);

end local-template;


/// canon-branch is the local template which expects canonicalisation such that
/// it closely maps to the IDVM instruction format for branches

define local-template canon-branch

  // result OP local case
  pattern (be, emitters, tag, s1 by res-ref, s2 by env/spill-ref)
    let emitter :: <function> = emitters[idvm-harp-res-loc];
    emitter(be, s2, tag);

  // result OP literal case
  pattern (be, emitters, tag, s1 by res-ref, s2)
    let emitter :: <function> = emitters[idvm-harp-res-lit];
    emitter(be, s2, tag);

  // local OP local case
  pattern (be, emitters, tag, s1 by env/spill-ref, s2 by env/spill-ref)
    let emitter :: <function> = emitters[idvm-harp-loc-loc];
    emitter(be, s1, s2, tag);

  // local OP literal case
  pattern (be, emitters, tag, s1 by env/spill-ref, s2)
    let emitter :: <function> = emitters[idvm-harp-loc-lit];
    emitter(be, s1, s2, tag);

end local-template;

