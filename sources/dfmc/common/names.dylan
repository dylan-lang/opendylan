Module:   dfmc-common
Synopsis: A name abstraction for use in the compiler.
Author:   Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Names.

// A lightweight abstraction around names. Names may not always be
// symbols due to potential character set / symbol GC issues in the
// future.

// Note that names are distinct from variable-name program fragments.
// However they're represented, I'd like <name>s to be interned so that
// we can use fast object-hash lookups on them in the guts of the
// namespace code. A variable-name fragment contains an instance of
// <name>.

// HACK: LOOSENED IT UP TO ALLOW BYTE_STRINGS FOR NOW

define constant <name> = type-union(<symbol>, <byte-string>);

define method coerce-name (x) => (res :: <symbol>)
  as(<symbol>, x)
end method;

// eof
