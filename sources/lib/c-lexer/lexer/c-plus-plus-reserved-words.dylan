Module: C-lexer-internal
Author: Toby Weinberg
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// This may not be the best class hierarchy for C++ reserved words.  I find
// I don't really use the class <reserved-word> at all in practice so maybe
// it doesn't matter.  Most of the messing with reserved words is done by
// remap-reserved-words which is called by get-next-token.

define abstract class <C++-reserved-word> (<token>) end;

define method lexer-string 
    (token :: <C++-reserved-word>) => (result :: <string>);
  as(<string>, token.parser-tag)
end method;

define sealed concrete class <asm> (<C++-reserved-word>)
  constant slot parser-tag #"asm";
end;

define sealed concrete class <bool> (<C++-reserved-word>)
  constant slot parser-tag #"bool";
end;

define sealed concrete class <catch> (<C++-reserved-word>)
  constant slot parser-tag #"catch";
end;

define sealed concrete class <class> (<C++-reserved-word>)
  constant slot parser-tag #"class";
end;

define sealed concrete class <const_cast> (<C++-reserved-word>)
  constant slot parser-tag #"const_cast";
end;

define sealed concrete class <delete> (<C++-reserved-word>)
  constant slot parser-tag #"delete";
end;

define sealed concrete class <dynamic_cast> (<C++-reserved-word>)
  constant slot parser-tag #"dynamic_cast";
end;

define sealed concrete class <false> (<C++-reserved-word>)
  constant slot parser-tag #"false";
end;

define sealed concrete class <friend> (<C++-reserved-word>)
  constant slot parser-tag #"friend";
end;

define sealed concrete class <inline> (<C++-reserved-word>)
  constant slot parser-tag #"inline";
end;

define sealed concrete class <mutable> (<C++-reserved-word>)
  constant slot parser-tag #"mutable";
end;

define sealed concrete class <namespace> (<C++-reserved-word>)
  constant slot parser-tag #"namespace";
end;

define sealed concrete class <new> (<C++-reserved-word>)
  constant slot parser-tag #"new";
end;

define sealed concrete class <operator> (<C++-reserved-word>)
  constant slot parser-tag #"operator";
end;

define sealed concrete class <private> (<C++-reserved-word>)
  constant slot parser-tag #"private";
end;

define sealed concrete class <protected> (<C++-reserved-word>)
  constant slot parser-tag #"protected";
end;

define sealed concrete class <public> (<C++-reserved-word>)
  constant slot parser-tag #"public";
end;

define sealed concrete class <reinterpret_cast> (<C++-reserved-word>)
  constant slot parser-tag #"reinterpret_cast";
end;

define sealed concrete class <static_cast> (<C++-reserved-word>)
  constant slot parser-tag #"static_cast";
end;

define sealed concrete class <template> (<C++-reserved-word>)
  constant slot parser-tag #"template";
end;

define sealed concrete class <this> (<C++-reserved-word>)
  constant slot parser-tag #"this";
end;

define sealed concrete class <throw> (<C++-reserved-word>)
  constant slot parser-tag #"throw";
end;

define sealed concrete class <true> (<C++-reserved-word>)
  constant slot parser-tag #"true";
end;

define sealed concrete class <try> (<C++-reserved-word>)
  constant slot parser-tag #"try";
end;

define sealed concrete class <typeid> (<C++-reserved-word>)
  constant slot parser-tag #"typeid";
end;

define sealed concrete class <using> (<C++-reserved-word>)
  constant slot parser-tag #"using";
end;

define sealed concrete class <virtual> (<C++-reserved-word>)
  constant slot parser-tag #"virtual";
end;
