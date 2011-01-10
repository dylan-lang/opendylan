Module: corba-protocol
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant corba/<null> = singleton(#f);

define constant corba/<void> = singleton(#f);

define constant corba/<long> = <integer>;

// define constant corba/<short> = limited(dylan/<integer>, min: - (2 ^ 15), max: (2 ^ 15) - 1);
// Workaround for bugs 3207, 3492:
define constant corba/<short> = limited(dylan/<integer>, min: -32768, max: 32767);

define constant corba/<unsigned-long> = <integer>; // limited(<integer>, min: 0);

define constant corba/<unsigned-short> = limited(dylan/<integer>, min: 0, max: #xffff);

define constant corba/<float> = <single-float>;

define constant corba/<double> = <double-float>;

define constant corba/<octet> = <byte>; // limited(dylan/<integer>, min: 0, max: #xff)

define constant corba/<char> = <byte-character>;

define constant corba/<boolean> = <boolean>;

define constant corba/<string> = <string>;

define constant corba/<sequence> = <stretchy-vector>;

define constant corba/<array> = <array>;

define open abstract class corba/<struct> (<object>)
end class;

define open abstract class corba/<exception> (corba/<struct>, <error>)
end class;

define open abstract class corba/<user-exception> (corba/<exception>)
end class;

define open abstract class corba/<system-exception> (corba/<exception>)
end class;

define constant corba/<encapsulation> = limited(corba/<sequence>, of: corba/<octet>);

define open abstract class corba/<union> (<object>)
  slot corba/union/value, init-keyword: value:;
  slot corba/union/discriminator, init-keyword: discriminator:;
end class;

define method \= (union1 :: corba/<union>, union2 :: corba/<union>)
  => (equal? :: <boolean>)
  (object-class(union1) = object-class(union2))
    & (corba/union/discriminator(union1) = corba/union/discriminator(union2))
    & (corba/union/value(union1) = corba/union/value(union2));
end method;

define method \= (struct1 :: corba/<struct>, struct2 :: corba/<struct>)
  => (equal? :: <boolean>)
  let typecode1 = object-typecode(struct1);
  let typecode2 = object-typecode(struct2);
  block (return)
    unless (typecode1 = typecode2)
      return(#f)
    end unless;
    for (member in typecode-members(typecode1))
      let getter = typecode-member-getter(member);
      unless (getter(struct1) = getter(struct2))
	return(#f)
      end unless;
    end for;
    return(#t);
  end block;
end method;

define macro typecode-functions-definer
  { define typecode-functions (?type-name:expression, ?typecode:expression) end }
    => 
    {
     define method object-typecode (object :: ?type-name)
      => (typecode :: <typecode>)
       ?typecode;
     end method;

     define method class-typecode (object == ?type-name)
      => (typecode :: <typecode>)
       ?typecode;
     end method;
    }
end macro;

define typecode-functions (corba/<short>, corba/$short-typecode) end;
define typecode-functions (corba/<long>, corba/$long-typecode) end;
define typecode-functions (corba/<unsigned-short>, corba/$unsigned-short-typecode) end;
// define typecode-functions (corba/<unsigned-long>, corba/$unsigned-long-typecode) end; // ---*** clashes with long case at moment
define typecode-functions (corba/<float>, corba/$float-typecode) end;
define typecode-functions (corba/<double>, corba/$double-typecode) end;
define typecode-functions (corba/<boolean>, corba/$boolean-typecode) end;
define typecode-functions (corba/<char>, corba/$char-typecode) end;
define typecode-functions (corba/<octet>, corba/$octet-typecode) end;
define typecode-functions (corba/<any>, corba/$any-typecode) end;
define typecode-functions (<typecode>, corba/$typecode-typecode) end;
define typecode-functions (corba/<string>, corba/$string-typecode) end;

define class <anonymous-object> (<object>)
  slot anonymous-object-properties :: <sequence>, required-init-keyword: properties:;
end class;

define sealed domain make (subclass(<anonymous-object>));
define sealed domain initialize (<anonymous-object>);

define class <anonymous-struct> (corba/<struct>, <anonymous-object>)
end class;

define class <anonymous-exception> (corba/<exception>, <anonymous-object>)
end class;

define class <anonymous-union> (corba/<union>)
end class;

