Module:    DFMC-Typist
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// moved from dfmc-optimization to make typist-specific.  (gts,98sep17)

define constant <type-or-est> = type-union(<&type>, <type-estimate>);

///
/// guaranteed-joint? (<type-or-est>, <type-or-est>) => (joint? :: <boolean>)
///
/// Returns true only if all instances conforming to the type-estimate
/// are guaranteed to be instances of the given Dylan type.
///
/// Test case: 
///
/// define sealed abstract class <a> (<object>) end; 
/// define open   concrete class <b> (<object>) end;
/// define        concrete class <c> (<a>, <b>) end;
///
/// guaranteed-joint?(lookup-value(#"<a>"), lookup-value(#"<b>")); should
///   return #t.  Note <b> is trying especially hard to be uncooperative.
///   <c> can be open or sealed, as you wish.
///
///  Given a generic function and type estimates for its required
///  arguments, guaranteed-joint? is a conservative test used to determine
///  whether the arguments will always lie within a sealed domain. If they
///  do (or if the generic function itself is completely sealed) the set of
///  potentially applicable methods can be determined using
///  potentially-joint?. If there's just one potentially applicable method,
///  and that method is always applicable (guaranteed-joint? again), we can
///  do a simple compile-time dispatch to that lone method.
///

define generic guaranteed-joint?(t1 :: <type-or-est>, t2 :: <type-or-est>)
   => (res :: <boolean>);

define method guaranteed-joint?(t1 :: <&type>, t2 :: <type-estimate>)
 => (res :: <boolean>)
  // Coerce first arg to a <type-estimate>.
  type-estimate-subtype?(as(<type-estimate>, t1), t2)
end;

define method guaranteed-joint?(t1 :: <type-estimate>, t2 :: <&type>)
 => (res :: <boolean>)
  // Coerce second arg to a <type-estimate>.
  type-estimate-subtype?(t1, as(<type-estimate>, t2))
end;

define method guaranteed-joint?(t1 :: <&type>, t2 :: <&type>)
 => (res :: <boolean>)
  // Coerce both args to <type-estimate>s.
  type-estimate-subtype?(as(<type-estimate>, t1), as(<type-estimate>, t2))
end;

define method guaranteed-joint?(t1 :: <type-estimate>, t2 :: <type-estimate>)
 => (res :: <boolean>)
  // Both are <type-estimate>s, so hand off.
  type-estimate-subtype?(t1, t2)
end;

define method guaranteed-joint?(t1 :: <type-estimate-limited-instance>,
				t2 :: <&type>)
 => (res :: <boolean>)
  // Singleton type estimates can be decided quickly, w/o typist overhead. 
  ^instance?(type-estimate-singleton(t1), t2)
end;
   
// Disambiguating methods
define method guaranteed-joint?(t1 :: <type-estimate-limited-instance>,
				t2 :: <&top-type>)
 => (res :: singleton(#t))
  #t
end;
define method guaranteed-joint?(t1 :: <type-estimate-limited-instance>,
				t2 :: <&bottom-type>)
 => (res :: singleton(#f))
  #f
end;

define method guaranteed-joint?(t1 :: <&singleton>, t2 :: <&type>)
 => (res :: <boolean>)
  // Singleton model types can be decided quickly, w/o typist overhead. 
  ^instance?(^singleton-object(t1), t2)
end;

// Disambiguating methods
define method guaranteed-joint?(t1 :: <&singleton>, t2 :: <&top-type>)
 => (res :: singleton(#t))
  #t
end;
define method guaranteed-joint?(t1 :: <&singleton>, t2 :: <&bottom-type>)
 => (res :: singleton(#f))
  #f
end;

define method guaranteed-joint?(t1 :: <type-estimate>, t2 :: <&top-type>)
 => (res :: singleton(#t))
  // Top types can be quickly decided, w/o typist overhead.
  #t
end;

define method guaranteed-joint?(t1 :: <&type>, t2 :: <&top-type>)
 => (res :: singleton(#t))
  // Top types can be quickly decided, w/o typist overhead.
  #t
end;

define method guaranteed-joint?(t1 :: <type-estimate>, t2 :: <&bottom-type>)
 => (res :: singleton(#f))
  // Bottom can be quickly decided, w/o typist overhead.
  #f
end;

define method guaranteed-joint?(t1 :: <&type>, t2 :: <&bottom-type>)
 => (res :: singleton(#f))
  // Bottom can be quickly decided, w/o typist overhead.
  #f
end;

define method guaranteed-joint? (t1 :: <type-estimate-bottom>, t2 :: <&bottom-type>)
 => (res :: singleton(#t))
  // But bottom is joint with itself.
  // Believe it or not, this comes up in checking the "return" type of error!
  #t
end;
  
define method guaranteed-joint? (t1 :: <&bottom-type>, t2 :: <type-estimate-bottom>)
 => (res :: singleton(#t))
  // But bottom is joint with itself.
  // Believe it or not, this comes up in checking the "return" type of error!
  #t
end;

define method guaranteed-joint? (t1 :: <type-estimate-bottom>, t2 :: <type-estimate-bottom>)
 => (res :: singleton(#t))
  // But bottom is joint with itself.
  // Believe it or not, this comes up in checking the "return" type of error!
  #t
end;

define method guaranteed-joint? (t1 :: <&bottom-type>, t2 :: <&bottom-type>)
 => (res :: singleton(#t))
  // But bottom is joint with itself.
  // Believe it or not, this comes up in checking the "return" type of error!
  #t
end;

///
/// guaranteed-disjoint?(t1, t2) -- #t if any instance of t1 is guaranteed 
///    never to be an instance of t2.  NB: If t1 is a subtype of t2, then EVERY
///    instance of t1 is an instance of t2.  If t2 is a subtype of t1, then SOME
///    (indirect) instances of t1 are (direct) instances of t2.  So t1 & t2
///    must be incomparable in order to be disjoint.
///
///    Disjointness is a symmetric, non-transitive, anti-reflexive relation.
///
///    DRM p.49 gives disjointness rules.
///

define generic guaranteed-disjoint?(t1 :: <type-or-est>, t2 :: <type-or-est>)
  => (res :: <boolean>);

define method guaranteed-disjoint?(t1 :: <&type>, t2 :: <type-estimate>)
 => (res :: <boolean>)
  // Coerce first arg to a <type-estimate>.
  type-estimate-disjoint?(as(<type-estimate>, t1), t2)
end;

define method guaranteed-disjoint?(t1 :: <type-estimate>, t2 :: <&type>)
 => (res :: <boolean>)
  // Coerce second arg to a <type-estimate>.
  type-estimate-disjoint?(t1, as(<type-estimate>, t2))
end;

define method guaranteed-disjoint?(t1 :: <&type>, t2 :: <&type>)
 => (res :: <boolean>)
  // Coerce both args to a <type-estimate>.
  type-estimate-disjoint?(as(<type-estimate>, t1), as(<type-estimate>, t2))
end;

define method guaranteed-disjoint?(t1 :: <type-estimate>, t2 :: <type-estimate>)
 => (res :: <boolean>)
  // Both are <type-estimate>s, so hand off
  type-estimate-disjoint?(t1, t2)
end;

///
/// These are attempts to short-circuit the typist in frequent but quick cases.
/// Metering indicates that the typist overhead was excessive for these simple,
/// frequently-occurring cases.
///

define method guaranteed-disjoint?(t1 :: <type-estimate-limited-instance>,
	                           t2 :: <&type>)
 => (res :: <boolean>)
  // Singleton type estimates can be quickly decided here.
  ~^instance?(type-estimate-singleton(t1), t2)
end;
   
define method guaranteed-disjoint?(t1 :: <&type>,
                                   t2 :: <type-estimate-limited-instance>)
 => (res :: <boolean>)
  // Singleton type estimates can be quickly decided here.
  ~^instance?(type-estimate-singleton(t2), t1)
end;

define method guaranteed-disjoint?(t1 :: <&singleton>, t2 :: <&type>)
 => (res :: <boolean>)
  // Singleton model types can be quickly decided here.
  ~^instance?(^singleton-object(t1), t2)
end;

define method guaranteed-disjoint?(t1 :: <&type>, t2 :: <&singleton>)
 => (res :: <boolean>)
  // Singleton model types can be quickly decided here.
  ~^instance?(^singleton-object(t2), t1)
end;

define method guaranteed-disjoint?(c1 :: <&class>, c2 :: <&class>)
 => (res :: <boolean>)
  // Going through the typist would involve wrapping & then unwrapping the 
  // <type-estimate>, so just apply directly to the eventual oracle.
  ^classes-guaranteed-disjoint?(c1, c2)
end;

define method guaranteed-disjoint?(c1 :: <&class>, c2 :: <type-estimate-class>)
 => (res :: <boolean>)
  // Going through the typist would involve wrapping & then unwrapping the 
  // <type-estimate>, so just apply directly to the eventual oracle.
  ^classes-guaranteed-disjoint?(c1, type-estimate-class(c2))
end;

define method guaranteed-disjoint?(c1 :: <type-estimate-class>, c2 :: <&class>)
 => (res :: <boolean>)
  // Going through the typist would involve wrapping & then unwrapping the 
  // <type-estimate>, so just apply directly to the eventual oracle.
  ^classes-guaranteed-disjoint?(type-estimate-class(c1), c2)
end;

define method guaranteed-disjoint?(c1 :: <type-estimate-limited-instance>,
				   c2 :: <&class>)
 => (res :: <boolean>)
  // Going through the typist would involve wrapping & then unwrapping the 
  // <type-estimate>, so just apply directly to the eventual oracle.
  ~^instance?(type-estimate-singleton(c1), c2)
end;


define method guaranteed-disjoint?(c1 :: <type-estimate-class>, 
                                   c2 :: <type-estimate-class>)
 => (res :: <boolean>)
  // Going through the typist would involve wrapping & then unwrapping the 
  // <type-estimate>, so just apply directly to the eventual oracle.
  ^classes-guaranteed-disjoint?(type-estimate-class(c1),type-estimate-class(c2))
end;

// I just happen to know that <top> will only occur on the right
define method guaranteed-disjoint?(c1 :: <type-estimate>, 
                                   c2 :: <&top-type>)
 => (res :: singleton(#f))
  #f
end;

define method guaranteed-disjoint?(c1 :: <&type>, 
                                   c2 :: <&top-type>)
 => (res :: singleton(#f))
  #f
end;

define method guaranteed-disjoint?(t1 :: <type-estimate>, t2 :: <&bottom-type>)
 => (res :: singleton(#t))
  #t
end;

define method guaranteed-disjoint?(t1 :: <&type>, t2 :: <&bottom-type>)
 => (res :: singleton(#t))
  #t
end;


///
///  potentially-joint? (<type-estimate>, <&type>) => (<boolean>)
///
///    Returns true unless all instances conforming to the type-estimate
///    are guaranteed not to be instances of the given Dylan type.
///

define function potentially-joint? 
    (type-estimate :: <type-estimate>, type :: <&type>) => (res :: <boolean>)
  // Not provably DISjoint.
  ~guaranteed-disjoint?(type-estimate, type)
end;


///
/// guaranteed-preceding-specializer? (type1, type2, type-estimate) => (boolean)
///
///    Type1 and type2 are Dylan language types that have appeared as
///    method specializers. Type-estimate is the inferred type of an
///    argument to a generic function. The predicate returns #t if type1
///    is more specific than type2 for all instances conforming to
///    type-estimate (according to the rules on page 94 of the September
///    '95 DRM). 
///
/// So, for example, in:
///
///  define class <a> (<object>) end;
///    define class <b> (<a>) end;
///    define class <c> (<a>) end;
///      define class <d> (<b>, <c>) end;
///
///  define method nurgle (o :: <a>) end;
///  define method nurgle (o :: <b>) end;
///  define method nurgle (o :: <c>) end;
///  define method nurgle (o :: <d>) end;
///
/// given a class estimate of <d> for an argument to nurgle, I should be
/// able to determine the complete sorted applicable method set, using the
/// above predicate to order the methods on <b> and <c>. And similarly if
/// the type estimate for the argument is more complex, such as an
/// intersection type estimate involving <d>. Let's hear it for
/// monotonicity!
///

define generic guaranteed-preceding-specializer?(t1  :: <type-or-est>,
                                                 t2  :: <type-or-est>,
                                                 arg :: <type-or-est>) 
  => (always-precedes? :: <boolean>);

// *** Stub for later improvement.
define method guaranteed-preceding-specializer? (type1  :: <&type>, 
                                                 type2  :: <&type>, 
                                                 arg-te :: <type-estimate>)
 => (res :: <boolean>)
  ^subtype?(type1, type2)
end;
