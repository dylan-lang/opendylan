module:    internal
author:    jonathan bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $end-count-<object> = 0;

define open generic initialize-packed-slots
    (instance, #rest all-keys, #key, #all-keys);

define method initialize-packed-slots
    (instance, #rest all-keys, #key, #all-keys)
end method;

define inline function pack-boolean 
    (x :: <boolean>) => (z :: <integer>)
  if (x) 1 else 0 end
end function;

define inline function unpack-boolean 
    (x :: <integer>) => (z :: <boolean>)
  if (x = 1) #t else #f end
end function;

define inline function pack-tristate (x) => (z :: <integer>)
  if (x) if (x == #"unknown") 0 else 2 end else 1 end
end function;

define inline function unpack-tristate (x :: <integer>) => (res)
  if (x = 2) #t elseif (x = 1) #f else #"unknown" end
end function;

define inline function pack-quadstate (x) => (z :: <integer>)
  select (x)
    #"unknown"    => 0;
    #"processing" => 1;
    #f            => 2;
    #t            => 3;
  end select;
end function;

define inline function unpack-quadstate (x :: <integer>) => (z)
  select (x)
    0 => #"unknown"    ;
    1 => #"processing" ;
    2 => #f            ;
    3 => #t            ;
  end select;
end function;

define macro packed-slots-definer
  { define ?adj:* packed-slots ?pslot:name (?class:name, ?superclass:name) ?slots:* end }
    => { define ?adj packed-slots-aux ?pslot (?class, ?superclass) 
           (?slots) (?slots) (?slots)
         end }
end macro;

define macro packed-slots-aux-definer
  { define ?adj:* packed-slots-aux ?pslot:name (?class:name, ?superclass:name)
      (?slots:*) (?kslots) (?islots) 
    end }
    => { define ?adj packed-accessors ?pslot (?class, "$end-count-" ## ?superclass)
            ?slots
          end packed-accessors;
         define method initialize-packed-slots
             (x :: ?class, #rest all-keys, #key ?kslots)
           ?=next-method();
	   ?islots
	 end method }
islots:
  { }
    => { }
  { ?islot; ... }
    => { ?islot; ... }
islot: 
  { ?adjectives:* slot ?variable-name, ?ignore:* }
    => { if (supplied?(?variable-name))
	   ?variable-name(x) := ?variable-name;
	 end if }
kslots:
  { }
    => { }
  { ?kslot; ... }
    => { ?kslot, ... }
  { ?slot:*; ... }
    => { ... }
kslot: 
  { ?adjectives:* slot ?:name = ?default:expression, 
      ?before:* init-keyword: ?:symbol ?after:* }
    => { ?symbol ?name = ?default }
  { ?adjectives:* slot ?:name = ?default:expression, 
      ?before:* required-init-keyword: ?:symbol ?after:* }
    => { ?symbol ?name = ?default }
  { ?adjectives:* slot ?:name, 
      ?before:* init-keyword: ?:symbol ?after:* }
    => { ?symbol ?name = unsupplied() }
  { ?adjectives:* slot ?:name, 
      ?before:* required-init-keyword: ?:symbol ?after:* }
    => { ?symbol ?name = unsupplied() }
  { ?adjectives:* slot ?:name = ?default:expression ?after:* }
    => { ?name = ?default }
  { ?adjectives:* slot ?:name ?after:* }
    => { ?name = unsupplied() }
variable-name: 
  { ?:name ?maybe-init-expression:* }
    => { ?name }
end macro;

define macro packed-accessors-definer
  { define packed-accessors ?pslot:name (?class:name, ?count:expression)
    end } 
    => { define constant "$end-count-" ## ?class = ?count }
  { define leaf packed-accessors ?pslot:name (?class:name, ?count:expression)
    end } 
    => { }
  { define ?adj:* packed-accessors ?pslot:name (?class:name, ?count:expression)
      boolean slot ?:name ?more:*; ?more-slots:*
    end } 
    => { define inline method ?name (x :: ?class) => (object :: <boolean>)
           logand(?pslot(x), ash(1, ?count)) ~= 0
         end method;
         define inline method ?name ## "-setter" 
             (z :: <boolean>, x :: ?class) => (z :: <boolean>)
	   let p = ?pslot(x);
	   ?pslot(x) :=
	     if (z)
	       logior(ash(1, ?count), p)
	     else
	       logand(lognot(ash(1, ?count)), p)
	     end if;
	   z
	 end method;
         define ?adj packed-accessors ?pslot (?class, ?count + 1)
           ?more-slots
         end }
  { define ?adj:* packed-accessors ?pslot:name (?class:name, ?count:expression)
      tristate slot ?:name ?more:*; ?more-slots:*
    end } 
    => { define ?adj packed-accessors ?pslot (?class, ?count) 
           eval slot ?name, field-size: 2, 
             pack-function: pack-tristate, unpack-function: unpack-tristate, 
             ?more; 
           ?more-slots end }
  { define ?adj:* packed-accessors ?pslot:name (?class:name, ?count:expression)
      quadstate slot ?:name ?more:*; ?more-slots:*
    end } 
    => { define ?adj packed-accessors ?pslot (?class, ?count) 
           eval slot ?name, field-size: 2, 
             pack-function: pack-quadstate, unpack-function: unpack-quadstate,
             ?more; 
           ?more-slots end }
  { define ?adj:* packed-accessors ?pslot:name (?class:name, ?count:expression)
      eval slot ?:name ?maybe-init:*, 
        field-size:      ?field-size:expression, 
        pack-function:   ?pack-function:expression,
        unpack-function: ?unpack-function:expression,
        ?more:*; 
      ?more-slots:*
    end } 
    => { define inline method ?name (x :: ?class) => (object)
	   ?unpack-function(?name ## "-field"(x))
         end method;
         define inline method ?name ## "-setter" (z, x :: ?class) => (object)
	   ?name ## "-field"(x) := ?pack-function(z)
	 end method;
         define ?adj packed-accessors ?pslot (?class, ?count) 
           field slot ?name ## "-field" ?maybe-init, 
             field-size: ?field-size, ?more; 
           ?more-slots 
         end }
  { define ?adj:* packed-accessors ?pslot:name (?class:name, ?count:expression)
      field slot ?:name ?maybe-init:*, 
        field-size: ?field-size:expression, ?props:*; ?more-slots:*
    end } 
    => { define inline method ?name (x :: ?class) => (object :: <integer>)
           ash(logand(?pslot(x), ash(ash(1, ?field-size) - 1, ?count)), 
               -?count)
         end method;
         define inline method ?name ## "-setter" 
             (z :: <integer>, x :: ?class) => (object :: <integer>)
	   let p = ?pslot(x);
	   ?pslot(x) 
	     := logior(logand(p, lognot(ash(ash(1, ?field-size) - 1, ?count))),
                       ash(z, ?count));
	 end method;
         define ?adj packed-accessors ?pslot (?class, ?count + ?field-size)
           ?more-slots
         end }
end macro;

// eof
