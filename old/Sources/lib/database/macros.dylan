module:    database
author:    jonathan bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define macro persistent-class-definer
  { define persistent-class ?:name (?supers:*) ?slots:* end }
    => { define persistent-class-aux ?name (?supers) (?slots) (?slots) end }
end macro;

define macro persistent-class-aux-definer
  { define persistent-class-aux ?:name (?supers:*)
      (?slots:*) (?aslots:*)
    end }
    => { define class ?name (?supers) ?slots end;
         define persistent-class-accessors ?name (0) (?supers) ?aslots end;
       }
aslots:
  { }
    => { }
  { ?aslot:*; ... }
    => { ?aslot; ... }
aslot:
  { slot ?name-type, ?stuff:* }
    => { slot ?name-type }
  { ephemeral slot ?name-type }
    => { ephemeral slot ?name-type }
  { ephemeral slot ?name-type, ?stuff:* }
    => { ephemeral slot ?name-type, ?stuff }
  { ?qualifier:name slot ?name-type, ?stuff:* }
    => { ?qualifier slot ?name-type }
name-type:
  { ?:name }
     => { ?name :: <object> }
  { ?:name :: ?:expression }
     => { ?name :: ?expression }
slots:
  { }
    => { }
  { ?slot:*; ... }
    => { ?slot; ... }
slot:
  { slot ?:name :: ?:expression, ?props:* }
    => { slot "private-" ## ?name :: ?expression = unswizzled(), ?props }
  { slot ?:name, ?props:* }
    => { slot "private-" ## ?name = unswizzled(), ?props }
  { ephemeral slot ?:name :: ?:expression, ?props:* }
    => { slot ?name :: ?expression, ?props }
  { ephemeral slot ?:name, ?props:* }
    => { slot ?name, ?props }
  { repeated slot ?:name, ?props:* }
    => { slot "private-" ## ?name }
  { repeated slot ?:name :: ?:expression, ?props:* }
    => { slot "private-" ## ?name }
  { ?other:* }
    => { ?other }
end macro;

define macro persistent-class-accessors-definer
  { define persistent-class-accessors ?class-name:name (?offset:expression)
        (?supers:*) 
    end } 
    => { }
  { define persistent-class-accessors ?class-name:name (?offset:expression)
        (?supers:*)
      slot ?:name :: ?type:expression; ?more-slots:*
    end } 
    => { define method ?name (x :: ?class-name) => (object)
             persistent-slot-value
               (x, "private-" ## ?name, "private-" ## ?name ## "-setter", 
                ?offset)
	   end method;
           define method ?name ## "-setter" 
               (v :: ?type, x :: ?class-name) => (object)
             persistent-slot-value
                 (x, "private-" ## ?name ## "-setter", ?offset) 
               := v;
	   end method;
	   define method repeated-slot-getter
               (class :: subclass(?class-name)) => (repeated?, offset)
	     values(#f, 0)
	   end method;
           define persistent-class-accessors ?class-name
                (?offset + 1) (?supers) 
             ?more-slots
          end }
  { define persistent-class-accessors ?class-name:name
         (?offset:expression) (?supers:*) 
      repeated slot ?repeated-name-type; ?more-slots:*
    end } 
    => { define persistent-class-repeated-accessors ?class-name 
             (?offset) (?supers) 
           ?repeated-name-type
         end }
  { define persistent-class-accessors ?class-name:name
        (?offset:expression) (?supers:*) 
      ephemeral slot ?:name :: ?type:expression, ?stuff:*; ?more-slots:*
    end } 
    => { define persistent-class-accessors ?class-name (?offset) (?supers) 
           ?more-slots
         end }
repeated-name-type:
  { ?:name :: ?:expression }
     => { (?name, ?expression, ?expression, ?expression) }
end macro;

define macro persistent-class-repeated-accessors-definer
  { define persistent-class-repeated-accessors 
        ?class-name:name (?offset:expression) (?supers:*) 
      (?:name, ?type:expression, ?accessor, ?slot-size)
    end }
    => { define method ?name (x :: ?class-name, index :: <integer>)
           ?accessor(x, "private-" ## ?name, ?offset, index);
	 end method;
         define method size (x :: ?class-name)
	   size("private-" ## ?name(x))
	 end method;
         define method repeated-slot-getter (class :: subclass(?class-name))
           values("private-" ## ?name, ?slot-size);
	 end method;
         define method ?name ## "-setter" 
             (value :: ?type, x :: ?class-name, index :: <integer>)
           ?accessor(x, "private-" ## ?name, ?offset, index) := value;
	 end method; }
accessor:
  { <byte-character> } => { persistent-byte-slot-element }
  { ?:expression }     => { persistent-slot-element }
slot-size:
  { <byte-character> } => { 1 }
  { ?:expression }     => { 4 }
end macro;

/*
define persistent-class <ppair> (<collection>)
  slot phead;
  slot ptail;
end;

define class <ppair> (<collection>)
  virtual slot phead, setter: phead-setter;
  slot private-phead, init-function: method () unswizzled() end;
  virtual slot ptail, setter: ptail-setter;
  slot private-ptail, init-function: method () unswizzled() end;
end;

define method phead (pp :: <ppair>) 
  persistent-getter(pp, private-phead, private-phead-setter, 0);
end;

define method phead-setter (value, pp :: <ppair>) 
  persistent-setter(value, pp, private-phead, private-phead-setter, 0);
end;

define method ptail (pp :: <ppair>) 
  persistent-getter(pp, private-ptail, private-ptail-setter, 1);
end;

define method ptail-setter (value, pp :: <ppair>) 
  persistent-setter(value, pp, private-ptail, private-ptail-setter, 1);
end;
*/
