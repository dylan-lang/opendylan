Module:       dood
Synopsis:     The Dylan object-oriented database
Author:       Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define macro dood-format
  { dood-format(?string:expression, ...) }
    => { }
    // => { block ()
    //	      format-out(?string, ...);
    //      exception (<error>)
    // 	    end block }
end macro;

define macro audit 
  { audit(?dood:expression, ?format-string:expression, ?arguments:*) }
    => { }
    // => { apply(format, dood-world-log-stream(dood-world(?dood)), 
    // 	          ?format-string, dood-index(?dood), ?arguments) }
end macro;

///
/// DOOD WEAK SLOT DEFINER
///

define macro weak-slots-definer
  { define weak-slots ?class:name using ?dood:name = { } }
    => { }
  { define weak-slots ?class:name using ?dood:name = { ?entries } }
    => { define method dood-weak-getters (class_ :: subclass(?class)) => (res :: <list>)
           concatenate(?=next-method(), list(?entries))
	 end method }
entries:
  { } 
    => { }
  { ?getter:name, ... } 
    => { pair(?getter, #f), ... }
  { ?getter:name => ?default:expression, ... } 
    => { pair(?getter, method (?=self) ?default end), ... }
end macro;

define macro lazy-slots-definer
  { define lazy-slots ?class:name using ?dood:name = { } }
    => { }
  { define lazy-slots ?class:name using ?dood:name = { ?entries:* } }
    => { define method dood-lazy-getters (class_ :: subclass(?class)) => (res :: <list>)
           concatenate(?=next-method(), list(?entries))
	 end method }
end macro;

///
/// DOOD CLASS DEFINER
///

define macro dood-class-definer
  { define ?adjectives:* dood-class ?:name (?supers:*) ?slots:* end }
    => { define ?adjectives dood-class-aux ?name (?supers) 
           (?slots) (?slots) (?slots) (?slots)
         end }
end macro;

//define method dood-getters (x) #[] end;

define macro dood-class-aux-definer
  { define ?adjectives:* dood-class-aux ?:name (?supers:*)
      (?slots) (?aslots) (?lslots) (?wslots)
    end }
    => { define ?adjectives class ?name (?supers) ?slots end;
         define dood-class-accessors ?name ?aslots end;
         define lazy-slots ?name using <dood> = { ?lslots };
         define weak-slots ?name using <dood> = { ?wslots } }
slots:
  { }
    => { }
  { ?slot:*; ... }
    => { ?slot; ... }
slot: // TODO: HANDLE OTHER SLOT ADJECTIVES
  { weak slot ?variable, ?standard-props }
    => { slot ?variable, ?standard-props }
  { weak constant slot ?variable, ?standard-props }
    => { constant /* volatile */ slot ?variable, ?standard-props }
  { lazy slot ?private-lazy-variable, ?props:* }
    => { slot ?private-lazy-variable, ?props }
  { lazy constant slot ?private-lazy-variable, ?props:* }
    => { constant /* volatile */ slot ?private-lazy-variable, ?props }
  { disk slot ?private-variable, ?props:* }
    => { slot ?private-variable, ?props }
  { ?other:* }
    => { ?other }
aslots:
  { }
    => { }
  { ?aslot; ... }
    => { ?aslot; ... }
aslot: // TODO: HANDLE OTHER SLOT ADJECTIVES
  { ?adjectives:* slot ?variable-name-and-type, ?props:* }
    => { ?adjectives slot ?variable-name-and-type, ?props }
  { ?other:* }
    => { ?other }
lslots: // TODO: HANDLE OTHER SLOT ADJECTIVES
  { }
    => { }
  { lazy slot ?private-variable-name, ?props:*; ... }
    => { ?private-variable-name, ... }
  { lazy constant slot ?private-variable-name, ?props:*; ... }
    => { ?private-variable-name, ... }
  { disk slot ?private-variable-name, ?props:*; ... }
    => { ?private-variable-name, ... }
  { ?other:*; ... }
    => { ... }
wslots:
  { }
    => { }
  // { weak slot ?variable-name, ?reinit-expression; ... }
  //   => { ?variable-name => ?reinit-expression, ... }
  { weak slot ?variable-name, reinit-expression: ?:expression, ?props:*; ... }
    => { ?variable-name => ?expression, ... }
  { weak constant slot ?variable-name, reinit-expression: ?:expression, ?props:*; ... }
    => { ?variable-name => ?expression, ... }
  { weak slot ?variable-name, ?props:*; ... }
    => { ?variable-name, ... }
  { weak constant slot ?variable-name, ?props:*; ... }
    => { ?variable-name, ... }
  { ?other:*; ... }
    => { ... }
variable:
  { ?:name :: ?:expression ?maybe-init-expression } 
    => { ?name :: ?expression ?maybe-init-expression }
variable-name: 
  { ?:name :: ?:expression ?maybe-init-expression }
    => { ?name }
variable-name-and-type: 
  { ?:name :: ?:expression ?maybe-init-expression }
    => { ?name :: ?expression }
private-variable-name: 
  { ?private-name :: ?:expression ?maybe-init-expression } 
    => { ?private-name }
private-lazy-variable:
  { ?private-name :: ?:expression ?maybe-init-expression } 
    => { ?private-name :: type-union(<dood-slot-value-proxy>, ?expression) 
	    ?maybe-init-expression }
private-variable:
  { ?private-name :: ?:expression ?maybe-init-expression } 
    => { ?private-name :: ?expression ?maybe-init-expression }
private-name:
  { ?:name } 
    => { "private-" ## ?name }
standard-props:
  { } 
    => { }
  { reinit-expression: ?:expression, ... }
    => { ... }
  { ?standard-prop:*, ... }
    => { ?standard-prop, ... }
reinit-expression:
  { reinit-expression: ?:expression, ?other-props:* }
    => { ?expression }
  { ?standard-prop:*, ... }
    => { ... }
maybe-init-expression:
  { = ?:expression } => { = ?expression }
  { }                => { }
end macro;

define macro dood-class-accessors-definer
  { define dood-class-accessors ?class-name:name 
    end } 
    => { }
  { define dood-class-accessors ?class-name:name 
      lazy slot ?:name :: ?type:expression, ?props:*; ?more-slots:*
    end } 
    => { define method ?name (x :: ?class-name) => (object :: ?type)
           dood-lazy-slot-value(x, "private-" ## ?name)
         end method;
         define method ?name ## "-setter" 
             (v :: ?type, x :: ?class-name) => (object :: ?type)
           "private-" ## ?name(x) := v
	 end method;
         define dood-class-accessors ?class-name ?more-slots end }
  { define dood-class-accessors ?class-name:name 
      lazy constant slot ?:name :: ?type:expression, ?props:*; ?more-slots:*
    end } 
    => { define method ?name (x :: ?class-name) => (object :: ?type)
           dood-lazy-slot-value(x, "private-" ## ?name)
         end method;
         define dood-class-accessors ?class-name ?more-slots end }
  { define dood-class-accessors ?class-name:name 
      disk slot ?:name :: ?type:expression, ?props:*; ?more-slots:*
    end } 
    => { define method ?name (x :: ?class-name) => (object :: ?type)
           dood-disk-slot-value(x, "private-" ## ?name)
         end method;
         // TODO: should just use setter: ?name ## "-setter" in the
         // slot definition and get rid of private-xxx-setter altogether.
         define method ?name ## "-setter" 
             (v :: ?type, x :: ?class-name) => (object :: ?type)
           "private-" ## ?name(x) := v
	 end method;
         define dood-class-accessors ?class-name ?more-slots end }
  { define dood-class-accessors ?class-name:name  
      ?adjectives:* slot ?:name :: ?type:expression, ?stuff:*; ?more-slots:*
    end } 
    => { define dood-class-accessors ?class-name ?more-slots end }
end macro;

define macro with-saved-position
  { with-saved-position (?dood:expression) ?:body end }
    => { let saved-position = dood-position(?dood);
	 let value = begin ?body end;
	 dood-position(?dood) := saved-position;
	 value }
end macro;
  
// eof

