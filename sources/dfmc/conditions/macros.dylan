Synopsis: Macros for defining and using program conditions.
Author:   haahr, jonathan, keith, swm
Module:   dfmc-conditions-implementation
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// define program-condition
// 
// This macro is used to define new condition classes.  It handles the
// condition-format as declarations within the class, which report
// information based on the hierarchy.
//
// The basic syntax is
//
//   define [modifier *] program-condition /name/ (/superclass/, *)
//     [/slot-spec/ | format-string /string/; | format-arguments /slot/, *;]*
//   end [program-condition] [/name/]
//
// which expands into a similar "define class" form, with a make
// method to handle the format-arguments, if present.
//
// As usual for interesting macros, it needs some auxiliary macros,
// because the slot-specs are parsed multiple times.

define macro program-condition-definer
  { define ?modifiers:* program-condition ?:name (?supers:*)
      ?specs:*
    end }
  => { define ?modifiers program-condition-aux ?name
         (?supers) (?modifiers) (?specs)
       end;
       define condition-make-method-maybe (?name)
         ?specs
       end;
       define method convert-condition-slots-to-ppml(?=%c :: ?name) => ();
         convert-slots-to-ppml ?specs end;
         ?=next-method()
       end;
       define condition-make-filter (?name) 
         ?specs
       end;
       /* Unused: Condition filtering.
       define sealed inline method program-note-filter
           (c == ?name) => (filter :: <program-note-filter>)
         ignore(c); 
         "*" ## ?name ## "-filter*"
       end;
       define sealed method program-note-filter-setter
           (filter :: <program-note-filter>, c == ?name)
        => (filter :: <program-note-filter>)
         ignore(c);
         "*" ## ?name ## "-filter*" := filter;
         ?=next-method()
       end
       */
     }
end macro program-condition-definer;


define macro program-condition-aux-definer

  { define ?modifiers program-condition-aux ?:name
      (?supers:*) (?mixins) (?specs)
    end }
  => { define ?modifiers class ?name (?supers, ?mixins) ?specs end; }

 modifiers:
  { ?:name ... }  => { ?name ... }
  { }             => { }

 mixins:
  { ?:name ... }  => { ... }
  { }             => { }

 specs:
  { }            => { }
  { ?spec; ... } => { ?spec; ... }

 spec:
  { format-arguments ?args:* } 
    => { }
  { format-string ?:expression }
    => { keyword format-string: = ?expression }
  { filter ?args:* }
    => { }
  // This hackery is needed to allow for ppml conversion back into the same
  // slot...
  { ?mods:* slot condition-context-id ?more:* }
    => { ?mods slot condition-context-id ?more }
  { ?mods:* slot subnotes ?more:* }
    => { ?mods slot subnotes ?more }
  { ?adjectives:* slot ?:name :: ?type:expression ?more:* }
    => { ?adjectives slot ?name :: type-union(?type, <ppml>) ?more }
  { ?other:* }                   
    => { ?other }

 adjectives:
  // Even if the user declares "constant", it can't be because of the 
  // ppml hackery, so we strip it out.
  { ?before:* constant ?after:* }
    => { ?before ?after }
  { ?other:* }
    => { ?other }

end macro program-condition-aux-definer;


define macro convert-slots-to-ppml
  { convert-slots-to-ppml end }
  => { }

  { convert-slots-to-ppml ?spec:*; ... end }
  => { ?spec
       convert-slots-to-ppml ... end; }

 spec:
  { format-arguments ?args:*   } => { }
  { format-string ?:expression } => { }
  { filter ?args:*             } => { }
  { keyword ?args:*            } => { }
  { ?mods:* slot condition-context-id ?rest:* } => { }
  { ?mods:* slot subnotes ?rest:* } 
    => { do(convert-condition-slots-to-ppml, ?=%c.subnotes); }
  { slot ?name:variable ?rest:* } => { ?=%c.?name := as(<ppml>, ?=%c.?name); }
  { constant slot ?name:variable ?rest:* } 
    => { ?=%c.?name := as(<ppml>, ?=%c.?name); }
end macro convert-slots-to-ppml;


define macro condition-make-method-maybe-definer

  { define condition-make-method-maybe (?class:name) end }
  => { }

  { define condition-make-method-maybe (?class:name)
      format-arguments ?args:*; ?ignored:*
    end }
  => { define condition-make-method-maybe (?class)
         format-arguments-aux (?args) (?args)
       end }

  { define condition-make-method-maybe (?class:name)
      format-arguments-aux (?call-args) (?decl-args)
    end }
  => { define method make
	   (class :: subclass(?class), #rest initargs, #key ?decl-args) 
        => (object :: ?class)
         // TODO: make this really check
         if (member?(format-arguments:, initargs))
           apply(?=next-method, class, initargs)
         else
           apply(?=next-method, class, 
                 format-arguments: list(?call-args), initargs)
         end if
       end method make }

  { define condition-make-method-maybe (?class:name)
      ?spec:*; ?rest:*
    end }
  => { define condition-make-method-maybe (?class)
         ?rest
       end }

 call-args:
  { }                           => { }
  { ?argument:name, ... }       => { ?argument, ... }
  { ?argument:name again, ... } => { ?argument, ... }

 decl-args:
  { }                           => { }
  { ?argument:name, ... }       => { ?argument, ... }
  { ?argument:name again, ... } => { ... }

end macro condition-make-method-maybe-definer;

define macro condition-make-filter-definer
  { define condition-make-filter (?class:name) end }
  => { // Inherit setting from parent note.
       /* Unused: Condition filtering. 
       define variable "*" ## ?class ## "-filter*" = 
         block (return)
           for (c in direct-superclasses(?class))
             if (subtype?(c, <program-note>))
               return(c.program-note-filter)
             end
           end;
         end;           
       */
     }

  { define condition-make-filter (?class:name)
      filter ?arg:expression; ?ignored:*
    end }
  /* Unused: Condition filtering. 
  => { define variable "*" ## ?class ## "-filter*" = ?arg; }
  */
  => { define sealed inline method program-note-filter
           (c :: subclass(?class)) => (filter :: <program-note-filter>)
         ?arg
       end; }

  { define condition-make-filter (?class:name) 
      ?spec:*; ?rest:* 
    end }
  => { define condition-make-filter (?class) ?rest end }
end macro condition-make-filter-definer;

// define program-condition-definer
//
// This macro defines another macro (using Functional Objects extension
// syntax for quoting pattern variables in macros-within-macros), which
// follows the same form as define-program-condition.
//
// The generated macros are used for convenient definition of specific
// condition classes.

define macro program-condition-definer-definer

  { define program-condition-definer ?:name }
  => { define program-condition-definer ?name :: "<" ## ?name ## ">" }

  { define program-condition-definer ?:name :: ?default:expression }
  => { define macro ?name ## "-definer"

         { define \?modifiers:* ?name \?cond-class:name (\?supers:*)
             \?specs:*
           end }
         => { define \?modifiers program-condition \?cond-class (\?supers)
                \?specs
              end program-condition \?cond-class;
              if (~subtype?(\?cond-class, ?default))
		error(\?"cond-class" " is not a subclass of " ?"default");
	      else
		values()
	      end if }

         { define \?modifiers:* ?name \?cond-class:name \?specs:* end }
         => { define \?modifiers ?name \?cond-class (?default)
                \?specs
              end ?name \?cond-class }

       end macro /* ?name ## "-definer" */ }

end macro program-condition-definer-definer;

// condition-block
//
// This is a simple convenience macro for defining restarts for
// handling program conditions.  The syntax is
//
//   condition-block
//     /body/
//   {[default] restart [/name/ ::] /type/ {, /init-arguments/}* [and /test/]
//     => /restart-body/}*
//   end [condition-block]
//
// It expands to a block statement with an exception clause for each
// restart clause.  The init-arguments are packaged into a sequence and
// passed as the init-arguments: exception option.  The test, if present,
// is turned into a method as used for the test: exception option.  The
// default option makes the restart a potential default restart -- the
// accompanying condition handler chooses the innermost appropriate default
// restart.
//
// It's not clear to me that having special syntax is really worth it,
// but the examples do look prettier this way.

define macro condition-block
  { condition-block ?:body ?restarts end }
  => { condition-block-aux ?body ?restarts end }

 restarts:
  { restart ?spec:* => ?:body ... }
  => { restart (?spec) => ?body }
  { default restart ?cond:* and ?guard:expression => ?:body ... }
  => { restart ?cond, default?: #t and ?guard => ?body ... }
  { default restart ?cond:* => ?:body ... }
  => { restart ?cond, default?: #t => ?body ... }
  { }
  => { }

end macro condition-block;

define macro condition-block-aux
  { condition-block-aux ?:body ?restarts end }
  => { with-program-restarts ?body ?restarts end }

 restarts:
  { restart ?spec => ?:body ... }
  => { restart (?spec) ?body ... }
  { }
  => { }

 spec:
  { ?type:expression, ?init-args:* and ?guard:expression }
  => // probably should be prohibited, because an unnamed condition
     // doesn't make sense with the guard.
     { _condition_ :: ?type,
         init-arguments: vector(?init-args),
         test: method (condition) ?guard end }
  { ?:name :: ?type:expression, ?init-args:* and ?guard:expression }
  => { ?name :: ?type,
         init-arguments: vector(?init-args),
         test: method (?name) ?guard end }
  { ?type:expression, ?init-args:* }
  => { _condition_ :: ?type, init-arguments: vector(?init-args) }
  { ?:name :: ?type:expression, ?init-args:* }
  => { ?name :: ?type, init-arguments: vector(?init-args) }

end macro condition-block-aux;


define macro maybe-note
  { maybe-note(?class:expression, ?rest:*) }
  => { if (?class.program-note-filter)
         note(?class, ?rest)         
       end if 
     }
end macro;


// Macros to support the construction of subnotes.

define macro accumulate-subnotes-during
  { accumulate-subnotes-during(?expr:expression) }
    => { dynamic-bind ( *subnotes-queue* = make(<deque>) )
           let result = ?expr;
           values(result, *subnotes-queue*)
         end dynamic-bind }
end macro accumulate-subnotes-during;

define macro note-during
  { note-during(?expr:expression, ?args:*) }
    => { let (result, subs) = accumulate-subnotes-during(?expr);
         note(?args, subnotes: subs);
         result }
end macro note-during;



