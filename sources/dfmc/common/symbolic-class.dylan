module: dfmc-common
author: jonathan bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define macro symbolic-class-definer
  { define symbolic-class ?:name (?supers:*) using ?resolver:expression
      ?slots:*
    end }
    => { define symbolic-class-aux ?name (?supers) using ?resolver
           (?slots) (?slots)
         end }
end macro symbolic-class-definer;

define macro symbolic-class-aux-definer
  { define symbolic-class-aux ?:name (?supers:*) using ?resolver:expression
      (?slots:*) (?symbolic-slots:*)
    end }
    => { define class ?name (?supers) ?slots end;
         define symbolic-accessors ?name (?supers) using ?resolver
           ?symbolic-slots
         end }
slots:
  { }
    => { }
  { ?slot:*; ... }
    => { ?slot; ... }
slot:
  { slot ?:name :: ?:expression, ?stuff:* }
    => { slot ?name :: ?expression, ?stuff }
  { constant slot ?:name :: ?:expression, ?stuff:* }
    => { constant slot ?name :: ?expression, ?stuff }
  { symbolic slot ?:name, required-init-keyword: ?keyword:token }
    => { // slot "%" ## ?name, init-value: $unfound ; 
         constant slot ?name ## "-name" :: <symbol>, 
           required-init-keyword: ?keyword
           // required-init-keyword: coagulate(?name ## "-name")
           // required-init-keyword: ?#"name" ## "-name" 
       }
  { ?other:* }
    => { ?other }
end macro;

define macro symbolic-accessors-definer
  { define symbolic-accessors ?class:name (?supers:*)
        using ?resolver:expression
      symbolic slot ?:name, ?stuff:*; ?rest:*
    end } 
    => { define sideways method ?name (object) => (res)
           /*
           let value = "%" ## ?name (object);
           if (unfound?(value))
             "%" ## ?name (object) := ?resolver(?name ## "-name" (object))
           else
             value
           end if
           */
           ?resolver(?name ## "-name" (object))
         end method;
         define symbolic-accessors ?class (?supers)
           using ?resolver
           ?rest
         end }
  { define symbolic-accessors ?class:name (?supers:*)
        using ?resolver:expression
      slot ?:name :: ?:expression, ?stuff:*; ?rest:*
    end } 
    => { define symbolic-accessors ?class (?supers) using ?resolver 
           ?rest 
         end }
  { define symbolic-accessors ?class:name (?supers:*)
        using ?resolver:expression
      constant slot ?:name :: ?:expression, ?stuff:*; ?rest:*
    end } 
    => { define symbolic-accessors ?class (?supers) using ?resolver 
           ?rest 
         end }
  { define symbolic-accessors ?:name (?supers:*) using ?resolver:expression
    end } 
    => { }
end macro;
