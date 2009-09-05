module: dfmc-common
author: keith playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// temporary fix for limitations in macro system
/// typical usage is coagulate-name("%" ## ?name ## "-at")
/// which would yield the symbol

define macro coagulate-name { coagulate-name(?:name) } => { ?#"name" } end;

// Defines a dynamic-bindable property with a more CL-like shared base
// value for all threads.

/*
define macro thread-property-definer
  { define thread-property "*" ## ?:name ## "*" \:: ?type:expression
      = ?init:expression }
    => { define variable "*default-" ## ?name ## "*" = ?init;
         define thread variable "*" ## ?name ## "*" = #f;
         define method "default-" ## ?name ()
           "*default-" ## ?name ## "*"
         end method;
         define method "default-" ## ?name ## "-setter" (value)
           "*default-" ## ?name ## "*" := value
         end method;
         define method "current-" ## ?name ()
           "*" ## ?name ## "*" | "*default-" ## ?name ## "*"
         end method;
         define method "do-with-" ## ?name (code :: <function>, value)
           dynamic-bind ("*" ## ?name ## "*" = value)
             code();
           end;
         end method;
         define macro "with-" ## ?name
           { "with-" ## ?name (\?:expression) \?:body end }
             => { "do-with-" ## ?name(method () \?body end, \?expression) }
         end; }
  { define thread-property "*" ## ?:name ## "*" ?more:* }
    => { define thread-property "*" ## ?name ## "*" :: <object> ?more }
end macro;
*/

define macro thread-property-definer
  { define thread-property ?:name :: ?type:expression
      = ?init:expression }
    => { define variable "*default-" ## ?name ## "*" = ?init;
         define thread variable "*" ## ?name ## "*" = #f;
         define method "default-" ## ?name ()
           "*default-" ## ?name ## "*"
         end method;
         define method "default-" ## ?name ## "-setter" (value)
           "*default-" ## ?name ## "*" := value
         end method;
         define method "current-" ## ?name ()
           "*" ## ?name ## "*" | "*default-" ## ?name ## "*"
         end method;
         define inline method "do-with-" ## ?name (code :: <function>, value)
           dynamic-bind ("*" ## ?name ## "*" = value)
             code();
           end;
         end method;
         define macro "with-" ## ?name
           { "with-" ## ?name (\?:expression) \?:body end }
             => { "do-with-" ## ?name(method () \?body end, \?expression) }
         end; }
end macro;

// list of keys to trigger output on.
define variable *debug-out* = #();

// Conditional debugging output.  E.g. debug-out(#"linker", "Foo")
// will output "Foo" only if *debug-out* contains #"linker".
define macro debug-out
  { debug-out (?key:expression, ?args:*) }
    => { if (member?(?key, *debug-out*))
	   without-dependency-tracking
	     format-out(?args);
	   end
         end }
end macro debug-out;

// Macro for sealing make/initialize
define macro sealed-constructor-definer
  { define sealed-constructor ?class:expression }
    => { define sealed domain make(singleton(?class));
         define sealed domain initialize(?class); }
end macro;
