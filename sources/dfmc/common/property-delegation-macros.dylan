module: dfmc-common
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define macro rare-slot-definer
  { define rare-slot ?:name :: ?type:expression = ?default:expression 
      on ?class:name using ?slot:name }
    => { define method ?name (x :: ?class) => (res :: ?type)
           get-property(?slot(x), ?#"name", default: ?default)
         end method;
         define method ?name ## "-setter" (value :: ?type, x :: ?class)
	   if (value == ?default)  
	     remove-property!(?slot(x), ?#"name")
	   else 
	     put-property!(?slot(x), ?#"name", value)
	   end if
         end method;
         define method "remove-" ## ?name ## "!" (x :: ?class)
           remove-property!(?slot(x), ?#"name")
         end method }
end macro;

define macro rare-slots-definer
  { define rare-slots (?class:name, ?slot:name) end}
    => { }
  { define rare-slots (?class:name, ?slot:name) 
      slot ?:variable ?default; ...
    end }
    => { define rare-slot ?variable = ?default on ?class using ?slot;
         define rare-slots (?class, ?slot) ... end}
default:
  { }                => { #f }
  { = ?:expression } => { ?expression }
end macro;

define macro property-delegation-definer
  { define property-delegation (?type:expression, ?delegator:expression) 
      ?properties:* 
    end }
    => { define property-delegation-getters (?type, ?delegator) 
           ?properties 
         end;
         define property-delegation-setters (?type, ?delegator)
           ?properties 
         end }
end macro;

define macro property-delegation-getters-definer
  { define property-delegation-getters
        (?type:expression, ?delegator:expression)
      ?property:name :: ?property-type:expression = ?default-value:expression,
      ?more:* 
    end }
    => { define method ?property (x :: ?type) => (obj :: ?property-type)
	   let p = ?delegator(x, default: #f);
	   if (p) ?property(p) else ?default-value end
	 end method;
         define property-delegation-getters (?type, ?delegator) ?more 
         end }
  { define property-delegation-getters
        (?type:expression, ?delegator:expression)
      ?property:name :: ?property-type:expression, ?more:* 
    end }
    => { define method ?property (x :: ?type) => (obj :: ?property-type)
           ?property(?delegator(x))
         end method;
         define property-delegation-getters (?type, ?delegator) ?more 
         end }
  { define property-delegation-getters 
        (?type:expression, ?delegator:expression) end }
    => { }
end macro;

define macro property-delegation-setters-definer
  { define property-delegation-setters
        (?type:expression, ?delegator:expression)
      ?property:name :: ?property-type:expression = ?default-value:expression,
      ?more:* 
    end }
    => { define method ?property ## "-setter"
	     (new-value :: ?property-type, x :: ?type)
	  => (obj :: ?property-type)
	   let p = if (new-value == ?default-value)
		     ?delegator(x, default: #f)
		   else
		     ?delegator(x)
		   end;
	   if (p)
	     ?property(p) := new-value
	   end;
	 end method;
         define property-delegation-setters (?type, ?delegator) ?more
         end }
  { define property-delegation-setters 
        (?type:expression, ?delegator:expression)
      ?property:name :: ?property-type:expression, ?more:* 
    end }
    => { define method ?property ## "-setter" 
             (new-value :: ?property-type, x :: ?type) 
          => (obj :: ?property-type)
           ?property(?delegator(x)) := new-value
         end method;
         define property-delegation-setters (?type, ?delegator) ?more 
         end }
  { define property-delegation-setters
        (?type:expression, ?delegator:expression)
    end }
    => { }
end macro;
