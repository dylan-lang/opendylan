module:    harp-instructions
Synopsis:  Macro support for defining instruction sets
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



/// define-instruction-set does everything necessary to define an instruction
/// set class, create a default instance of it, initialize all the slots from
/// the parent default, and obey the inheritance protocol so that children 
/// may inherit instructions correctly.


define macro instruction-set-definer
  { define instruction-set ?class:name (?parent:name, ?backend:name)
      create ?default:name, inheriting: ?pdefault:name;
      ?ops:*
    end }
    => { define-instruction-set-aux ?class 
           (?parent, ?backend, ?default, ?pdefault) 
           (?ops)
           ?ops
         end }

end macro;

define macro define-instruction-set-aux
  { define-instruction-set-aux ?class:name 
      (?parent:name, ?backend:name, ?default:name, ?pdefault:name)
      (?ops:*)
      ?names:*
    end }
    => { define instruction-class ?class (?parent) ?names end;
         define-instruction-defaulter(?class, ?names);
         define method parent-instruction-set 
             (set :: ?class) => (r :: ?parent)
           ?pdefault
         end;
         define-instruction-make-method ?class (?names)
	   ?ops
	 end;
	define-instruction-outputters ?backend ?ops end;
	define variable ?default :: ?class = make-instruction-set(?class);
       }

names:
  { } => { }
  { ?group:* ; ... } => { ?group, ... }

group:
  { op ?group-data:* } => { ?group-data }
  { ?type:name op ?group-data:* } => { ?group-data }

group-data:
  { } => { }
  { ?op:name, ... } => { ?op, ... }
  { #rest ?plist:* } => { }

end macro;


define macro instruction-class-definer
  { define instruction-class ?class:name (?parent:name)
      ?slots:*
    end }
    => { define open primary class ?class (?parent)
           ?slots
         end }
slots:
  { } => { }
  { ?slot:name, ... } => { slot "harp-" ## ?slot :: <op> ; ... }
end macro;


define macro define-instruction-defaulter
  { define-instruction-defaulter(?class:name, ?set-names:*) }
    => { define method initialize-instruction-set-defaults
             (child :: ?class, default :: ?class) => ()
           ?=next-method();
           ?set-names
         end method }
set-names:
  { } => { }
  { ?harp-name:name, ... } 
    => { child . ?harp-name := copy-op(default . ?harp-name);
         ... }
harp-name:
  { ?:name } => { "harp-" ## ?name }
end macro;


define macro define-instruction-make-method
  { define-instruction-make-method ?class:name (?make-ops:*)
      ?instructions:*
    end }
    => { define method make-instruction-set (class == ?class)
             => (new :: ?class)
           let new = make(?class);
           ?make-ops;
           initialize-instruction-options new ?instructions end;
           new
         end }

make-ops:
  { } => { }
  { ?:name, ... }
    => { new . "harp-" ## ?name := make-op(?"name");  ... }

end macro;


define macro initialize-instruction-options 
  { initialize-instruction-options ?new:name end }
     => {  }

  { initialize-instruction-options ?new:name 
      ?init-options:* ;
      ?next-options:*
    end }
     => { initialize-instructions-of-one-type ?new ?init-options end;
	  initialize-instruction-options ?new ?next-options end }

init-options:
  { } => { }
  { op ?group-data:* } 
    => { ?group-data }
  { ?type:name op ?group-data:*  } 
    => { ?group-data, spread: "spread-" ## ?type }

group-data:
  { } => { ; op-options }
  { ?arg1:name, ... } => { , ?arg1 ... }
  { #rest ?plist:* } => { ; op-options, ?plist }

end macro;


define macro initialize-instructions-of-one-type
  { initialize-instructions-of-one-type ?new:name,
      ?ops:* ; op-options, ?options:* 
    end }
    => { begin
           let new = ?new;
           local method do-op (op :: <op>) => ()
                   modify-op(op, ?options)
                 end method;
           ?ops
         end }
ops:
  { } => { }
  { ?op:name, ... } => { do-op(op-element(new, ?op)); ... }
end macro;


define macro define-instruction-outputters
  { define-instruction-outputters ?backend:name end }
    => { }

  { define-instruction-outputters ?backend:name 
      ?ops:*; ?next-options:*
    end }
    => { define-instruction-outputters-for-type ?backend ?ops end;
         define-instruction-outputters ?backend ?next-options end}
end macro;


define macro define-instruction-outputters-for-type
  { define-instruction-outputters-for-type ?backend:name op ?stuff:* end }
    => { }

  { define-instruction-outputters-for-type ?backend:name
      ?type:name op ?op-names:* 
    end }
    => { "define-" ## ?type ?backend (?op-names) end }

op-names:
  { } => { }
  { ?op:name, ... } => { ?op, ... }
  { #rest ?plist:* } => { }
end macro;
