module:     access-path-implementation
synopsis:   Implementation of functions for memory access
author:     Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



///// Error constants.


define constant $access-ok = 0;
define constant $byte-count-mismatch = 1;
define constant $access-violation-error = 2;

///// Page protection constants

define constant $permitted = 1;
define constant $unpermitted = 0;


///// <REMOTE-REGISTER>


define sealed abstract class <remote-register> (<object>)

       // The register is a handle on a descriptor for that register,
       // held within the nub.

       slot nub-descriptor :: <integer>,
            required-init-keyword: descriptor:;

       slot C-name :: <string>,
            required-init-keyword: name:;

//       slot register-type :: <remote-type>,
//            required-init-keyword: type:;

       slot register-category :: <symbol>,
            required-init-keyword: category:;

end class;


///// <UNASSIGNED-REMOTE-REGISTER>


define class <unassigned-remote-register> (<remote-register>)
end class;


///// <ACTIVE-REMOTE-REGISTER>


define class <active-remote-register> (<remote-register>)

       slot thread :: <remote-thread>,
            required-init-keyword: thread:;

end class;


///// <REMOTE-ACCESS-VIOLATION-ERROR>


define class <remote-access-violation-error> (<error>)
end class;


///// <REMOTE-TYPE>


define abstract class <remote-type> (<object>)
end class;

define class <hacked-remote-type> (<remote-type>)
end class;

define method make (class == <remote-type>, #rest keys, #key, #all-keys)
       apply (make, <hacked-remote-type>, keys)
end method;

define constant $hacked-remote-type = make (<hacked-remote-type>);


///// <REMOTE-LOCATION>


define constant <remote-location> = type-union(<remote-value>,
                                               <remote-register>);

///// EXPORTED GENERIC FUNCTIONS


define generic do-registers (f :: <function>, ap :: <access-path>,
                             #key type = #f) => ();

define generic active-register (ap :: <access-path>, thread :: <remote-thread>,
                                register :: <unassigned-remote-register>)
                                => (_ :: <active-remote-register>);

define generic register-name (r :: <remote-register>) => (_ :: <symbol>);

define generic read-value (ap :: <access-path>, address :: <remote-location>)
                            => (_ :: <remote-value>);

define generic write-value (ap :: <access-path>, address :: <remote-location>,
                            value :: <remote-value>) => (_ :: <remote-value>);

///// DO-REGISTERS


define method do-registers (f :: <function>, ap :: <access-path>,
                            #key type = #f) => ()
end method;


///// REGISTER-NAME


define method register-name (r :: <remote-register>) => (_ :: <symbol>)
  as (<symbol>, r.C-name);
end method;


///// ACTIVE-REGISTER

define method active-register 
    (ap :: <access-path>, thread :: <remote-thread>,
     register :: <unassigned-remote-register>)
       => (_ :: <active-remote-register>)

  // Just create an <active-remote-register> instance whose attributes
  // are identical to the <unassigned-remote-register>, and include the
  // thread.

  make (<active-remote-register>,
        name: register.C-name,
        category: register.register-category,
        descriptor: register.nub-descriptor,
        thread: thread);
end method;


///// READ-VALUE

define method read-value 
    (ap :: <access-path>, address :: <active-remote-register>)
      => (_ :: <remote-value>)
  0;
end method;

define method read-value (ap :: <access-path>, address :: <remote-value>)
    => (_ :: <remote-value>)
  ap.connection.process.memory.contents[address];
end method;


///// READ-BYTE-STRING

define method read-byte-string 
    (ap :: <access-path>, address :: <remote-value>, length :: <integer>) 
       => (_ :: <byte-string>)
  let s = make (<byte-string>, size: length);
  for (i from 0 below length)
    let cval = read-value (ap, indexed-remote-value (address, i));
    s[i] := tagged-remote-value-as-character (cval);
  end for;
  s;
end method;

///// WRITE-VALUE

define method write-value 
    (ap :: <access-path>, address :: <active-remote-register>,
     value :: <remote-value>) => (_ :: <remote-value>)
  value;
end method;

define method write-value 
    (ap :: <access-path>, address :: <remote-value>,
     value :: <remote-value>) => (_ :: <remote-value>)
  ap.connection.process.memory.contents[address] := value;
  value;
end method;


///// CALCULATE-STACK-ADDRESS

define method calculate-stack-address
   (ap :: <access-path>, thread :: <remote-thread>, offset :: <integer>)
      => (addr :: <remote-value>)
  thread.nub-descriptor.esp + offset;
end method;


///// REMOTE-VIRTUAL-PAGE-SIZE
//    Pants method

define method remote-virtual-page-size (ap :: <access-path>)
    => (i :: <integer>)
  512
end method;


///// PAGE-RELATIVE-ADDRESS
//    Pants method

define method page-relative-address 
     (ap :: <access-path>, addr :: <remote-value>)
        => (page-num :: <integer>, offset :: <integer>)
  truncate/(addr, 512);
end method;


///// REMOTE-VALUE-BYTE-SIZE
//    Pants method.

define method remote-value-byte-size (ap :: <access-path>)
     => (i :: <integer>)
  1
end method;


///// PERFORM-COFF-RELOCATION
//    Semi-kosher

define method perform-coff-relocation
    (ap :: <access-path>, ra :: <remote-value>, da :: <remote-value>,
     #key relative? = #f)
  let x = ap.connection.process.memory.contents[ra];
  if (relative?)
    ap.connection.process.memory.contents[ra] := x + da;
  else
    ap.connection.process.memory.contents[ra] := (x + da) - ra;
  end if;
end method;

