module:     access-path-implementation
synopsis:   Implementation of functions for memory access
author:     Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// Error constants.

define constant $access-ok = 0;


///// <REMOTE-REGISTER>

define sealed abstract class <remote-register> (<object>)

  // The register is a handle on a descriptor for that register,
  // held within the nub.

  constant slot nub-descriptor :: <integer>,
    required-init-keyword: descriptor:;

  constant slot nub-enumeration-code :: <integer>,
    required-init-keyword: code:;

  constant slot C-name :: <byte-string>,
    required-init-keyword: name:;

  constant slot register-category :: <symbol>,
    required-init-keyword: category:;

end class;


///// <UNASSIGNED-REMOTE-REGISTER>

define class <unassigned-remote-register> (<remote-register>)
end class;


///// <ACTIVE-REMOTE-REGISTER>

define class <active-remote-register> (<remote-register>)

  constant slot register-thread :: <remote-thread>,
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
    => (typ)
  apply (make, <hacked-remote-type>, keys)
end method;


///// <REMOTE-LOCATION>

define constant <remote-location> =
    type-union(<remote-value>, <remote-register>);


///// EXPORTED GENERIC FUNCTIONS

define generic do-registers 
    (f :: <function>, ap :: <access-path>, #key type = #f) 
      => ();

define generic active-register 
    (ap :: <access-path>, thread :: <remote-thread>,
     register :: <unassigned-remote-register>)
       => (reg :: <active-remote-register>);

define generic register-name (r :: <remote-register>) => (val :: <string>);

define generic read-value 
    (ap :: <access-path>, address :: <remote-location>,
     #key stack-frame)
       => (val :: <remote-value>);

define generic write-value 
    (ap :: <access-path>, address :: <remote-location>,
     value :: <remote-value>) 
       => (val :: <remote-value>);

define generic read-8b 
    (ap :: <access-path>, address :: <remote-location>)
       => (val :: <integer>);

define generic write-8b 
    (ap :: <access-path>, address :: <remote-location>,
     value :: <integer>) 
       => (val :: <integer>);

define generic read-16b 
    (ap :: <access-path>, address :: <remote-location>)
       => (val :: <integer>);

define generic write-16b 
    (ap :: <access-path>, address :: <remote-location>,
     value :: <integer>) 
       => (val :: <integer>);

define generic read-32b 
    (ap :: <access-path>, address :: <remote-location>)
       => (val :: <integer>);

define generic write-32b 
    (ap :: <access-path>, address :: <remote-location>,
     value :: <integer>) 
       => (val :: <integer>);

define generic read-64b 
    (ap :: <access-path>, address :: <remote-location>)
       => (val :: <integer>);

define generic write-64b 
    (ap :: <access-path>, address :: <remote-location>,
     value :: <integer>) 
       => (val :: <integer>);

define generic read-single-float 
    (ap :: <access-path>, address :: <remote-location>)
       => (val :: <single-float>);

define generic write-single-float 
    (ap :: <access-path>, address :: <remote-location>,
     value :: <single-float>)
       => (val :: <single-float>);

define generic read-double-float 
    (ap :: <access-path>, address :: <remote-location>)
       => (val :: <double-float>);

define generic write-double-float 
    (ap :: <access-path>, address :: <remote-location>,
     value :: <double-float>)
       => (val :: <double-float>);

define generic read-byte-string 
    (ap :: <access-path>, address :: <remote-value>,
     length :: <integer>)
       => (val :: <byte-string>);

define generic write-byte-string 
    (ap :: <access-path>, address :: <remote-value>,
     value :: <byte-string>, #key ending-index)
       => (val :: <byte-string>);


///// DO-REGISTERS

define method do-registers 
    (f :: <function>, ap :: <access-path>,
     #key type = #f) => ()

  // Acquire the vector of register descriptors if we need it.

  if (ap.register-set = #[])
    read-access-path-register-set (ap)
  end if;

  // Now iterate over all registers with the function.

  for (r in ap.register-set)
    if (r)
      if (type)
        if (type == r.register-category)
          f(r)
        end if
      else
        f(r)
      end if
    end if
  end for;
end method;


///// FIND-REGISTER
//    Given an integer nub descriptor for a register, returns an
//    <unassigned-remote-register> corresponding to that shit.

define method find-register
    (ap :: <access-path>, nub-register :: <integer>)
       => (descriptor :: <unassigned-remote-register>)

  let found = #f;
  let i = 0;

  // If we haven't read the register model yet, do so now.
  if (ap.register-set = #[])
    read-access-path-register-set(ap)
  end if;

  // Here we make an assumption: that there will _certainly in all
  // cases_ be a remote register with the correct descriptor. If
  // there isn't, that's an access-path/debugger-nub internal error,
  // and everything will go wrong.

  while ((~found) & (i < size(ap.register-set)))
    if (ap.register-set[i])
      if (ap.register-set[i].nub-descriptor == nub-register)
        found := ap.register-set[i];
      else
        i := i + 1;
      end if
    end if
  end while;
  found;
end method;

    
define method read-access-path-register-set 
    (ap :: <access-path>) => (registers :: <vector>)
  ap.register-set := register-vector-on-connection (ap.connection);
end method;

define open generic register-vector-on-connection
    (conn :: <access-connection>) => (vec :: <vector>);


///// REGISTER-TO-ENUMERATION-CODE

define method register-to-enumeration-code
    (ap :: <access-path>, register :: <unassigned-remote-register>)
       => (code :: <integer>)
  register.nub-enumeration-code
end method;


///// ENUMERATION-CODE-TO-REGISTER

define method enumeration-code-to-register
    (ap :: <access-path>, code :: <integer>)
       => (register :: <unassigned-remote-register>)
  unless (ap.register-tables-built?)
    build-register-tables(ap)
  end unless;
  let reg = element(ap.register-code-to-descriptor, code, default: #f);
  if (reg)
    reg
  else
    error("No such register for the supplied enumeration code!");
    ap.register-set[0];
  end if
end method;

define method enumeration-code-to-register
    (ap :: <access-path>, code :: <byte-string>)
       => (register :: <unassigned-remote-register>)
  unless (ap.register-tables-built?)
    build-register-tables(ap)
  end unless;
  let reg = element(ap.register-name-to-descriptor, as-lowercase(code), 
                    default: #f);
  if (reg)
    reg
  else
    error("No such register for the supplied name");
    ap.register-set[0];
  end if
end method;


///// BUILD-REGISTER-TABLES
//    The <access-path> class keeps two fast-access tables for registers,
//    one that can map a register name (string) to a <remote-register>
//    descriptor, and one that can map a register enumeration code to a
//    descriptor. 
//    This function ensures that both tables have been created.

define method build-register-tables (ap :: <access-path>) => ()
  unless (ap.register-tables-built?)

    // If we haven't read the register model yet, do so now.
    if (ap.register-set = #[])
      read-access-path-register-set(ap)
    end if;
 
    // Create the entry in the table for each known register.
    for (r in ap.register-set)
      if (r)
        let name = as-lowercase(r.C-name);
        let code = r.nub-enumeration-code;
        ap.register-name-to-descriptor[name] := r;
        ap.register-code-to-descriptor[code] := r;
      end if;
    end for;

    // And make sure this effort is never duplicated.
    ap.register-tables-built? := #t
  end unless
end method;

///// REGISTER-NAME

define method register-name (r :: <remote-register>) 
    => (sym :: <string>)
  r.C-name;
end method;


///// ACTIVE-REGISTER

define method active-register 
    (ap :: <access-path>, thread :: <remote-thread>,
     register :: <unassigned-remote-register>)
       => (reg :: <active-remote-register>)
  // Just create an <active-remote-register> instance whose attributes
  // are identical to the <unassigned-remote-register>, and include the
  // thread.
  make (<active-remote-register>,
        name: register.C-name,
        category: register.register-category,
        descriptor: register.nub-descriptor,
        code: register.nub-enumeration-code,
        thread: thread);
end method;


///// READ-VALUE

define open generic read-value-from-register 
    (conn :: <access-connection>, register :: <active-remote-register>,
     #key)
      => (val :: <remote-value>);

define open generic read-value-from-memory 
    (conn :: <access-connection>, location :: <remote-value>)
      => (val :: <remote-value>);

define method read-value 
    (ap :: <access-path>, address :: <active-remote-register>,
     #key stack-frame = #f)
      => (val :: <remote-value>)
  if (stack-frame)
    read-value-from-register 
       (ap.connection, address, 
        frame-index: stack-frame.nub-vector-frame-index)
  else
    read-value-from-register
       (ap.connection, address)
  end if
end method;

define method read-value 
    (ap :: <access-path>, address :: <remote-value>, #key stack-frame = #f)
      => (val :: <remote-value>)
  read-value-from-memory (ap.connection, address)
end method;


///// WRITE-VALUE

define open generic write-value-to-register 
    (conn :: <access-connection>,
     register :: <active-remote-register>,
     value :: <remote-value>) => ();

define open generic write-value-to-memory 
    (conn :: <access-connection>, address :: <remote-value>,
     value :: <remote-value>) => ();

define method write-value 
    (ap :: <access-path>, address :: <active-remote-register>,
     value :: <remote-value>) => (val :: <remote-value>)
  write-value-to-register (ap.connection, address, value);
  value;
end method;

define method write-value 
    (ap :: <access-path>, address :: <remote-value>,
     value :: <remote-value>) => (val :: <remote-value>)
  write-value-to-memory (ap.connection, address, value);
  value;
end method;


///// READ-SINGLE-FLOAT

define open generic read-single-float-from-register 
    (conn :: <access-connection>,
     register :: <active-remote-register>)
       => (val :: <single-float>);

define open generic read-single-float-from-memory 
    (conn :: <access-connection>, location :: <remote-value>)
       => (val :: <single-float>);

define method read-single-float 
    (ap :: <access-path>, address :: <active-remote-register>)
       => (val :: <single-float>)
  read-single-float-from-register (ap.connection, address)
end method;

define method read-single-float 
    (ap :: <access-path>, address :: <remote-value>)
       => (val :: <single-float>)
  read-single-float-from-memory (ap.connection, address)
end method;


///// WRITE-SINGLE-FLOAT

define open generic write-single-float-to-register 
    (conn :: <access-connection>, 
     register :: <active-remote-register>,
     value :: <single-float>) 
 => ();

define open generic write-single-float-to-memory 
    (conn :: <access-connection>, address :: <remote-value>,
     value :: <single-float>) 
 => ();

define method write-single-float 
    (ap :: <access-path>, address :: <active-remote-register>,
     value :: <single-float>) 
       => (val :: <single-float>)
  write-single-float-to-register (ap.connection, address, value);
  value;
end method;

define method write-single-float 
    (ap :: <access-path>, address :: <remote-value>, value :: <single-float>) 
      => (val :: <single-float>)
  write-single-float-to-memory (ap.connection, address, value);
  value;
end method;


///// READ-DOUBLE-FLOAT

define open generic read-double-float-from-register 
    (conn :: <access-connection>,
     register :: <active-remote-register>)
       => (val :: <double-float>);

define open generic read-double-float-from-memory 
    (conn :: <access-connection>, location :: <remote-value>)
       => (val :: <double-float>);

define method read-double-float 
    (ap :: <access-path>, address :: <active-remote-register>)
       => (val :: <double-float>)
  read-double-float-from-register (ap.connection, address)
end method;

define method read-double-float 
    (ap :: <access-path>, address :: <remote-value>)
       => (val :: <double-float>)
  read-double-float-from-memory (ap.connection, address)
end method;


///// WRITE-DOUBLE-FLOAT

define open generic write-double-float-to-register 
    (conn :: <access-connection>, 
     register :: <active-remote-register>,
     value :: <double-float>) 
 => ();

define open generic write-double-float-to-memory 
    (conn :: <access-connection>, address :: <remote-value>,
     value :: <double-float>) 
       => ();

define method write-double-float 
    (ap :: <access-path>, address :: <active-remote-register>,
     value :: <double-float>) 
       => (val :: <double-float>)
  write-double-float-to-register (ap.connection, address, value);
  value;
end method;

define method write-double-float 
    (ap :: <access-path>, address :: <remote-value>, value :: <double-float>) 
      => (val :: <double-float>)
  write-double-float-to-memory (ap.connection, address, value);
  value;
end method;


///// READ-BYTE-STRING

define open generic read-byte-string-from-memory 
   (conn :: <access-connection>, address :: <remote-value>,
    length :: <integer>)
 => (val :: <byte-string>);

define method read-byte-string 
    (ap :: <access-path>, address :: <remote-value>, length :: <integer>)
      => (val :: <byte-string>)
  read-byte-string-from-memory (ap.connection, address, length)
end method;


///// WRITE-BYTE-STRING

define open generic write-byte-string-to-memory
    (conn :: <access-connection>, address :: <remote-value>,
     string-source :: <byte-string>, ending-index :: <integer>) => ();

define method write-byte-string 
    (ap :: <access-path>, address :: <remote-value>, value :: <byte-string>,
     #key ending-index = #f)
       => (val :: <byte-string>)
  unless (ending-index)
    ending-index := size(value) - 1
  end unless;
  write-byte-string-to-memory 
     (ap.connection, address, value, ending-index);
  value;
end method;


///// CALCULATE-STACK-ADDRESS
//    Returns the address of a position on the stack of the application's
//    thread. Offset 0 is the top of the stack. Offset 1 is the position
//    1 remote-value below the top of the stack, etc...

define method calculate-stack-address
    (ap :: <access-path>, thread :: <remote-thread>, offset :: <integer>)
       => (addr :: <remote-value>)
  calculate-stack-address-on-connection(ap.connection, thread, offset);
end method;

define open generic calculate-stack-address-on-connection
    (conn :: <access-connection>, thread :: <remote-thread>, 
     offset :: <integer>)
       => (addr :: <remote-value>);


///// REMOTE-VIRTUAL-PAGE-SIZE
//    Returns the size of a memory page on the remote machine. This is
//    given as an integer, measured in remote-value-sized units.

define method remote-virtual-page-size
    (ap :: <access-path>) => (page-size :: <integer>)
  virtual-page-size-on-connection(ap.connection)
end method;

define open generic virtual-page-size-on-connection
    (conn :: <access-connection>) => (page-size :: <integer>);


///// REMOTE-VALUE-BYTE-SIZE
//    Returns the size, in bytes, of a <remote-value> in the runtime.

define method remote-value-byte-size
   (ap :: <access-path>) => (value-size :: <integer>)
  if (ap.remote-value-size-known?)
    ap.cached-remote-value-size;
  else
    ap.cached-remote-value-size :=
      remote-value-byte-size-on-connection(ap.connection);
    ap.remote-value-size-known? := #t;
    ap.cached-remote-value-size;
  end if;
end method;

define open generic remote-value-byte-size-on-connection
    (conn :: <access-connection>) => (value-size :: <integer>);


///// PAGE-READ-PERMISSION?
//    Queries whether the given address lies within a read-protected page.

define method page-read-permission?
    (ap :: <access-path>, address :: <remote-value>) => (ans :: <boolean>)
  page-read-permission-on-connection?(ap.connection, address);
end method;

define open generic page-read-permission-on-connection?
    (conn :: <access-connection>, address :: <remote-value>)
 => (answer :: <boolean>);


///// PAGE-WRITE-PERMISSION?
//    Queries whether the given address lies within a write-protected page.

define method page-write-permission?
    (ap :: <access-path>, address :: <remote-value>) => (ans :: <boolean>)
  page-write-permission-on-connection?(ap.connection, address);
end method;

define open generic page-write-permission-on-connection?
    (conn :: <access-connection>, address :: <remote-value>)
 => (answer :: <boolean>);


///// PAGE-EXECUTE-PERMISSION?
//    Queries whether the given address lies within an execute-protected page.
//    (Dummy implementation)

define method page-execute-permission?
    (ap :: <access-path>, address :: <remote-value>) => (ans :: <boolean>)
  #t
end method;


///// REMOTE-ADDRESS-PAGE-NUMBER
//    Turns an address into an integer-enumerated memory page ID.

define method remote-address-page-number
    (ap :: <access-path>, addr :: <remote-value>) => (id :: <integer>)
  let (id, offset) = page-relative-address(ap, addr);
  id
end method;


///// PAGE-RELATIVE-ADDRESS
//    Turns an address into an integer-enumerated memory page ID, and an
//    offset into the page.

define method page-relative-address
    (ap :: <access-path>, addr :: <remote-value>) 
       => (id :: <integer>, offset :: <integer>)
  page-relative-address-on-connection(ap.connection, addr);
end method;

define open generic page-relative-address-on-connection
    (conn :: <access-connection>, addr :: <remote-value>)
       => (id :: <integer>, offset :: <integer>);


///// PERFORM-COFF-RELOCATION
//    Alters the contents of an address 'ra' according to COFF-file relocation
//    semantics. This is used by the interactive downloader.

define method perform-coff-relocation
    (ap :: <access-path>, ra :: <remote-value>, da :: <remote-value>,
     #key relative? = #f)
       => (worked? :: <boolean>)
  perform-coff-relocation-on-connection
      (ap.connection, ra, da, relative?: relative?);
end method;

define open generic perform-coff-relocation-on-connection
    (conn :: <access-connection>, 
     ra :: <remote-value>, da :: <remote-value>,
     #key)
 => (worked? :: <boolean>);

