module:      environment-protocols
synopsis:    The <register-object> class, and its associated protocols.
author:      Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// <REGISTER-CATEGORY>
//    Describes an abstract categorization for the runtime register set.

define constant <register-category> =
  one-of(#"general-purpose",
         #"special-purpose",
         #"floating-point");


///// <REGISTER-OBJECT>
//    A subclass of <APPLICATION-OBJECT>.
//    Represents a hardware-level register.

define class <register-object> (<application-object>)
end class;


///// DO-APPLICATION-REGISTERS
//    Synopsis: Iterates over all runtime registers.
//    Inputs:
//      f          - A function of signature (<REGISTER-OBJECT>) => ()
//      server     - The dispatching context.
//      category   - If supplied, must be a <REGISTER-CATEGORY>. The
//                   iteration will be restricted to registers of this
//                   category.
//                   If not supplied, the iteration will include all
//                   available platform registers.
//    Results: None.

define open generic do-application-registers
    (f :: <function>, server :: <server>, #key category) => ();

define method do-application-registers
    (f :: <function>, project :: <project-object>,
     #key category = #f)
 => ()
  let application = project.project-application;
  if (application)
    do-application-registers(f, application, category: category)
  end if
end method;


///// REGISTER-CONTENTS
//    Synopsis: Within the appropriate context, retrieves the value stored
//              in a register, and represents it as an application object.
//    Inputs:
//      server      - The dispatching context.
//      reg         - The register to examine.
//      thread      - The thread context in which to read the register.
//                    (It is assumed that registers are a thread-local
//                    resource on all platforms).
//      stack-frame-context
//                    On different platforms, varying numbers of registers
//                    are saved per stack frame. If a <stack-frame-object>
//                    is supplied via this keyword, and the corresponding
//                    register is stack-frame local, then the appropriate
//                    value will be retrieved. Where this is not possible,
//                    the basic thread-local value will be used regardless of
//                    the frame context.
//    Results:
//      obj         - The register's context, interpreted as an
//                    <application-object>.

define open generic register-contents
    (server :: <server>, reg :: <register-object>, thread :: <thread-object>,
     #key stack-frame-context)
 => (obj :: false-or(<application-object>));

define method register-contents
    (project :: <project-object>, reg :: <register-object>,
     thread :: <thread-object>,
     #key stack-frame-context = #f)
  => (obj :: false-or(<application-object>))
  let server = choose-server(project, reg);
  server & register-contents(server, reg, thread,
                             stack-frame-context: stack-frame-context)
end method;


///// REGISTER-CONTENTS-ADDRESS
//    Synopsis: Within the appropriate context, retrieves the value stored
//              in a register, and represents it as an address
//    Inputs:
//      server      - The dispatching context.
//      reg         - The register to examine.
//      thread      - The thread context in which to read the register.
//                    (It is assumed that registers are a thread-local
//                    resource on all platforms).
//      stack-frame-context
//                    On different platforms, varying numbers of registers
//                    are saved per stack frame. If a <stack-frame-object>
//                    is supplied via this keyword, and the corresponding
//                    register is stack-frame local, then the appropriate
//                    value will be retrieved. Where this is not possible,
//                    the basic thread-local value will be used regardless of
//                    the frame context.
//    Results:
//      obj         - The register's context, interpreted as an
//                    address <application-object>.

define open generic register-contents-address
    (server :: <server>, reg :: <register-object>, thread :: <thread-object>,
     #key stack-frame-context)
 => (obj :: false-or(<address-object>));

define method register-contents-address
    (project :: <project-object>, reg :: <register-object>,
     thread :: <thread-object>,
     #key stack-frame-context = #f)
  => (obj :: false-or(<address-object>))
  let server = choose-server(project, reg);
  server & register-contents-address(server, reg, thread,
                                     stack-frame-context: stack-frame-context)
end method;


///// LOOKUP-REGISTER-BY-NAME
//    Synopsis: Tries to find a register object corresponding to a given
//              name.
//      server      - The dispatching context.
//      name        - The candidate register name.
//    Outputs:
//      reg         - If successful, returns a <register-object>.
//                    Returns #f if no match is found, or if the application
//                    is not tethered.

define open generic lookup-register-by-name
    (server :: <server>, name :: <string>)
 => (reg :: false-or(<register-object>));

define method lookup-register-by-name
    (project :: <project-object>, name :: <string>)
 => (reg :: false-or(<register-object>))
  let server = project.project-application;
  server & lookup-register-by-name(server, name)
end method;


/// Some convenience functions built on these protocols

define function application-registers
    (server :: <server>, #key category = #f) => (classes :: <sequence>)
  let results = make(<stretchy-vector>);
  do-application-registers
    (method (register :: <register-object>)
       add!(results, register)
     end,
     server,
     category: category);
  results
end function application-registers;
