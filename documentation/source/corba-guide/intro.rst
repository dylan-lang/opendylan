**********************
About Open Dylan CORBA
**********************

About CORBA
=============

Object Management Group, Inc. describe their CORBA architecture as follows:

    The Common Object Request Broker Architecture (CORBA), is the
    Object Management Group’s answer to the need for interoperability
    among the rapidly proliferating number of hardware and software
    products available today. Simply stated, CORBA allows applications
    to communicate with one another no matter where they are located
    or who has designed them. CORBA 1.1 was introduced in 1991 by
    Object Management Group (OMG) and defined the Interface Definition
    Language (IDL) and the Application Programming Interfaces (API)
    that enable client/server object interaction within a specific
    implementation of an Object Request Broker (ORB). CORBA 2.0,
    adopted in December of 1994, defines true interoperability by
    specifying how ORBs from different vendors can interoperate.

    The ORB is the middleware that establishes the client-server
    relationships between objects. Using an ORB, a client can
    transparently invoke a method on a server object, which can be on
    the same machine or across a network. The ORB intercepts the call
    and is responsible for finding an object that can implement the
    request, pass it the parameters, invoke its method, and return the
    results. The client does not have to be aware of where the object
    is located, its programming language, its operating system, or any
    other system aspects that are not part of an object's
    interface. In so doing, the ORB provides interoperability between
    applications on different machines in heterogeneous distributed
    environments and seamlessly interconnects multiple object systems.

    In fielding typical client/server applications, developers use
    their own design or a recognized standard to define the protocol
    to be used between the devices. Protocol definition depends on the
    implementation language, network transport and a dozen other
    factors. ORBs simplify this process. With an ORB, the protocol is
    defined through the application interfaces via a single
    implementation language-independent specification, the IDL. And
    ORBs provide flexibility. They let programmers choose the most
    appropriate operating system, execution environment and even
    programming language to use for each component of a system under
    construction. More importantly, they allow the integration of
    existing components. In an ORB-based solution, developers simply
    model the legacy component using the same IDL they use for
    creating new objects, then write “wrapper” code that translates
    between the standardized bus and the legacy interfaces.

    CORBA is a signal step on the road to object-oriented
    standardization and interoperability. With CORBA, users gain
    access to information transparently, without them having to know
    what software or hardware platform it resides on or where it is
    located on an enterprise’s network. The communications heart of
    object-oriented systems, CORBA brings true interoperability to
    today's computing environment.


(At the time of writing, the text above was available at:
https://www.omg.org/corba/
It has been reproduced with permission.)

About the Open Dylan ORB
========================

The Open Dylan ORB is a CORBA-compliant ORB written in Dylan, with a
native implementation of the Internet Inter-ORB Protocol (IIOP).

The Open Dylan ORB lets you build and run distributed
applications in Dylan, straight out of the box. When combined with the
Dylan interface to ODBC, you can build 3-tier client-server
applications completely in Dylan.

However, the fundamental purpose of the CORBA architecture is
interoperability. The ORB's IIOP implementation provides immediate
interoperation with any other ORB you may be using. For example, given
a Java ORB you could write GUI clients in Swing or AWT that
communicate to servers written in Dylan. Conversely, given a C++ ORB
you can build DUIM (Dylan User Interface Manager) clients that talk to
C++ servers.

Apart from proving that Dylan could tackle another complex domain, the
advantages of building an ORB in Dylan were:

ORB-vendor independence
   The Open Dylan ORB can be married to any existing ORB
   infrastructure, or introduced first without affecting later ORB
   procurement decisions.

"Batteries included"
   No need to purchase a separate ORB to get a full system. The
   Open Dylan ORB provides "instant CORBA" to get distributed
   Dylan applications up and running without additional procurement or
   installation.

Lower impedance mismatch
   There is no need to trampoline from Dylan to IIOP via another
   language binding or via a non-Dylan IIOP engine API.

Customization
   The Open Dylan ORB is open source, allowing bug fixes and
   customizations.

100% pure Dylan
   Providing users with enhanced debugging and interaction facilities
   available in a homogeneous implementation model.

Inter-language compatibility
   ORB implementations are designed to be interoperable; Open Dylan's
   ORB should interoperate with other ORBs that support IIOP version 1.1.

Features of Open Dylan CORBA
============================

The following features are supported in Open Dylan's CORBA implementation:

- CORBA 2.0 with parts of CORBA 2.2 (notably the POA)
- Internet Inter-ORB Protocol (IIOP) 1.1 (GIOP 1.1)
- Portable Object Adapter (POA)
- Dynamic Invocation Interface (DII)
- Dynamic Skeleton Interface (DSI)
- Dylan Language Binding

CORBA examples
==============

Open Dylan includes example applications to help you start writing
client/server applications in CORBA.

Hello World
   A client/server implementation of the standard Hello World
   application.

Bank
   A three-tier client/server implementation of a banking application.

Chat
   An implementation of a chat program that allows users to
   communicate across a network.

We study the Hello World and Bank applications later in this book. The
Hello World example is ready to build and run, straight out of the
box. Before you can use the Bank example, you might need to install
some (free) Microsoft ODBC drivers, depending on what you have
installed on your computer already. The documentation for the Bank
example provides instructions.
