Module:    repository
Author:    Hugh Greene, Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// ---*** Initially these are Dylan top-level init forms but they should be
/// derived from some config file.

/// ---*** How about putting library and module information into
/// actual source files so that we don't duplicate stuff and automatically
/// keep up to date?  Or deriving it from the databases?  But then how do
/// we know what to hide in a release-specific way?  Maybe we have to state
/// what to show, and hide the rest?

/// Information Source
/// The libraries and modules in this repository are taken from the
/// "Dylan Libraries" database in Spring, as of 1997/10/12, from the
/// view "Customer Libraries\by Release".  I have taken only libraries
/// from the "Kansas Personal" release and have omitted the "Applications",
/// "Examples", and "QA" groups.  The modules listed as exported are only
/// those noted as "Customer" modules in the DB.
/// ---*** The groups below are not based on the shipping groups, but on
/// what I hope will be useful groups for new programmers.
/// --- I've also noted which volume of the documentation each library
/// appears in.  It might be nice if the Doc and the Wizard matched!

/// ---*** TO ADD:
///   Now that we have several combined libraries, we could really do with
///     allowing modules to be hidden in a release-specific way, not just
///     groups and libraries.

/// ----------------------------------------------------------------------
/// LIBRARY-GROUPS

begin
  make(<project-library-group>,
       id: #"core",
       libraries: #[#(#"dylan"),
		    #(#"functional-extensions"),
		    #"functional-dylan"],
       documentation: "Basic Dylan language and extension libraries");

  make(<project-library-group>,
       id: #"collections",
       libraries: #[#"collections"],
       documentation: "Additional collection classes");

  make(<project-library-group>,
       id: #"databases",
       // If you change this, change the #"interoperability" one at the end
       libraries: #[#"odbc-ffi",
		    #"sql",
		    #"sql-odbc",
		    #"c-ffi"],
       documentation: "Dylan database interface libraries");

  make(<project-library-group>,
       id: #"duim",
       label: "DUIM",
       libraries: #[#"duim",
		    #(#"duim-ole-server"),
		    #(#"duim-ole-control")],
       documentation: "Dylan User Interface Manager libraries");

  make(<project-library-group>,
       id: #"system",
       libraries: #[#"system"],
       documentation: "Portable interface to general platform-specific facilities");

  make(<project-library-group>,
       id: #"io",
       label: "I/O",
       libraries: #[#"io"],
       documentation: "Input/Output libraries");

  make(<project-library-group>,
       id: #"mathematics",
       libraries: #[#"big-integers",
		    #"generic-arithmetic"],
       documentation: "Extra mathematics libraries");

  make(<project-library-group>,
       id: #"network",
       libraries: #[#"network"],
       documentation: "Portable and platform-specific network facilities");

  make(<project-library-group>,
       id: #"ole",
       label: "OLE",
       // If you change this, change the #"interoperability" one at the end
       libraries: #[#"com",
		    #(#"duim-ole-server"),
		    #(#"duim-ole-control"),
		    #"ole",
		    #"ole-automation",
		    #"ole-control-framework",
		    #"ole-controls",
		    #"ole-dialogs",
		    #"ole-server",
		    #"c-ffi"],
       documentation: "OLE and COM interface libraries");

  make(<project-library-group>,
       id: #"win32",
       // If you change this, change the #"interoperability" one at the end
       libraries: #[#"win32-common",
		    #"win32-controls",
		    #(#"win32-dde"),
		    #"win32-dialog",
		    #"win32-gdi",
		    #(#"win32-gl"),
		    #(#"win32-glu"),
		    #"win32-kernel",
		    #(#"win32-multimedia"),
		    #(#"win32-rich-edit"),
		    #(#"win32-registry"),
		    #(#"win32-shell"),
		    #"win32-user",
		    #(#"win32-version"),
		    #"c-ffi"],
       documentation: "Win32 interface libraries");

  make(<project-library-group>,
       id: #"corba",
       label: "CORBA",
       // If you change this, change the #"interoperability" one at the end
       libraries: #[#"dylan-orb"],
       documentation: format-to-string("%s CORBA library", release-product-name()));

  // --- This must come after the Databases, OLE and Win32 groups are created.
  make(<project-library-group>,
       id: #"interoperability",
       libraries: #[// Databases library group
		    #"odbc-ffi",
		    #"sql",
		    #"sql-odbc",
		    // OLE library group
		    #"com",
		    #(#"duim-ole-server"),
		    #(#"duim-ole-control"),
		    #"ole",
		    #"ole-automation",
		    #"ole-control-framework",
		    #"ole-controls",
		    #"ole-dialogs",
		    #"ole-server",
		    // Win32 library group
		    #"win32-common",
		    #"win32-controls",
		    #(#"win32-dde"),
		    #"win32-dialog",
		    #"win32-gdi",
		    #(#"win32-gl"),
		    #(#"win32-glu"),
		    #"win32-kernel",
		    #(#"win32-multimedia"),
		    #(#"win32-rich-edit"),
		    #(#"win32-registry"),
		    #(#"win32-shell"),
		    #"win32-user",
		    #(#"win32-version"),
		    // CORBA library group
		    #"dylan-orb",
		    // We always want the C FFI
		    #"c-ffi"],
       documentation: "Win32, OLE, SQL and ODBC interfaces plus low-level C access");
end;


/// ----------------------------------------------------------------------
/// LIBRARIES

begin
  //////
  /// "Core" group

  // Doc volume: "Core Features and Mathematics".
  make(<project-library>,
       id: #"dylan",
       modules: #[#"dylan"],
       documentation: "Dylan language library");
  
  // Doc volume: "Core Features and Mathematics".
  make(<project-library>,
       id: #"functional-extensions",
       modules: #[#"functional-extensions",
		  // from 'dylan'
		  #(#"finalization"),
		  // from 'functional-extensions'
		  #"simple-format",
		  #(#"simple-random"),
		  // from respective libraries
		  #(#"byte-vector"),
		  #(#"machine-word"),
		  #"threads",
		  #(#"transcendentals")],
       documentation: format-to-string("Dylan language and %s language extensions",
                                       release-product-name()));

  // Doc volume: "Core Features and Mathematics".
  make(<project-library>,
       id: #"functional-dylan",
       modules: #[#"functional-dylan",
		  #(#"dylan"),
		  #(#"functional-extensions"),
		  // from 'dylan'
		  #(#"finalization"),
		  // from 'functional-extensions'
		  #"simple-format",
		  #(#"simple-random"),
		  // from respective libraries
		  #(#"byte-vector"),
		  #(#"machine-word"),
		  #"threads",
		  #(#"transcendentals")],
       documentation: format-to-string("Dylan language and %s language extensions",
                                       release-product-name()));


  //////
  /// "Collections" group

  // Doc volume: ???
  make(<project-library>,
       id: #"collections",
       //--- Modules also include 'bit-set', 'bit-vector', 'plists', 'set'
       library-packs: library-packs(#[#"Core"]),
       modules: #[#"table-extensions"],
       documentation: "Additional collection classes");


  //////
  /// "Databases" group

  // Doc volume: ---*** NONE!
  make(<project-library>,
       id: #"odbc-ffi",
       label: "ODBC-FFI",
       library-packs: library-packs(#[#"Win32", #"ODBC"]),
       modules: #[#"odbc-ffi"],
       documentation: "ODBC C-FFI layer");

  // Doc volume: ---*** NONE!
  make(<project-library>,
       id: #"sql",
       label: "SQL",
       library-packs: library-packs(#[#"Win32", #"ODBC"]),
       modules: #[#"sql"],
       documentation: "A generic protocol for accessing relational databases");

  // Doc volume: ---*** NONE!
  make(<project-library>,
       id: #"sql-odbc",
       label: "SQL-ODBC",
       library-packs: library-packs(#[#"ODBC", #"Win32"]),
       modules: #[#"sql-odbc"],
       documentation: "An ODBC-specific implementation of the SQL library");


  //////
  /// "DUIM" group

  // Doc volume: "DUIM".
  make(<project-library>,
       id: #"duim",
       label: "DUIM",
       library-packs: library-packs(#[#"GUI"]),
       modules: #[#"duim",
		  #(#"duim-extended-geometry"),
		  #(#"win32-duim")],
       documentation: "The Dylan User Interface Manager");

  // The next two need a specific release tag as they're contained in
  // two groups, one of which is available in the Personal release, so
  // we have to exclude them there.
  // Doc volume: ---*** NONE!
  make(<project-library>,
       id: #"duim-ole-server",
       label: "DUIM-OLE-Server",
       library-packs: library-packs(#[#"GUI", #"OLE", #"Win32"]),
       modules: #[#"duim-ole-server"],
       documentation: "A framework for OLE Server applications using DUIM");

  // Doc volume: ---*** NONE!
  make(<project-library>,
       id: #"duim-ole-control",
       label: "DUIM-OLE-Control",
       library-packs: library-packs(#[#"GUI", #"OLE", #"Win32"]),
       modules: #[#"duim-ole-control"],
       documentation: "A framework for OLE Controls using DUIM");


  //////
  /// "FFI" group

  // Doc volume: "Interoperability".
  make(<project-library>,
       id: #"c-ffi",
       label: "C-FFI",
       library-packs: library-packs(#[#"Core"]),
       modules: #[#"c-ffi"],
       documentation: "A Foreign Function Interface to C");


  //////
  /// "I/O" group

  // Doc volume: ???
  make(<project-library>,
       id: #"io",
       label: "IO",
       library-packs: library-packs(#[#"Core"]),
       modules: #[#"format",
		  #"format-out",
		  #(#"print"),
		  #"standard-io",
		  #"streams"],
       documentation: "Input-output facilities");

  /* Previous per-library documentation
     format: "Support for formatted printing"
     format-out: "Implementation of the format-out function"
     print: "Support for printing objects"
     standard-io: "Handling input and output streams"
     streams: "A stream model for input and output, for files and strings"
  */


  //////
  /// "Mathematics" group

  // Doc volume: "Core Features and Mathematics".
  make(<project-library>, 
       id: #"big-integers",
       library-packs: library-packs(#[#"Core"]),
       modules: #[#"big-integers"],
       documentation: "Double precision integers"); // 64-bit?

  // Doc volume: "Core Features and Mathematics".
  // Have to exclude most of these modules by default, as they are
  // mutually exclusive.
  make(<project-library>,
       id: #"generic-arithmetic",
       library-packs: library-packs(#[#"Core"]),
       modules: #[#"generic-arithmetic",
		  #(#"generic-arithmetic-common-dylan"),
		  #(#"generic-arithmetic-dylan")],
       documentation: "Generic arithmetic, allowing various implementations"
		      " but providing none");


  //////
  /// "Network" group

  // Doc volume: ---*** NONE!
  make(<project-library>,
       id: #"network",
       library-packs: library-packs(#[#"Network"]),
       modules: #[#"sockets",
		  #(#"winsock2")],
       documentation: "Portable and platform-specific network facilities");


  //////
  /// "OLE" group

  // Doc volume: ---*** NONE!
  make(<project-library>,
       id: #"com",
       label: "COM",
       library-packs: library-packs(#[#"OLE", #"Win32"]),
       modules: #[#"com"],
       documentation: "An interface to COM");

  // Doc volume: ---*** NONE!
  make(<project-library>,
       id: #"ole",
       label: "OLE",
       library-packs: library-packs(#[#"OLE", #"Win32"]),
       modules: #[#"ole"],
       documentation: "An interface to OLE");

  // Doc volume: ---*** NONE!
  make(<project-library>,
       id: #"ole-automation",
       label: "OLE-Automation",
       library-packs: library-packs(#[#"OLE", #"Win32"]),
       modules: #[#"ole-automation"],
       documentation: "An interface to OLE Automation");

  // Doc volume: ---*** NONE!
  make(<project-library>,
       id: #"ole-control-framework",
       label: "OLE-Control-Framework",
       library-packs: library-packs(#[#"OLE", #"Win32"]),
       modules: #[#"ole-control-framework"],
       documentation: "A framework for OLE Controls without DUIM");

  // Doc volume: ---*** NONE!
  make(<project-library>,
       id: #"ole-controls",
       label: "OLE-Controls",
       library-packs: library-packs(#[#"OLE", #"Win32"]),
       modules: #[#"ole-controls"],
       documentation: "A low-level interface to OLE Controls");

  // Doc volume: ---*** NONE!
  make(<project-library>,
       id: #"ole-dialogs",
       label: "OLE-Dialogs",
       library-packs: library-packs(#[#"OLE", #"Win32"]),
       modules: #[#"ole-dialogs"],
       documentation: "An interface to OLE Dialogs");

  // Doc volume: ---*** NONE!
  make(<project-library>,
       id: #"ole-server",
       label: "OLE-Server",
       library-packs: library-packs(#[#"OLE", #"Win32"]),
       modules: #[#"ole-server"],
       documentation: "A framework for creating OLE Servers");


  //////
  /// "System" group

  // Doc volume: ???
  make(<project-library>,
       id: #"system",
       library-packs: library-packs(#[#"Core"]),
       //--- Modules also include 'commands' and 'settings'.
       modules: #[#"date",
		  #"file-system",
		  #"operating-system"],
       documentation: "Portable interface to general platform-specific facilities");


  //////
  /// "Win32 API" group

  make(<project-library>,
       id: #"win32-common",
       library-packs: library-packs(#[#"Win32"]),
       modules: #[#"win32-common"],
       documentation: "A support library for the other Win32 API libraries");

  make(<project-library>,
       id: #"win32-controls",
       library-packs: library-packs(#[#"Win32"]),
       modules: #[#"win32-controls"],
       documentation: "An FFI interface to the Win32 Common Controls library");

  make(<project-library>,
       id: #"win32-dde",
       label: "Win32-DDE",
       library-packs: library-packs(#[#"Win32"]),
       modules: #[#"win32-dde"],
       documentation: "An FFI interface to the Win32 DDE library");

  make(<project-library>,
       id: #"win32-dialog",
       library-packs: library-packs(#[#"Win32"]),
       modules: #[#"win32-dialog"],
       documentation: "An FFI interface to the Win32 Dialog library");

  make(<project-library>,
       id: #"win32-gdi",
       label: "Win32-GDI",
       library-packs: library-packs(#[#"Win32"]),
       modules: #[#"win32-gdi"],
       documentation: "An FFI interface to the Win32 Graphics Device Interface");

  make(<project-library>,
       id: #"win32-gl",
       label: "Win32-GL",
       library-packs: library-packs(#[#"Win32"]),
       modules: #[#"win32-gl"],
       documentation: "An FFI interface to the Win32 OpenGL library");

  make(<project-library>,
       id: #"win32-glu",
       label: "Win32-GLU",
       library-packs: library-packs(#[#"Win32"]),
       modules: #[#"win32-glu"],
       documentation: "An FFI interface to the Win32 OpenGL Utility library");

  make(<project-library>,
       id: #"win32-kernel",
       library-packs: library-packs(#[#"Win32"]),
       modules: #[#"win32-kernel"],
       documentation: "An FFI interface to the Win32 Kernel library");

  make(<project-library>,
       id: #"win32-registry",
       library-packs: library-packs(#[#"Win32"]),
       modules: #[#"win32-registry"],
       documentation: "An FFI interface to the Win32 Registry library");

  make(<project-library>,
       id: #"win32-multimedia",
       label: "Win32-Multimedia",
       library-packs: library-packs(#[#"Win32"]),
       modules: #[#"win32-multimedia"],
       documentation: "An FFI interface to the Win32 Multimedia APIs");

  make(<project-library>,
       id: #"win32-shell",
       library-packs: library-packs(#[#"Win32"]),
       modules: #[#"win32-shell"],
       documentation: "An FFI interface to the Win32 Shell library");
  
  make(<project-library>, 
       id: #"win32-rich-edit",
       library-packs: library-packs(#[#"Win32"]),
       modules: #[#"win32-rich-edit"],
       documentation: "An FFI interface to the Win32 Rich Text Edit library");

  make(<project-library>,
       id: #"win32-user",
       library-packs: library-packs(#[#"Win32"]),
       modules: #[#"win32-user"],
       documentation: "An FFI interface to the Win32 User library");

  make(<project-library>,
       id: #"win32-version",
       library-packs: library-packs(#[#"Win32"]),
       modules: #[#"win32-version"],
       documentation: "An FFI interface to the Win32 Version library");


  //////
  /// "CORBA" group

  // Doc volume: ---*** NONE!
  make(<project-library>,
       id: #"dylan-orb",
       label: "Dylan-ORB",
       library-packs: library-packs(#[#"CORBA", #"Network"]),
       modules: #[#"dylan-orb"],
       documentation: format-to-string("%s CORBA ORB", release-product-name()));


/* ---------- Other libraries, not yet advertised/shipped ----------
  // "Native"

  make(<project-library>,
       id: #"locators",
       library-packs: library-packs(#[#"Core"]),
       modules: #[#"locators"],
       documentation: "");
---------- Other libraries END ---------- */
end;

/*

/// ----------------------------------------------------------------------
/// FILES

begin
  make(<project-file>,
       id: #"lid",
       extension: ".lid",
       base: "project",
       headers: #[#"author",
		  #"copyright",
		  #"synopsis",
		  #"version",
		  #"major-version",
		  #"minor-version",
		  #"library",
		  #"files"],
       documentation: "Dylan library interchange format file");

  make(<project-file>,
       id: #"library",
       name: "library.dylan",
       headers: #[#"author",
		  #"copyright",
		  #"synopsis",
		  #"version"],
       //  contents: #"library", // ???
       documentation: "Library definition file");

  make(<project-file>,
       id: #"module",
       name: "module.dylan",
       headers: #[#"author",
		  #"copyright",
		  #"synopsis",
		  #"version"],
       //  contents: #"module", // ???
       documentation: "Modules definition file");

  make(<project-file>,
       id: #"implementation",
       name: "implementation.dylan",
       headers: #[#"author",
		  #"copyright",
		  #"synopsis",
		  #"version",
		  #"module"],
       documentation: "Implementation file");

  make(<project-file>,
       id: #"readme",
       name: "README.txt",
       documentation: "Description of other files");
end;

/// ----------------------------------------------------------------------
/// HEADERS

begin
  make(<project-file-header>,
       id: #"author",
       default-value: #["Joe Bloggs"],
       documentation: "Name of authors of file");

  make(<project-file-header>,
       id: #"copyright",
       default-value: #["(c) 2000 Joe Bloggs Inc.  All rights reserved."],
       documentation: "Copyright text");

  make(<project-file-header>,
       id: #"synopsis",
       default-value: #["Stuff"],
       documentation: "Brief description of project");

  make(<project-file-header>,
       id: #"version",
       default-value: #["$revision$"],
       documentation: "Identifies version of file (eg $revision$ for RCS keyword expansion)");

  make(<project-file-header>,
       id: #"major-version",
       default-value: #["1"],
       documentation: "Identifies major version of library (using and used libraries have to agree on major version of the used library)");

  make(<project-file-header>,
       id: #"minor-version",
       default-value: #["0"],
       documentation: "Identifies minor version of library (used library must not have smaller minor version than using library is expecting)");

  make(<project-file-header>,
       id: #"library",
       default-value: #[#"library"],
       documentation: "Gives library name");

  make(<project-file-header>,
       id: #"files",
       default-value: #[#"lid", #"library", #"module", #"implementation"],
       documentation: "Gives library files");
end;

*/

/// ----------------------------------------------------------------------
/// PROJECT TYPES

begin
  //--- hughg, 1998/09/07: We could omit the explicit initialization of the
  //--- "order:" and just fill it in inside a make method.

  // Personal and above

  make(<project-type>,
       id: #"gui-exe",
       label: "GUI Application (EXE)",
       documentation:
"A project which will build an Application running in the Windows GUI"
" subsystem.  It will use GUI libraries by default, if you follow the Simple"
" path for choosing which libraries your project will use.");

  make(<project-type>,
       id: #"console-exe",
       label: "Console Application (EXE)",
       documentation:
"A project which will build a Console Application (that is, one running in"
" the Windows Console subsystem).  It will not use any GUI libraries by"
" default.");

  make(<project-type>,
       id: #"dll",
       label: "Dynamic Link Library (DLL)",
       documentation:
"A project which will build a DLL.  "
"It will not use any GUI libraries by default.");

  // Enhanced and above

  make(<project-type>,
       id: #"motley",
       label: "Interface to COM Type Library",
    documentation:
"A project based around an interface to a COM type library.  It will run in"
" the Win32 GUI subsystem but will not use any GUI libraries by default, and"
" you do not necessarily have to add GUI libraries.  Depending on which kinds"
" of interface you choose to include, the project may build an Application or"
" a DLL.  If it builds a DLL, the project is intended to be used as a"
" subproject of another project.");

  make(<project-type>,
       id: #"ole-control",
       label: "OLE Control (DLL)",
       documentation:
"A project for creating an OLE Control, which will be a DLL.  It will use"
" GUI libraries by default, if you follow the Simple path for choosing"
" which libraries your project will use.  If you follow the Minimal or"
" Custom paths, you will have to choose GUI libraries to use.");

  make(<project-type>,
       id: #"ole-server",
       label: "OLE Compound Document Server (EXE)",
       documentation:
"A project for creating an Application which is an OLE Compound Document"
" Server.  It will run in the Windows GUI subsystem.  It will use GUI"
" libraries by default, if you follow the Simple path for choosing which"
" libraries your project will use.  If you follow the Minimal or Custom"
" paths, you will have to choose GUI libraries to use.");

  make(<project-type>,
       id: #"scepter",
       label: "CORBA Client and/or Server",
       documentation:
"Projects based around a CORBA IDL file.  You can create either a Client or"
" Server project or both.  These projects will use CORBA interface libraries"
" which are generated automatically from the IDL file when you build the"
" project.  The Client and Server libraries may be built as Applications or"
" DLLs.  If they are built as DLLs, they are intended to be used as"
" subprojects of other projects.");
end;
