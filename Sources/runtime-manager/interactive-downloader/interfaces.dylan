module:     interactive-downloader-internals
synopsis:   Top level processing of the interactive <coff-file>.
author:     Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// DOWNLOAD-OBJECT-FILES
//    A top-level interface to the functionality of the downloader.
//    Downloads a sequence of <coff-file> objects into the interactive
//    dylan application defined by the <debug-target>.

define method download-object-files
    (application :: <debug-target>, coff-files :: <sequence>,
     #key library = "dylan") => ()
  let transaction = 
    open-interactive-transaction(application, coff-files, dll-name: library);
  download-all-raw-data(transaction);
  define-all-symbols(transaction);
  perform-all-relocations(transaction);
  perform-all-registrations(transaction);
end method;


///// DOWNLOAD-FOR-INTERACTIVE-EXECUTION
//    The main interface to the interactive linker.

define sideways method download-for-interactive-execution
    (context :: <runtime-context>, coff-files :: <sequence>, 
     library :: <byte-string>, entry-point :: <byte-string>)
        => (transaction-id)

  // Unpick the execution context to get the debug target, and the
  // required thread.

  let application = context.runtime-context-debug-target;
  let thread = context.runtime-context-thread;

  call-debugger-function(application,
			 download-for-interactive-execution-internal,
			 application, thread, coff-files, 
			 library, entry-point);

end method;


///// DOWNLOAD-FOR-INTERACTIVE-EXECUTION-INTERNAL
//    The internal interface must be called on the thread managing the 
//    application

define method download-for-interactive-execution-internal
    (application :: <debug-target>, thread :: <remote-thread>, 
     coff-files :: <sequence>, library :: <byte-string>, 
     entry-point :: <byte-string>)
        => (transaction-id)

  // If we've been given a thread suitable for interactivity, we know
  // we can use that same thread for any spy activity during the download.

  use-thread-for-spy-functions(application, thread);

  // Now open the interactive transaction, and perform all of the COFF
  // object file processing.

  let transaction = 
     open-interactive-transaction
       (application, coff-files, dll-name: library, thread: thread);
  download-all-raw-data(transaction);
  define-all-symbols(transaction);
  perform-all-relocations(transaction);
  perform-all-registrations(transaction);

  // Call the appropriate DM functionality to begin executing interactive
  // Dylan code. This will return a (pre-registered) breakpoint, which we
  // can use as our transaction ID.

  setup-interactor(application, thread, entry-point, library,
                   #"multiple-value");

end method;

