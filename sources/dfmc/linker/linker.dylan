Module: dfmc-linker
Author: Jonathan Bachrach, Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define open class <linker> (<object>) end;

define open generic emit-library-records
  (back-end :: <back-end>, ld :: <library-description>, #rest flags, #key);

define open generic emit-library-record
  (back-end :: <back-end>, cr :: <compilation-record>, ld :: <library-description>, 
   #rest flags, #key);

define open generic emit-gluefile
  (back-end :: <back-end>, ld, cr-names, #rest flags, #key);
define open generic emit-mainfile
  (back-end :: <back-end>, ld, #rest flags, #key);

define open generic emit-makefile (ld, cr-names, #rest build-keys, #key);

define open generic emit-glue 
   (back-end :: <back-end>, ld :: <library-description>,
    #rest keys, #key, #all-keys);

define method emit-glue (back-end :: <back-end>, ld :: <library-description>,
			 #rest keys, #key build-settings = #(), #all-keys)
  let cr-names = compilation-context-object-names(ld);
  apply(emit-gluefile, back-end, ld, cr-names, keys);
  apply(emit-mainfile, back-end, ld, keys);
  apply(emit-makefile, ld, cr-names, build-settings)
end method;

define open generic link-and-download 
    (back-end :: <back-end>, il :: <interactive-layer>, runtime-context, #key)
 => transaction-id;

define variable *fake-transaction-id-counter* = 0;
define method link-and-download 
    (back-end :: <back-end>, il :: <interactive-layer>, runtime-context,
     #key, #all-keys)
 => transaction-id;
  // let cr-names = compilation-context-object-names(il);
  break("Interactive execution not implemented for %s, "
	  "continue from break to pretend that download completed", back-end);
  // Make sure to return distinct transaction ids, so can test condition
  // lookup, which is based on the transaction id's.
  *fake-transaction-id-counter* := *fake-transaction-id-counter* + 1
end method;


// The downloader must define a method on this to enable downloading.
// It will be called by the real implementation of link-and-download
define open generic download-for-interactive-execution
    (runtime-context, downloadable-records :: <sequence>,
     component-name :: <byte-string>, init-function :: <byte-string>)
 => transaction-id;



// eof

