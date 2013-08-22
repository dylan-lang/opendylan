module:    main-harp
Synopsis:  The class definition for span dependent instructions, <new-sdi>.
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define primary open class <new-sdi> (<object>)

  slot new-sdi-preceding-sdis :: <integer>, 
	init-value: 0;
	// where we fit in the global sdi vector

  constant slot new-sdi-dest-tag :: <tag>, 
	init-keyword: dest-tag:;
	// where we're going

  constant slot new-sdi-dest-offset :: <integer>, 
	init-value: 0, init-keyword: dest-offset:;
	// + a fixed offset

  slot new-sdi-cached-span :: <integer>,
	init-value: 0, init-keyword: cached-span:;
	// current estimate of required span

  slot new-sdi-cached-size :: <integer>, 
	init-value: 0, init-keyword: cached-size:;
	// current estimate of instruction size

  // Not referenced or exported.  https://github.com/dylan-lang/opendylan/issues/561
  // slot new-sdi-my-block :: <basic-block>;
	// sdi's own block

  slot new-sdi-fixed-offset :: <integer>, init-value: 0;
	// sdi's fixed offset from start of code vector

  slot new-sdi-diff :: false-or(<integer>) = #f;
	// span difference from cached size

  slot new-sdi-method-index :: <integer>, 
	init-value: 0, init-keyword: method-index:;
	// current size's index in vector

  constant slot new-sdi-method-vector :: <simple-object-vector>, 
	init-keyword: method-vector:;
	// vector of methods to make sdis

  slot new-sdi-code-fragment, init-keyword: code-fragment:;
	// implementation dependent

  slot new-sdi-code-holder :: <sequence>, init-keyword: code-holder:;
	// final piece of code

  slot new-sdi-checked :: <boolean>, init-value: #f;	
	// whether an sdi has been ok'd with respect to it's size and span

  /* Not referenced or exported.  https://github.com/dylan-lang/opendylan/issues/561
  constant slot new-sdi-defines;     // used in scheduling
  constant slot new-sdi-uses;        // used in scheduling
  constant slot new-sdi-transfer;    // used in scheduling
  */
end;




