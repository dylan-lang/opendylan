Module:    test-editor-manager
Synopsis:  Environment-Editor Interface Test -- platform-specific part
Author:    Hugh Greene
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Setup platform-specific information.
/*
define constant $test-hWnd :: <HWND> = $NULL-HWND;
*editor-call* := rcurry(editor-call, hwnd: $test-hWnd);
*/
*editor-call* := editor-call;

// ---*** The following needs to be replaced with some suitable,
// dependable file, probably in some QA directory.
*test-file-path* := "C:\\users\\hughg\\";
*test-file-name* := "foo.txt";


/// Perform the test suite.
perform-suite(test-editor-manager);
