Library:     doss
Synopsis:    DOSS -- Dylan Object Storage System, a simple object storage mechanism.
Author:      Eliot Miranda
Description: This library provides an object storage system for Dylan objects.
             It allows one to store a description of a graph of objects in
             a byte stream (either an internal stream or a file stream) and
             to subsequently reconstruct an isomorphic object-graph by suitably
             interpreting the byte stream.
             The library contains two client modules, doss and emulator-doss.
             Clients using the emulator should import emulator-doss, which
             copes with some emulator wrinkles in the implementation of certain
             Dylan objects.
             For a proper description of DOSS see doss.doc in ~dylan/env/doc/libraries.
Files:       library
	     module
	     doss
	     doss-in
             utilities
	     doss-out
	     doss-policies
	     doss-configuration
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND
Other-files: Open-Source-License.txt

