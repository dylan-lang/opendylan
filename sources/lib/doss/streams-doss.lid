Library:     streams-doss
Synopsis:    Required DOSS support for streams
Author:      Eliot Miranda
Description: Need to arrange that positionable-stream slots are dumped in the right
             order so that read-end & write-end are bound before stream-position is set
             on loading. Also need to dump external streams in a closed state.
Files:       streams-doss-library
	     streams-doss
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND
Other-files: Open-Source-License.txt

