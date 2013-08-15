Library:      life
Author:       Carl Gay
Synopsis:     The game of Life
Files:  library
        util
        logic
        events
        patterns
        frame
        display
        life
Comment:        'C-Header-Files' is a kludge to get the bitmaps linked in
RC-Files:       life-resources.rc
C-Header-Files: erase.ico
                stop.ico
                step.ico
                play.ico
                life-large.ico
Linker-Options: $(guilflags)
Major-version: 2
Minor-version: 1
Start-Function: life
Compilation-mode: tight
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND
Other-files: README.html
