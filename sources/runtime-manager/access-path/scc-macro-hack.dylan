module:      access-path-implementation
synopsis:    Paul's first ever macro!
             emulates define C-pointer-type
author:      Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define macro C-pointer-type-definer
  {define C-pointer-type ?pointer-class:name => ?desiclass:name}
    => {define constant ?pointer-class = ?desiclass.pointer-type}
end macro;

