Module:       system-internals
Synopsis:     Abstract modeling of locations
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant <native-file-system-locator>  = <microsoft-file-system-locator>;
define constant <native-directory-locator> = <microsoft-directory-locator>;
define constant <native-file-locator>      = <microsoft-file-locator>;

define function file-system-separator
    () => (separator :: <character>)
  $microsoft-separators[0]
end function file-system-separator;
