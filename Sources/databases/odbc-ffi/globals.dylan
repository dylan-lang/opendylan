module: dylan-odbc-internal
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//These are here so the error handling file will notice them.
//This file should probably go away.  Something more clever should be
//done about global state.

define variable henv = $SQL-NULL-HENV;  
define variable hdbc = $SQL-NULL-HDBC;  
define variable hstmt = $SQL-NULL-HSTMT; 
