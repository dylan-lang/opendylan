Module: dfmc-namespace
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Environments.

// The <environment> class is an abstraction for a simple set of
// associations between names and values. Perhaps it should be
// an actual explicit key collection?

// For documentation, the type of names and values, currently
// unrestricted.

define open abstract class <environment> (<object>) end;

// Define a new name in the environment. An error is signaled if a
// definition already exists.

define generic define-name
    (env :: <environment>, name :: <name>, value) => ();

// Undefine an existing name in the environment. An error is signaled if
// no definition exists.

define generic undefine-name
    (env :: <environment>, name :: <name>) => ();

// Lookup the value of a name in the environment. An error is signaled
// if no definition exists unless a default value is supplied, in which
// case that value is returned.

define generic lookup-name
    (env :: <environment>, name :: <name>, #key default) => (value);
