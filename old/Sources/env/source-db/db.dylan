Module: source-db
Language: infix-dylan
Author: Roman Budzianowski
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <source-db-error> (<simple-error>) end;

define method source-db-error (string :: <string>, #rest args)
  error (make (<source-db-error>, format-string: string, format-arguments: args));
end;

define class <no-file-in-branch> (<source-db-error>) end;

define method  no-file-in-branch (string :: <string>, #rest args)
  error (make (<no-file-in-branch>, format-string: string, format-arguments: args));
end;

define class <no-file-in-system> (<source-db-error>) end;

define method  no-file-in-system (string :: <string>, #rest args)
  error (make (<no-file-in-system>, format-string: string, format-arguments: args));
end;

define class <db-object> (<object>)

end class;

define generic describe (object :: <db-object>, #key verbose? = #f);

