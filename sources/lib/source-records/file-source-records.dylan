Module: file-source-records-implementation
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define abstract class <file-source-record> (<source-record>)
end class;

define variable *default-file-source-record-class* = #f;

define method make
    (class == <file-source-record>, #rest keys, #key, #all-keys)
 => (res :: <file-source-record>)
  apply(make, *default-file-source-record-class*, keys)
end method;

// Returns all the source record ids (in order) for the given file.
// I.e. this is where one would plug in a sectionalizer.
define generic file-source-record-ids (class :: subclass(<file-source-record>),
                                       directory :: <locator>,
                                       file :: <locator>)
 => sr-id* :: <sequence>;

define method file-source-record-ids (class == <file-source-record>,
                                      directory :: <locator>,
                                      file :: <locator>)
 => sr-id* :: <sequence>;
  file-source-record-ids(*default-file-source-record-class*, directory, file);
end method;

define method id-as-source-record (class == <file-source-record>,
                                   project :: <object>,
                                   location :: <locator>,
                                   id)
 => sr :: <file-source-record>;
  id-as-source-record
    (*default-file-source-record-class*, project, location, id);
end method;

define method print-source-line-location (sr :: <file-source-record>,
                                          line-number :: <integer>,
                                          stream) => ();
  format(stream, "%s:%s",
                 locator-name(source-record-location(sr, check-if-exists?: #f)),
                 line-number + source-record-start-line(sr));
end method;

define method source-line-location
  (sr :: <file-source-record>, line-number :: <integer>)
    => (name, real-line-number);
  values(locator-name(source-record-location(sr)),
         line-number + source-record-start-line(sr));
end method;



