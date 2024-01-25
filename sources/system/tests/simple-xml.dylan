Module:       system-test-suite
Synopsis:     System library test suite
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Simple XML tests

define sideways method make-test-instance
    (class == <xml-document>) => (document :: <xml-document>)
  make(<xml-document>, location: as(<file-locator>, "c:/test/test/xml"))
end method;

define sideways method make-test-instance
    (class == <xml-element>) => (document :: <xml-element>)
  make(<xml-element>, name: "test")
end method;

define test test-<xml-error> ()
  //---*** Fill this in...
end test;

define test test-<xml-document> ()
  //---*** Fill this in...
end test;

define test test-<xml-node> ()
  //---*** Fill this in...
end test;

define test test-<xml-element> ()
  //---*** Fill this in...
end test;


/// XML functions

define test test-document-location ()
  //---*** Fill this in...
end test;

define test test-document-location-setter ()
  //---*** Fill this in...
end test;

define test test-document-element ()
  //---*** Fill this in...
end test;

define test test-document-element-setter ()
  //---*** Fill this in...
end test;

define test test-read-xml-document ()
  //---*** Fill this in...
end test;

define test test-node-attribute ()
  //---*** Fill this in...
end test;

define test test-node-attribute-setter ()
  //---*** Fill this in...
end test;

define test test-node-attributes ()
  //---*** Fill this in...
end test;

define test test-node-children ()
  //---*** Fill this in...
end test;

define test test-node-name ()
  //---*** Fill this in...
end test;

define test test-node-text ()
  //---*** Fill this in...
end test;

define test test-node-text-setter ()
  //---*** Fill this in...
end test;

define test test-select-node-text ()
//---*** Fill this in...
end test;

define test test-select-nodes ()
//---*** Fill this in...
end test;

define test test-select-single-node ()
//---*** Fill this in...
end test;

define suite simple-xml-test-suite ()
  test test-<xml-error>;
  test test-<xml-document>;
  test test-<xml-node>;
  test test-<xml-element>;
  test test-document-location;
  test test-document-location-setter;
  test test-document-element;
  test test-document-element-setter;
  test test-read-xml-document;
  test test-node-attribute;
  test test-node-attribute-setter;
  test test-node-attributes;
  test test-node-children;
  test test-node-name;
  test test-node-text;
  test test-node-text-setter;
  test test-select-node-text;
  test test-select-nodes;
  test test-select-single-node;
end;
