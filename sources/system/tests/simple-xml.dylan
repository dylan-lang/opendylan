Module:       system-test-suite
Synopsis:     System library test suite
Author:	      Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Simple XML tests

define sideways method make-test-instance
    (class == <xml-document>) => (document :: <xml-document>)
  make(<xml-document>, location: as(<file-locator>, "c:/test/test/xml"))
end method make-test-instance;

define sideways method make-test-instance
    (class == <xml-element>) => (document :: <xml-element>)
  make(<xml-element>, name: "test")
end method make-test-instance;

define simple-xml class-test <xml-error> ()
  //---*** Fill this in...
end class-test <xml-error>;

define simple-xml class-test <xml-document> ()
  //---*** Fill this in...
end class-test <xml-document>;

define simple-xml class-test <xml-node> ()
  //---*** Fill this in...
end class-test <xml-node>;

define simple-xml class-test <xml-element> ()
  //---*** Fill this in...
end class-test <xml-element>;


/// XML functions

define simple-xml function-test document-location ()
  //---*** Fill this in...
end function-test document-location;

define simple-xml function-test document-location-setter ()
  //---*** Fill this in...
end function-test document-location-setter;

define simple-xml function-test document-element ()
  //---*** Fill this in...
end function-test document-element;

define simple-xml function-test document-element-setter ()
  //---*** Fill this in...
end function-test document-element-setter;

define simple-xml function-test read-xml-document ()
  //---*** Fill this in...
end function-test read-xml-document;

define simple-xml function-test node-attribute ()
  //---*** Fill this in...
end function-test node-attribute;

define simple-xml function-test node-attribute-setter ()
  //---*** Fill this in...
end function-test node-attribute-setter;

define simple-xml function-test node-attributes ()
  //---*** Fill this in...
end function-test node-attributes;

define simple-xml function-test node-children ()
  //---*** Fill this in...
end function-test node-children;

define simple-xml function-test node-name ()
  //---*** Fill this in...
end function-test node-name;

define simple-xml function-test node-text ()
  //---*** Fill this in...
end function-test node-text;

define simple-xml function-test node-text-setter ()
  //---*** Fill this in...
end function-test node-text-setter;

define simple-xml function-test select-node-text ()
//---*** Fill this in...
end function-test select-node-text;

define simple-xml function-test select-nodes ()
//---*** Fill this in...
end function-test select-nodes;

define simple-xml function-test select-single-node ()
//---*** Fill this in...
end function-test select-single-node;
