Module:    apple-dylan-test-suite
Filename:  test-character.dylan
Summary:   Apple Dylan test suite, test-character
Version:   29-Oct-93
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/*---------------------------------------------
Modified by: Shri Amit(amit) &
	     James Kirsch(jkirsch)
Date: August 24 1996
Summary: Converted to new testworks protocol
Copyright: (c) 1996 Functional Objects, Inc. 
           All rights reserved.  
----------------------------------------------*/

// Chapter 16. Characters
// as-uppercase on character

define test as-character-stuff ()
   let example
        = concatenate
            (*uppercase-alphabet*,
             *lowercase-alphabet*,
             *digit-characters*,
             *misc-characters*);
   check("", sequences-element-id?,
	 example,
	 map(compose(*as-character*, *as-integer*), as(<vector>, example)));
   check("", sequences-element-id?,
        map(as-uppercase, example),
         concatenate
           (*uppercase-alphabet*,
            *uppercase-alphabet*,
            *digit-characters*,
            *misc-characters*));
   check("", sequences-element-id?,
        map(as-lowercase, example),
         concatenate
           (*lowercase-alphabet*,
            *lowercase-alphabet*,
            *digit-characters*,
            *misc-characters*));
   
  check("", every?,
	\==.complement,
	map(as-uppercase, *lowercase-alphabet*),
	*lowercase-alphabet*);
  check("", every?,
	\==.complement,
	map(as-lowercase, *uppercase-alphabet*),
	*uppercase-alphabet*);
end test;

define suite test-character-suite ()
  test as-character-stuff;
end suite; 
