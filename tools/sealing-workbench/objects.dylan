module:   sealing-workbench
author:   Paul Haahr
synopsis: An initial set of problem domain objects.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define crossover-class <number> (<object>) open;
 define crossover-class <real> (<number>) open;
  define crossover-class <rational> (<real>) open;
   define crossover-class <integer> (<rational>);
  define crossover-class <float> (<real>) open;
   define crossover-class <single-float> (<float>);
   define crossover-class <double-float> (<float>);

define crossover-class <collection> (<object>) open;
 define crossover-class <explicit-key-collection> (<collection>) open;
 define crossover-class <stretchy-collection> (<collection>) open;
 define crossover-class <mutable-collection> (<collection>) open;
 define crossover-class <sequence> (<collection>) open;
  define crossover-class <mutable-explicit-key-collection>
		(<explicit-key-collection>, <mutable-collection>) open;
  define crossover-class <mutable-sequence>
		(<sequence>, <mutable-collection>) open;
   define crossover-class <table>
		(<mutable-explicit-key-collection>,
		 <stretchy-collection>) open;
    define crossover-class <object-table> (<table>);
   define crossover-class <array> (<mutable-collection>) open;
    define crossover-class <vector> (<array>) open;
     define crossover-class <stretchy-vector>
		(<vector>, <stretchy-collection>) open;
     define crossover-class <simple-object-vector> (<vector>);
    define crossover-class <deque>
		(<mutable-sequence>, <stretchy-collection>) open;
    define crossover-class <string> (<mutable-sequence>) open;
     define crossover-class <unicode-string> (<string>, <vector>);
     define crossover-class <byte-string> (<string>, <vector>);
    define crossover-class <list> (<mutable-sequence>);
     define crossover-class <pair> (<list>);
     define crossover-class <empty-list> (<list>);
   define crossover-class <range> (<sequence>);
