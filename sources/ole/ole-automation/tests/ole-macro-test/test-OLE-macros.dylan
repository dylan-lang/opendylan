Module: ole-macro-test
Creator: created by 'motley c:\users\sethml\Projects/bin/ole-macro-test-0-0-0.TLB' at 19980702T210408Z.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/* Pointer definitions: */

/* Type library: test OLE macros version 0.0
 * GUID: {94CCDC4A-92AD-11D1-9A5E-006097C90313}
 */


/* COM class: test OLE macros version 0.0
 * GUID: {94CCDC4A-92AD-11D1-9A5E-006097C90313}
 */
define constant $test-OLE-macros-class-id = as(<REFCLSID>, 
        "{94CCDC4A-92AD-11D1-9A5E-006097C90313}");
define function make-test-OLE-macros () => (default-interface :: 
        <disp-type-info-1>, interface-2 :: <disp-type-info-2>, interface-3 
        :: <disp-type-info-3>, interface-4 :: <disp-type-info-4>)
  let default-interface = make(<disp-type-info-1>, class-id: 
        $test-OLE-macros-class-id);
  values(default-interface,
         make(<disp-type-info-2>, disp-interface: default-interface),
         make(<disp-type-info-3>, disp-interface: default-interface),
         make(<disp-type-info-4>, disp-interface: default-interface))
end function make-test-OLE-macros;


/* Dispatch interface: disp-type-info-2 version 0.0
 * GUID: {94CCDC4B-92AD-11D1-9A5E-006097C90313}
 * Description: disp-type-info-2 for testing purposes 
 */
define dispatch-client <disp-type-info-2>
  uuid "{94CCDC4B-92AD-11D1-9A5E-006097C90313}";
  property disp-type-info-2/boolean :: <boolean>, name: "boolean", disp-id: 
        12288;
  property disp-type-info-2/character :: <character>, name: "character", 
        disp-id: 12289;
  member-function disp-type-info-2/does-string-equal-hello? (arg-string :: 
        <string>) => (arg-result :: <boolean>), name: 
        "does-string-equal-hello?", disp-id: 24576;
end dispatch-client <disp-type-info-2>;


/* Dispatch interface: disp-type-info-1 version 0.0
 * GUID: {00020400-0000-0000-C000-000000000046}
 * Description: disp-type-info-1 for testing purposes 
 */
define dispatch-client <disp-type-info-1>
  uuid "{00020400-0000-0000-C000-000000000046}";
  property disp-type-info-1/integer :: type-union(<integer>, 
        <machine-word>), name: "integer", disp-id: 12288;
  member-function disp-type-info-1/double-internal-integer-value () => (), 
        name: "double-internal-integer-value", disp-id: 24576;
end dispatch-client <disp-type-info-1>;


/* Dispatch interface: disp-type-info-3 version 0.0
 * GUID: {94CCDC4C-92AD-11D1-9A5E-006097C90313}
 * Description: disp-type-info-3 for testing purposes 
 */
define dispatch-client <disp-type-info-3>
  uuid "{94CCDC4C-92AD-11D1-9A5E-006097C90313}";
  property disp-type-info-3/another-integer :: type-union(<integer>, 
        <machine-word>), name: "another-integer", disp-id: 12288;
  member-function disp-type-info-3/multiply (arg-arg1 :: 
        type-union(<integer>, <machine-word>), arg-arg2 :: 
        type-union(<integer>, <machine-word>)) => (arg-result :: 
        type-union(<integer>, <machine-word>)), name: "multiply", disp-id: 
        24576;
end dispatch-client <disp-type-info-3>;


/* Dispatch interface: disp-type-info-4 version 0.0
 * GUID: {94CCDC4D-92AD-11D1-9A5E-006097C90313}
 */
define dispatch-client <disp-type-info-4>
  uuid "{94CCDC4D-92AD-11D1-9A5E-006097C90313}";
  property disp-type-info-4/another-integer :: type-union(<integer>, 
        <machine-word>), name: "another-integer", disp-id: 12288;
  property disp-type-info-4/id :: type-union(<integer>, <machine-word>), 
        name: "id", disp-id: 12289;
  member-function disp-type-info-4/multiply (arg-arg1 :: 
        type-union(<integer>, <machine-word>), arg-arg2 :: 
        type-union(<integer>, <machine-word>)) => (arg-result :: 
        type-union(<integer>, <machine-word>)), name: "multiply", disp-id: 
        24576;
end dispatch-client <disp-type-info-4>;
