Module:   dfmc-conversion
Synopsis: The macro definition processor.
Author:   Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Macro models.

// Macro models are just constructed for consistency with other definitions.
// They contain no extra information above and beyond the corresponding
// definition object.

// The <&macro> class is defined earlier for use in the boot.

define compiler-sideways method compute-form-model-object 
    (form :: <macro-definition>, var :: <variable-name-fragment>) => (object)
  let macro-object = form-macro-object(form);
  // Fake some references.
  for (name in macro-referenced-names(macro-object))
    lookup-binding(name);
  end;
  make(<&macro>, 
       definition:   form,
       macro-object: macro-object);
end method;
