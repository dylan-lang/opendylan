Module: dfmc-c-ffi
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//
// General errors
//
define program-error <unrecognized-clause>
  slot condition-definition-name, required-init-keyword: definition-name:;
  format-string "Unrecognized clause in the definition of %= - ignoring.";
  format-arguments definition-name;
end;

define program-error <invalid-c-name-value>
  slot condition-definition-name, init-keyword: definition-name:;
  slot condition-c-name-expression, init-keyword: c-name-expression:;
  format-string "The expression supplied for the c-name keyword in the "
    "definition of %= does not evaluate to a string constant.";
  format-arguments definition-name;
end;

define program-error <missing-c-name>
  slot condition-definition-name, required-init-keyword: definition-name:;
  format-string "Missing c-name keyword in the definition of %=.";
  format-arguments definition-name;
end;

define program-error <no-designator-superclass>
  slot condition-designator-name, required-init-keyword: designator-name:;
  format-string "The designator class %= does not have a superclass "
                "which is a designator class.";
  format-arguments designator-name;
end;

define program-error <invalid-pointer-type-name-value>
  slot condition-definition-name, required-init-keyword: definition-name:;
  slot condition-pointer-type-name-expression,
    required-init-keyword: pointer-type-name-expression:;
  format-string "Invalid pointer-type-name expression %=, in the "
                "definition of %= - ignoring.";
  format-arguments pointer-type-name-expression, definition-name;
end;

//
// C-address specific errors
//
define program-error <designator-not-a-pointer>
  slot condition-definition-name, init-keyword: definition-name:;
  slot condition-designator-name, init-keyword: designator-name:;
  format-string "The designator class %=, in the definition of "
                "C-address %=, is not a pointer designator class.";
  format-arguments designator-name, definition-name;
end;


//
// C-variable specific errors
//
define program-error <aggregate-designator-type>
  slot condition-definition-name, init-keyword: definition-name:;
  slot condition-designator-name, init-keyword: designator-name:;
  format-string "The designator class %=, in the definition of "
                "C-variable %=, is not an instantiable Dylan class "
                "- substituting <C-void*>.";
  format-arguments designator-name, definition-name;
end;

define program-error <invalid-c-variable-setter>
  slot condition-form-name, required-init-keyword: form-name:;
  slot condition-setter-expr, required-init-keyword: setter-expr:;
  format-string "Invalid expression %=, for the setter option in "
                "the definition of C-variable %= - ignoring.";
  format-arguments setter-expr, form-name;
end;


//
// C-pointer-type specific errors
//


//
// C-function specific errors
//
define program-error <c-function-syntax-error>
  format-string "Invalid syntax in C-function macro call - skipping.";
end;

define program-error <invalid-c-function-indirect-value>
  slot condition-definition-name, init-keyword: definition-name:;
  slot condition-indirect-expression, init-keyword: indirect-expression:;
  format-string "The expression supplied for the indirect keyword option "
                "in the definition of %= is not a boolean literal "
                "(#t or #f.)";
  format-arguments definition-name;
end;

define program-error <invalid-c-function-gf-method-value>
  slot condition-definition-name, init-keyword: definition-name:;
  slot condition-gf-method-expression, init-keyword: gf-method-expression:;
  format-string "The expression supplied for the generic-function-method "
                "keyword option in the definition of %= is not a boolean "
                "literal (#t or #f.)";
  format-arguments definition-name;
end;

define program-error <no-c-name-or-indirect-option>
  slot condition-definition-name, init-keyword: definition-name:;
  format-string "No c-name or indirect option supplied in the "
                "definition of C-function %=.";
  format-arguments definition-name;
end;

define program-error <conflicting-c-name-and-indirect-options>
  slot condition-definition-name, init-keyword: definition-name:;
  format-string "Conlicting c-name and indirect options in the definition "
                "of C-function %= - ignoring c-name.";
  format-arguments definition-name;
end;


//
// C-callable-wrapper specific errors
//
define program-error <invalid-c-callable-wrapper-export-value>
  slot condition-definition-name, init-keyword: definition-name:;
  slot condition-export-expression, init-keyword: export-expression:;
  format-string "The expression supplied for the export keyword option "
                "in the definition of %= is not a boolean constant.";
  format-arguments definition-name;
end;


//
// C-function and C-callable-wrapper errors
//
define program-error <output-parameter-not-a-pointer>
  slot condition-designator-name, init-keyword: designator-name:;
  slot condition-parameter-name, init-keyword: parameter-name:;
  format-string "The designator class %=, for output parameter %= "
                "is not a pointer designator class.";
  format-arguments designator-name, parameter-name;
end;

define program-error <input-output-parameter-not-a-pointer>
  slot condition-designator-name, init-keyword: designator-name:;
  slot condition-parameter-name, init-keyword: parameter-name:;
  format-string "The designator class %=, for input output parameter %= "
                "is not a pointer designator class.";
  format-arguments designator-name, parameter-name;
end;

define program-error <multiple-return-clauses>
  slot condition-definition-name, required-init-keyword: definition-name:;
  format-string "Extra return clause in the definition of %= - ignoring.";
  format-arguments definition-name;
end;

define program-error <invalid-c-modifiers-value>
  slot condition-definition-name, init-keyword: definition-name:;
  slot condition-modifiers-expression, init-keyword: modifiers-expression:;
  format-string "The expression supplied for the c-modifiers keyword "
                "option in the definition of %= is not a string constant.";
  format-arguments definition-name;
end;


//
// C-struct / C-union specific errors
//
define program-error <invalid-address-getter-value>
  slot condition-definition-name, required-init-keyword: definition-name:;
  slot condition-address-getter-expression,
    required-init-keyword: address-getter-expression:;
  format-string "Invalid address-getter expression %=, in the "
                "definition of %= - ignoring.";
  format-arguments address-getter-expression, definition-name;
end program-error;

define program-error <invalid-getter-value>
  slot condition-definition-name, required-init-keyword: definition-name:;
  slot condition-getter-expression, required-init-keyword: getter-expression:;
  format-string "Invalid getter expression %=, in the "
                "definition of %= - ignoring.";
  format-arguments getter-expression, definition-name;
end program-error;

define program-error <invalid-setter-value>
  slot condition-definition-name, required-init-keyword: definition-name:;
  slot condition-setter-expression, required-init-keyword: setter-expression:;
  format-string "Invalid setter expression %=, in the "
                "definition of %= - ignoring.";
  format-arguments setter-expression, definition-name;
end program-error;

define program-error <invalid-constant-value>
  slot condition-definition-name, required-init-keyword: definition-name:;
  slot condition-constant-expression, required-init-keyword: constant-expression:;
  format-string "Invalid constant expression %=, in the "
                "definition of %= - ignoring.";
  format-arguments constant-expression, definition-name;
end program-error;

define program-error <constant-setter-value>
  slot condition-definition-name, required-init-keyword: definition-name:;
  slot condition-setter-expression, required-init-keyword: setter-expression:;
  format-string "Invalid setter expression %= on a constant slot, in the "
                "definition of %= - ignoring.";
  format-arguments setter-expression, definition-name;
end program-error;

define program-error <missing-length-keyword-option>
  slot condition-definition-name, required-init-keyword: definition-name:;
  format-string "Missing length keyword option in array slot in the "
                "definition of %=.";
  format-arguments slot-name, definition-name;
end program-error;

define program-error <missing-width-keyword-option>
  slot condition-definition-name, required-init-keyword: definition-name:;
  format-string "Missing width keyword option in bitfield slot in the "
                "definition of %=.";
  format-arguments slot-name, definition-name;
end program-error;

define program-error <invalid-array-slot-bounds>
  slot condition-definition-name, required-init-keyword: definition-name:;
  slot condition-length-expression, required-init-keyword: length-expression:;
  format-string "Invalid length expression %=, in array slot in "
                "the definition of %=.";
  format-arguments length-expression, slot-name, definition-name;
end program-error;

define program-error <invalid-bitfield-slot-width>
  slot condition-definition-name, required-init-keyword: definition-name:;
  slot condition-width-expression, required-init-keyword: width-expression:;
  format-string "Invalid width expression %=, in bitfield slot in "
                "the definition of %=.";
  format-arguments width-expression, slot-name, definition-name;
end program-error;

define program-error <invalid-pack-value>
  slot condition-pack-expression, required-init-keyword: pack-expression:;
  format-string "Invalid expression %=, given for pack option.";
  format-arguments pack-expression;
end;

//
// C-mapped-subtype specific errors
//
define program-error <multiple-export-mappings>
  slot condition-definition-name, required-init-keyword: definition-name:;
  format-string "Multiple export mappings in the definition of %=.";
  format-arguments definition-name;
end;

define program-error <multiple-import-mappings>
  slot condition-definition-name, required-init-keyword: definition-name:;
  format-string "Multiple import mappings in the definition of %=.";
  format-arguments definition-name;
end;

define program-error <missing-import-function>
  slot condition-definition-name, required-init-keyword: definition-name:;
  format-string "No import function in import clause in the "
                "definition of %= - ignoring clause.";
  format-arguments definition-name;
end;

define program-error <missing-export-function>
  slot condition-definition-name, required-init-keyword: definition-name:;
  format-string "No export function in export clause in the "
                "definition of %= - ignoring clause.";
  format-arguments definition-name;
end;

define program-error <unresolved-import-type>
  slot condition-definition-name, required-init-keyword: definition-name:;
  format-string "Cannot resolve import type in the definition of %="
                " - using <object> instead.";
  format-arguments definition-name;
end;

define program-error <unresolved-export-type>
  slot condition-definition-name, required-init-keyword: definition-name:;
  format-string "Cannot resolve export type in the definition of %="
                " - using <object> instead.";
  format-arguments definition-name;
end;


//
// Unresolved designator errors
//
define program-error <unresolved-struct-slot-type>
  slot condition-form-name, required-init-keyword: form-name:;
  slot condition-type-name, required-init-keyword: type-name:;
  slot condition-other-info, required-init-keyword: other-info:;
  format-string "Unresolved struct slot type, %=, in the definition of %=.";
  format-arguments type-name, form-name;
end;

define program-error <unresolved-union-slot-type>
  slot condition-form-name, required-init-keyword: form-name:;
  slot condition-type-name, required-init-keyword: type-name:;
  slot condition-other-info, required-init-keyword: other-info:;
  format-string "Unresolved union slot type, %=, in the definition of %=.";
  format-arguments type-name, form-name;
end;

define program-error <unresolved-c-function-parameter-type>
  slot condition-form-name, required-init-keyword: form-name:;
  slot condition-type-name, required-init-keyword: type-name:;
  slot condition-other-info, required-init-keyword: other-info:;
  format-string "Unresolved C-function parameter type, %=, in "
                "the definition of %=.";
  format-arguments type-name, form-name;
end;

define program-error <unresolved-c-function-result-type>
  slot condition-form-name, required-init-keyword: form-name:;
  slot condition-type-name, required-init-keyword: type-name:;
  slot condition-other-info, required-init-keyword: other-info:;
  format-string "Unresolved C-function result type, %=, in the "
                "definition of %=.";
  format-arguments type-name, form-name;
end;

define program-error <unresolved-c-callable-result-type>
  slot condition-form-name, required-init-keyword: form-name:;
  slot condition-type-name, required-init-keyword: type-name:;
  slot condition-other-info, required-init-keyword: other-info:;
  format-string "Unresolved C-callable-wrapper result type, %=, in "
                "the definition of %=.";
  format-arguments type-name, form-name;
end;

define program-error <unresolved-c-callable-parameter-type>
  slot condition-form-name, required-init-keyword: form-name:;
  slot condition-type-name, required-init-keyword: type-name:;
  slot condition-other-info, required-init-keyword: other-info:;
  format-string "Unresolved C-callable-wrapper parameter type, %=, in "
                "the definition of %=.";
  format-arguments type-name, form-name;
end;

define program-error <unresolved-c-pointer-type-referenced-type>
  slot condition-form-name, required-init-keyword: form-name:;
  slot condition-type-name, required-init-keyword: type-name:;
  slot condition-other-info, required-init-keyword: other-info:;
  format-string "Cannot resolve referenced type %=, in "
                "C-pointer-type %= - substituting <C-void*>.";
  format-arguments referenced-type, variable-name;
end program-error;

define program-error <unresolved-designator>
  slot condition-type-name, required-init-keyword: type-name:;
  format-string "Unresolved designator class %=, in the definition of %=.";
  format-arguments type-name, form-name;
end;

define method generate-unresolved-designator-error
    (type :: <fragment>, object-name, kind, other-info)
  let condition
    = macro-case (kind)
        { struct } => <unresolved-struct-slot-type>;
        { union } => <unresolved-union-slot-type>;
        { c-function-parameter } => <unresolved-c-function-parameter-type>;
        { c-function-result } => <unresolved-c-function-result-type>;
        { c-callable-parameter } => <unresolved-c-callable-parameter-type>;
        { c-callable-result } => <unresolved-c-callable-result-type>;
        { ?x:* } => <unresolved-designator>;
      end;
  note(condition,
       source-location: fragment-source-location(type),
       form-name: object-name,
       type-name: type,
       other-info: other-info);
end;
