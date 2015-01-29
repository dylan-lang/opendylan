Module:   dfmc-definitions
Synopsis: Shared function processing.
Author:   Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define dood-class <function-defining-form> (<variable-defining-form>) /* abstract */
  lazy constant slot form-signature,
    required-init-keyword: signature:;
end;

define packed-slots form-properties (<function-defining-form>, <variable-defining-form>)
end packed-slots;

define generic form-signature
    (form :: <function-defining-form>) => (signature);

// This is currently not used, but it's part of the defined API, so define it..
// The class of "define function" definitions.
define class <function-definition> (<function-defining-form>)
end;

define method form-define-word (form :: <function-definition>) => (w :: <symbol>)
  #"function";
end;


define &macro function-definer
  { define ?mods:* function ?:name ?signature-and-body:* end }
    => with-expansion-source-location (fragment-record(form),
                                       fragment-source-position(form))
         #{ define ?mods constant ?name = method ?signature-and-body end }
       end;
end &macro;
