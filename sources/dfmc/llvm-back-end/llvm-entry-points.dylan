Module: dfmc-llvm-back-end
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              Additional code is Copyright 2009-2012 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// References

// Code

define constant $number-xeps = 10;
define constant $number-meps = $number-xeps;

define constant $xep-string             = "xep";
define constant $rest-xep-string        = "rest_xep";
define constant $rest-key-xep-string    = "rest_key_xep";
define constant $gf-xep-string          = "gf_xep";
define constant $key-mep-string         = "key_mep";
define constant $gf-optional-xep-string = "gf_optional_xep";

define method emit-name-internal
    (back-end :: <llvm-back-end>, m, ep :: <&lambda-xep>)
 => (name :: <string>);
  let req-size = ^entry-point-number-required(ep);
  let size
    = if (^entry-point-key?(ep))
        req-size + ^entry-point-number-keys(ep) + 1
      else
        req-size
      end if;
  format-to-string(if (size < $number-xeps) "%s_%d" else "%s" end,
                   if (^entry-point-key?(ep))
                     $rest-key-xep-string
                   elseif (^entry-point-rest?(ep))
                     $rest-xep-string
                   else
                     $xep-string
                   end if,
                   size)
end method;

define method emit-name-internal
    (back-end :: <llvm-back-end>, m, ep :: <&slot-accessor-xep>)
  ^entry-point-name(ep)
end method;

// This reflects the number of implementation args (required args plus
// maybe optionals vector) the GF takes.  Zero through this many
// implementation args are passed spread as separate C arguments by
// the dispatch engine routines; more will be passed as a single
// (hopefully) stack-allocated vector.

define constant $special-gf-engine-max-args = 7;

define method emit-name-internal
    (back-end :: <llvm-back-end>, m, ep :: <&generic-function-xep>)
 => (name :: <string>);
  let req-size :: <integer> = ^entry-point-number-required(ep);
  let optionals? = ^entry-point-optionals?(ep);
  let impargs :: <integer> = if (optionals?) req-size + 1 else req-size end;
  format-to-string(if (impargs <= $special-gf-engine-max-args)
                     "%s_%d"
                   else
                     "%s"
                   end,
                   if (optionals?)
                     $gf-optional-xep-string
                   else
                     $gf-xep-string
                   end if,
                   req-size)
end method;

define method emit-name-internal
    (back-end :: <llvm-back-end>, m, o :: <&keyword-method-mep>)
 => (name :: <string>);
  let size
    = ^entry-point-number-required(o) + ^entry-point-number-keys(o) + 1;
  format-to-string(if (size < $number-meps) "%s_%d" else "%s" end,
                   $key-mep-string, size)
end method;
