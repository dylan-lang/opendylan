module: cairo
synopsis: Support functions for the Cairo library.
copyright: See LICENSE file in this distribution.

define method g-value-to-dylan-helper
    (type == #"CairoContext", address)
 => (dylan-instance :: <C-void*>)
  make(<CairoContext>, address: address);
end method g-value-to-dylan-helper;
