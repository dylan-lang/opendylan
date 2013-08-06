module: gobject-properties
synopsis: generated bindings for the GObject library
copyright: See LICENSE file in this distribution.

define property-setter target-property :: <C-string> on <GBinding> end;
define property-getter target-property :: <C-string> on <GBinding> end;
define property-setter target :: <GObject> on <GBinding> end;
define property-getter target :: <GObject> on <GBinding> end;
define property-setter source-property :: <C-string> on <GBinding> end;
define property-getter source-property :: <C-string> on <GBinding> end;
define property-setter source :: <GObject> on <GBinding> end;
define property-getter source :: <GObject> on <GBinding> end;
define property-setter flags :: <GBindingFlags> on <GBinding> end;
define property-getter flags :: <GBindingFlags> on <GBinding> end;
