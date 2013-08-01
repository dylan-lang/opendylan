module: atk-properties
synopsis: generated bindings for the Atk library
copyright: See LICENSE file in this distribution.

define property-setter target :: <GValueArray> on <AtkRelation> end;
define property-getter target :: <GValueArray> on <AtkRelation> end;
define property-setter relation-type :: <AtkRelationType> on <AtkRelation> end;
define property-getter relation-type :: <AtkRelationType> on <AtkRelation> end;
define property-setter accessible-value :: <C-double> on <AtkObject> end;
define property-getter accessible-value :: <C-double> on <AtkObject> end;
define property-setter accessible-table-summary :: <AtkObject> on <AtkObject> end;
define property-getter accessible-table-summary :: <AtkObject> on <AtkObject> end;
define property-setter accessible-table-row-header :: <AtkObject> on <AtkObject> end;
define property-getter accessible-table-row-header :: <AtkObject> on <AtkObject> end;
define property-setter accessible-table-row-description :: <C-string> on <AtkObject> end;
define property-getter accessible-table-row-description :: <C-string> on <AtkObject> end;
define property-setter accessible-table-column-header :: <AtkObject> on <AtkObject> end;
define property-getter accessible-table-column-header :: <AtkObject> on <AtkObject> end;
define property-setter accessible-table-column-description :: <C-string> on <AtkObject> end;
define property-getter accessible-table-column-description :: <C-string> on <AtkObject> end;
define property-setter accessible-table-caption-object :: <AtkObject> on <AtkObject> end;
define property-getter accessible-table-caption-object :: <AtkObject> on <AtkObject> end;
define property-setter accessible-table-caption :: <C-string> on <AtkObject> end;
define property-getter accessible-table-caption :: <C-string> on <AtkObject> end;
define property-setter accessible-role :: <C-signed-int> on <AtkObject> end;
define property-getter accessible-role :: <C-signed-int> on <AtkObject> end;
define property-setter accessible-parent :: <AtkObject> on <AtkObject> end;
define property-getter accessible-parent :: <AtkObject> on <AtkObject> end;
define property-setter accessible-name :: <C-string> on <AtkObject> end;
define property-getter accessible-name :: <C-string> on <AtkObject> end;
define property-getter accessible-hypertext-nlinks :: <C-signed-int> on <AtkObject> end;
define property-setter accessible-description :: <C-string> on <AtkObject> end;
define property-getter accessible-description :: <C-string> on <AtkObject> end;
define property-getter accessible-component-mdi-zorder :: <C-signed-int> on <AtkObject> end;
define property-getter accessible-component-layer :: <C-signed-int> on <AtkObject> end;
define property-getter start-index :: <C-signed-int> on <AtkHyperlink> end;
define property-getter selected-link :: <C-boolean> on <AtkHyperlink> end;
define property-getter number-of-anchors :: <C-signed-int> on <AtkHyperlink> end;
define property-getter end-index :: <C-signed-int> on <AtkHyperlink> end;
