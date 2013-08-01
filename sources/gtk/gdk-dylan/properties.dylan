module: gdk-properties
synopsis: generated bindings for the Gdk library
copyright: See LICENSE file in this distribution.

define property-setter cursor :: <GdkCursor> on <GdkWindow> end;
define property-getter cursor :: <GdkCursor> on <GdkWindow> end;
define property-setter resolution :: <C-double> on <GdkScreen> end;
define property-getter resolution :: <C-double> on <GdkScreen> end;
define property-setter font-options :: <C-void*> on <GdkScreen> end;
define property-getter font-options :: <C-void*> on <GdkScreen> end;
define property-setter default-display :: <GdkDisplay> on <GdkDisplayManager> end;
define property-getter default-display :: <GdkDisplay> on <GdkDisplayManager> end;
define property-setter display :: <GdkDisplay> on <GdkDeviceManager> end;
define property-getter display :: <GdkDisplay> on <GdkDeviceManager> end;
define property-setter type :: <GdkDeviceType> on <GdkDevice> end;
define property-getter type :: <GdkDeviceType> on <GdkDevice> end;
define property-setter name :: <C-string> on <GdkDevice> end;
define property-getter name :: <C-string> on <GdkDevice> end;
define property-getter n-axes :: <C-unsigned-int> on <GdkDevice> end;
define property-setter input-source :: <GdkInputSource> on <GdkDevice> end;
define property-getter input-source :: <GdkInputSource> on <GdkDevice> end;
define property-setter input-mode :: <GdkInputMode> on <GdkDevice> end;
define property-getter input-mode :: <GdkInputMode> on <GdkDevice> end;
define property-setter has-cursor :: <C-boolean> on <GdkDevice> end;
define property-getter has-cursor :: <C-boolean> on <GdkDevice> end;
define property-setter display :: <GdkDisplay> on <GdkDevice> end;
define property-getter display :: <GdkDisplay> on <GdkDevice> end;
define property-setter device-manager :: <GdkDeviceManager> on <GdkDevice> end;
define property-getter device-manager :: <GdkDeviceManager> on <GdkDevice> end;
define property-getter associated-device :: <GdkDevice> on <GdkDevice> end;
define property-setter display :: <GdkDisplay> on <GdkCursor> end;
define property-getter display :: <GdkDisplay> on <GdkCursor> end;
define property-setter cursor-type :: <GdkCursorType> on <GdkCursor> end;
define property-getter cursor-type :: <GdkCursorType> on <GdkCursor> end;
define property-setter display :: <GdkDisplay> on <GdkAppLaunchContext> end;
define property-getter display :: <GdkDisplay> on <GdkAppLaunchContext> end;
