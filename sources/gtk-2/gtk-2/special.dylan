Module:        GTK-2
Synopsis:      Manually coded additions to the automatic translation.
Author:        Andy Armstrong
Copyright:     Copyright (c) 1999  Functional Objects, Inc.
Copyright:     Copyright (c) 2005  Daniel Brockman
License:       Functional Objects Library Public License Version 1.0
Dual-License:  GNU Lesser General Public License
Warranty:      Distributed WITHOUT WARRANTY OF ANY KIND

define C-pointer-type <GtkIMContextInfo***> => <GtkIMContextInfo**>;

define opaque-structure <_GtkAccelMap>;
define opaque-structure <_GtkAccelMapClass>;
define opaque-structure <_GtkActionGroupPrivate>;
define opaque-structure <_GtkActionPrivate>;
define opaque-structure <_GtkAlignmentPrivate>;
define opaque-structure <_GtkCalendar>;
define opaque-structure <_GtkCellEditable>;
define opaque-structure <_GtkCellLayout>;
define opaque-structure <_GtkCellRendererProgressPrivate>;
define opaque-structure <_GtkCellViewPrivate>;
define opaque-structure <_GtkClipboard>;
define opaque-structure <_GtkColorButtonPrivate>;
define opaque-structure <_GtkComboBoxEntryPrivate>;
define opaque-structure <_GtkComboBoxPrivate>;
define opaque-structure <_GtkEditable>;
define opaque-structure <_GtkEntryCompletionPrivate>;
define opaque-structure <_GtkExpanderPrivate>;
define opaque-structure <_GtkFileChooser>;
define opaque-structure <_GtkFileChooserButtonPrivate>;
define opaque-structure <_GtkFileChooserDialogPrivate>;
define opaque-structure <_GtkFileChooserWidgetPrivate>;
define opaque-structure <_GtkFileFilter>;
define opaque-structure <_GtkFileFolder>;
define opaque-structure <_GtkFileInfo>;
define opaque-structure <_GtkFilePath>;
define opaque-structure <_GtkFileSystem>;
define opaque-structure <_GtkFileSystemVolume>;
define opaque-structure <_GtkFontButtonPrivate>;
define opaque-structure <_GtkIMMulticontextPrivate>;
define opaque-structure <_GtkIconInfo>;
define opaque-structure <_GtkIconSet>;
define opaque-structure <_GtkIconSource>;
define opaque-structure <_GtkIconThemePrivate>;
define opaque-structure <_GtkIconViewPrivate>;
define opaque-structure <_GtkLabelSelectionInfo>;
define opaque-structure <_GtkMenuToolButtonPrivate>;
define opaque-structure <_GtkNotebookPage>;
define opaque-structure <_GtkPanedPrivate>;
define opaque-structure <_GtkRadioActionPrivate>;
define opaque-structure <_GtkRangeLayout>;
define opaque-structure <_GtkRangeStepTimer>;
define opaque-structure <_GtkRcContext>;
define opaque-structure <_GtkSeparatorToolItemPrivate>;
define opaque-structure <_GtkSettingsPropertyValue>;
define opaque-structure <_GtkTextBTree>;
define opaque-structure <_GtkTextLine>;
define opaque-structure <_GtkTextLineData>;
define opaque-structure <_GtkTextLogAttrCache>;
define opaque-structure <_GtkTextPendingScroll>;
define opaque-structure <_GtkTextWindow>;
define opaque-structure <_GtkThemeEngine>;
define opaque-structure <_GtkToggleActionPrivate>;
define opaque-structure <_GtkToggleToolButtonPrivate>;
define opaque-structure <_GtkToolButtonPrivate>;
define opaque-structure <_GtkToolItemPrivate>;
define opaque-structure <_GtkToolbarPrivate>;
define opaque-structure <_GtkTreeDragDest>;
define opaque-structure <_GtkTreeDragSource>;
define opaque-structure <_GtkTreeModel>;
define opaque-structure <_GtkTreeModelFilterPrivate>;
define opaque-structure <_GtkTreePath>;
define opaque-structure <_GtkTreeRowReference>;
define opaque-structure <_GtkTreeSortable>;
define opaque-structure <_GtkTreeViewPrivate>;
define opaque-structure <_GtkUIManagerPrivate>;
define opaque-structure <_GtkWindowGeometryInfo>;

/// Type coercion functions

// define macro gtk-type-cast-function-definer
//   { define gtk-type-cast-function ?name:name => ?type:name }
//  => { define inline function ?name
// 	  (pointer :: <C-pointer>) => (object :: ?type)
// 	pointer-cast(?type, pointer)
//       end }
// end macro gtk-type-cast-function-definer;

// define gtk-type-cast-function GTK-OBJECT        => <GtkObject*>;
// define gtk-type-cast-function GTK-CONTAINER     => <GtkContainer*>;
// define gtk-type-cast-function GTK-BOX           => <GtkBox*>;
// define gtk-type-cast-function GTK-WIDGET        => <GtkWidget*>;
// define gtk-type-cast-function GTK-ADJUSTMENT    => <GtkAdjustment*>;
// define gtk-type-cast-function GTK-FIXED         => <GtkFixed*>;
// define gtk-type-cast-function GTK-WINDOW        => <GtkWindow*>;
// define gtk-type-cast-function GTK-DRAWING-AREA  => <GtkDrawingArea*>;


/// Useful functions

define function initialize-gtk
    () => ()
  let name = application-name();
  with-c-string (string = name)
    let string* = make(<C-string*>, element-count: 1);
    string*[0] := string;
    let string** = make(<C-string**>);
    string**[0] := string*;
    let int* = make(<C-int*>);
    int*[0] := 1;
    gtk-init(int*, string**);
    destroy(string*);
    destroy(string**);
    destroy(int*)
  end
end function initialize-gtk;
