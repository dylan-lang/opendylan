Module:    gtk-support
Author:    Hannes Mehnert, Andreas Bogk
Copyright: (C) 2007,.  All rights reserved.

define constant $G-TYPE-INVALID = as(<machine-word>, 0);
define constant $G-TYPE-NONE = as(<machine-word>, 4);
define constant $G-TYPE-CHAR = as(<machine-word>, 12);
define constant $G-TYPE-UCHAR = as(<machine-word>, 16);
define constant $G-TYPE-BOOLEAN = as(<machine-word>, 20);
define constant $G-TYPE-INT = as(<machine-word>, 24);
define constant $G-TYPE-UINT = as(<machine-word>, 28);
define constant $G-TYPE-LONG = as(<machine-word>, 32);
define constant $G-TYPE-ULONG = as(<machine-word>, 36);
define constant $G-TYPE-INT64 = as(<machine-word>, 40);
define constant $G-TYPE-UINT64 = as(<machine-word>, 44);
define constant $G-TYPE-ENUM = as(<machine-word>, 48);
define constant $G-TYPE-FLAGS = as(<machine-word>, 52);
define constant $G-TYPE-FLOAT = as(<machine-word>, 56);
define constant $G-TYPE-DOUBLE = as(<machine-word>, 60);
define constant $G-TYPE-STRING = as(<machine-word>, 64);
define constant $G-TYPE-POINTER = as(<machine-word>, 68);
define constant $G-TYPE-BOXED = as(<machine-word>, 72);
define constant $G-TYPE-PARAM = as(<machine-word>, 76);
define constant $G-TYPE-OBJECT = as(<machine-word>, 80);

define C-function g-type-from-instance
  input parameter instance :: <GTypeInstance>;
  result type :: <GType>;
  c-name: "g_type_from_instance";
end;

define C-function g-value-type
  input parameter instance :: <GValue>;
  result type :: <GType>;
  c-name: "g_value_type";
end;

define C-function g-is-value
  input parameter instance :: <GValue>;
  result type :: <C-int>;
  c-name: "g_is_value";
end;

define C-function gtk-widget-get-window
  input parameter widget :: <GtkWidget>;
  result window :: <GdkWindow>;
  c-name: "gtk_widget_get_window";
end;

define C-function gtk-widget-get-state
  input parameter widget :: <GtkWidget>;
  result state :: <C-int>;
  c-name: "gtk_widget_get_state";
end;

define C-function gtk-widget-get-allocation
  input parameter widget :: <GtkWidget>;
  result allocation :: <GtkAllocation>;
  c-name: "gtk_widget_get_allocation";
end;

define C-function gtk-dialog-get-vbox
  input parameter dialog :: <GtkDialog>;
  result vbox :: <GtkWidget>;
  c-name: "gtk_dialog_get_vbox";
end;

define C-function gtk-dialog-get-action-area
  input parameter dialog :: <GtkDialog>;
  result vbox :: <GtkWidget>;
  c-name: "gtk_dialog_get_action_area";
end;

define C-function gtk-menu-shell-set-ignore-enter
  input parameter menu :: <GtkMenuShell>;
  input parameter enabled? :: <gboolean>;
  c-name: "gtk_menu_shell_set_ignore_enter";
end;

define C-function popup-gtk-menu
  input parameter menu :: <GtkMenu>;
  input parameter button :: <guint>;
  c-name: "popup_gtk_menu";
end;

define C-function gtk-set-button-time
  input parameter event :: <GdkEventButton>;
  c-name: "gtk_set_button_time";
end;

define macro with-gdk-lock
  { with-gdk-lock ?:body end }
 =>
  {  block()
       *holding-gdk-lock* > 0 | gdk-threads-enter();
       *holding-gdk-lock* := *holding-gdk-lock* + 1;
       ?body
     cleanup
       *holding-gdk-lock* := *holding-gdk-lock* - 1;
       *holding-gdk-lock* > 0 | gdk-threads-leave();
     end }
end;


define method make(type :: subclass(<GTypeInstance>), #rest args, 
                   #key address, #all-keys)
 => (result :: <GTypeInstance>)
  if(address)
    let instance = next-method(<GTypeInstance>, address: address);
    if (~ null-pointer?(instance))
      let g-type = g-type-from-instance(instance);
      let dylan-type = find-gtype(g-type);
      unless (dylan-type)
        
        error("Unknown GType %= encountered. Re-run melange or implement dynamic class generation.",
              as(<byte-string>, g-type-name(g-type)));
      end;
      let result = next-method(dylan-type, address: address);
      g-object-ref-sink(result);
      finalize-when-unreachable(result);
      result;
    else
      next-method();
    end
  else
    // possible route: convert #rest args into GParamSpec, call g_object_newv()
    error("Can't create GTypeInstance on my own from %=", type.debug-name);
  end if;
end method make;

define method finalize (instance :: <GTypeInstance>)
 => ();
  with-gdk-lock
    g-object-unref(instance)
  end with-gdk-lock;
end;

define function all-subclasses(x :: <class>)
  => (subclasses :: <collection>)
  apply(concatenate, x.direct-subclasses, 
        map(all-subclasses, x.direct-subclasses))
end;

define function find-gtype-by-name(name :: <string>)
  block(return)
    for(i in $all-gtype-instances)
      if(as-uppercase(i.debug-name) = as-uppercase(concatenate("<_", name, ">")))
        return(i)
      end if;
//    finally
//      error("Unknown GType %= encountered.", as(<byte-string>, name))
    end for;
  end block;
end function find-gtype-by-name;

define function find-gtype(g-type)
 => (type :: false-or(<class>));
  let dylan-type = element($gtype-table, g-type, default: #f);
  unless(dylan-type)
    let type-name = g-type-name(g-type);
    dylan-type := find-gtype-by-name(type-name);
    $gtype-table[g-type] := dylan-type;
  end unless;
  dylan-type
end function find-gtype;

define constant $all-gtype-instances = all-subclasses(<_GTypeInstance>);

define thread variable *holding-gdk-lock* = 0;

define function dylan-meta-marshaller (closure :: <GClosure>,
                                       return-value :: <GValue>,
                                       n-param-values :: <integer>,
                                       param-values :: <GValue>,
                                       invocation-hint :: <gpointer>,
                                       marshal-data :: <gpointer>)
  let values = #();
  for(i from 0 below n-param-values)

//    let address = integer-as-raw(param-values.raw-pointer-address.raw-as-integer + i * sizeof-gvalue());
//    let value* = make(<GValue>, address: address);

    let value = make-c-pointer(<GValue>,
                               primitive-machine-word-add
                                 (primitive-cast-pointer-as-raw
                                   (primitive-unwrap-c-pointer(param-values)),
                                  integer-as-raw
                                    (i * sizeof-gvalue())),
                               #[]);
    values := pair(g-value-to-dylan(value), values);
//    value*;
  end for;
  values := reverse!(values);
  *holding-gdk-lock* := *holding-gdk-lock* + 1;
  let res = apply(import-c-dylan-object(c-type-cast(<C-dylan-object>, marshal-data)), values);
  *holding-gdk-lock* := *holding-gdk-lock* - 1;
  if(return-value ~= null-pointer(<gvalue>))
    select(g-value-type(return-value))
      $G-TYPE-BOOLEAN => g-value-set-boolean(return-value,
                                             if(res) 1 else 0 end);
      $G-TYPE-NONE, $G-TYPE-INVALID => ;
      otherwise =>
        error("Unsupported GType in return from signal handler: %=.",
              g-value-type(return-value));
    end select;
  end if;
end;


define C-callable-wrapper _dylan-meta-marshaller of dylan-meta-marshaller
  parameter closure         :: <GClosure>;
  parameter return-value    :: <GValue>;
  parameter n-param-values  :: <guint>;
  parameter param-values    :: <GValue>;
  parameter invocation-hint :: <gpointer>;
  parameter marshal-data    :: <gpointer>;
  c-name: "foo";
end;

define C-function sizeof-gvalue
  result size :: <C-int>;
  c-name: "sizeof_gvalue";
end;

define C-function sizeof-gclosure
  result size :: <C-int>;
  c-name: "sizeof_gclosure";
end;



define function g-signal-connect(instance :: <GObject>, 
                                 signal :: <string>,
                                 function :: <function>,
                                 #key run-after? :: <boolean>)
  register-c-dylan-object(function);
  let closure = g-closure-new-simple(sizeof-gclosure(),
                                     null-pointer(<gpointer>));
  g-closure-set-meta-marshal
    (closure, export-c-dylan-object(function), _dylan-meta-marshaller);
  g-signal-connect-closure(instance, 
                           signal, 
                           closure,
                           if(run-after?) 1 else 0 end)
end function g-signal-connect;

define function initialize-gtk
    () => ()
  g-thread-init(null-pointer(<GThreadFunctions>));
  gdk-threads-init();
  with-gdk-lock
    let name = application-name();
    with-c-string (string = name)
      let string* = make(<C-string*>, element-count: 1);
      string*[0] := string;
      let string** = make(<char***>);
      string**[0] := string*;
      let int* = make(<C-int*>);
      int*[0] := 1;
      %gtk-init(int*, string**);
      destroy(string*);
      destroy(string**);
      destroy(int*)
    end;
  end;
  automatic-finalization-enabled?() := #t;
end function initialize-gtk;


// map GTK type IDs to Dylan classes
define table $gtype-table = {
                             $G-TYPE-CHAR         => <gchar>,
                             $G-TYPE-UCHAR        => <guchar>,
                             $G-TYPE-INT          => <gint>,
                             $G-TYPE-UINT         => <guint>,
                             $G-TYPE-LONG         => <glong>,
                             $G-TYPE-ULONG        => <gulong>,
                             $G-TYPE-INT64        => <gint64>,
                             $G-TYPE-UINT64       => <guint64>,
                             $G-TYPE-FLOAT        => <gfloat>,
                             $G-TYPE-DOUBLE       => <gdouble>,
                             $G-TYPE-STRING       => <gstring>,
                             $G-TYPE-POINTER      => <gpointer>
                             };

define function make-gdk-event(address)
 => (instance :: <C-void*>)
  let event = make(<GdkEventAny>, address: address);
  make(select(event.GdkEventAny-type)
         $GDK-NOTHING           => <GdkEventAny>;
         $GDK-DELETE            => <GdkEventAny>;
         $GDK-DESTROY           => <GdkEventAny>;
         $GDK-EXPOSE            => <GdkEventExpose>;
         $GDK-MOTION-NOTIFY     => <GdkEventMotion>;
         $GDK-BUTTON-PRESS      => <GdkEventButton>;
         $GDK-2BUTTON-PRESS     => <GdkEventButton>;
         $GDK-3BUTTON-PRESS     => <GdkEventButton>;
         $GDK-BUTTON-RELEASE    => <GdkEventButton>;
         $GDK-KEY-PRESS         => <GdkEventKey>;
         $GDK-KEY-RELEASE       => <GdkEventKey>;
         $GDK-ENTER-NOTIFY      => <GdkEventCrossing>;
         $GDK-LEAVE-NOTIFY      => <GdkEventCrossing>;
         $GDK-FOCUS-CHANGE      => <GdkEventFocus>;
         $GDK-CONFIGURE         => <GdkEventConfigure>;
         $GDK-MAP               => <GdkEventAny>;
         $GDK-UNMAP             => <GdkEventAny>;
         $GDK-PROPERTY-NOTIFY   => <GdkEventProperty>;
         $GDK-SELECTION-CLEAR   => <GdkEventSelection>;
         $GDK-SELECTION-REQUEST => <GdkEventSelection>;
         $GDK-SELECTION-NOTIFY  => <GdkEventSelection>;
         $GDK-PROXIMITY-IN      => <GdkEventProximity>;
         $GDK-PROXIMITY-OUT     => <GdkEventProximity>;
         $GDK-DRAG-ENTER        => <GdkEventDND>;
         $GDK-DRAG-LEAVE        => <GdkEventDND>;
         $GDK-DRAG-MOTION       => <GdkEventDND>;
         $GDK-DRAG-STATUS       => <GdkEventDND>;
         $GDK-DROP-START        => <GdkEventDND>;
         $GDK-DROP-FINISHED     => <GdkEventDND>;
         $GDK-CLIENT-EVENT      => <GdkEventClient>;
         $GDK-VISIBILITY-NOTIFY => <GdkEventAny>;
         $GDK-NO-EXPOSE         => <GdkEventNoExpose>;
         $GDK-SCROLL            => <GdkEventScroll>;
         $GDK-WINDOW-STATE      => <GdkEventWindowState>;
         $GDK-SETTING           => <GdkEventSetting>;
         $GDK-OWNER-CHANGE      => <GdkEventOwnerChange>;
         $GDK-GRAB-BROKEN       => <GdkEventGrabBroken>;
         otherwise              => <GdkEventAny>;
       end, address: address);
end;

define function g-value-to-dylan(instance :: <GValue>)
 => (dylan-instance);
  let g-type = g-value-type(instance);
  if(g-type ~= $G-TYPE-INVALID)
    let dylan-type = find-gtype(g-type);
    let address-thunk = curry(compose(pointer-address, g-value-peek-pointer),
                              instance);
    if(dylan-type & subtype?(dylan-type, <GTypeInstance>))
      make(dylan-type, address: address-thunk())
    else
      select(g-type)
        $G-TYPE-NONE    => #f;
        $G-TYPE-CHAR    => g-value-get-char(instance);
        $G-TYPE-UCHAR   => g-value-get-uchar(instance);
        $G-TYPE-BOOLEAN => (g-value-get-boolean(instance) = 1);
        $G-TYPE-INT     => g-value-get-int(instance);
        $G-TYPE-UINT    => g-value-get-uint(instance);
        $G-TYPE-LONG    => g-value-get-long(instance);
        $G-TYPE-ULONG   => g-value-get-ulong(instance);
        $G-TYPE-INT64   => g-value-get-int64(instance);
        $G-TYPE-UINT64  => g-value-get-uint64(instance);
        $G-TYPE-ENUM    => error("Can't handle $G-TYPE-ENUM yet.");
        $G-TYPE-FLAGS   => error("Can't handle $G-TYPE-FLAGS yet.");
        $G-TYPE-FLOAT   => g-value-get-float(instance);
        $G-TYPE-DOUBLE  => g-value-get-double(instance);
        $G-TYPE-STRING  => g-value-get-string(instance);
        $G-TYPE-POINTER => g-value-get-pointer(instance);
        $G-TYPE-BOXED   => #f;
        $G-TYPE-PARAM   => #f;
        $G-TYPE-OBJECT  => #f;
        gdk-event-get-type() 
          => make-gdk-event(address-thunk());
        gtk-tree-iter-get-type() 
          => make(<GtkTreeIter>, address: address-thunk());
        gtk-tree-path-get-type()
          => make(<GtkTreePath>, address: address-thunk());
        otherwise       => error("Unknown Gtype %=", g-type);
      end select;
    end if;
  end if;
end function g-value-to-dylan;

define method g-value-set-value (gvalue :: <GValue>, value :: <double-float>)
  g-value-init(gvalue, $G-TYPE-DOUBLE);
  g-value-set-double(gvalue, value);
end;
define method g-value-set-value (gvalue :: <GValue>, value :: <single-float>)
  g-value-init(gvalue, $G-TYPE-FLOAT);
  g-value-set-float(gvalue, value);
end;
define method g-value-set-value (gvalue :: <GValue>, value :: <integer>)
  g-value-init(gvalue, $G-TYPE-INT);
  g-value-set-int(gvalue, value);
end;
define method g-value-set-value (gvalue :: <GValue>, value :: <boolean>)
  g-value-init(gvalue, $G-TYPE-BOOLEAN);
  g-value-set-boolean(gvalue, if (value) 1 else 0 end);
end;
define method g-value-set-value (gvalue :: <GValue>, value :: <GTypeInstance>)
  g-value-init(gvalue, g-type-from-instance(value));
  g-value-set-object(gvalue, value);
end;

define method g-value-set-value (gvalue :: <GValue>, string :: <string>)
  g-value-init(gvalue, $G-TYPE-STRING);
  g-value-set-string(gvalue, string);
end;

define macro property-getter-definer
  { define property-getter ?:name :: ?type:name on ?class:name end }
  => { define method "@" ## ?name (object :: ?class) => (res)
         with-stack-structure (foo :: <GValue>)
           g-object-get-property(object, ?"name", foo);
           g-value-to-dylan(foo);
         end;
       end; 
  }
end;

define C-function g-value-nullify
  parameter gvalue :: <GValue>;
  c-name: "g_value_nullify";
end;

define macro property-setter-definer
  { define property-setter ?:name :: ?type:name on ?class:name end }
  => { define method "@" ## ?name ## "-setter" (res, object :: ?class) => (res)
         with-stack-structure (gvalue :: <GValue>)
           // FIXME: hack, because we cannot request initialization with zero
           // from with-stack-structure
           g-value-nullify(gvalue);
           g-value-set-value(gvalue, res);
           g-object-set-property(object, ?"name", gvalue);
         end;
         res;
       end; 
  }
end;
