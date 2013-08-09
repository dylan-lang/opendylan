module: gobject-glue
copyright: See LICENSE file in this distribution.

define constant <gsize> = <C-unsigned-long>;
define constant <guint> = <C-unsigned-int>;
define constant <gpointer> = <C-void*>;
define constant <gchar> = <C-signed-char>;
define constant <guchar> = <C-unsigned-char>;
define constant <gint> = <C-signed-int>;
define constant <glong> = <C-signed-long>;
define constant <gulong> = <C-unsigned-long>;
define constant <gint64> = <C-signed-long>;
define constant <guint64> = <C-unsigned-long>;
define constant <gfloat> = <C-float>;
define constant <gdouble> = <C-double>;
define constant <guint32> = <C-unsigned-int>;
define constant <gint32> = <C-signed-int>;
define constant <gint8> = <C-signed-char>;
define constant <guint8> = <C-unsigned-char>;
define constant <gint16> = <C-signed-short>;
define constant <guint16> = <C-unsigned-short>;
define constant <gshort> = <C-signed-short>;
define constant <gushort> = <C-unsigned-short>;
define constant <gssize> = <C-signed-long>;
define constant <GQuark> = <guint32>;
define constant <GType> = <gsize>;
define C-pointer-type <GType*> => <GType>;

define constant $G-TYPE-INVALID = 0;
define constant $G-TYPE-NONE = 4;
define constant $G-TYPE-CHAR = 12;
define constant $G-TYPE-UCHAR = 16;
define constant $G-TYPE-BOOLEAN = 20;
define constant $G-TYPE-INT = 24;
define constant $G-TYPE-UINT = 28;
define constant $G-TYPE-LONG = 32;
define constant $G-TYPE-ULONG = 36;
define constant $G-TYPE-INT64 = 40;
define constant $G-TYPE-UINT64 = 44;
define constant $G-TYPE-ENUM = 48;
define constant $G-TYPE-FLAGS = 52;
define constant $G-TYPE-FLOAT = 56;
define constant $G-TYPE-DOUBLE = 60;
define constant $G-TYPE-STRING = 64;
define constant $G-TYPE-POINTER = 68;
define constant $G-TYPE-BOXED = 72;
define constant $G-TYPE-PARAM = 76;
define constant $G-TYPE-OBJECT = 80;


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
  g-value-set-boolean(gvalue, value);
end;
define method g-value-set-value (gvalue :: <GValue>, value :: <GTypeInstance>)
  g-value-init(gvalue, g-type-from-instance(value));
  g-value-set-object(gvalue, value);
end;

define method g-value-set-value (gvalue :: <GValue>, string :: <string>)
  g-value-init(gvalue, $G-TYPE-STRING);
  g-value-set-string(gvalue, string);
end;


define C-function g-closure-set-meta-marshal
  input parameter self :: <GClosure>;
  input parameter marshal_data_ :: <C-void*>;
  input parameter meta_marshal_ :: <C-function-pointer>;
  c-name: "g_closure_set_meta_marshal";
end;

define C-function gdk-threads-enter
  c-name: "gdk_threads_enter";
end;

define C-function gdk-threads-leave
  c-name: "gdk_threads_leave";
end;

define thread variable *holding-gdk-lock* = 0;

define macro with-gdk-lock
  { with-gdk-lock ?:body end }
 =>
  {  block()
       unless (*holding-gdk-lock* > 0) gdk-threads-enter() end;
       *holding-gdk-lock* := *holding-gdk-lock* + 1;
       ?body
     cleanup
       *holding-gdk-lock* := *holding-gdk-lock* - 1;
       unless (*holding-gdk-lock* > 0) gdk-threads-leave() end;
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
        format(*standard-error*, "Unknown GType %= encountered. Re-run melange or implement dynamic class generation.\n",
               as(<byte-string>, g-type-name(g-type)));
        dylan-type := <GObject>;
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
  end;
end;

define function all-subclasses (x :: <class>)
 => (subclasses :: <collection>)
  // Visit each node of the class tree
  let classes-to-visit = make(<deque>, size: 1, fill: x);
  let subclasses = #[];
  while (size(classes-to-visit) > 0)
    let current-class = pop(classes-to-visit);
    map(curry(push, classes-to-visit), current-class.direct-subclasses);
    subclasses := add(subclasses, current-class);
  end while;
  subclasses
end;

define function find-gtype-by-name(name :: <string>)
  block(return)
    let dylan-name = as-uppercase(concatenate("<", name, ">"));
    for(i in all-gtype-instances())
      if(as-uppercase(i.debug-name) = dylan-name)
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

define variable $all-gtype-instances = #[];

define function all-gtype-instances () => (instances :: <collection>)
  if (empty?($all-gtype-instances))
    $all-gtype-instances := all-subclasses(<GTypeInstance>);
  end if;
  $all-gtype-instances
end function all-gtype-instances;

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
                             $G-TYPE-STRING       => <GString>,
                             $G-TYPE-POINTER      => <gpointer>
                             };

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
      $G-TYPE-BOOLEAN => g-value-set-boolean(return-value, res);
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
                           run-after?);
end function g-signal-connect;

define open generic g-value-to-dylan-helper (id, address) => (dylan-instance);

define method g-value-to-dylan-helper (type, address) => (dylan-instance)
  error("Unknown type %=", type);
end method g-value-to-dylan-helper;

define function g-value-to-dylan (instance :: <GValue>)
 => (dylan-instance);
  let g-type = g-value-type(instance);
  if(g-type ~= $G-TYPE-INVALID)
    let dylan-type = find-gtype(g-type);
    let address-thunk = method () pointer-address(g-value-peek-pointer(instance)) end;
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
        g-type-from-name("GdkEvent") => g-value-to-dylan-helper(#"GdkEvent", address-thunk());
        otherwise       => g-value-to-dylan-helper(g-type, address-thunk());
      end select;
    end if;
  end if;
end function g-value-to-dylan;

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
