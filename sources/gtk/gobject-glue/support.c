#include <glib-object.h>

GType g_type_from_instance(GTypeInstance *instance) {
  return G_TYPE_FROM_INSTANCE(instance);
}

GType g_value_type(GValue *gvalue) {
  return G_VALUE_TYPE(gvalue);
}

int sizeof_gvalue() {
  return sizeof(GValue);
}

int sizeof_gclosure() {
  return sizeof(GClosure);
}

int g_is_value(GValue *value) {
  return G_IS_VALUE(value);
}

void g_value_nullify(GValue *gvalue) {
  char *foo = (char*)gvalue;
  int i;

  for (i = 0; i < sizeof(GValue); i++, foo++) {
    *foo = 0;
  }
}
