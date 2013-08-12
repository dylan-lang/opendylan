#include <gtk/gtk.h>

guint32 mytime = 0;
void gtk_set_button_time (GdkEventButton *button) {
  mytime = button->time;
}
  
void popup_gtk_menu (GtkMenu* menu, guint button) {
  gtk_menu_popup(menu, NULL, NULL, NULL, NULL, button, mytime);
}
