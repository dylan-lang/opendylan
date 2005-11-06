/* ATK -  Accessibility Toolkit
 * Copyright 2001 Sun Microsystems Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#ifndef __ATK_UTIL_H__
#define __ATK_UTIL_H__

#include <atk/atkobject.h>

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#define ATK_TYPE_UTIL                   (atk_util_get_type ())
#define ATK_IS_UTIL(obj)                G_TYPE_CHECK_INSTANCE_TYPE ((obj), ATK_TYPE_UTIL)
#define ATK_UTIL(obj)                   G_TYPE_CHECK_INSTANCE_CAST ((obj), ATK_TYPE_UTIL, AtkUtil)
#define ATK_UTIL_CLASS(klass)                   (G_TYPE_CHECK_CLASS_CAST ((klass), ATK_TYPE_UTIL, AtkUtilClass))
#define ATK_IS_UTIL_CLASS(klass)                (G_TYPE_CHECK_CLASS_TYPE ((klass), ATK_TYPE_UTIL))
#define ATK_UTIL_GET_CLASS(obj)                 (G_TYPE_INSTANCE_GET_CLASS ((obj), ATK_TYPE_UTIL, AtkUtilClass))


#ifndef _TYPEDEF_ATK_UTIL_
#define _TYPEDEF_ATK_UTIL_
typedef struct _AtkUtil      AtkUtil;
typedef struct _AtkUtilClass AtkUtilClass;
typedef struct _AtkKeyEventStruct AtkKeyEventStruct;
#endif

/*
 * A focus tracker is a function which is called when an object 
 * receives focus.
 */
typedef void  (*AtkEventListener) (AtkObject*);
typedef void  (*AtkEventListenerInit) (void);
typedef gint  (*AtkKeySnoopFunc)  (AtkKeyEventStruct *event,
				   gpointer func_data);

struct _AtkKeyEventStruct {
  gint type;
  guint state;
  guint keyval;
  gint length;
  gchar *string;
  guint16 keycode;
  guint32 timestamp;	
};

/**
 *AtkKeyEventType:
 *@ATK_KEY_EVENT_PRESS: specifies a key press event
 *@ATK_KEY_EVENT_RELEASE: specifies a key release event
 *@ATK_KEY_EVENT_LAST_DEFINED: Not a valid value; specifies end of enumeration
 *
 *Specifies the type of a keyboard evemt.
 **/
typedef enum
{
  ATK_KEY_EVENT_PRESS,
  ATK_KEY_EVENT_RELEASE,
  ATK_KEY_EVENT_LAST_DEFINED
} AtkKeyEventType;

struct _AtkUtil
{
  GObject parent;
};

struct _AtkUtilClass
{
   GObjectClass parent;
   guint        (* add_global_event_listener)    (GSignalEmissionHook listener,
						  const gchar        *event_type);
   void         (* remove_global_event_listener) (guint               listener_id);
   guint	(* add_key_event_listener) 	 (AtkKeySnoopFunc     listener,
						  gpointer data);
   void         (* remove_key_event_listener)    (guint               listener_id);
   AtkObject*   (* get_root)                     (void);
   G_CONST_RETURN gchar* (* get_toolkit_name)    (void);
   G_CONST_RETURN gchar* (* get_toolkit_version) (void);
};
GType atk_util_get_type (void);

/**
 *AtkCoordType:
 *@ATK_XY_SCREEN: specifies xy coordinates relative to the screen
 *@ATK_XY_WINDOW: specifies xy coordinates relative to the widget's 
 * top-level window
 *
 *Specifies how xy coordinates are to be interpreted. Used by functions such
 *as atk_component_get_position() and atk_text_get_character_extents() 
 **/
typedef enum {
  ATK_XY_SCREEN,
  ATK_XY_WINDOW
}AtkCoordType;

/*
 * Adds the specified function to the list of functions to be called
 * when an object receives focus.
 */
guint    atk_add_focus_tracker     (AtkEventListener      focus_tracker);

/*
 * Removes the specified focus tracker from the list of function
 * to be called when any object receives focus
 */
void     atk_remove_focus_tracker  (guint                tracker_id);

/*
 * Specifies the function to be called for focus tracker initialization.
 * removal. This function should be called by an implementation of the
 * ATK interface if any specific work needs to be done to enable
 * focus tracking.
 */
void     atk_focus_tracker_init    (AtkEventListenerInit  add_function);

/*
 * Cause the focus tracker functions which have been specified to be
 * executed for the object.
 */
void     atk_focus_tracker_notify  (AtkObject            *object);

/*
 * Adds the specified function to the list of functions to be called
 * when an event of type event_type occurs.
 */
guint	atk_add_global_event_listener (GSignalEmissionHook listener,
				       const gchar        *event_type);

/*
 * Removes the specified event listener
 */
void	atk_remove_global_event_listener (guint listener_id);

/*
 * Adds the specified function to the list of functions to be called
 * when an keyboard event occurs.
 */
guint	atk_add_key_event_listener (AtkKeySnoopFunc listener, gpointer data);

/*
 * Removes the specified event listener
 */
void	atk_remove_key_event_listener (guint listener_id);

/*
 * Returns the root accessible container for the current application.
 */
AtkObject* atk_get_root(void);

AtkObject* atk_get_focus_object (void);

/*
 * Returns name string for the GUI toolkit.
 */
G_CONST_RETURN gchar *atk_get_toolkit_name (void);

/*
 * Returns version string for the GUI toolkit.
 */
G_CONST_RETURN gchar *atk_get_toolkit_version (void);

#ifdef __cplusplus
}
#endif /* __cplusplus */


#endif /* __ATK_UTIL_H__ */
