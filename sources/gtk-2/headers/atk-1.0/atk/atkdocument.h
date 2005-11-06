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

#ifndef __ATK_DOCUMENT_H__
#define __ATK_DOCUMENT_H__

#include <atk/atkobject.h>
#include <atk/atkutil.h>

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/*
 * The AtkDocument interface should be supported by any object that has an 
 * associated document object model (DOM). This interface provides the
 * standard mechanism allowing an assistive technology access to the DOM.
 */

#define ATK_TYPE_DOCUMENT                   (atk_document_get_type ())
#define ATK_IS_DOCUMENT(obj)                G_TYPE_CHECK_INSTANCE_TYPE ((obj), ATK_TYPE_DOCUMENT)
#define ATK_DOCUMENT(obj)                   G_TYPE_CHECK_INSTANCE_CAST ((obj), ATK_TYPE_DOCUMENT, AtkDocument)
#define ATK_DOCUMENT_GET_IFACE(obj)         (G_TYPE_INSTANCE_GET_INTERFACE ((obj), ATK_TYPE_DOCUMENT, AtkDocumentIface))

#ifndef _TYPEDEF_ATK_DOCUMENT_
#define _TYPEDEF_ATK_DOCUMENT_
typedef struct _AtkDocument AtkDocument;
#endif
typedef struct _AtkDocumentIface AtkDocumentIface;

struct _AtkDocumentIface
{
  GTypeInterface parent;
  G_CONST_RETURN gchar* ( *get_document_type) (AtkDocument              *document);
  gpointer              ( *get_document)      (AtkDocument              *document);

  AtkFunction pad1;
  AtkFunction pad2;
  AtkFunction pad3;
  AtkFunction pad4;
  AtkFunction pad5;
  AtkFunction pad6;
  AtkFunction pad7;
  AtkFunction pad8;
};

GType  atk_document_get_type             (void);

G_CONST_RETURN gchar* atk_document_get_document_type (AtkDocument   *document);
gpointer atk_document_get_document (AtkDocument   *document);

#ifdef __cplusplus
}
#endif /* __cplusplus */
#endif /* __ATK_DOCUMENT_H__ */
