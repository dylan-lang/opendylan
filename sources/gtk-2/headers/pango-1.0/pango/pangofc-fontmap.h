/* Pango
 * pangofc-fontmap.h: Base fontmap type for fontconfig-based backends
 *
 * Copyright (C) 2003 Red Hat Software
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#ifndef __PANGO_FC_FONT_MAP_H__
#define __PANGO_FC_FONT_MAP_H__

#include <fontconfig/fontconfig.h>
#include <pango/pango-fontmap.h>
#include <pango/pangofc-decoder.h>
#include <pango/pangofc-font.h>

G_BEGIN_DECLS

#define PANGO_TYPE_FC_FONT_MAP              (pango_fc_font_map_get_type ())
#define PANGO_FC_FONT_MAP(object)           (G_TYPE_CHECK_INSTANCE_CAST ((object), PANGO_TYPE_FC_FONT_MAP, PangoFcFontMap))
#define PANGO_IS_FC_FONT_MAP(object)        (G_TYPE_CHECK_INSTANCE_TYPE ((object), PANGO_TYPE_FC_FONT_MAP))

typedef struct _PangoFcFontMap        PangoFcFontMap;
typedef struct _PangoFcFontMapClass   PangoFcFontMapClass;
typedef struct _PangoFcFontMapPrivate PangoFcFontMapPrivate;

#ifdef PANGO_ENABLE_BACKEND

#define PANGO_FC_FONT_MAP_CLASS(klass)      (G_TYPE_CHECK_CLASS_CAST ((klass), PANGO_TYPE_FC_FONT_MAP, PangoFcFontMapClass))
#define PANGO_IS_FC_FONT_MAP_CLASS(klass)   (G_TYPE_CHECK_CLASS_TYPE ((klass), PANGO_TYPE_FC_FONT_MAP))
#define PANGO_FC_FONT_MAP_GET_CLASS(obj)    (G_TYPE_INSTANCE_GET_CLASS ((obj), PANGO_TYPE_FC_FONT_MAP, PangoFcFontMapClass))

/**
 * PangoFcFontMap:
 * 
 * #PangoFcFontMap is a base class for font map implementations
 * using the FontConfig and FreeType libraries. To create a new
 * backend using Fontconfig and FreeType, you derive from this class
 * and implement a new_font() virtual function that creates an
 * instance deriving from #PangoFcFont.
 **/
struct _PangoFcFontMap
{
  PangoFontMap parent_instance;

  PangoFcFontMapPrivate *priv;
};

/**
 * PangoFcFontMapClass:
 * @default_substitute: Substitutes in default values for
 *  unspecified fields in a #FcPattern. This will be called
 *  prior to creating a font for the pattern. May be %NULL.
 * @new_font: Creates a new #PangoFcFont for the specified
 *  pattern of the appropriate type for this font map. The
 *  @pattern argument must be passed to the "pattern" property
 *  of #PangoFcFont when you call g_object_new()
 *
 * Class structure for #PangoFcFontMap.
 **/
struct _PangoFcFontMapClass
{
  /*< private >*/
  PangoFontMapClass parent_class;

  /*< public >*/
  void         (*default_substitute) (PangoFcFontMap   *fontmap,
			              FcPattern        *pattern);
  PangoFcFont  *(*new_font)          (PangoFcFontMap  *fontmap,
			              FcPattern       *pattern);

  /*< private >*/

  /* Padding for future expansion */
  void (*_pango_reserved1) (void);
  void (*_pango_reserved2) (void);
  void (*_pango_reserved3) (void);
  void (*_pango_reserved4) (void);
};

PangoContext * pango_fc_font_map_create_context (PangoFcFontMap *fcfontmap);
void           pango_fc_font_map_cache_clear    (PangoFcFontMap *fcfontmap);
void           pango_fc_font_map_shutdown       (PangoFcFontMap *fcfontmap);

#endif

GType pango_fc_font_map_get_type (void);

/**
 * PangoFcDecoderFindFunc:
 * @pattern: a fully resolved #FcPattern specifying the font on the system
 * @user_data: user data passed to pango_fc_font_map_add_decoder_find_func()
 * 
 * Callback function passed to pango_fc_font_map_add_decoder_find_func().
 * 
 * Return value: a new reference to a custom decoder for this pattern,
 *  or %NULL if the default decoder handling should be used.
 **/
typedef PangoFcDecoder * (*PangoFcDecoderFindFunc) (FcPattern *pattern,
						    gpointer   user_data);

void pango_fc_font_map_add_decoder_find_func (PangoFcFontMap        *fcfontmap,
					      PangoFcDecoderFindFunc findfunc,
					      gpointer               user_data,
					      GDestroyNotify         dnotify);

PangoFontDescription *pango_fc_font_description_from_pattern (FcPattern *pattern,
							      gboolean   include_size);

G_END_DECLS

#endif /* __PANGO_FC_FONT_MAP_H__ */
