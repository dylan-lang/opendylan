Module:        Pango
Synopsis:      Manually coded additions to the automatic translation.
Copyright:     Copyright (C) 2005  Daniel Brockman
License:       Functional Objects Library Public License Version 1.0
Dual-License:  GNU Lesser General Public License
Warranty:      Distributed WITHOUT WARRANTY OF ANY KIND

define C-pointer-type <C-int**> => <C-int*>;
define C-pointer-type <PangoFontFace***> => <PangoFontFace**>;
define C-pointer-type <PangoFontFamily***> => <PangoFontFamily**>;

define opaque-structure <_PangoAttrIterator>;
define opaque-structure <_PangoAttrList>;
define opaque-structure <_PangoContext>;
define opaque-structure <_PangoContextClass>;
define opaque-structure <_PangoCoverage>;
define opaque-structure <_PangoEngineLang>;
define opaque-structure <_PangoEngineShape>;
define opaque-structure <_PangoFont>;
define opaque-structure <_PangoFontDescription>;
define opaque-structure <_PangoFontFace>;
define opaque-structure <_PangoFontFamily>;
define opaque-structure <_PangoFontMetrics>;
define opaque-structure <_PangoFontset>;
// define opaque-structure <_PangoFontsetSimple>;
// define opaque-structure <_PangoFontsetSimpleClass>;
define opaque-structure <_PangoFontMap>;
define opaque-structure <_PangoLanguage>;
define opaque-structure <_PangoLayout>;
define opaque-structure <_PangoLayoutClass>;
define opaque-structure <_PangoLayoutIter>;
define opaque-structure <_PangoRendererPrivate>;
define opaque-structure <_PangoScriptIter>;
define opaque-structure <_PangoTabArray>;
