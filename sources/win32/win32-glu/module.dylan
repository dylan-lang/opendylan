Module:    dylan-user
Synopsis:  Raw interface to Win32 GLU
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module win32-glu
  use functional-dylan;
  use simple-format;
  use c-ffi;
  use win32-common;
  use win32-gl;

  // from "GLU.H":
  export gluErrorString, gluErrorUnicodeStringEXT, gluGetString,
	gluOrtho2D, gluPerspective, gluPickMatrix, gluLookAt, gluProject,
	gluUnProject;
  export gluScaleImage;
  export gluBuild1DMipmaps, gluBuild2DMipmaps, <GLUnurbs*>,
	<GLUquadric*>, <GLUtesselator*>, <GLUnurbsObj*>, <GLUquadricObj*>,
	<GLUtesselatorObj*>, <GLUtriangulatorObj*>;
  export gluNewQuadric, gluDeleteQuadric, gluQuadricNormals,
	gluQuadricTexture, gluQuadricOrientation, gluQuadricDrawStyle,
	gluCylinder, gluDisk, gluPartialDisk, gluSphere, gluNewTess,
	gluDeleteTess, gluTessBeginPolygon, gluTessBeginContour,
	gluTessVertex, gluTessEndContour, gluTessEndPolygon, gluTessProperty,
	gluTessNormal, gluGetTessProperty, gluNewNurbsRenderer,
	gluDeleteNurbsRenderer, gluBeginSurface, gluBeginCurve, gluEndCurve,
	gluEndSurface, gluBeginTrim, gluEndTrim, gluPwlCurve, gluNurbsCurve,
	gluNurbsSurface, gluLoadSamplingMatrices, gluNurbsProperty,
	gluGetNurbsProperty;
  export <GLUquadricErrorProc>, <GLUtessBeginProc>,
	<GLUtessEdgeFlagProc>, <GLUtessVertexProc>, <GLUtessEndProc>,
	<GLUtessErrorProc>, <GLUtessBeginDataProc>,
	<GLUtessEdgeFlagDataProc>, <GLUtessVertexDataProc>,
	<GLUtessEndDataProc>, <GLUtessErrorDataProc>, <GLUnurbsErrorProc>;
  export $GLU-VERSION-1-1, $GLU-VERSION-1-2, $GLU-INVALID-ENUM,
	$GLU-INVALID-VALUE, $GLU-OUT-OF-MEMORY, $GLU-INCOMPATIBLE-GL-VERSION,
	$GLU-VERSION, $GLU-EXTENSIONS, $GLU-TRUE, $GLU-FALSE, $GLU-SMOOTH,
	$GLU-FLAT, $GLU-NONE, $GLU-POINT, $GLU-LINE, $GLU-FILL,
	$GLU-SILHOUETTE, $GLU-OUTSIDE, $GLU-INSIDE;
  export $GLU-TESS-WINDING-RULE, $GLU-TESS-BOUNDARY-ONLY,
	$GLU-TESS-TOLERANCE, $GLU-TESS-WINDING-ODD,
	$GLU-TESS-WINDING-NONZERO, $GLU-TESS-WINDING-POSITIVE,
	$GLU-TESS-WINDING-NEGATIVE, $GLU-TESS-WINDING-ABS-GEQ-TWO,
	$GLU-TESS-BEGIN, $GLU-TESS-VERTEX, $GLU-TESS-END, $GLU-TESS-ERROR,
	$GLU-TESS-EDGE-FLAG, $GLU-TESS-COMBINE, $GLU-TESS-BEGIN-DATA,
	$GLU-TESS-VERTEX-DATA, $GLU-TESS-END-DATA, $GLU-TESS-ERROR-DATA,
	$GLU-TESS-EDGE-FLAG-DATA, $GLU-TESS-COMBINE-DATA, $GLU-TESS-ERROR1,
	$GLU-TESS-ERROR2, $GLU-TESS-ERROR3, $GLU-TESS-ERROR4,
	$GLU-TESS-ERROR5, $GLU-TESS-ERROR6, $GLU-TESS-ERROR7,
	$GLU-TESS-ERROR8, $GLU-TESS-MISSING-BEGIN-POLYGON,
	$GLU-TESS-MISSING-BEGIN-CONTOUR, $GLU-TESS-MISSING-END-POLYGON,
	$GLU-TESS-MISSING-END-CONTOUR, $GLU-TESS-COORD-TOO-LARGE,
	$GLU-TESS-NEED-COMBINE-CALLBACK, $GLU-AUTO-LOAD-MATRIX, $GLU-CULLING,
	$GLU-SAMPLING-TOLERANCE, $GLU-DISPLAY-MODE,
	$GLU-PARAMETRIC-TOLERANCE, $GLU-SAMPLING-METHOD, $GLU-U-STEP,
	$GLU-V-STEP, $GLU-PATH-LENGTH, $GLU-PARAMETRIC-ERROR,
	$GLU-DOMAIN-DISTANCE, $GLU-MAP1-TRIM-2, $GLU-MAP1-TRIM-3,
	$GLU-OUTLINE-POLYGON, $GLU-OUTLINE-PATCH, $GLU-NURBS-ERROR1,
	$GLU-NURBS-ERROR2, $GLU-NURBS-ERROR3, $GLU-NURBS-ERROR4,
	$GLU-NURBS-ERROR5, $GLU-NURBS-ERROR6, $GLU-NURBS-ERROR7,
	$GLU-NURBS-ERROR8, $GLU-NURBS-ERROR9, $GLU-NURBS-ERROR10,
	$GLU-NURBS-ERROR11, $GLU-NURBS-ERROR12, $GLU-NURBS-ERROR13,
	$GLU-NURBS-ERROR14, $GLU-NURBS-ERROR15, $GLU-NURBS-ERROR16,
	$GLU-NURBS-ERROR17, $GLU-NURBS-ERROR18, $GLU-NURBS-ERROR19,
	$GLU-NURBS-ERROR20, $GLU-NURBS-ERROR21, $GLU-NURBS-ERROR22,
	$GLU-NURBS-ERROR23, $GLU-NURBS-ERROR24, $GLU-NURBS-ERROR25,
	$GLU-NURBS-ERROR26, $GLU-NURBS-ERROR27, $GLU-NURBS-ERROR28,
	$GLU-NURBS-ERROR29, $GLU-NURBS-ERROR30, $GLU-NURBS-ERROR31,
	$GLU-NURBS-ERROR32, $GLU-NURBS-ERROR33, $GLU-NURBS-ERROR34,
	$GLU-NURBS-ERROR35, $GLU-NURBS-ERROR36, $GLU-NURBS-ERROR37,
	gluBeginPolygon, gluNextContour, gluEndPolygon, $GLU-CW, $GLU-CCW,
	$GLU-INTERIOR, $GLU-EXTERIOR, $GLU-UNKNOWN, $GLU-BEGIN, $GLU-VERTEX,
	$GLU-END, $GLU-ERROR, $GLU-EDGE-FLAG;

end module win32-glu;

// eof

