Module: type-library-module
Creator: created from "D:\dc-projects\lotus\type-library.spec" at 1: 6 1999- 4- 3 British Summer Time.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/* Type library: lotus version 1.0
 * Description: Lotus Notes OLE Types
 * GUID: {1CF542E0-D988-11CF-B485-00805F98FACE}
 */


/* Dispatch interface: BUTTON version 0.0
 * GUID: {29131601-2EED-1069-BF5D-00DD011186B7}
 */
define dispatch-client <BUTTON> ()
  uuid "{29131601-2EED-1069-BF5D-00DD011186B7}";
end dispatch-client <BUTTON>;


/* Dispatch interface: FIELD version 0.0
 * GUID: {29131603-2EED-1069-BF5D-00DD011186B7}
 */
define dispatch-client <FIELD> ()
  uuid "{29131603-2EED-1069-BF5D-00DD011186B7}";
end dispatch-client <FIELD>;


/* Dispatch interface: NAVIGATOR version 0.0
 * GUID: {29131605-2EED-1069-BF5D-00DD011186B7}
 */
define dispatch-client <NAVIGATOR> ()
  uuid "{29131605-2EED-1069-BF5D-00DD011186B7}";
end dispatch-client <NAVIGATOR>;


/* Dispatch interface: NOTESUIDOCUMENT version 0.0
 * GUID: {29131504-2EED-1069-BF5D-00DD011186B7}
 */
define dispatch-client <NOTESUIDOCUMENT> ()
  uuid "{29131504-2EED-1069-BF5D-00DD011186B7}";

  property NOTESUIDOCUMENT/WINDOWTITLE :: <string>, name: "WINDOWTITLE", 
        disp-id: 1700;

  property NOTESUIDOCUMENT/EDITMODE :: <object>, name: "EDITMODE", disp-id: 
        1701;

  property NOTESUIDOCUMENT/PREVIEWPARENTDOC :: <object>, name: 
        "PREVIEWPARENTDOC", disp-id: 1702;

  property NOTESUIDOCUMENT/PREVIEWDOCLINK :: <object>, name: 
        "PREVIEWDOCLINK", disp-id: 1703;

  property NOTESUIDOCUMENT/DOCUMENT :: <object>, name: "DOCUMENT", disp-id: 
        1704;

  property NOTESUIDOCUMENT/RULER :: <object>, name: "RULER", disp-id: 1705;

  property NOTESUIDOCUMENT/HORZSCROLLBAR :: <object>, name: 
        "HORZSCROLLBAR", disp-id: 1706;

  property NOTESUIDOCUMENT/HIDDENCHARS :: <object>, name: "HIDDENCHARS", 
        disp-id: 1707;

  property NOTESUIDOCUMENT/FIELDHELP :: <object>, name: "FIELDHELP", 
        disp-id: 1708;

  property NOTESUIDOCUMENT/CURRENTFIELD :: <string>, name: "CURRENTFIELD", 
        disp-id: 1709;

  property NOTESUIDOCUMENT/ISNEWDOC :: <object>, name: "ISNEWDOC", disp-id: 
        1710;

  property NOTESUIDOCUMENT/AUTORELOAD :: <object>, name: "AUTORELOAD", 
        disp-id: 1711;

  property NOTESUIDOCUMENT/INPREVIEWPANE :: <object>, name: 
        "INPREVIEWPANE", disp-id: 1712;

  function NOTESUIDOCUMENT/CLOSE () => (), name: "CLOSE", disp-id: 1751;

  function NOTESUIDOCUMENT/REFRESH () => (), name: "REFRESH", disp-id: 
        1752;

  function NOTESUIDOCUMENT/SAVE () => (), name: "SAVE", disp-id: 1753;

  function NOTESUIDOCUMENT/SEND () => (), name: "SEND", disp-id: 1754;

  function NOTESUIDOCUMENT/FORWARD () => (), name: "FORWARD", disp-id: 
        1755;

  function NOTESUIDOCUMENT/CATEGORIZE (arg-CATEGORY :: <object>) => (), 
        name: "CATEGORIZE", disp-id: 1756;

  function NOTESUIDOCUMENT/PRINT (arg-NUMCOPIES :: <object>, arg-FROMPAGE 
        :: <object>, arg-TOPAGE :: <object>, arg-DRAFT :: <object>) => (), 
        name: "PRINT", disp-id: 1757;

  function NOTESUIDOCUMENT/RELOAD () => (), name: "RELOAD", disp-id: 1758;

  function NOTESUIDOCUMENT/FIELDGETTEXT (arg-FIELDNAME :: <object>) => 
        (arg-result :: <string>), name: "FIELDGETTEXT", disp-id: 1759;

  function NOTESUIDOCUMENT/FIELDSETTEXT (arg-FIELDNAME :: <object>, 
        arg-TEXT :: <object>) => (), name: "FIELDSETTEXT", disp-id: 1760;

  function NOTESUIDOCUMENT/FIELDAPPENDTEXT (arg-FIELDNAME :: <object>, 
        arg-TEXT :: <object>) => (), name: "FIELDAPPENDTEXT", disp-id: 
        1761;

  function NOTESUIDOCUMENT/FIELDCLEAR (arg-FIELDNAME :: <object>) => (), 
        name: "FIELDCLEAR", disp-id: 1762;

  function NOTESUIDOCUMENT/GOTOFIELD (arg-FIELDNAME :: <object>) => (), 
        name: "GOTOFIELD", disp-id: 1764;

  function NOTESUIDOCUMENT/GOTONEXTFIELD () => (), name: "GOTONEXTFIELD", 
        disp-id: 1765;

  function NOTESUIDOCUMENT/GOTOPREVFIELD () => (), name: "GOTOPREVFIELD", 
        disp-id: 1766;

  function NOTESUIDOCUMENT/GOTOTOP () => (), name: "GOTOTOP", disp-id: 
        1767;

  function NOTESUIDOCUMENT/GOTOBOTTOM () => (), name: "GOTOBOTTOM", 
        disp-id: 1768;

  function NOTESUIDOCUMENT/DELETEDOCUMENT () => (), name: "DELETEDOCUMENT", 
        disp-id: 1769;

  function NOTESUIDOCUMENT/INSERTTEXT (arg-TEXT :: <object>) => (), name: 
        "INSERTTEXT", disp-id: 1770;

  function NOTESUIDOCUMENT/COPY () => (), name: "COPY", disp-id: 1771;

  function NOTESUIDOCUMENT/CUT () => (), name: "CUT", disp-id: 1772;

  function NOTESUIDOCUMENT/PASTE () => (), name: "PASTE", disp-id: 1773;

  function NOTESUIDOCUMENT/CLEAR () => (), name: "CLEAR", disp-id: 1774;

  function NOTESUIDOCUMENT/SELECTALL () => (), name: "SELECTALL", disp-id: 
        1775;

  function NOTESUIDOCUMENT/DESELECTALL () => (), name: "DESELECTALL", 
        disp-id: 1776;

  function NOTESUIDOCUMENT/EXPANDALLSECTIONS () => (), name: 
        "EXPANDALLSECTIONS", disp-id: 1777;

  function NOTESUIDOCUMENT/COLLAPSEALLSECTIONS () => (), name: 
        "COLLAPSEALLSECTIONS", disp-id: 1778;

  function NOTESUIDOCUMENT/SAVENEWVERSION () => (), name: "SAVENEWVERSION", 
        disp-id: 1763;

  function NOTESUIDOCUMENT/FIELDCONTAINS (arg-FIELDNAME :: <object>, 
        arg-TEXT :: <object>) => (arg-result :: <object>), name: 
        "FIELDCONTAINS", disp-id: 1780;

  function NOTESUIDOCUMENT/CREATEOBJECT (arg-NAME :: <object>, arg-OBJECT 
        :: <object>, arg-FILENAME :: <object>) => (arg-result :: <object>), 
        name: "CREATEOBJECT", disp-id: 1781;

  function NOTESUIDOCUMENT/GETOBJECT (arg-NAME :: <object>) => (arg-result 
        :: <object>), name: "GETOBJECT", disp-id: 1782;

  function NOTESUIDOCUMENT/REFRESHHIDEFORMULAS () => (), name: 
        "REFRESHHIDEFORMULAS", disp-id: 1783;

  function NOTESUIDOCUMENT/FINDFREETIMEDIALOG () => (), name: 
        "FINDFREETIMEDIALOG", disp-id: 1779;
end dispatch-client <NOTESUIDOCUMENT>;


/* Dispatch interface: NOTESUIDATABASE version 0.0
 * GUID: {29131503-2EED-1069-BF5D-00DD011186B7}
 */
define dispatch-client <NOTESUIDATABASE> ()
  uuid "{29131503-2EED-1069-BF5D-00DD011186B7}";

  property NOTESUIDATABASE/DATABASE :: <object>, name: "DATABASE", disp-id: 
        1563;

  property NOTESUIDATABASE/DOCUMENTS :: <object>, name: "DOCUMENTS", 
        disp-id: 1564;

  function NOTESUIDATABASE/OPENVIEW (arg-VIEWNAME :: <object>) => (), name: 
        "OPENVIEW", disp-id: 1599;
end dispatch-client <NOTESUIDATABASE>;


/* Dispatch interface: NOTESUIVIEW version 0.0
 * GUID: {29131506-2EED-1069-BF5D-00DD011186B7}
 */
define dispatch-client <NOTESUIVIEW> ()
  uuid "{29131506-2EED-1069-BF5D-00DD011186B7}";

  property NOTESUIVIEW/VIEW :: <object>, name: "VIEW", disp-id: 1784;

  property NOTESUIVIEW/DOCUMENTS :: <object>, name: "DOCUMENTS", disp-id: 
        1786;

  property NOTESUIVIEW/CALENDARDATETIME :: <object>, name: 
        "CALENDARDATETIME", disp-id: 1785;
end dispatch-client <NOTESUIVIEW>;


/* Dispatch interface: NOTESUIWORKSPACE version 0.0
 * GUID: {29131502-2EED-1069-BF5D-00DD011186B7}
 */
define dispatch-client <NOTESUIWORKSPACE> ()
  uuid "{29131502-2EED-1069-BF5D-00DD011186B7}";

  property NOTESUIWORKSPACE/CURRENTDOCUMENT :: <object>, name: 
        "CURRENTDOCUMENT", disp-id: 1506;

  function NOTESUIWORKSPACE/NEW () => (arg-result :: <object>), name: 
        "NEW", disp-id: 1536;

  function NOTESUIWORKSPACE/COMPOSEDOCUMENT (arg-SERVER :: <object>, 
        arg-DATABASEFILENAME :: <object>, arg-FORMNAME :: <object>, 
        arg-WINDOWWIDTH :: <object>, arg-WINDOWHEIGHT :: <object>) => 
        (arg-result :: <object>), name: "COMPOSEDOCUMENT", disp-id: 1532;

  function NOTESUIWORKSPACE/OPENDATABASE (arg-SERVER :: <object>, 
        arg-DATABASEFILENAME :: <object>, arg-VIEWNAME :: <object>, arg-KEY 
        :: <object>, arg-NEWINSTANCE :: <object>, arg-TEMPORARY :: 
        <object>) => (), name: "OPENDATABASE", disp-id: 1535;

  function NOTESUIWORKSPACE/EDITDOCUMENT (arg-EDITMODE :: <object>, 
        arg-NOTESDOCUMENT :: <object>, arg-NOTESDOCUMENTREADONLY :: 
        <object>) => (arg-result :: <object>), name: "EDITDOCUMENT", 
        disp-id: 1531;

  function NOTESUIWORKSPACE/DIALOGBOX (arg-FORMNAME :: <object>, 
        arg-AUTOHORZFIT :: <object>, arg-AUTOVERTFIT :: <object>, 
        arg-NOCANCEL :: <object>, arg-NONEWFIELDS :: <object>, 
        arg-NOFIELDUPDATE :: <object>, arg-READONLY :: <object>, arg-TITLE 
        :: <object>, arg-NOTESDOCUMENT :: <object>) => (arg-result :: 
        <object>), name: "DIALOGBOX", disp-id: 1530;

  function NOTESUIWORKSPACE/USELSX (arg-LSXPATH :: <object>) => (), name: 
        "USELSX", disp-id: 1537;

  function NOTESUIWORKSPACE/VIEWREFRESH () => (), name: "VIEWREFRESH", 
        disp-id: 1538;

  function NOTESUIWORKSPACE/ADDDATABASE (arg-SERVER :: <object>, 
        arg-DATABASEFILENAME :: <object>) => (), name: "ADDDATABASE", 
        disp-id: 1528;

  function NOTESUIWORKSPACE/EDITPROFILE (arg-PROFILENAME :: <object>, 
        arg-USERNAME :: <object>) => (arg-result :: <object>), name: 
        "EDITPROFILE", disp-id: 1539;

  function NOTESUIWORKSPACE/URLOPEN (arg-URL :: <object>, arg-RELOAD :: 
        <object>, arg-URLLIST :: <object>, arg-CHARSET :: <object>, 
        arg-WEBUSERNAME :: <object>, arg-WEBPASSWORD :: <object>, 
        arg-PROXYWEBUSERNAME :: <object>, arg-PROXYWEBPASSWORD :: <object>) 
        => (arg-result :: <object>), name: "URLOPEN", disp-id: 1540;

  function NOTESUIWORKSPACE/CHECKALARMS () => (), name: "CHECKALARMS", 
        disp-id: 1541;

  function NOTESUIWORKSPACE/ENABLEALARMS (arg-ENABLE :: <object>) => (), 
        name: "ENABLEALARMS", disp-id: 1542;
end dispatch-client <NOTESUIWORKSPACE>;


/* Dispatch interface: NOTESITEM version 0.0
 * GUID: {29131405-2EED-1069-BF5D-00DD011186B7}
 */
define dispatch-client <NOTESITEM> ()
  uuid "{29131405-2EED-1069-BF5D-00DD011186B7}";

  property NOTESITEM/NAME :: <string>, name: "NAME", disp-id: 1050;

  property NOTESITEM/TYPE :: type-union(<integer>, <machine-word>), name: 
        "TYPE", disp-id: 1051;

  property NOTESITEM/VALUES :: <object>, name: "VALUES", disp-id: 1053;

  property NOTESITEM/VALUELENGTH :: type-union(<integer>, <machine-word>), 
        name: "VALUELENGTH", disp-id: 1052;

  property NOTESITEM/ISENCRYPTED :: <object>, name: "ISENCRYPTED", disp-id: 
        1054;

  property NOTESITEM/ISSIGNED :: <object>, name: "ISSIGNED", disp-id: 1055;

  property NOTESITEM/ISSUMMARY :: <object>, name: "ISSUMMARY", disp-id: 
        1056;

  property NOTESITEM/ISPROTECTED :: <object>, name: "ISPROTECTED", disp-id: 
        1057;

  property NOTESITEM/PARENT :: <object>, name: "PARENT", disp-id: 1058;

  property NOTESITEM/TEXT :: <string>, name: "TEXT", disp-id: 1059;

  property NOTESITEM/ISNAMES :: <object>, name: "ISNAMES", disp-id: 1060;

  property NOTESITEM/ISREADERS :: <object>, name: "ISREADERS", disp-id: 
        1061;

  property NOTESITEM/ISAUTHORS :: <object>, name: "ISAUTHORS", disp-id: 
        1062;

  property NOTESITEM/DATETIMEVALUE :: <object>, name: "DATETIMEVALUE", 
        disp-id: 1063;

  property NOTESITEM/SAVETODISK :: <object>, name: "SAVETODISK", disp-id: 
        1091;

  property NOTESITEM/LASTMODIFIED :: <object>, name: "LASTMODIFIED", 
        disp-id: 1092;

  function NOTESITEM/NEW (arg-DOCUMENT :: <object>, arg-ITEMNAME :: 
        <string>, arg-VALUE :: <object>, arg-TYPE :: <object>) => 
        (arg-result :: <object>), name: "NEW", disp-id: 1064;

  function NOTESITEM/REMOVE () => (), name: "REMOVE", disp-id: 1066;

  function NOTESITEM/COPYITEMTODOCUMENT (arg-DOCUMENT :: <object>, 
        arg-ITEMNAME :: <string>) => (arg-result :: <object>), name: 
        "COPYITEMTODOCUMENT", disp-id: 1065;

  function NOTESITEM/CONTAINS (arg-VALUE :: <object>) => (arg-result :: 
        <integer>), name: "CONTAINS", disp-id: 1067;

  function NOTESITEM/ABSTRACT (arg-MAXLENGTH :: type-union(<integer>, 
        <machine-word>), arg-DROPVOWELS :: <integer>, arg-USEDICTIONARY :: 
        <integer>) => (arg-result :: <string>), name: "ABSTRACT", disp-id: 
        1068;

  function NOTESITEM/APPENDTOTEXTLIST (arg-VALUE :: <object>) => (), name: 
        "APPENDTOTEXTLIST", disp-id: 1368;
end dispatch-client <NOTESITEM>;


/* Dispatch interface: NOTESRICHTEXTITEM version 0.0
 * GUID: {29131406-2EED-1069-BF5D-00DD011186B7}
 */
define dispatch-client <NOTESRICHTEXTITEM> ()
  uuid "{29131406-2EED-1069-BF5D-00DD011186B7}";

  property NOTESRICHTEXTITEM/EMBEDDEDOBJECTS :: <object>, name: 
        "EMBEDDEDOBJECTS", disp-id: 1352;

  property NOTESRICHTEXTITEM/NAME :: <string>, name: "NAME", disp-id: 1050;

  property NOTESRICHTEXTITEM/TYPE :: type-union(<integer>, <machine-word>), 
        name: "TYPE", disp-id: 1051;

  property NOTESRICHTEXTITEM/VALUES :: <object>, name: "VALUES", disp-id: 
        1053;

  property NOTESRICHTEXTITEM/VALUELENGTH :: type-union(<integer>, 
        <machine-word>), name: "VALUELENGTH", disp-id: 1052;

  property NOTESRICHTEXTITEM/ISENCRYPTED :: <object>, name: "ISENCRYPTED", 
        disp-id: 1054;

  property NOTESRICHTEXTITEM/ISSIGNED :: <object>, name: "ISSIGNED", 
        disp-id: 1055;

  property NOTESRICHTEXTITEM/ISSUMMARY :: <object>, name: "ISSUMMARY", 
        disp-id: 1056;

  property NOTESRICHTEXTITEM/ISPROTECTED :: <object>, name: "ISPROTECTED", 
        disp-id: 1057;

  property NOTESRICHTEXTITEM/PARENT :: <object>, name: "PARENT", disp-id: 
        1058;

  property NOTESRICHTEXTITEM/TEXT :: <string>, name: "TEXT", disp-id: 1059;

  property NOTESRICHTEXTITEM/ISNAMES :: <object>, name: "ISNAMES", disp-id: 
        1060;

  property NOTESRICHTEXTITEM/ISREADERS :: <object>, name: "ISREADERS", 
        disp-id: 1061;

  property NOTESRICHTEXTITEM/ISAUTHORS :: <object>, name: "ISAUTHORS", 
        disp-id: 1062;

  property NOTESRICHTEXTITEM/DATETIMEVALUE :: <object>, name: 
        "DATETIMEVALUE", disp-id: 1063;

  property NOTESRICHTEXTITEM/SAVETODISK :: <object>, name: "SAVETODISK", 
        disp-id: 1091;

  property NOTESRICHTEXTITEM/LASTMODIFIED :: <object>, name: 
        "LASTMODIFIED", disp-id: 1092;

  function NOTESRICHTEXTITEM/NEW (arg-PARENT :: <object>, arg-ITEMNAME :: 
        <string>) => (arg-result :: <object>), name: "NEW", disp-id: 1353;

  function NOTESRICHTEXTITEM/ADDNEWLINE (arg-COUNT :: <integer>, 
        arg-NEWPARAGRAPH :: <object>) => (), name: "ADDNEWLINE", disp-id: 
        1354;

  function NOTESRICHTEXTITEM/ADDTAB (arg-COUNT :: <integer>) => (), name: 
        "ADDTAB", disp-id: 1355;

  function NOTESRICHTEXTITEM/APPENDRTITEM (arg-RTITEM :: <object>) => (), 
        name: "APPENDRTITEM", disp-id: 1356;

  function NOTESRICHTEXTITEM/APPENDRTFILE (arg-PATHNAME :: <string>) => (), 
        name: "APPENDRTFILE", disp-id: 1357;

  function NOTESRICHTEXTITEM/GETFORMATTEDTEXT (arg-STRIPTABS :: <integer>, 
        arg-LINELENGTH :: <integer>) => (arg-result :: <string>), name: 
        "GETFORMATTEDTEXT", disp-id: 1358;

  function NOTESRICHTEXTITEM/APPENDTEXT (arg-TEXT :: <string>) => (), name: 
        "APPENDTEXT", disp-id: 1359;

  function NOTESRICHTEXTITEM/APPENDDOCLINK (arg-LINKOBJECT :: <object>, 
        arg-COMMENT :: <string>) => (), name: "APPENDDOCLINK", disp-id: 
        1360;

  function NOTESRICHTEXTITEM/EMBEDOBJECT (arg-TYPE :: <integer>, arg-CLASS 
        :: <string>, arg-SOURCE :: <string>, arg-OBJECTNAME :: <object>) => 
        (arg-result :: <object>), name: "EMBEDOBJECT", disp-id: 1361;

  function NOTESRICHTEXTITEM/GETEMBEDDEDOBJECT (arg-OBJECTNAME :: <string>) 
        => (arg-result :: <object>), name: "GETEMBEDDEDOBJECT", disp-id: 
        1362;

  function NOTESRICHTEXTITEM/REMOVE () => (), name: "REMOVE", disp-id: 
        1066;

  function NOTESRICHTEXTITEM/COPYITEMTODOCUMENT (arg-DOCUMENT :: <object>, 
        arg-ITEMNAME :: <string>) => (arg-result :: <object>), name: 
        "COPYITEMTODOCUMENT", disp-id: 1065;

  function NOTESRICHTEXTITEM/CONTAINS (arg-VALUE :: <object>) => 
        (arg-result :: <integer>), name: "CONTAINS", disp-id: 1067;

  function NOTESRICHTEXTITEM/ABSTRACT (arg-MAXLENGTH :: 
        type-union(<integer>, <machine-word>), arg-DROPVOWELS :: <integer>, 
        arg-USEDICTIONARY :: <integer>) => (arg-result :: <string>), name: 
        "ABSTRACT", disp-id: 1068;

  function NOTESRICHTEXTITEM/APPENDTOTEXTLIST (arg-VALUE :: <object>) => 
        (), name: "APPENDTOTEXTLIST", disp-id: 1368;
end dispatch-client <NOTESRICHTEXTITEM>;


/* Dispatch interface: NOTESEMBEDDEDOBJECT version 0.0
 * GUID: {29131410-2EED-1069-BF5D-00DD011186B7}
 */
define dispatch-client <NOTESEMBEDDEDOBJECT> ()
  uuid "{29131410-2EED-1069-BF5D-00DD011186B7}";

  property NOTESEMBEDDEDOBJECT/NAME :: <string>, name: "NAME", disp-id: 
        1440;

  property NOTESEMBEDDEDOBJECT/CLASS :: <string>, name: "CLASS", disp-id: 
        1441;

  property NOTESEMBEDDEDOBJECT/SOURCE :: <string>, name: "SOURCE", disp-id: 
        1442;

  property NOTESEMBEDDEDOBJECT/TYPE :: <integer>, name: "TYPE", disp-id: 
        1443;

  property NOTESEMBEDDEDOBJECT/FILESIZE :: type-union(<integer>, 
        <machine-word>), name: "FILESIZE", disp-id: 1444;

  property NOTESEMBEDDEDOBJECT/PARENT :: <object>, name: "PARENT", disp-id: 
        1445;

  property NOTESEMBEDDEDOBJECT/VERBS :: <object>, name: "VERBS", disp-id: 
        1446;

  property NOTESEMBEDDEDOBJECT/OBJECT :: <object>, name: "OBJECT", disp-id: 
        1447;

  function NOTESEMBEDDEDOBJECT/EXTRACTFILE (arg-PATHNAME :: <string>) => 
        (), name: "EXTRACTFILE", disp-id: 1449;

  function NOTESEMBEDDEDOBJECT/REMOVE () => (), name: "REMOVE", disp-id: 
        1451;

  function NOTESEMBEDDEDOBJECT/ACTIVATE (arg-SHOW :: <integer>) => 
        (arg-result :: <object>), name: "ACTIVATE", disp-id: 1448;

  function NOTESEMBEDDEDOBJECT/DOVERB (arg-VERBNAME :: <string>) => 
        (arg-result :: <object>), name: "DOVERB", disp-id: 1450;
end dispatch-client <NOTESEMBEDDEDOBJECT>;


/* Dispatch interface: NOTESDATETIME version 0.0
 * GUID: {29131408-2EED-1069-BF5D-00DD011186B7}
 */
define dispatch-client <NOTESDATETIME> ()
  uuid "{29131408-2EED-1069-BF5D-00DD011186B7}";

  property NOTESDATETIME/LOCALTIME :: <string>, name: "LOCALTIME", disp-id: 
        1260;

  property NOTESDATETIME/GMTTIME :: <string>, name: "GMTTIME", disp-id: 
        1261;

  property NOTESDATETIME/LSLOCALTIME :: <object>, name: "LSLOCALTIME", 
        disp-id: 1262;

  property NOTESDATETIME/LSGMTTIME :: <object>, name: "LSGMTTIME", disp-id: 
        1263;

  property NOTESDATETIME/TIMEZONE :: <integer>, name: "TIMEZONE", disp-id: 
        1264;

  property NOTESDATETIME/ISDST :: <integer>, name: "ISDST", disp-id: 1265;

  property NOTESDATETIME/ZONETIME :: <string>, name: "ZONETIME", disp-id: 
        1278;

  function NOTESDATETIME/NEW (arg-DATEEXPR :: <string>) => (arg-result :: 
        <object>), name: "NEW", disp-id: 1266;

  function NOTESDATETIME/ADJUSTSECOND (arg-ADJUSTMENT :: <integer>) => (), 
        name: "ADJUSTSECOND", disp-id: 1267;

  function NOTESDATETIME/ADJUSTMINUTE (arg-ADJUSTMENT :: <integer>) => (), 
        name: "ADJUSTMINUTE", disp-id: 1268;

  function NOTESDATETIME/ADJUSTHOUR (arg-ADJUSTMENT :: <integer>) => (), 
        name: "ADJUSTHOUR", disp-id: 1269;

  function NOTESDATETIME/ADJUSTDAY (arg-ADJUSTMENT :: <integer>) => (), 
        name: "ADJUSTDAY", disp-id: 1270;

  function NOTESDATETIME/ADJUSTMONTH (arg-ADJUSTMENT :: <integer>) => (), 
        name: "ADJUSTMONTH", disp-id: 1271;

  function NOTESDATETIME/ADJUSTYEAR (arg-ADJUSTMENT :: <integer>) => (), 
        name: "ADJUSTYEAR", disp-id: 1272;

  function NOTESDATETIME/SETNOW () => (), name: "SETNOW", disp-id: 1273;

  function NOTESDATETIME/SETANYTIME () => (), name: "SETANYTIME", disp-id: 
        1274;

  function NOTESDATETIME/SETANYDATE () => (), name: "SETANYDATE", disp-id: 
        1275;

  function NOTESDATETIME/TIMEDIFFERENCE (arg-DATETIME :: <object>) => 
        (arg-result :: type-union(<integer>, <machine-word>)), name: 
        "TIMEDIFFERENCE", disp-id: 1276;

  function NOTESDATETIME/CONVERTTOZONE (arg-NEWZONE :: <integer>, arg-DST 
        :: <integer>) => (), name: "CONVERTTOZONE", disp-id: 1277;
end dispatch-client <NOTESDATETIME>;


/* Dispatch interface: NOTESDOCUMENT version 0.0
 * GUID: {29131404-2EED-1069-BF5D-00DD011186B7}
 */
define dispatch-client <NOTESDOCUMENT> ()
  uuid "{29131404-2EED-1069-BF5D-00DD011186B7}";

  property NOTESDOCUMENT/ISSIGNED :: <object>, name: "ISSIGNED", disp-id: 
        1140;

  property NOTESDOCUMENT/LASTMODIFIED :: <object>, name: "LASTMODIFIED", 
        disp-id: 1141;

  property NOTESDOCUMENT/LASTACCESSED :: <object>, name: "LASTACCESSED", 
        disp-id: 1142;

  property NOTESDOCUMENT/CREATED :: <object>, name: "CREATED", disp-id: 
        1143;

  property NOTESDOCUMENT/ISRESPONSE :: <object>, name: "ISRESPONSE", 
        disp-id: 1144;

  property NOTESDOCUMENT/FTSEARCHSCORE :: <integer>, name: "FTSEARCHSCORE", 
        disp-id: 1145;

  property NOTESDOCUMENT/ISNEWNOTE :: <object>, name: "ISNEWNOTE", disp-id: 
        1146;

  property NOTESDOCUMENT/AUTHORS :: <object>, name: "AUTHORS", disp-id: 
        1147;

  property NOTESDOCUMENT/NOTEID :: <string>, name: "NOTEID", disp-id: 1148;

  property NOTESDOCUMENT/UNIVERSALID :: <string>, name: "UNIVERSALID", 
        disp-id: 1149;

  property NOTESDOCUMENT/ITEMS :: <object>, name: "ITEMS", disp-id: 1150;

  property NOTESDOCUMENT/HASEMBEDDED :: <object>, name: "HASEMBEDDED", 
        disp-id: 1151;

  property NOTESDOCUMENT/PARENTDATABASE :: <object>, name: 
        "PARENTDATABASE", disp-id: 1152;

  property NOTESDOCUMENT/PARENTVIEW :: <object>, name: "PARENTVIEW", 
        disp-id: 1153;

  property NOTESDOCUMENT/PARENTDOCUMENTUNID :: <string>, name: 
        "PARENTDOCUMENTUNID", disp-id: 1154;

  property NOTESDOCUMENT/ENCRYPTONSEND :: <object>, name: "ENCRYPTONSEND", 
        disp-id: 1155;

  property NOTESDOCUMENT/SIGNONSEND :: <object>, name: "SIGNONSEND", 
        disp-id: 1156;

  property NOTESDOCUMENT/SAVEMESSAGEONSEND :: <object>, name: 
        "SAVEMESSAGEONSEND", disp-id: 1157;

  property NOTESDOCUMENT/SIGNER :: <string>, name: "SIGNER", disp-id: 1158;

  property NOTESDOCUMENT/VERIFIER :: <string>, name: "VERIFIER", disp-id: 
        1159;

  property NOTESDOCUMENT/RESPONSES :: <object>, name: "RESPONSES", disp-id: 
        1160;

  property NOTESDOCUMENT/ENCRYPTIONKEYS :: <object>, name: 
        "ENCRYPTIONKEYS", disp-id: 1161;

  property NOTESDOCUMENT/SENTBYAGENT :: <object>, name: "SENTBYAGENT", 
        disp-id: 1162;

  property NOTESDOCUMENT/SIZE :: type-union(<integer>, <machine-word>), 
        name: "SIZE", disp-id: 1163;

  property NOTESDOCUMENT/COLUMNVALUES :: <object>, name: "COLUMNVALUES", 
        disp-id: 1164;

  property NOTESDOCUMENT/EMBEDDEDOBJECTS :: <object>, name: 
        "EMBEDDEDOBJECTS", disp-id: 2950;

  property NOTESDOCUMENT/ISUIDOCOPEN :: <object>, name: "ISUIDOCOPEN", 
        disp-id: 2951;

  property NOTESDOCUMENT/ISPROFILE :: <object>, name: "ISPROFILE", disp-id: 
        2952;

  property NOTESDOCUMENT/NAMEOFPROFILE :: <string>, name: "NAMEOFPROFILE", 
        disp-id: 2953;

  property NOTESDOCUMENT/KEY :: <string>, name: "KEY", disp-id: 2954;

  function NOTESDOCUMENT/NEW (arg-DATABASE :: <object>) => (arg-result :: 
        <object>), name: "NEW", disp-id: 1165;

  function NOTESDOCUMENT/GETFIRSTITEM (arg-arg0 :: <string>) => (arg-result 
        :: <object>), name: "GETFIRSTITEM", disp-id: 1166;

  function NOTESDOCUMENT/GETNEXTITEM (arg-arg0 :: <object>) => (arg-result 
        :: <object>), name: "GETNEXTITEM", disp-id: 1167;

  function NOTESDOCUMENT/COPYITEM (arg-ITEM :: <object>, arg-ITEMNAME :: 
        <string>) => (arg-result :: <object>), name: "COPYITEM", disp-id: 
        1168;

  function NOTESDOCUMENT/REMOVEITEM (arg-ITEMNAME :: <string>) => (), name: 
        "REMOVEITEM", disp-id: 1169;

  function NOTESDOCUMENT/HASITEM (arg-ITEMNAME :: <string>) => (arg-result 
        :: <integer>), name: "HASITEM", disp-id: 1170;

  function NOTESDOCUMENT/REMOVE (arg-FORCE :: <integer>) => (arg-result :: 
        <integer>), name: "REMOVE", disp-id: 1171;

  function NOTESDOCUMENT/SAVE (arg-FORCE :: <integer>, arg-MAKERESPONSE :: 
        <integer>, arg-MARKREAD :: <object>) => (arg-result :: <integer>), 
        name: "SAVE", disp-id: 1172;

  function NOTESDOCUMENT/GETITEMVALUE (arg-ITEMNAME :: <string>) => 
        (arg-result :: <object>), name: "GETITEMVALUE", disp-id: 1173;

  function NOTESDOCUMENT/APPENDITEMVALUE (arg-ITEMNAME :: <string>, 
        arg-NEWVALUE :: <object>) => (arg-result :: <object>), name: 
        "APPENDITEMVALUE", disp-id: 1174;

  function NOTESDOCUMENT/REPLACEITEMVALUE (arg-ITEMNAME :: <string>, 
        arg-NEWVALUE :: <object>) => (arg-result :: <object>), name: 
        "REPLACEITEMVALUE", disp-id: 1175;

  function NOTESDOCUMENT/COPYTODATABASE (arg-DESTINATION :: <object>) => 
        (arg-result :: <object>), name: "COPYTODATABASE", disp-id: 1176;

  function NOTESDOCUMENT/SIGN () => (), name: "SIGN", disp-id: 1177;

  function NOTESDOCUMENT/CREATEREPLYMESSAGE (arg-REPLYTOALL :: <integer>) 
        => (arg-result :: <object>), name: "CREATEREPLYMESSAGE", disp-id: 
        1179;

  function NOTESDOCUMENT/SEND (arg-ATTACHFORM :: <integer>, arg-RECIPIENTS 
        :: <object>) => (), name: "SEND", disp-id: 1180;

  function NOTESDOCUMENT/ENCRYPT () => (), name: "ENCRYPT", disp-id: 1182;

  function NOTESDOCUMENT/MAKERESPONSE (arg-NEWPARENT :: <object>) => (), 
        name: "MAKERESPONSE", disp-id: 1183;

  function NOTESDOCUMENT/PUTINFOLDER (arg-FOLDERNAME :: <string>) => (), 
        name: "PUTINFOLDER", disp-id: 1184;

  function NOTESDOCUMENT/REMOVEFROMFOLDER (arg-FOLDERNAME :: <string>) => 
        (), name: "REMOVEFROMFOLDER", disp-id: 1185;

  function NOTESDOCUMENT/RENDERTORTITEM (arg-DESTINATION :: <object>) => 
        (arg-result :: <object>), name: "RENDERTORTITEM", disp-id: 1178;

  function NOTESDOCUMENT/COMPUTEWITHFORM (arg-DATATYPEVALIDATION :: 
        <integer>, arg-RAISEERROR :: <integer>) => (arg-result :: 
        <object>), name: "COMPUTEWITHFORM", disp-id: 1186;

  function NOTESDOCUMENT/COPYALLITEMS (arg-DESTINATION :: <object>, 
        arg-REPLACE :: <object>) => (), name: "COPYALLITEMS", disp-id: 
        1181;

  function NOTESDOCUMENT/GETATTACHMENT (arg-FILENAME :: <string>) => 
        (arg-result :: <object>), name: "GETATTACHMENT", disp-id: 1188;

  function NOTESDOCUMENT/CREATERICHTEXTITEM (arg-ITEMNAME :: <string>) => 
        (arg-result :: <object>), name: "CREATERICHTEXTITEM", disp-id: 
        1189;
end dispatch-client <NOTESDOCUMENT>;


/* Dispatch interface: NOTESDOCUMENTCOLLECTION version 0.0
 * GUID: {2913140B-2EED-1069-BF5D-00DD011186B7}
 */
define dispatch-client <NOTESDOCUMENTCOLLECTION> ()
  uuid "{2913140B-2EED-1069-BF5D-00DD011186B7}";

  property NOTESDOCUMENTCOLLECTION/COUNT :: type-union(<integer>, 
        <machine-word>), name: "COUNT", disp-id: 1330;

  property NOTESDOCUMENTCOLLECTION/ISSORTED :: <object>, name: "ISSORTED", 
        disp-id: 1331;

  property NOTESDOCUMENTCOLLECTION/QUERY :: <string>, name: "QUERY", 
        disp-id: 1332;

  property NOTESDOCUMENTCOLLECTION/PARENT :: <object>, name: "PARENT", 
        disp-id: 1333;

  function NOTESDOCUMENTCOLLECTION/GETFIRSTDOCUMENT () => (arg-result :: 
        <object>), name: "GETFIRSTDOCUMENT", disp-id: 1334;

  function NOTESDOCUMENTCOLLECTION/GETLASTDOCUMENT () => (arg-result :: 
        <object>), name: "GETLASTDOCUMENT", disp-id: 1335;

  function NOTESDOCUMENTCOLLECTION/GETNEXTDOCUMENT (arg-CURRENTDOC :: 
        <object>) => (arg-result :: <object>), name: "GETNEXTDOCUMENT", 
        disp-id: 1336;

  function NOTESDOCUMENTCOLLECTION/GETPREVDOCUMENT (arg-CURRENTDOC :: 
        <object>) => (arg-result :: <object>), name: "GETPREVDOCUMENT", 
        disp-id: 1337;

  function NOTESDOCUMENTCOLLECTION/GETNTHDOCUMENT (arg-INDEX :: 
        type-union(<integer>, <machine-word>)) => (arg-result :: <object>), 
        name: "GETNTHDOCUMENT", disp-id: 1338;

  function NOTESDOCUMENTCOLLECTION/FTSEARCH (arg-QUERY :: <string>, 
        arg-MAXDOCS :: <integer>) => (), name: "FTSEARCH", disp-id: 1339;

  function NOTESDOCUMENTCOLLECTION/REMOVEALL (arg-FORCE :: <integer>) => 
        (), name: "REMOVEALL", disp-id: 1340;

  function NOTESDOCUMENTCOLLECTION/UPDATEALL () => (), name: "UPDATEALL", 
        disp-id: 1341;

  function NOTESDOCUMENTCOLLECTION/STAMPALL (arg-ITEMNAME :: <string>, 
        arg-VALUE :: <object>) => (), name: "STAMPALL", disp-id: 1342;

  function NOTESDOCUMENTCOLLECTION/PUTALLINFOLDER (arg-FOLDERNAME :: 
        <string>) => (), name: "PUTALLINFOLDER", disp-id: 1344;

  function NOTESDOCUMENTCOLLECTION/REMOVEALLFROMFOLDER (arg-FOLDERNAME :: 
        <string>) => (), name: "REMOVEALLFROMFOLDER", disp-id: 1345;
end dispatch-client <NOTESDOCUMENTCOLLECTION>;


/* Dispatch interface: NOTESVIEWCOLUMN version 0.0
 * GUID: {2913140F-2EED-1069-BF5D-00DD011186B7}
 */
define dispatch-client <NOTESVIEWCOLUMN> ()
  uuid "{2913140F-2EED-1069-BF5D-00DD011186B7}";

  property NOTESVIEWCOLUMN/TITLE :: <string>, name: "TITLE", disp-id: 1430;

  property NOTESVIEWCOLUMN/ITEMNAME :: <string>, name: "ITEMNAME", disp-id: 
        1431;

  property NOTESVIEWCOLUMN/POSITION :: <integer>, name: "POSITION", 
        disp-id: 1432;

  property NOTESVIEWCOLUMN/FORMULA :: <string>, name: "FORMULA", disp-id: 
        1433;

  property NOTESVIEWCOLUMN/ISSORTED :: <object>, name: "ISSORTED", disp-id: 
        1434;

  property NOTESVIEWCOLUMN/ISCATEGORY :: <object>, name: "ISCATEGORY", 
        disp-id: 1435;

  property NOTESVIEWCOLUMN/ISHIDDEN :: <object>, name: "ISHIDDEN", disp-id: 
        1436;

  property NOTESVIEWCOLUMN/ISRESPONSE :: <object>, name: "ISRESPONSE", 
        disp-id: 1437;
end dispatch-client <NOTESVIEWCOLUMN>;


/* Dispatch interface: NOTESVIEW version 0.0
 * GUID: {29131403-2EED-1069-BF5D-00DD011186B7}
 */
define dispatch-client <NOTESVIEW> ()
  uuid "{29131403-2EED-1069-BF5D-00DD011186B7}";

  property NOTESVIEW/NAME :: <string>, name: "NAME", disp-id: 1280;

  property NOTESVIEW/LASTMODIFIED :: <object>, name: "LASTMODIFIED", 
        disp-id: 1281;

  property NOTESVIEW/CREATED :: <object>, name: "CREATED", disp-id: 1282;

  property NOTESVIEW/UNIVERSALID :: <string>, name: "UNIVERSALID", disp-id: 
        1283;

  property NOTESVIEW/PARENT :: <object>, name: "PARENT", disp-id: 1284;

  property NOTESVIEW/ISDEFAULTVIEW :: <object>, name: "ISDEFAULTVIEW", 
        disp-id: 1285;

  property NOTESVIEW/COLUMNS :: <object>, name: "COLUMNS", disp-id: 1286;

  property NOTESVIEW/ISFOLDER :: <object>, name: "ISFOLDER", disp-id: 1287;

  property NOTESVIEW/READERS :: <object>, name: "READERS", disp-id: 1302;

  property NOTESVIEW/AUTOUPDATE :: <object>, name: "AUTOUPDATE", disp-id: 
        1303;

  property NOTESVIEW/ALIASES :: <object>, name: "ALIASES", disp-id: 1304;

  property NOTESVIEW/ISCALENDAR :: <object>, name: "ISCALENDAR", disp-id: 
        1305;

  property NOTESVIEW/PROTECTREADERS :: <object>, name: "PROTECTREADERS", 
        disp-id: 1309;

  function NOTESVIEW/GETFIRSTDOCUMENT () => (arg-result :: <object>), name: 
        "GETFIRSTDOCUMENT", disp-id: 1288;

  function NOTESVIEW/GETLASTDOCUMENT () => (arg-result :: <object>), name: 
        "GETLASTDOCUMENT", disp-id: 1289;

  function NOTESVIEW/GETNEXTDOCUMENT (arg-DOCUMENT :: <object>) => 
        (arg-result :: <object>), name: "GETNEXTDOCUMENT", disp-id: 1290;

  function NOTESVIEW/GETPREVDOCUMENT (arg-DOCUMENT :: <object>) => 
        (arg-result :: <object>), name: "GETPREVDOCUMENT", disp-id: 1291;

  function NOTESVIEW/GETNEXTSIBLING (arg-DOCUMENT :: <object>) => 
        (arg-result :: <object>), name: "GETNEXTSIBLING", disp-id: 1293;

  function NOTESVIEW/GETPREVSIBLING (arg-DOCUMENT :: <object>) => 
        (arg-result :: <object>), name: "GETPREVSIBLING", disp-id: 1294;

  function NOTESVIEW/GETPARENTDOCUMENT (arg-DOCUMENT :: <object>) => 
        (arg-result :: <object>), name: "GETPARENTDOCUMENT", disp-id: 1301;

  function NOTESVIEW/GETCHILD (arg-DOCUMENT :: <object>) => (arg-result :: 
        <object>), name: "GETCHILD", disp-id: 1295;

  function NOTESVIEW/GETNTHDOCUMENT (arg-INDEX :: type-union(<integer>, 
        <machine-word>)) => (arg-result :: <object>), name: 
        "GETNTHDOCUMENT", disp-id: 1292;

  function NOTESVIEW/GETDOCUMENTBYKEY (arg-KEYARRAY :: <object>, 
        arg-EXACTMATCH :: <object>) => (arg-result :: <object>), name: 
        "GETDOCUMENTBYKEY", disp-id: 1296;

  function NOTESVIEW/FTSEARCH (arg-QUERY :: <string>, arg-MAXDOCS :: 
        <object>) => (arg-result :: type-union(<integer>, <machine-word>)), 
        name: "FTSEARCH", disp-id: 1297;

  function NOTESVIEW/REFRESH () => (), name: "REFRESH", disp-id: 1298;

  function NOTESVIEW/REMOVE () => (), name: "REMOVE", disp-id: 1299;

  function NOTESVIEW/CLEAR () => (), name: "CLEAR", disp-id: 1300;

  function NOTESVIEW/GETALLDOCUMENTSBYKEY (arg-KEYARRAY :: <object>, 
        arg-EXACTMATCH :: <object>) => (arg-result :: <object>), name: 
        "GETALLDOCUMENTSBYKEY", disp-id: 1306;
end dispatch-client <NOTESVIEW>;


/* Dispatch interface: NOTESAGENT version 0.0
 * GUID: {29131409-2EED-1069-BF5D-00DD011186B7}
 */
define dispatch-client <NOTESAGENT> ()
  uuid "{29131409-2EED-1069-BF5D-00DD011186B7}";

  property NOTESAGENT/NAME :: <string>, name: "NAME", disp-id: 1110;

  property NOTESAGENT/OWNER :: <string>, name: "OWNER", disp-id: 1111;

  property NOTESAGENT/LASTRUN :: <object>, name: "LASTRUN", disp-id: 1112;

  property NOTESAGENT/ISENABLED :: <object>, name: "ISENABLED", disp-id: 
        1113;

  property NOTESAGENT/SERVERNAME :: <string>, name: "SERVERNAME", disp-id: 
        1114;

  property NOTESAGENT/QUERY :: <string>, name: "QUERY", disp-id: 1115;

  property NOTESAGENT/COMMENT :: <string>, name: "COMMENT", disp-id: 1116;

  property NOTESAGENT/PARENT :: <object>, name: "PARENT", disp-id: 1117;

  property NOTESAGENT/ISPUBLIC :: <object>, name: "ISPUBLIC", disp-id: 
        1118;

  property NOTESAGENT/COMMONOWNER :: <string>, name: "COMMONOWNER", 
        disp-id: 1119;

  function NOTESAGENT/RUN () => (), name: "RUN", disp-id: 1120;

  function NOTESAGENT/REMOVE () => (), name: "REMOVE", disp-id: 1121;
end dispatch-client <NOTESAGENT>;


/* Dispatch interface: NOTESDATABASE version 0.0
 * GUID: {29131402-2EED-1069-BF5D-00DD011186B7}
 */
define dispatch-client <NOTESDATABASE> ()
  uuid "{29131402-2EED-1069-BF5D-00DD011186B7}";

  property NOTESDATABASE/ISFTINDEXED :: <object>, name: "ISFTINDEXED", 
        disp-id: 1000;

  property NOTESDATABASE/TITLE :: <string>, name: "TITLE", disp-id: 1001;

  property NOTESDATABASE/CATEGORIES :: <object>, name: "CATEGORIES", 
        disp-id: 1002;

  property NOTESDATABASE/TEMPLATENAME :: <string>, name: "TEMPLATENAME", 
        disp-id: 1003;

  property NOTESDATABASE/DESIGNTEMPLATENAME :: <string>, name: 
        "DESIGNTEMPLATENAME", disp-id: 1004;

  property NOTESDATABASE/FILENAME :: <string>, name: "FILENAME", disp-id: 
        1005;

  property NOTESDATABASE/FILEPATH :: <string>, name: "FILEPATH", disp-id: 
        1006;

  property NOTESDATABASE/LASTMODIFIED :: <object>, name: "LASTMODIFIED", 
        disp-id: 1007;

  property NOTESDATABASE/MANAGERS :: <object>, name: "MANAGERS", disp-id: 
        1008;

  property NOTESDATABASE/ISOPEN :: <object>, name: "ISOPEN", disp-id: 1009;

  property NOTESDATABASE/VIEWS :: <object>, name: "VIEWS", disp-id: 1010;

  property NOTESDATABASE/AGENTS :: <object>, name: "AGENTS", disp-id: 1011;

  property NOTESDATABASE/REPLICAID :: <string>, name: "REPLICAID", disp-id: 
        1012;

  property NOTESDATABASE/PARENT :: <object>, name: "PARENT", disp-id: 1013;

  property NOTESDATABASE/LASTFTINDEXED :: <object>, name: "LASTFTINDEXED", 
        disp-id: 1014;

  property NOTESDATABASE/ISPUBLICADDRESSBOOK :: <object>, name: 
        "ISPUBLICADDRESSBOOK", disp-id: 1015;

  property NOTESDATABASE/ISPRIVATEADDRESSBOOK :: <object>, name: 
        "ISPRIVATEADDRESSBOOK", disp-id: 1016;

  property NOTESDATABASE/ACL :: <object>, name: "ACL", disp-id: 1017;

  property NOTESDATABASE/CREATED :: <object>, name: "CREATED", disp-id: 
        1018;

  property NOTESDATABASE/ALLDOCUMENTS :: <object>, name: "ALLDOCUMENTS", 
        disp-id: 1047;

  property NOTESDATABASE/UNPROCESSEDDOCUMENTS :: <object>, name: 
        "UNPROCESSEDDOCUMENTS", disp-id: 1048;

  property NOTESDATABASE/SERVER :: <string>, name: "SERVER", disp-id: 1501;

  property NOTESDATABASE/CURRENTACCESSLEVEL :: <integer>, name: 
        "CURRENTACCESSLEVEL", disp-id: 1502;

  property NOTESDATABASE/SIZE :: <double-float>, name: "SIZE", disp-id: 
        1503;

  property NOTESDATABASE/SIZEQUOTA :: type-union(<integer>, 
        <machine-word>), name: "SIZEQUOTA", disp-id: 1504;

  property NOTESDATABASE/PERCENTUSED :: <double-float>, name: 
        "PERCENTUSED", disp-id: 1506;

  property NOTESDATABASE/DELAYUPDATES :: <object>, name: "DELAYUPDATES", 
        disp-id: 1514;

  property NOTESDATABASE/FORMS :: <object>, name: "FORMS", disp-id: 1515;

  property NOTESDATABASE/ISMULTIDBSEARCH :: <object>, name: 
        "ISMULTIDBSEARCH", disp-id: 1517;

  function NOTESDATABASE/NEW (arg-SERVER :: <string>, arg-FILE :: <string>) 
        => (arg-result :: <object>), name: "NEW", disp-id: 1042;

  function NOTESDATABASE/OPEN (arg-SERVER :: <string>, arg-FILE :: 
        <string>) => (arg-result :: <object>), name: "OPEN", disp-id: 1026;

  function NOTESDATABASE/CLOSE () => (), name: "CLOSE", disp-id: 1045;

  function NOTESDATABASE/FTSEARCH (arg-QUERY :: <string>, arg-MAXDOCS :: 
        <integer>, arg-SORTOPTION :: <object>, arg-OTHEROPTIONS :: 
        <object>) => (arg-result :: <object>), name: "FTSEARCH", disp-id: 
        1027;

  function NOTESDATABASE/SEARCH (arg-FORMULA :: <string>, arg-DATETIME :: 
        <object>, arg-MAXDOCS :: <integer>) => (arg-result :: <object>), 
        name: "SEARCH", disp-id: 1028;

  function NOTESDATABASE/UNPROCESSEDFTSEARCH (arg-QUERY :: <string>, 
        arg-MAXDOCS :: <integer>, arg-SORTOPTION :: <object>, 
        arg-OTHEROPTIONS :: <object>) => (arg-result :: <object>), name: 
        "UNPROCESSEDFTSEARCH", disp-id: 1029;

  function NOTESDATABASE/UNPROCESSEDSEARCH (arg-FORMULA :: <string>, 
        arg-DATETIME :: <object>, arg-MAXDOCS :: <integer>) => (arg-result 
        :: <object>), name: "UNPROCESSEDSEARCH", disp-id: 1030;

  function NOTESDATABASE/CREATECOPY (arg-SERVER :: <string>, arg-FILE :: 
        <string>) => (arg-result :: <object>), name: "CREATECOPY", disp-id: 
        1031;

  function NOTESDATABASE/OPENIFMODIFIED (arg-SERVER :: <string>, arg-FILE 
        :: <string>, arg-DATETIME :: <object>) => (arg-result :: 
        <integer>), name: "OPENIFMODIFIED", disp-id: 1032;

  function NOTESDATABASE/OPENMAIL () => (), name: "OPENMAIL", disp-id: 
        1033;

  function NOTESDATABASE/CREATE (arg-SERVER :: <string>, arg-FILE :: 
        <string>, arg-OPENFLAG :: <integer>) => (), name: "CREATE", 
        disp-id: 1034;

  function NOTESDATABASE/UPDATEFTINDEX (arg-CREATEFLAG :: <integer>) => (), 
        name: "UPDATEFTINDEX", disp-id: 1035;

  function NOTESDATABASE/REMOVE () => (), name: "REMOVE", disp-id: 1036;

  function NOTESDATABASE/GETDOCUMENTBYID (arg-NOTEID :: <string>) => 
        (arg-result :: <object>), name: "GETDOCUMENTBYID", disp-id: 1037;

  function NOTESDATABASE/GETDOCUMENTBYUNID (arg-NOTEUNID :: <string>) => 
        (arg-result :: <object>), name: "GETDOCUMENTBYUNID", disp-id: 1038;

  function NOTESDATABASE/GRANTACCESS (arg-USERNAME :: <string>, arg-LEVEL 
        :: <integer>) => (), name: "GRANTACCESS", disp-id: 1039;

  function NOTESDATABASE/QUERYACCESS (arg-USERNAME :: <string>) => 
        (arg-result :: <integer>), name: "QUERYACCESS", disp-id: 1040;

  function NOTESDATABASE/REVOKEACCESS (arg-USERNAME :: <string>) => (), 
        name: "REVOKEACCESS", disp-id: 1041;

  function NOTESDATABASE/CREATEFROMTEMPLATE (arg-SERVER :: <string>, 
        arg-FILE :: <string>, arg-INHERITFLAG :: <integer>) => (arg-result 
        :: <object>), name: "CREATEFROMTEMPLATE", disp-id: 1043;

  function NOTESDATABASE/CREATEREPLICA (arg-SERVER :: <string>, arg-FILE :: 
        <string>) => (arg-result :: <object>), name: "CREATEREPLICA", 
        disp-id: 1044;

  function NOTESDATABASE/GETVIEW (arg-VIEWNAME :: <string>) => (arg-result 
        :: <object>), name: "GETVIEW", disp-id: 1046;

  function NOTESDATABASE/REPLICATE (arg-SERVER :: <string>) => (arg-result 
        :: <object>), name: "REPLICATE", disp-id: 1049;

  function NOTESDATABASE/COMPACT () => (arg-result :: type-union(<integer>, 
        <machine-word>)), name: "COMPACT", disp-id: 1500;

  function NOTESDATABASE/OPENBYREPLICAID (arg-SERVER :: <string>, 
        arg-REPLICAID :: <string>) => (arg-result :: <object>), name: 
        "OPENBYREPLICAID", disp-id: 1505;

  function NOTESDATABASE/CREATEDOCUMENT () => (arg-result :: <object>), 
        name: "CREATEDOCUMENT", disp-id: 1507;

  function NOTESDATABASE/OPENURLDB () => (arg-result :: <object>), name: 
        "OPENURLDB", disp-id: 1509;

  function NOTESDATABASE/GETDOCUMENTBYURL (arg-URL :: <string>, arg-RELOAD 
        :: <object>, arg-URLLIST :: <object>, arg-CHARSET :: <object>, 
        arg-WEBUSERNAME :: <object>, arg-WEBPSWD :: <object>, 
        arg-PROXYUSERNAME :: <object>, arg-PROXYPSWD :: <object>) => 
        (arg-result :: <object>), name: "GETDOCUMENTBYURL", disp-id: 1510;

  function NOTESDATABASE/GETURLHEADERINFO (arg-URL :: <string>, 
        arg-HEADERNAME :: <string>, arg-WEBUSERNAME :: <object>, 
        arg-WEBPSWD :: <object>, arg-PROXYUSERNAME :: <object>, 
        arg-PROXYPSWD :: <object>) => (arg-result :: <string>), name: 
        "GETURLHEADERINFO", disp-id: 1511;

  function NOTESDATABASE/OPENWITHFAILOVER (arg-SERVER :: <string>, arg-FILE 
        :: <string>) => (arg-result :: <object>), name: "OPENWITHFAILOVER", 
        disp-id: 1508;

  function NOTESDATABASE/GETPROFILEDOCUMENT (arg-PROFILENAME :: <string>, 
        arg-PROFILEUSERNAME :: <object>) => (arg-result :: <object>), name: 
        "GETPROFILEDOCUMENT", disp-id: 1512;

  function NOTESDATABASE/GETAGENT (arg-AGENTNAME :: <string>) => 
        (arg-result :: <object>), name: "GETAGENT", disp-id: 1513;

  function NOTESDATABASE/GETFORM (arg-FORMNAME :: <string>) => (arg-result 
        :: <object>), name: "GETFORM", disp-id: 1516;
end dispatch-client <NOTESDATABASE>;


/* Dispatch interface: NOTESDBDIRECTORY version 0.0
 * GUID: {2913140A-2EED-1069-BF5D-00DD011186B7}
 */
define dispatch-client <NOTESDBDIRECTORY> ()
  uuid "{2913140A-2EED-1069-BF5D-00DD011186B7}";

  property NOTESDBDIRECTORY/NAME :: <string>, name: "NAME", disp-id: 1240;

  function NOTESDBDIRECTORY/NEW (arg-SERVER :: <string>) => (arg-result :: 
        <object>), name: "NEW", disp-id: 1242;

  function NOTESDBDIRECTORY/GETFIRSTDATABASE (arg-FILETYPE :: <integer>) => 
        (arg-result :: <object>), name: "GETFIRSTDATABASE", disp-id: 1244;

  function NOTESDBDIRECTORY/GETNEXTDATABASE () => (arg-result :: <object>), 
        name: "GETNEXTDATABASE", disp-id: 1243;
end dispatch-client <NOTESDBDIRECTORY>;


/* Dispatch interface: NOTESSESSION version 0.0
 * GUID: {29131401-2EED-1069-BF5D-00DD011186B7}
 */
define dispatch-client <NOTESSESSION> ()
  uuid "{29131401-2EED-1069-BF5D-00DD011186B7}";

  property NOTESSESSION/USERNAME :: <string>, name: "USERNAME", disp-id: 
        1190;

  property NOTESSESSION/PLATFORM :: <string>, name: "PLATFORM", disp-id: 
        1191;

  property NOTESSESSION/CURRENTDATABASE :: <object>, name: 
        "CURRENTDATABASE", disp-id: 1192;

  property NOTESSESSION/CURRENTAGENT :: <object>, name: "CURRENTAGENT", 
        disp-id: 1193;

  property NOTESSESSION/LASTRUN :: <object>, name: "LASTRUN", disp-id: 
        1194;

  property NOTESSESSION/EFFECTIVEUSERNAME :: <string>, name: 
        "EFFECTIVEUSERNAME", disp-id: 1195;

  property NOTESSESSION/ISONSERVER :: <object>, name: "ISONSERVER", 
        disp-id: 1196;

  property NOTESSESSION/LASTEXITSTATUS :: type-union(<integer>, 
        <machine-word>), name: "LASTEXITSTATUS", disp-id: 1197;

  property NOTESSESSION/SAVEDDATA :: <object>, name: "SAVEDDATA", disp-id: 
        1199;

  property NOTESSESSION/ADDRESSBOOKS :: <object>, name: "ADDRESSBOOKS", 
        disp-id: 1198;

  property NOTESSESSION/NOTESVERSION :: <string>, name: "NOTESVERSION", 
        disp-id: 1200;

  property NOTESSESSION/COMMONUSERNAME :: <string>, name: "COMMONUSERNAME", 
        disp-id: 1201;

  property NOTESSESSION/INTERNATIONAL :: <object>, name: "INTERNATIONAL", 
        disp-id: 1213;

  property NOTESSESSION/DOCUMENTCONTEXT :: <object>, name: 
        "DOCUMENTCONTEXT", disp-id: 1216;

  function NOTESSESSION/NEW () => (arg-result :: <object>), name: "NEW", 
        disp-id: 1202;

  function NOTESSESSION/CLOSE () => (), name: "CLOSE", disp-id: 1203;

  function NOTESSESSION/SETENVIRONMENTVAR (arg-VARNAME :: <string>, 
        arg-VALUE :: <object>, arg-ISSYSTEMVAR :: <object>) => (), name: 
        "SETENVIRONMENTVAR", disp-id: 1204;

  function NOTESSESSION/GETENVIRONMENTVALUE (arg-VARNAME :: <string>, 
        arg-ISSYSTEMVAR :: <object>) => (arg-result :: <object>), name: 
        "GETENVIRONMENTVALUE", disp-id: 1205;

  function NOTESSESSION/GETENVIRONMENTSTRING (arg-VARNAME :: <string>, 
        arg-ISSYSTEMVAR :: <object>) => (arg-result :: <string>), name: 
        "GETENVIRONMENTSTRING", disp-id: 1206;

  function NOTESSESSION/UPDATEPROCESSEDDOC (arg-DOCUMENT :: <object>) => 
        (), name: "UPDATEPROCESSEDDOC", disp-id: 1207;

  function NOTESSESSION/GETDATABASE (arg-SERVER :: <string>, arg-FILE :: 
        <string>) => (arg-result :: <object>), name: "GETDATABASE", 
        disp-id: 1208;

  function NOTESSESSION/CREATEDATETIME (arg-DATEEXPR :: <string>) => 
        (arg-result :: <object>), name: "CREATEDATETIME", disp-id: 1209;

  function NOTESSESSION/CREATELOG (arg-PROGRAMNAME :: <string>) => 
        (arg-result :: <object>), name: "CREATELOG", disp-id: 1210;

  function NOTESSESSION/CREATENEWSLETTER (arg-COLLECTION :: <object>) => 
        (arg-result :: <object>), name: "CREATENEWSLETTER", disp-id: 1211;

  function NOTESSESSION/GETDBDIRECTORY (arg-SERVER :: <string>) => 
        (arg-result :: <object>), name: "GETDBDIRECTORY", disp-id: 1212;

  function NOTESSESSION/CREATEDATERANGE () => (arg-result :: <object>), 
        name: "CREATEDATERANGE", disp-id: 1214;

  function NOTESSESSION/FREETIMESEARCH (arg-WINDOW :: <object>, 
        arg-DURATION :: <integer>, arg-NAMES :: <object>, arg-FIRSTFIT :: 
        <object>) => (arg-result :: <object>), name: "FREETIMESEARCH", 
        disp-id: 1215;

  function NOTESSESSION/CREATETIMER () => (arg-result :: <object>), name: 
        "CREATETIMER", disp-id: 1217;

  function NOTESSESSION/CREATENAME (arg-NAME :: <string>) => (arg-result :: 
        <object>), name: "CREATENAME", disp-id: 1218;
end dispatch-client <NOTESSESSION>;


/* Dispatch interface: NOTESNEWSLETTER version 0.0
 * GUID: {29131407-2EED-1069-BF5D-00DD011186B7}
 */
define dispatch-client <NOTESNEWSLETTER> ()
  uuid "{29131407-2EED-1069-BF5D-00DD011186B7}";

  property NOTESNEWSLETTER/DOSUBJECT :: <object>, name: "DOSUBJECT", 
        disp-id: 1220;

  property NOTESNEWSLETTER/DOSCORE :: <object>, name: "DOSCORE", disp-id: 
        1221;

  property NOTESNEWSLETTER/SUBJECTITEMNAME :: <string>, name: 
        "SUBJECTITEMNAME", disp-id: 1222;

  function NOTESNEWSLETTER/NEW (arg-COLLECTION :: <object>) => (arg-result 
        :: <object>), name: "NEW", disp-id: 1223;

  function NOTESNEWSLETTER/FORMATMSGWITHDOCLINKS (arg-DATABASE :: <object>) 
        => (arg-result :: <object>), name: "FORMATMSGWITHDOCLINKS", 
        disp-id: 1224;

  function NOTESNEWSLETTER/FORMATDOCUMENT (arg-DATABASE :: <object>, 
        arg-INDEX :: <integer>) => (arg-result :: <object>), name: 
        "FORMATDOCUMENT", disp-id: 1225;
end dispatch-client <NOTESNEWSLETTER>;


/* Dispatch interface: NOTESLOG version 0.0
 * GUID: {2913140C-2EED-1069-BF5D-00DD011186B7}
 */
define dispatch-client <NOTESLOG> ()
  uuid "{2913140C-2EED-1069-BF5D-00DD011186B7}";

  property NOTESLOG/LOGERRORS :: <object>, name: "LOGERRORS", disp-id: 
        1311;

  property NOTESLOG/LOGACTIONS :: <object>, name: "LOGACTIONS", disp-id: 
        1312;

  property NOTESLOG/NUMACTIONS :: <integer>, name: "NUMACTIONS", disp-id: 
        1313;

  property NOTESLOG/NUMERRORS :: <integer>, name: "NUMERRORS", disp-id: 
        1314;

  property NOTESLOG/OVERWRITEFILE :: <object>, name: "OVERWRITEFILE", 
        disp-id: 1315;

  property NOTESLOG/PROGRAMNAME :: <string>, name: "PROGRAMNAME", disp-id: 
        1316;

  function NOTESLOG/NEW (arg-PROGRAMNAME :: <string>) => (arg-result :: 
        <object>), name: "NEW", disp-id: 1317;

  function NOTESLOG/OPENNOTESLOG (arg-SERVER :: <string>, arg-FILE :: 
        <string>) => (), name: "OPENNOTESLOG", disp-id: 1318;

  function NOTESLOG/OPENFILELOG (arg-PATHNAME :: <string>) => (), name: 
        "OPENFILELOG", disp-id: 1319;

  function NOTESLOG/LOGACTION (arg-ACTION :: <string>) => (), name: 
        "LOGACTION", disp-id: 1320;

  function NOTESLOG/LOGERROR (arg-ERRORCODE :: type-union(<integer>, 
        <machine-word>), arg-ERRORTEXT :: <string>) => (), name: 
        "LOGERROR", disp-id: 1321;

  function NOTESLOG/OPENMAILLOG (arg-RECIPIENTS :: <object>, arg-SUBJECT :: 
        <string>) => (), name: "OPENMAILLOG", disp-id: 1322;

  function NOTESLOG/CLOSE () => (), name: "CLOSE", disp-id: 1323;

  function NOTESLOG/LOGEVENT (arg-TEXT :: <string>, arg-QUEUENAME :: 
        <string>, arg-EVENTTYPE :: <integer>, arg-SEVERITY :: <integer>) => 
        (), name: "LOGEVENT", disp-id: 1324;

  function NOTESLOG/OPENAGENTLOG () => (), name: "OPENAGENTLOG", disp-id: 
        1325;
end dispatch-client <NOTESLOG>;


/* Dispatch interface: NOTESACL version 0.0
 * GUID: {2913140D-2EED-1069-BF5D-00DD011186B7}
 */
define dispatch-client <NOTESACL> ()
  uuid "{2913140D-2EED-1069-BF5D-00DD011186B7}";

  property NOTESACL/ROLES :: <object>, name: "ROLES", disp-id: 1400;

  property NOTESACL/PARENT :: <object>, name: "PARENT", disp-id: 1401;

  property NOTESACL/UNIFORMACCESS :: <object>, name: "UNIFORMACCESS", 
        disp-id: 1423;

  function NOTESACL/GETFIRSTENTRY () => (arg-result :: <object>), name: 
        "GETFIRSTENTRY", disp-id: 1402;

  function NOTESACL/GETNEXTENTRY (arg-CURRENTENTRY :: <object>) => 
        (arg-result :: <object>), name: "GETNEXTENTRY", disp-id: 1403;

  function NOTESACL/GETENTRY (arg-ENTRYNAME :: <string>) => (arg-result :: 
        <object>), name: "GETENTRY", disp-id: 1404;

  function NOTESACL/SAVE () => (), name: "SAVE", disp-id: 1405;

  function NOTESACL/RENAMEROLE (arg-OLDNAME :: <string>, arg-NEWNAME :: 
        <string>) => (), name: "RENAMEROLE", disp-id: 1406;

  function NOTESACL/ADDROLE (arg-ROLENAME :: <string>) => (), name: 
        "ADDROLE", disp-id: 1407;

  function NOTESACL/DELETEROLE (arg-ROLENAME :: <string>) => (), name: 
        "DELETEROLE", disp-id: 1408;

  function NOTESACL/CREATEACLENTRY (arg-ENTRYNAME :: <string>, arg-LEVEL :: 
        <integer>) => (arg-result :: <object>), name: "CREATEACLENTRY", 
        disp-id: 1409;
end dispatch-client <NOTESACL>;


/* Dispatch interface: NOTESACLENTRY version 0.0
 * GUID: {2913140E-2EED-1069-BF5D-00DD011186B7}
 */
define dispatch-client <NOTESACLENTRY> ()
  uuid "{2913140E-2EED-1069-BF5D-00DD011186B7}";

  property NOTESACLENTRY/ROLES :: <object>, name: "ROLES", disp-id: 1410;

  property NOTESACLENTRY/NAME :: <string>, name: "NAME", disp-id: 1411;

  property NOTESACLENTRY/LEVEL :: <integer>, name: "LEVEL", disp-id: 1412;

  property NOTESACLENTRY/CANCREATEPERSONALAGENT :: <object>, name: 
        "CANCREATEPERSONALAGENT", disp-id: 1413;

  property NOTESACLENTRY/CANCREATEPERSONALFOLDER :: <object>, name: 
        "CANCREATEPERSONALFOLDER", disp-id: 1414;

  property NOTESACLENTRY/CANCREATEDOCUMENTS :: <object>, name: 
        "CANCREATEDOCUMENTS", disp-id: 1415;

  property NOTESACLENTRY/CANDELETEDOCUMENTS :: <object>, name: 
        "CANDELETEDOCUMENTS", disp-id: 1416;

  property NOTESACLENTRY/PARENT :: <object>, name: "PARENT", disp-id: 1417;

  function NOTESACLENTRY/NEW (arg-ACL :: <object>, arg-ENTRYNAME :: 
        <string>, arg-LEVEL :: <integer>) => (arg-result :: <object>), 
        name: "NEW", disp-id: 1418;

  function NOTESACLENTRY/ENABLEROLE (arg-ROLENAME :: <string>) => (), name: 
        "ENABLEROLE", disp-id: 1419;

  function NOTESACLENTRY/DISABLEROLE (arg-ROLENAME :: <string>) => (), 
        name: "DISABLEROLE", disp-id: 1420;

  function NOTESACLENTRY/ISROLEENABLED (arg-ROLENAME :: <string>) => 
        (arg-result :: <object>), name: "ISROLEENABLED", disp-id: 1421;

  function NOTESACLENTRY/REMOVE () => (), name: "REMOVE", disp-id: 1422;
end dispatch-client <NOTESACLENTRY>;


/* Dispatch interface: NOTESTIMER version 0.0
 * GUID: {29131412-2EED-1069-BF5D-00DD011186B7}
 */
define dispatch-client <NOTESTIMER> ()
  uuid "{29131412-2EED-1069-BF5D-00DD011186B7}";

  property NOTESTIMER/INTERVAL :: <integer>, name: "INTERVAL", disp-id: 
        1600;

  property NOTESTIMER/COMMENT :: <string>, name: "COMMENT", disp-id: 1601;

  property NOTESTIMER/ENABLED :: <object>, name: "ENABLED", disp-id: 1602;

  function NOTESTIMER/NEW (arg-INTERVAL :: <object>, arg-COMMENT :: 
        <object>) => (arg-result :: <object>), name: "NEW", disp-id: 1603;
end dispatch-client <NOTESTIMER>;


/* Dispatch interface: NOTESNAME version 0.0
 * GUID: {29131413-2EED-1069-BF5D-00DD011186B7}
 */
define dispatch-client <NOTESNAME> ()
  uuid "{29131413-2EED-1069-BF5D-00DD011186B7}";

  property NOTESNAME/ADMD :: <string>, name: "ADMD", disp-id: 1620;

  property NOTESNAME/ABBREVIATED :: <string>, name: "ABBREVIATED", disp-id: 
        1621;

  property NOTESNAME/COUNTRY :: <string>, name: "COUNTRY", disp-id: 1622;

  property NOTESNAME/CANONICAL :: <string>, name: "CANONICAL", disp-id: 
        1623;

  property NOTESNAME/COMMON :: <string>, name: "COMMON", disp-id: 1624;

  property NOTESNAME/GIVEN :: <string>, name: "GIVEN", disp-id: 1625;

  property NOTESNAME/INITIALS :: <string>, name: "INITIALS", disp-id: 1626;

  property NOTESNAME/ORGANIZATION :: <string>, name: "ORGANIZATION", 
        disp-id: 1627;

  property NOTESNAME/ORGUNIT1 :: <string>, name: "ORGUNIT1", disp-id: 1628;

  property NOTESNAME/ORGUNIT2 :: <string>, name: "ORGUNIT2", disp-id: 1629;

  property NOTESNAME/ORGUNIT3 :: <string>, name: "ORGUNIT3", disp-id: 1630;

  property NOTESNAME/ORGUNIT4 :: <string>, name: "ORGUNIT4", disp-id: 1631;

  property NOTESNAME/PRMD :: <string>, name: "PRMD", disp-id: 1632;

  property NOTESNAME/GENERATION :: <string>, name: "GENERATION", disp-id: 
        1633;

  property NOTESNAME/SURNAME :: <string>, name: "SURNAME", disp-id: 1634;

  property NOTESNAME/KEYWORD :: <string>, name: "KEYWORD", disp-id: 1635;

  property NOTESNAME/ISHIERARCHICAL :: <object>, name: "ISHIERARCHICAL", 
        disp-id: 1636;

  function NOTESNAME/NEW (arg-NAME :: <string>) => (arg-result :: 
        <object>), name: "NEW", disp-id: 1637;
end dispatch-client <NOTESNAME>;


/* Dispatch interface: NOTESFORM version 0.0
 * GUID: {29131414-2EED-1069-BF5D-00DD011186B7}
 */
define dispatch-client <NOTESFORM> ()
  uuid "{29131414-2EED-1069-BF5D-00DD011186B7}";

  property NOTESFORM/ISSUBFORM :: <object>, name: "ISSUBFORM", disp-id: 
        1640;

  property NOTESFORM/NAME :: <string>, name: "NAME", disp-id: 1641;

  property NOTESFORM/ALIASES :: <object>, name: "ALIASES", disp-id: 1642;

  property NOTESFORM/READERS :: <object>, name: "READERS", disp-id: 1643;

  property NOTESFORM/FORMUSERS :: <object>, name: "FORMUSERS", disp-id: 
        1644;

  property NOTESFORM/FIELDS :: <object>, name: "FIELDS", disp-id: 1646;

  property NOTESFORM/PROTECTREADERS :: <object>, name: "PROTECTREADERS", 
        disp-id: 1647;

  property NOTESFORM/PROTECTUSERS :: <object>, name: "PROTECTUSERS", 
        disp-id: 1648;

  function NOTESFORM/REMOVE () => (), name: "REMOVE", disp-id: 1645;
end dispatch-client <NOTESFORM>;


/* Dispatch interface: NOTESINTERNATIONAL version 0.0
 * GUID: {29131415-2EED-1069-BF5D-00DD011186B7}
 */
define dispatch-client <NOTESINTERNATIONAL> ()
  uuid "{29131415-2EED-1069-BF5D-00DD011186B7}";

  property NOTESINTERNATIONAL/ISCURRENCYSUFFIX :: <object>, name: 
        "ISCURRENCYSUFFIX", disp-id: 1660;

  property NOTESINTERNATIONAL/ISCURRENCYSPACE :: <object>, name: 
        "ISCURRENCYSPACE", disp-id: 1661;

  property NOTESINTERNATIONAL/ISCURRENCYZERO :: <object>, name: 
        "ISCURRENCYZERO", disp-id: 1662;

  property NOTESINTERNATIONAL/ISTIME24HOUR :: <object>, name: 
        "ISTIME24HOUR", disp-id: 1663;

  property NOTESINTERNATIONAL/ISDST :: <object>, name: "ISDST", disp-id: 
        1664;

  property NOTESINTERNATIONAL/ISDATEMDY :: <object>, name: "ISDATEMDY", 
        disp-id: 1665;

  property NOTESINTERNATIONAL/ISDATEDMY :: <object>, name: "ISDATEDMY", 
        disp-id: 1666;

  property NOTESINTERNATIONAL/ISDATEYMD :: <object>, name: "ISDATEYMD", 
        disp-id: 1667;

  property NOTESINTERNATIONAL/CURRENCYDIGITS :: <integer>, name: 
        "CURRENCYDIGITS", disp-id: 1668;

  property NOTESINTERNATIONAL/TIMEZONE :: <integer>, name: "TIMEZONE", 
        disp-id: 1669;

  property NOTESINTERNATIONAL/AMSTRING :: <string>, name: "AMSTRING", 
        disp-id: 1670;

  property NOTESINTERNATIONAL/PMSTRING :: <string>, name: "PMSTRING", 
        disp-id: 1671;

  property NOTESINTERNATIONAL/CURRENCYSYMBOL :: <string>, name: 
        "CURRENCYSYMBOL", disp-id: 1672;

  property NOTESINTERNATIONAL/THOUSANDSSEP :: <string>, name: 
        "THOUSANDSSEP", disp-id: 1673;

  property NOTESINTERNATIONAL/DECIMALSEP :: <string>, name: "DECIMALSEP", 
        disp-id: 1674;

  property NOTESINTERNATIONAL/DATESEP :: <string>, name: "DATESEP", 
        disp-id: 1675;

  property NOTESINTERNATIONAL/TIMESEP :: <string>, name: "TIMESEP", 
        disp-id: 1676;

  property NOTESINTERNATIONAL/YESTERDAY :: <string>, name: "YESTERDAY", 
        disp-id: 1677;

  property NOTESINTERNATIONAL/TODAY :: <string>, name: "TODAY", disp-id: 
        1678;

  property NOTESINTERNATIONAL/TOMORROW :: <string>, name: "TOMORROW", 
        disp-id: 1679;
end dispatch-client <NOTESINTERNATIONAL>;


/* Dispatch interface: NOTESDATERANGE version 0.0
 * GUID: {29131416-2EED-1069-BF5D-00DD011186B7}
 */
define dispatch-client <NOTESDATERANGE> ()
  uuid "{29131416-2EED-1069-BF5D-00DD011186B7}";

  property NOTESDATERANGE/STARTDATETIME :: <object>, name: "STARTDATETIME", 
        disp-id: 1690;

  property NOTESDATERANGE/ENDDATETIME :: <object>, name: "ENDDATETIME", 
        disp-id: 1691;

  property NOTESDATERANGE/TEXT :: <string>, name: "TEXT", disp-id: 1692;
end dispatch-client <NOTESDATERANGE>;
