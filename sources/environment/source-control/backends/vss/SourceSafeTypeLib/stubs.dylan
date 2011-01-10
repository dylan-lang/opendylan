Module: type-library-module
Creator: created from "C:\Users\Palter\Dylan\Sources\environment\source-control\backends\vss\SourceSafeTypeLib\type-library.spec" at 19:54 1999- 5- 5 Eastern Daylight Time.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/* Type library: SourceSafeTypeLib version 5.1
 * Description: Microsoft SourceSafe 6.0 Type Library
 * GUID: {783CD4E0-9D54-11CF-B8EE-00608CC9A71F}
 */


/* Enumeration: VSSFlags
 * Description: SourceSafe operation flags
 */
define constant <VSSFlags> = type-union(<integer>, <machine-word>);
define constant $VSSFLAG-USERRONO = 1;
define constant $VSSFLAG-USERROYES = 2;
define constant $VSSFLAG-TIMENOW = 4;
define constant $VSSFLAG-TIMEMOD = 8;
define constant $VSSFLAG-TIMEUPD = 12;
define constant $VSSFLAG-EOLCR = 16;
define constant $VSSFLAG-EOLLF = 32;
define constant $VSSFLAG-EOLCRLF = 48;
define constant $VSSFLAG-REPASK = 64;
define constant $VSSFLAG-REPREPLACE = 128;
define constant $VSSFLAG-REPSKIP = 192;
define constant $VSSFLAG-REPMERGE = 256;
define constant $VSSFLAG-CMPFULL = 512;
define constant $VSSFLAG-CMPTIME = 1024;
define constant $VSSFLAG-CMPCHKSUM = 1536;
define constant $VSSFLAG-CMPFAIL = 2048;
define constant $VSSFLAG-RECURSNO = 4096;
define constant $VSSFLAG-RECURSYES = 8192;
define constant $VSSFLAG-FORCEDIRNO = 16384;
define constant $VSSFLAG-FORCEDIRYES = 32768;
define constant $VSSFLAG-KEEPNO = 65536;
define constant $VSSFLAG-KEEPYES = 131072;
define constant $VSSFLAG-DELNO = 262144;
define constant $VSSFLAG-DELYES = 524288;
define constant $VSSFLAG-DELNOREPLACE = 786432;
define constant $VSSFLAG-BINTEST = 1048576;
define constant $VSSFLAG-BINBINARY = 2097152;
define constant $VSSFLAG-BINTEXT = 3145728;
define constant $VSSFLAG-DELTAYES = 4194304;
define constant $VSSFLAG-DELTANO = 8388608;
define constant $VSSFLAG-UPDASK = 16777216;
define constant $VSSFLAG-UPDUPDATE = 33554432;
define constant $VSSFLAG-UPDUNCH = 50331648;
define constant $VSSFLAG-GETYES = 67108864;
define constant $VSSFLAG-GETNO = 134217728;
define constant $VSSFLAG-CHKEXCLUSIVEYES = 268435456;
define constant $VSSFLAG-CHKEXCLUSIVENO = as(<machine-word>, #x20000000);
define constant $VSSFLAG-HISTIGNOREFILES = as(<machine-word>, #x40000000);


/* Enumeration: VSSFileStatus
 * Description: SourceSafe file checkout status
 */
define constant <VSSFileStatus> = type-union(<integer>, <machine-word>);
define constant $VSSFILE-NOTCHECKEDOUT = 0;
define constant $VSSFILE-CHECKEDOUT = 1;
define constant $VSSFILE-CHECKEDOUT-ME = 2;


/* Enumeration: VSSItemType
 * Description: SourceSafe item type
 */
define constant <VSSItemType> = type-union(<integer>, <machine-word>);
define constant $VSSITEM-PROJECT = 0;
define constant $VSSITEM-FILE = 1;


/* Enumeration: VSSRights
 * Description: SourceSafe rights values
 */
define constant <VSSRights> = type-union(<integer>, <machine-word>);
define constant $VSSRIGHTS-READ = 1;
define constant $VSSRIGHTS-CHKUPD = 2;
define constant $VSSRIGHTS-ADDRENREM = 4;
define constant $VSSRIGHTS-DESTROY = 8;
define constant $VSSRIGHTS-ALL = 15;
define constant $VSSRIGHTS-INHERITED = 16;


/* Dispatch interface: IVSSItemOld version 0.0
 * GUID: {783CD4E1-9D54-11CF-B8EE-00608CC9A71F}
 * Description: SourceSafe item interface
 */
define dispatch-client <IVSSItemOld> ()
  uuid "{783CD4E1-9D54-11CF-B8EE-00608CC9A71F}";

  constant property IVSSItemOld/Spec :: <string>, name: "Spec", disp-id: 1;

  property IVSSItemOld/Binary :: <boolean>, name: "Binary", disp-id: 2;

  property IVSSItemOld/Deleted :: <boolean>, name: "Deleted", disp-id: 3;

  constant property IVSSItemOld/Type :: type-union(<integer>, 
        <machine-word>), name: "Type", disp-id: 4;

  property IVSSItemOld/LocalSpec :: <string>, name: "LocalSpec", disp-id: 
        5;

  property IVSSItemOld/Name :: <string>, name: "Name", disp-id: 6;

  constant property IVSSItemOld/Parent :: <IVSSItem>, name: "Parent", 
        disp-id: 7;

  constant property IVSSItemOld/VersionNumber :: type-union(<integer>, 
        <machine-word>), name: "VersionNumber", disp-id: 8;

  constant property IVSSItemOld/Items (/*optional*/ arg-IncludeDeleted :: 
        <boolean>) :: <IVSSItems>, name: "Items", disp-id: 9;

  function IVSSItemOld/Get (/*optional*/ arg-Local :: inout-ref(<BSTR>), 
        /*optional*/ arg-iFlags :: type-union(<integer>, <machine-word>)) 
        => (), name: "Get", disp-id: 10;

  function IVSSItemOld/Checkout (/*optional*/ arg-Comment :: <string>, 
        /*optional*/ arg-Local :: <string>, /*optional*/ arg-iFlags :: 
        type-union(<integer>, <machine-word>)) => (), name: "Checkout", 
        disp-id: 11;

  function IVSSItemOld/Checkin (/*optional*/ arg-Comment :: <string>, 
        /*optional*/ arg-Local :: <string>, /*optional*/ arg-iFlags :: 
        type-union(<integer>, <machine-word>)) => (), name: "Checkin", 
        disp-id: 12;

  function IVSSItemOld/UndoCheckout (/*optional*/ arg-Local :: <string>, 
        /*optional*/ arg-iFlags :: type-union(<integer>, <machine-word>)) 
        => (), name: "UndoCheckout", disp-id: 13;

  constant property IVSSItemOld/IsCheckedOut :: type-union(<integer>, 
        <machine-word>), name: "IsCheckedOut", disp-id: 14;

  constant property IVSSItemOld/Checkouts :: <IVSSCheckouts>, name: 
        "Checkouts", disp-id: 15;

  constant property IVSSItemOld/IsDifferent (/*optional*/ arg-Local :: 
        <string>) :: <boolean>, name: "IsDifferent", disp-id: 16;

  function IVSSItemOld/Add (arg-Local :: <string>, /*optional*/ arg-Comment 
        :: <string>, /*optional*/ arg-iFlags :: type-union(<integer>, 
        <machine-word>)) => (arg-result :: <IVSSItem>), name: "Add", 
        disp-id: 17;

  function IVSSItemOld/NewSubproject (arg-Name :: <string>, /*optional*/ 
        arg-Comment :: <string>) => (arg-result :: <IVSSItem>), name: 
        "NewSubproject", disp-id: 18;

  function IVSSItemOld/Share (arg-pIItem :: <IVSSItem>, /*optional*/ 
        arg-Comment :: <string>, /*optional*/ arg-iFlags :: 
        type-union(<integer>, <machine-word>)) => (), name: "Share", 
        disp-id: 19;

  function IVSSItemOld/Destroy () => (), name: "Destroy", disp-id: 20;

  function IVSSItemOld/Move (arg-pINewParent :: <IVSSItem>) => (), name: 
        "Move", disp-id: 21;

  function IVSSItemOld/Label (arg-Label :: <string>, /*optional*/ 
        arg-Comment :: <string>) => (), name: "Label", disp-id: 22;

  constant property IVSSItemOld/Versions (/*optional*/ arg-iFlags :: 
        type-union(<integer>, <machine-word>)) :: <IVSSVersions>, name: 
        "Versions", disp-id: 23;

  constant property IVSSItemOld/Version (/*optional*/ arg-Version :: 
        <object>) :: <IVSSItem>, name: "Version", disp-id: 24;

end dispatch-client <IVSSItemOld>;

/* Dispatch interface: IVSSItem version 0.0
 * GUID: {2A0DE0E7-2E9F-11D0-9236-00AA00A1EB95}
 * Description: SourceSafe item interface
 */
define dispatch-client <IVSSItem> ()
  uuid "{2A0DE0E7-2E9F-11D0-9236-00AA00A1EB95}";

  constant property IVSSItem/Spec :: <string>, name: "Spec", disp-id: 1;

  property IVSSItem/Binary :: <boolean>, name: "Binary", disp-id: 2;

  property IVSSItem/Deleted :: <boolean>, name: "Deleted", disp-id: 3;

  constant property IVSSItem/Type :: type-union(<integer>, <machine-word>), 
        name: "Type", disp-id: 4;

  property IVSSItem/LocalSpec :: <string>, name: "LocalSpec", disp-id: 5;

  property IVSSItem/Name :: <string>, name: "Name", disp-id: 6;

  constant property IVSSItem/Parent :: <IVSSItem>, name: "Parent", disp-id: 
        7;

  constant property IVSSItem/VersionNumber :: type-union(<integer>, 
        <machine-word>), name: "VersionNumber", disp-id: 8;

  constant property IVSSItem/Items (/*optional*/ arg-IncludeDeleted :: 
        <boolean>) :: <IVSSItems>, name: "Items", disp-id: 9;

  function IVSSItem/Get (/*optional*/ arg-Local :: inout-ref(<BSTR>), 
        /*optional*/ arg-iFlags :: type-union(<integer>, <machine-word>)) 
        => (), name: "Get", disp-id: 10;

  function IVSSItem/Checkout (/*optional*/ arg-Comment :: <string>, 
        /*optional*/ arg-Local :: <string>, /*optional*/ arg-iFlags :: 
        type-union(<integer>, <machine-word>)) => (), name: "Checkout", 
        disp-id: 11;

  function IVSSItem/Checkin (/*optional*/ arg-Comment :: <string>, 
        /*optional*/ arg-Local :: <string>, /*optional*/ arg-iFlags :: 
        type-union(<integer>, <machine-word>)) => (), name: "Checkin", 
        disp-id: 12;

  function IVSSItem/UndoCheckout (/*optional*/ arg-Local :: <string>, 
        /*optional*/ arg-iFlags :: type-union(<integer>, <machine-word>)) 
        => (), name: "UndoCheckout", disp-id: 13;

  constant property IVSSItem/IsCheckedOut :: type-union(<integer>, 
        <machine-word>), name: "IsCheckedOut", disp-id: 14;

  constant property IVSSItem/Checkouts :: <IVSSCheckouts>, name: 
        "Checkouts", disp-id: 15;

  constant property IVSSItem/IsDifferent (/*optional*/ arg-Local :: 
        <string>) :: <boolean>, name: "IsDifferent", disp-id: 16;

  function IVSSItem/Add (arg-Local :: <string>, /*optional*/ arg-Comment :: 
        <string>, /*optional*/ arg-iFlags :: type-union(<integer>, 
        <machine-word>)) => (arg-result :: <IVSSItem>), name: "Add", 
        disp-id: 17;

  function IVSSItem/NewSubproject (arg-Name :: <string>, /*optional*/ 
        arg-Comment :: <string>) => (arg-result :: <IVSSItem>), name: 
        "NewSubproject", disp-id: 18;

  function IVSSItem/Share (arg-pIItem :: <IVSSItem>, /*optional*/ 
        arg-Comment :: <string>, /*optional*/ arg-iFlags :: 
        type-union(<integer>, <machine-word>)) => (), name: "Share", 
        disp-id: 19;

  function IVSSItem/Destroy () => (), name: "Destroy", disp-id: 20;

  function IVSSItem/Move (arg-pINewParent :: <IVSSItem>) => (), name: 
        "Move", disp-id: 21;

  function IVSSItem/Label (arg-Label :: <string>, /*optional*/ arg-Comment 
        :: <string>) => (), name: "Label", disp-id: 22;

  constant property IVSSItem/Versions (/*optional*/ arg-iFlags :: 
        type-union(<integer>, <machine-word>)) :: <IVSSVersions>, name: 
        "Versions", disp-id: 23;

  constant property IVSSItem/Version (/*optional*/ arg-Version :: <object>) 
        :: <IVSSItem>, name: "Version", disp-id: 24;

  constant property IVSSItem/Links :: <IVSSItems>, name: "Links", disp-id: 
        25;

  function IVSSItem/Branch (/*optional*/ arg-Comment :: <string>, 
        /*optional*/ arg-iFlags :: type-union(<integer>, <machine-word>)) 
        => (arg-result :: <IVSSItem>), name: "Branch", disp-id: 26;
end dispatch-client <IVSSItem>;


/* Dispatch interface: IVSSVersions version 0.0
 * GUID: {783CD4E7-9D54-11CF-B8EE-00608CC9A71F}
 * Description: SourceSafe versions collection
 */
define dispatch-client <IVSSVersions> ()
  uuid "{783CD4E7-9D54-11CF-B8EE-00608CC9A71F}";

  function IVSSVersions/_NewEnum () => (arg-result :: <LPUNKNOWN>), name: 
        "_NewEnum", disp-id: -4;
end dispatch-client <IVSSVersions>;


/* Dispatch interface: IVSSVersionOld version 0.0
 * GUID: {783CD4E8-9D54-11CF-B8EE-00608CC9A71F}
 * Description: SourceSafe version information
 */
define dispatch-client <IVSSVersionOld> ()
  uuid "{783CD4E8-9D54-11CF-B8EE-00608CC9A71F}";

  constant property IVSSVersionOld/Username :: <string>, name: "Username", 
        disp-id: 1;

  constant property IVSSVersionOld/VersionNumber :: type-union(<integer>, 
        <machine-word>), name: "VersionNumber", disp-id: 2;

  constant property IVSSVersionOld/Action :: <string>, name: "Action", 
        disp-id: 3;

  constant property IVSSVersionOld/Date :: <double-float>, name: "Date", 
        disp-id: 4;

  constant property IVSSVersionOld/Comment :: <string>, name: "Comment", 
        disp-id: 5;

  constant property IVSSVersionOld/Label :: <string>, name: "Label", 
        disp-id: 6;

  constant property IVSSVersionOld/VSSItem :: <IVSSItem>, name: "VSSItem", 
        disp-id: 7;
end dispatch-client <IVSSVersionOld>;


/* Dispatch interface: IVSSVersion version 0.0
 * GUID: {2A0DE0E9-2E9F-11D0-9236-00AA00A1EB95}
 * Description: SourceSafe version information
 */
define dispatch-client <IVSSVersion> ()
  uuid "{2A0DE0E9-2E9F-11D0-9236-00AA00A1EB95}";

  constant property IVSSVersion/Username :: <string>, name: "Username", 
        disp-id: 1;

  constant property IVSSVersion/VersionNumber :: type-union(<integer>, 
        <machine-word>), name: "VersionNumber", disp-id: 2;

  constant property IVSSVersion/Action :: <string>, name: "Action", 
        disp-id: 3;

  constant property IVSSVersion/Date :: <double-float>, name: "Date", 
        disp-id: 4;

  constant property IVSSVersion/Comment :: <string>, name: "Comment", 
        disp-id: 5;

  constant property IVSSVersion/Label :: <string>, name: "Label", disp-id: 
        6;

  constant property IVSSVersion/VSSItem :: <IVSSItem>, name: "VSSItem", 
        disp-id: 7;

  constant property IVSSVersion/LabelComment :: <string>, name: 
        "LabelComment", disp-id: 8;
end dispatch-client <IVSSVersion>;


/* Dispatch interface: IVSSItems version 0.0
 * GUID: {783CD4E5-9D54-11CF-B8EE-00608CC9A71F}
 * Description: SourceSafe items collection interface
 */
define dispatch-client <IVSSItems> ()
  uuid "{783CD4E5-9D54-11CF-B8EE-00608CC9A71F}";

  size constant property IVSSItems/Count :: type-union(<integer>, 
        <machine-word>), name: "Count", disp-id: 1;

  element constant property IVSSItems/Item (arg-sItem :: <object>) :: 
        <IVSSItem>, name: "Item", disp-id: 0;

  function IVSSItems/_NewEnum () => (arg-result :: <LPUNKNOWN>), name: 
        "_NewEnum", disp-id: -4;
end dispatch-client <IVSSItems>;


/* Dispatch interface: IVSSCheckouts version 0.0
 * GUID: {8903A770-F55F-11CF-9227-00AA00A1EB95}
 * Description: SourceSafe checkouts collection interface
 */
define dispatch-client <IVSSCheckouts> ()
  uuid "{8903A770-F55F-11CF-9227-00AA00A1EB95}";

  size constant property IVSSCheckouts/Count :: type-union(<integer>, 
        <machine-word>), name: "Count", disp-id: 1;

  element constant property IVSSCheckouts/Item (arg-sItem :: <object>) :: 
        <IVSSCheckout>, name: "Item", disp-id: 0;

  function IVSSCheckouts/_NewEnum () => (arg-result :: <LPUNKNOWN>), name: 
        "_NewEnum", disp-id: -4;
end dispatch-client <IVSSCheckouts>;


/* Dispatch interface: IVSSCheckout version 0.0
 * GUID: {783CD4E6-9D54-11CF-B8EE-00608CC9A71F}
 * Description: SourceSafe checkouts interface
 */
define dispatch-client <IVSSCheckout> ()
  uuid "{783CD4E6-9D54-11CF-B8EE-00608CC9A71F}";

  constant property IVSSCheckout/Username :: <string>, name: "Username", 
        disp-id: 1;

  constant property IVSSCheckout/Date :: <double-float>, name: "Date", 
        disp-id: 2;

  constant property IVSSCheckout/LocalSpec :: <string>, name: "LocalSpec", 
        disp-id: 3;

  constant property IVSSCheckout/Machine :: <string>, name: "Machine", 
        disp-id: 4;

  constant property IVSSCheckout/Project :: <string>, name: "Project", 
        disp-id: 5;

  constant property IVSSCheckout/Comment :: <string>, name: "Comment", 
        disp-id: 6;

  constant property IVSSCheckout/VersionNumber :: type-union(<integer>, 
        <machine-word>), name: "VersionNumber", disp-id: 7;
end dispatch-client <IVSSCheckout>;


/* Dispatch interface: IVSSDatabaseOld version 0.0
 * GUID: {783CD4E2-9D54-11CF-B8EE-00608CC9A71F}
 * Description: SourceSafe database object interface
 */
define dispatch-client <IVSSDatabaseOld> ()
  uuid "{783CD4E2-9D54-11CF-B8EE-00608CC9A71F}";

  function IVSSDatabaseOld/Open (/*optional*/ arg-SrcSafeIni :: <string>, 
        /*optional*/ arg-Username :: <string>, /*optional*/ arg-Password :: 
        <string>) => (), name: "Open", disp-id: 1;

  constant property IVSSDatabaseOld/SrcSafeIni :: <string>, name: 
        "SrcSafeIni", disp-id: 2;

  constant property IVSSDatabaseOld/DatabaseName :: <string>, name: 
        "DatabaseName", disp-id: 3;

  constant property IVSSDatabaseOld/Username :: <string>, name: "Username", 
        disp-id: 4;

  property IVSSDatabaseOld/CurrentProject :: <string>, name: 
        "CurrentProject", disp-id: 5;

  constant property IVSSDatabaseOld/VSSItem (arg-Spec :: <string>, 
        /*optional*/ arg-Deleted :: <boolean>) :: <IVSSItem>, name: 
        "VSSItem", disp-id: 6;
end dispatch-client <IVSSDatabaseOld>;


/* Dispatch interface: IVSSDatabase version 0.0
 * GUID: {2A0DE0E2-2E9F-11D0-9236-00AA00A1EB95}
 * Description: SourceSafe database object interface
 */
define dispatch-client <IVSSDatabase> ()
  uuid "{2A0DE0E2-2E9F-11D0-9236-00AA00A1EB95}";

  function IVSSDatabase/Open (/*optional*/ arg-SrcSafeIni :: <string>, 
        /*optional*/ arg-Username :: <string>, /*optional*/ arg-Password :: 
        <string>) => (), name: "Open", disp-id: 1;

  constant property IVSSDatabase/SrcSafeIni :: <string>, name: 
        "SrcSafeIni", disp-id: 2;

  constant property IVSSDatabase/DatabaseName :: <string>, name: 
        "DatabaseName", disp-id: 3;

  constant property IVSSDatabase/Username :: <string>, name: "Username", 
        disp-id: 4;

  property IVSSDatabase/CurrentProject :: <string>, name: "CurrentProject", 
        disp-id: 5;

  constant property IVSSDatabase/VSSItem (arg-Spec :: <string>, 
        /*optional*/ arg-Deleted :: <boolean>) :: <IVSSItem>, name: 
        "VSSItem", disp-id: 6;

  function IVSSDatabase/AddUser (arg-User :: <string>, arg-Password :: 
        <string>, arg-ReadOnly :: <boolean>) => (arg-result :: <IVSSUser>), 
        name: "AddUser", disp-id: 7;

  constant property IVSSDatabase/User (arg-Name :: <string>) :: <IVSSUser>, 
        name: "User", disp-id: 8;

  constant property IVSSDatabase/Users :: <IVSSUsers>, name: "Users", 
        disp-id: 9;

  property IVSSDatabase/ProjectRightsEnabled :: <boolean>, name: 
        "ProjectRightsEnabled", disp-id: 10;

  property IVSSDatabase/DefaultProjectRights :: type-union(<integer>, 
        <machine-word>), name: "DefaultProjectRights", disp-id: 11;
end dispatch-client <IVSSDatabase>;


/* Dispatch interface: IVSSUser version 0.0
 * GUID: {2A0DE0E3-2E9F-11D0-9236-00AA00A1EB95}
 * Description: SourceSafe user object interface
 */
define dispatch-client <IVSSUser> ()
  uuid "{2A0DE0E3-2E9F-11D0-9236-00AA00A1EB95}";

  function IVSSUser/Delete () => (), name: "Delete", disp-id: 1;

  property IVSSUser/Name :: <string>, name: "Name", disp-id: 2;

  write-only property IVSSUser/Password :: <string>, name: "Password", 
        disp-id: 3;

  property IVSSUser/ReadOnly :: <boolean>, name: "ReadOnly", disp-id: 4;

  property IVSSUser/ProjectRights (/*optional*/ arg-Project :: <string>) :: 
        type-union(<integer>, <machine-word>), name: "ProjectRights", 
        disp-id: 5;

  function IVSSUser/RemoveProjectRights (arg-Project :: <string>) => (), 
        name: "RemoveProjectRights", disp-id: 6;
end dispatch-client <IVSSUser>;


/* Dispatch interface: IVSSUsers version 0.0
 * GUID: {2A0DE0E4-2E9F-11D0-9236-00AA00A1EB95}
 * Description: SourceSafe users collection interface
 */
define dispatch-client <IVSSUsers> ()
  uuid "{2A0DE0E4-2E9F-11D0-9236-00AA00A1EB95}";

  size constant property IVSSUsers/Count :: type-union(<integer>, 
        <machine-word>), name: "Count", disp-id: 1;

  element constant property IVSSUsers/Item (arg-sItem :: <object>) :: 
        <IVSSUser>, name: "Item", disp-id: 0;

  function IVSSUsers/_NewEnum () => (arg-result :: <LPUNKNOWN>), name: 
        "_NewEnum", disp-id: -4;
end dispatch-client <IVSSUsers>;


/* COM class: VSSItem version 0.0
 * GUID: {783CD4E3-9D54-11CF-B8EE-00608CC9A71F}
 */
define constant $VSSItem-class-id = as(<REFCLSID>, 
        "{783CD4E3-9D54-11CF-B8EE-00608CC9A71F}");

define function make-VSSItem () => (default-interface :: <IVSSItem>, 
        interface-2 :: <IVSSItemOld>)
  let default-interface = make(<IVSSItem>, class-id: $VSSItem-class-id);
  values(default-interface,
         make(<IVSSItemOld>, disp-interface: default-interface))
end function make-VSSItem;


/* COM class: VSSVersion version 0.0
 * GUID: {783CD4EC-9D54-11CF-B8EE-00608CC9A71F}
 */
define constant $VSSVersion-class-id = as(<REFCLSID>, 
        "{783CD4EC-9D54-11CF-B8EE-00608CC9A71F}");

define function make-VSSVersion () => (default-interface :: <IVSSVersion>)
  let default-interface = make(<IVSSVersion>, class-id: 
        $VSSVersion-class-id);
  values(default-interface)
end function make-VSSVersion;


/* COM class: VSSCheckout version 0.0
 * GUID: {2A0DE0E0-2E9F-11D0-9236-00AA00A1EB95}
 */
define constant $VSSCheckout-class-id = as(<REFCLSID>, 
        "{2A0DE0E0-2E9F-11D0-9236-00AA00A1EB95}");

define function make-VSSCheckout () => (default-interface :: 
        <IVSSCheckout>)
  let default-interface = make(<IVSSCheckout>, class-id: 
        $VSSCheckout-class-id);
  values(default-interface)
end function make-VSSCheckout;


/* COM class: VSSDatabase version 0.0
 * GUID: {783CD4E4-9D54-11CF-B8EE-00608CC9A71F}
 */
define constant $VSSDatabase-class-id = as(<REFCLSID>, 
        "{783CD4E4-9D54-11CF-B8EE-00608CC9A71F}");

define function make-VSSDatabase () => (default-interface :: 
        <IVSSDatabase>, interface-2 :: <IVSSDatabaseOld>)
  let default-interface = make(<IVSSDatabase>, class-id: 
        $VSSDatabase-class-id);
  values(default-interface,
         make(<IVSSDatabaseOld>, disp-interface: default-interface))
end function make-VSSDatabase;


/* COM class: VSSUser version 0.0
 * GUID: {2A0DE0E5-2E9F-11D0-9236-00AA00A1EB95}
 */
define constant $VSSUser-class-id = as(<REFCLSID>, 
        "{2A0DE0E5-2E9F-11D0-9236-00AA00A1EB95}");

define function make-VSSUser () => (default-interface :: <IVSSUser>)
  let default-interface = make(<IVSSUser>, class-id: $VSSUser-class-id);
  values(default-interface)
end function make-VSSUser;


/* Dispatch interface: IVSS version 0.0
 * GUID: {783CD4EB-9D54-11CF-B8EE-00608CC9A71F}
 * Description: SourceSafe application interface
 */
define dispatch-client <IVSS> ()
  uuid "{783CD4EB-9D54-11CF-B8EE-00608CC9A71F}";

  constant property IVSS/VSSDatabase :: <IVSSDatabase>, name: 
        "VSSDatabase", disp-id: 1;
end dispatch-client <IVSS>;


/* COM class: VSSApp version 0.0
 * GUID: {2A0DE0E1-2E9F-11D0-9236-00AA00A1EB95}
 */
define constant $VSSApp-class-id = as(<REFCLSID>, 
        "{2A0DE0E1-2E9F-11D0-9236-00AA00A1EB95}");

define function make-VSSApp () => (default-interface :: <IVSS>)
  /* Translation error: source interface VSSApp not supported. */
  /* Translation error: source interface VSSApp not supported. */
  let default-interface = make(<IVSS>, class-id: $VSSApp-class-id);
  values(default-interface)
end function make-VSSApp;

