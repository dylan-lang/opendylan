Module: dylan-user
Creator: created from "C:\Users\Palter\Dylan\Sources\environment\source-control\backends\vss\SourceSafeTypeLib\type-library.spec" at 19:54 1999- 5- 5 Eastern Daylight Time.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define module type-library-module
  use functional-dylan;
  use ole-automation;
  use c-ffi;
  export <VSSFlags>, $VSSFLAG-USERRONO, $VSSFLAG-USERROYES, 
        $VSSFLAG-TIMENOW, $VSSFLAG-TIMEMOD, $VSSFLAG-TIMEUPD, 
        $VSSFLAG-EOLCR, $VSSFLAG-EOLLF, $VSSFLAG-EOLCRLF, $VSSFLAG-REPASK, 
        $VSSFLAG-REPREPLACE, $VSSFLAG-REPSKIP, $VSSFLAG-REPMERGE, 
        $VSSFLAG-CMPFULL, $VSSFLAG-CMPTIME, $VSSFLAG-CMPCHKSUM, 
        $VSSFLAG-CMPFAIL, $VSSFLAG-RECURSNO, $VSSFLAG-RECURSYES, 
        $VSSFLAG-FORCEDIRNO, $VSSFLAG-FORCEDIRYES, $VSSFLAG-KEEPNO, 
        $VSSFLAG-KEEPYES, $VSSFLAG-DELNO, $VSSFLAG-DELYES, 
        $VSSFLAG-DELNOREPLACE, $VSSFLAG-BINTEST, $VSSFLAG-BINBINARY, 
        $VSSFLAG-BINTEXT, $VSSFLAG-DELTAYES, $VSSFLAG-DELTANO, 
        $VSSFLAG-UPDASK, $VSSFLAG-UPDUPDATE, $VSSFLAG-UPDUNCH, 
        $VSSFLAG-GETYES, $VSSFLAG-GETNO, $VSSFLAG-CHKEXCLUSIVEYES, 
        $VSSFLAG-CHKEXCLUSIVENO, $VSSFLAG-HISTIGNOREFILES;
  export <VSSFileStatus>, $VSSFILE-NOTCHECKEDOUT, $VSSFILE-CHECKEDOUT, 
        $VSSFILE-CHECKEDOUT-ME;
  export <VSSItemType>, $VSSITEM-PROJECT, $VSSITEM-FILE;
  export <VSSRights>, $VSSRIGHTS-READ, $VSSRIGHTS-CHKUPD, 
        $VSSRIGHTS-ADDRENREM, $VSSRIGHTS-DESTROY, $VSSRIGHTS-ALL, 
        $VSSRIGHTS-INHERITED;
  export <IVSSItemOld>, IVSSItemOld/Spec, IVSSItemOld/Binary, 
        IVSSItemOld/Binary-setter, IVSSItemOld/Deleted, 
        IVSSItemOld/Deleted-setter, IVSSItemOld/Type, 
        IVSSItemOld/LocalSpec, IVSSItemOld/LocalSpec-setter, 
        IVSSItemOld/Name, IVSSItemOld/Name-setter, IVSSItemOld/Parent, 
        IVSSItemOld/VersionNumber, IVSSItemOld/Items, IVSSItemOld/Get, 
        IVSSItemOld/Checkout, IVSSItemOld/Checkin, 
        IVSSItemOld/UndoCheckout, IVSSItemOld/IsCheckedOut, 
        IVSSItemOld/Checkouts, IVSSItemOld/IsDifferent, IVSSItemOld/Add, 
        IVSSItemOld/NewSubproject, IVSSItemOld/Share, IVSSItemOld/Destroy, 
        IVSSItemOld/Move, IVSSItemOld/Label, IVSSItemOld/Versions, 
        IVSSItemOld/Version;
  export <IVSSItem>, IVSSItem/Spec, IVSSItem/Binary, 
        IVSSItem/Binary-setter, IVSSItem/Deleted, IVSSItem/Deleted-setter, 
        IVSSItem/Type, IVSSItem/LocalSpec, IVSSItem/LocalSpec-setter, 
        IVSSItem/Name, IVSSItem/Name-setter, IVSSItem/Parent, 
        IVSSItem/VersionNumber, IVSSItem/Items, IVSSItem/Get, 
        IVSSItem/Checkout, IVSSItem/Checkin, IVSSItem/UndoCheckout, 
        IVSSItem/IsCheckedOut, IVSSItem/Checkouts, IVSSItem/IsDifferent, 
        IVSSItem/Add, IVSSItem/NewSubproject, IVSSItem/Share, 
        IVSSItem/Destroy, IVSSItem/Move, IVSSItem/Label, IVSSItem/Versions, 
        IVSSItem/Version, IVSSItem/Links, IVSSItem/Branch;
  export <IVSSVersions>, IVSSVersions/_NewEnum;
  export <IVSSVersionOld>, IVSSVersionOld/Username, 
        IVSSVersionOld/VersionNumber, IVSSVersionOld/Action, 
        IVSSVersionOld/Date, IVSSVersionOld/Comment, IVSSVersionOld/Label, 
        IVSSVersionOld/VSSItem;
  export <IVSSVersion>, IVSSVersion/Username, IVSSVersion/VersionNumber, 
        IVSSVersion/Action, IVSSVersion/Date, IVSSVersion/Comment, 
        IVSSVersion/Label, IVSSVersion/VSSItem, IVSSVersion/LabelComment;
  export <IVSSItems>, IVSSItems/Count, IVSSItems/Item, IVSSItems/_NewEnum;
  export <IVSSCheckouts>, IVSSCheckouts/Count, IVSSCheckouts/Item, 
        IVSSCheckouts/_NewEnum;
  export <IVSSCheckout>, IVSSCheckout/Username, IVSSCheckout/Date, 
        IVSSCheckout/LocalSpec, IVSSCheckout/Machine, IVSSCheckout/Project, 
        IVSSCheckout/Comment, IVSSCheckout/VersionNumber;
  export <IVSSDatabaseOld>, IVSSDatabaseOld/Open, 
        IVSSDatabaseOld/SrcSafeIni, IVSSDatabaseOld/DatabaseName, 
        IVSSDatabaseOld/Username, IVSSDatabaseOld/CurrentProject, 
        IVSSDatabaseOld/CurrentProject-setter, IVSSDatabaseOld/VSSItem;
  export <IVSSDatabase>, IVSSDatabase/Open, IVSSDatabase/SrcSafeIni, 
        IVSSDatabase/DatabaseName, IVSSDatabase/Username, 
        IVSSDatabase/CurrentProject, IVSSDatabase/CurrentProject-setter, 
        IVSSDatabase/VSSItem, IVSSDatabase/AddUser, IVSSDatabase/User, 
        IVSSDatabase/Users, IVSSDatabase/ProjectRightsEnabled, 
        IVSSDatabase/ProjectRightsEnabled-setter, 
        IVSSDatabase/DefaultProjectRights, 
        IVSSDatabase/DefaultProjectRights-setter;
  export <IVSSUser>, IVSSUser/Delete, IVSSUser/Name, IVSSUser/Name-setter, 
        IVSSUser/Password-setter, IVSSUser/ReadOnly, 
        IVSSUser/ReadOnly-setter, IVSSUser/ProjectRights, 
        IVSSUser/ProjectRights-setter, IVSSUser/RemoveProjectRights;
  export <IVSSUsers>, IVSSUsers/Count, IVSSUsers/Item, IVSSUsers/_NewEnum;
  export $VSSItem-class-id, make-VSSItem;
  export $VSSVersion-class-id, make-VSSVersion;
  export $VSSCheckout-class-id, make-VSSCheckout;
  export $VSSDatabase-class-id, make-VSSDatabase;
  export $VSSUser-class-id, make-VSSUser;
  export <IVSS>, IVSS/VSSDatabase;
  export $VSSApp-class-id, make-VSSApp;
end module type-library-module;
