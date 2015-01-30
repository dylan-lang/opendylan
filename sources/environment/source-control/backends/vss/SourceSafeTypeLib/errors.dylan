Module:    SourceSafeTypeLib
Synopsis:  Microsoft Visual Source Safe Type Library
Author:    Gary Palter
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Hand translation of ssauterr.h

define macro vss-error-code-definer
  { define vss-error-code ?:name MAKEHR(?code:expression) }
        => { define inline-only constant ?name :: <integer> = logand(?code, #xFFFF) }
end macro vss-error-code-definer;

  // File %s may be corrupt
define vss-error-code $ESS-CORRUPT                 MAKEHR(-10600);

  // Invalid date string: "%s"
define vss-error-code $ESS-DT-BADDATESTR           MAKEHR(-10159);

  // Invalid time or date string
define vss-error-code $ESS-DT-INVALID              MAKEHR(-10161);

  // Too many file handles open.
define vss-error-code $ESS-NOMORE-HANDLES          MAKEHR(-10164);

  // Access to file "%s" denied
define vss-error-code $ESS-FILE-ACCESSDENIED       MAKEHR(-10165);

  // Invalid drive: %s
define vss-error-code $ESS-FILE-BADDRIVE           MAKEHR(-10166);

  // Invalid handle.
define vss-error-code $ESS-FILE-BADHANDLE          MAKEHR(-10167);

  // Invalid filename: "%s"
define vss-error-code $ESS-FILE-BADNAME            MAKEHR(-10168);

  // Invalid access code (bad parameter);
define vss-error-code $ESS-FILE-BADPARAM           MAKEHR(-10170);

  // Invalid DOS path: %s
define vss-error-code $ESS-FILE-BADPATH            MAKEHR(-10171);

  // Folder %s is in use
define vss-error-code $ESS-FILE-CURRENTDIR         MAKEHR(-10172);

  // Disk full
define vss-error-code $ESS-FILE-DISKFULL           MAKEHR(-10173);

  // File "%s" already exists
define vss-error-code $ESS-FILE-EXISTS             MAKEHR(-10175);

  // File "%s" is locked
define vss-error-code $ESS-FILE-LOCKED             MAKEHR(-10176);

  // File "%s" not found
define vss-error-code $ESS-FILE-NOTFOUND           MAKEHR(-10178);

  // Error reading from file
define vss-error-code $ESS-FILE-READ               MAKEHR(-10180);

  // File %s is already open
define vss-error-code $ESS-FILE-SHARE              MAKEHR(-10181);

  // Too many file handles open
define vss-error-code $ESS-FILE-TOOMANY            MAKEHR(-10182);

  // Cannot rename to another volume
define vss-error-code $ESS-FILE-VOLNOTSAME         MAKEHR(-10183);

  // Error writing to file
define vss-error-code $ESS-FILE-WRITE              MAKEHR(-10184);

  // Initialization variable "%s" must be set to "Yes" or "No"
define vss-error-code $ESS-INI-BADBOOL             MAKEHR(-10200);

  // Invalid syntax on line %d of file %s
define vss-error-code $ESS-INI-BADLINE             MAKEHR(-10201);

  // Initialization variable ""%s"" set to invalid number
define vss-error-code $ESS-INI-BADNUMBER           MAKEHR(-10202);

  // Initialization variable ""%s"" set to invalid path
define vss-error-code $ESS-INI-BADPATH             MAKEHR(-10203);

  // Initialization variable ""%s"" set to invalid value
define vss-error-code $ESS-INI-BADVALUE            MAKEHR(-10205);

  // Cannot find initialization variable "%s"
define vss-error-code $ESS-INI-NOSUCHVAR           MAKEHR(-10206);

  // Initialization variable "%s" must be between %d and %d
define vss-error-code $ESS-INI-NUMRANGE            MAKEHR(-10207);

  // Too many SS.INI environment strings
define vss-error-code $ESS-INI-TOO-MANY-ENV        MAKEHR(-10208);

  // Timeout locking file: %s
define vss-error-code $ESS-LOCK-TIMEOUT            MAKEHR(-10266);

  // Out of memory
define vss-error-code $ESS-MEM-NOMEMORY            MAKEHR(-10270);

  // You cannot modify the properties of a file that is checked out.
define vss-error-code $ESS-NO-TWEAK-CHKDOUT        MAKEHR(-10625);

  // You cannot perform a merge on a binary file, or a file that stores latest version only.
define vss-error-code $ESS-NOMERGE-BIN-NODELTA     MAKEHR(-10279);

  // Cannot check out %s. It is binary and is already checked out.
define vss-error-code $ESS-NOMULTI-BINARY          MAKEHR(-10280);

  // %s stores only the latest version and is already checked out.
define vss-error-code $ESS-NOMULTI-NODELTA         MAKEHR(-10281);

  // Error executing: %s
define vss-error-code $ESS-OS-NOT-EXE              MAKEHR(-10285);

  // %s is a SourceSafe configuration file and cannot be added.
define vss-error-code $ESS-SS-ADDPRJASSOCFILE      MAKEHR(-10626);

  // The SourceSafe database has been locked by the Administrator.
define vss-error-code $ESS-SS-ADMIN-LOCKOUT        MAKEHR(-10456);

  // Unable to rename %s to %s.
define vss-error-code $ESS-SS-BADRENAME            MAKEHR(-10402);

  // Cannot find SS.INI file for user %s
define vss-error-code $ESS-SS-CANT-FIND-SSINI      MAKEHR(-10403);

  // File %s is currently checked out by %s
define vss-error-code $ESS-SS-CHECKED-OUT          MAKEHR(-10405);

  // You currently have file %s checked out
define vss-error-code $ESS-SS-CHECKED-OUT-YOU      MAKEHR(-10406);

  // Cannot check out an old version of a file
define vss-error-code $ESS-SS-CHECKOUT-OLD         MAKEHR(-10408);

  // File %s is currently checked out by %s
define vss-error-code $ESS-SS-CHKOUT-USER          MAKEHR(-10413);

  // An automatic merge has occurred and there are conflicts.\nEdit %s to resolve them.
define vss-error-code $ESS-SS-CONFLICTS            MAKEHR(-10415);

  // Cannot delete the root project
define vss-error-code $ESS-SS-DEL-ROOT             MAKEHR(-10418);

  // A deleted link to %s already exists
define vss-error-code $ESS-SS-DEL-SHARED           MAKEHR(-10419);

  // File ""%s"" not found
define vss-error-code $ESS-SS-FILE-NOTFOUND        MAKEHR(-10421);

  // A history operation is already in progress
define vss-error-code $ESS-SS-HISTOPEN             MAKEHR(-10404);

  // You do not have access rights to %s
define vss-error-code $ESS-SS-INSUFRIGHTS          MAKEHR(-10423);

  // A more recent version is checked out
define vss-error-code $ESS-SS-LATERCHKEDOUT        MAKEHR(-10426);

  // A writable copy of %s already exists
define vss-error-code $ESS-SS-LOCALRW              MAKEHR(-10427);

  // Move does not change the name of a project
define vss-error-code $ESS-SS-MOVE-CHANGENAME      MAKEHR(-10428);

  // Project %s does not exist
define vss-error-code $ESS-SS-MOVE-NOPARENT        MAKEHR(-10429);

  // Cannot move the root project
define vss-error-code $ESS-SS-MOVE-ROOT            MAKEHR(-10430);

  // Cannot roll back to the most recent version of %s
define vss-error-code $ESS-SS-MUST-USE-VERS        MAKEHR(-10431);

  // Files have no common ancestor
define vss-error-code $ESS-SS-NOCOMMANCESTOR       MAKEHR(-10432);

  // %s has been merged with no conflicts.
define vss-error-code $ESS-SS-NOCONFLICTS2         MAKEHR(-10434);

  // File %s is invalid. Files may not begin with $.
define vss-error-code $ESS-SS-NODOLLAR             MAKEHR(-10435);

  // File %s is not checked out
define vss-error-code $ESS-SS-NOT-CHKEDOUT         MAKEHR(-10436);

  // File %s is not shared by any other projects
define vss-error-code $ESS-SS-NOT-SHARED           MAKEHR(-10437);

  // Files are not branched
define vss-error-code $ESS-SS-NOTSEPARATED         MAKEHR(-10438);

  // Unable to open user login file %s.
define vss-error-code $ESS-SS-OPEN-LOGGIN          MAKEHR(-10457);

  // Path %s too long
define vss-error-code $ESS-SS-PATHTOOLONG          MAKEHR(-10439);

  // Rename does not move an item to another project
define vss-error-code $ESS-SS-RENAME-MOVE          MAKEHR(-10442);

  // Cannot Rename the root project
define vss-error-code $ESS-SS-RENAME-ROOT          MAKEHR(-10443);

  // Cannot Rollback to the most recent version of %s
define vss-error-code $ESS-SS-ROLLBACK-NOTOLD      MAKEHR(-10447);

  // A project cannot be shared under a descendant.
define vss-error-code $ESS-SS-SHARE-ANCESTOR       MAKEHR(-10449);

  // File %s is already shared by this project
define vss-error-code $ESS-SS-SHARED               MAKEHR(-10450);

  // Invalid SourceSafe syntax: "%s"
define vss-error-code $ESS-SSPEC-SYNTAX            MAKEHR(-10515);

  // Bad username syntax: "%s"
define vss-error-code $ESS-UM-BAD-CHAR             MAKEHR(-10550);

  // Invalid password
define vss-error-code $ESS-UM-BAD-PASSWORD         MAKEHR(-10551);

  // Incompatible database version
define vss-error-code $ESS-UM-BADVERSION           MAKEHR(-10552);

  // Cannot delete the Admin user
define vss-error-code $ESS-UM-DEL-ADMIN            MAKEHR(-10553);

  // Permission denied
define vss-error-code $ESS-UM-PERM-DENIED          MAKEHR(-10554);

  // Can not rename the Admin user
define vss-error-code $ESS-UM-RENAME-ADMIN         MAKEHR(-10555);

  // Username too long
define vss-error-code $ESS-UM-TOO-LONG             MAKEHR(-10556);

  // User "%s" already exists
define vss-error-code $ESS-UM-USER-EXISTS          MAKEHR(-10557);

  // User "%s" not found
define vss-error-code $ESS-UM-USER-NOT-FOUND       MAKEHR(-10558);

  // The URL for project %s was not set properly.
define vss-error-code $ESS-URL-BADPATH             MAKEHR(-10192);

  // File %s checked out
define vss-error-code $ESS-VS-CHECKED-OUT          MAKEHR(-10601);

  // Subproject or file not found
define vss-error-code $ESS-VS-CHILD-NOT-FOUND      MAKEHR(-10602);

  // Collision accessing database, please try again.
define vss-error-code $ESS-VS-COLLISION            MAKEHR(-10603);

 // File %s is exclusively checked out.
define vss-error-code $ESS-VS-EXCLUSIVE-CHECKED-OUT MAKEHR(-10614);

  // An item with the name %s already exists
define vss-error-code $ESS-VS-ITEMEXISTS           MAKEHR(-10604);

  // %s is an invalid %s name
define vss-error-code $ESS-VS-LONGNAME             MAKEHR(-10605);

  // You can not move a project under itself
define vss-error-code $ESS-VS-MOVE-CYCLE           MAKEHR(-10606);

  // File %s does not retain old versions of itself
define vss-error-code $ESS-VS-NO-DELTA             MAKEHR(-10607);

  // File %s cannot be checked into this project
define vss-error-code $ESS-VS-NOT-CHECKED-OUT      MAKEHR(-10608);

  // File or project not found
define vss-error-code $ESS-VS-NOT-FOUND            MAKEHR(-10609);

  // Parent not found
define vss-error-code $ESS-VS-PARENT-NOT-FOUND     MAKEHR(-10610);

  // Version not found
define vss-error-code $ESS-VS-VERS-NOT-FOUND       MAKEHR(-10615);

  // This command only works on files.
define vss-error-code $ESS-VS-WANT-FILE            MAKEHR(-10616);

  // This command only works on projects.
define vss-error-code $ESS-VS-WANT-PRJ             MAKEHR(-10617);

  // A link in %s was ignored because it was longer than SourceSafe can understand
define vss-error-code $ESS-URL-BUFOVERFLOW         MAKEHR(-10194);

  // An error occurred while  trying to check hyperlinks for %s
define vss-error-code $ESS-URL-CANTCHECKHTML       MAKEHR(-10193);

  // Error loading SourceSafe add-in: %s
define vss-error-code $ESS-SS-ADDINFAILED          MAKEHR(-10440);

define vss-error-code $ESS-CANCEL                  MAKEHR(-32766);

  // Error loading resource string
define vss-error-code $ESS-LOADSTRING-FAILED       MAKEHR(-10999);



// SourceSafe questions answered affirmatively.
//
// A deleted copy of this %s file already exists in this project.\nDo you want to recover the existing file?
// Folder %s not found, create?
// Have any conflicts in %s been properly resolved?
// File %s is currently checked out by %s.\nProceed anyway?
// File %s was checked out to folder %s.\nProceed in %s?
// File %s is checked out to project %s, and you are in %s.\nProceed anyway?
// File %s is currently checked out by %s.  Delete anyway?
// You currently have file %s checked out.  Delete anyway?
// An item named %s was already deleted from this project.\nPurge the old item and delete this one now?
// This version of %s already has a label: overwrite?
// The label %s is already used.  Remove the old label?
// %s has been merged with no conflicts.\nCheck in now?
// Redo the automatic merge?
// Delete local file: %s?
// %s is already checked out, continue?
// File %s has been destroyed, and cannot be rebuilt.\nContinue anyway?
// Project $%s has been destroyed, and cannot be rebuilt.\nContinue anyway?
// $%s was moved out of this project, and cannot be rebuilt.\nContinue anyway?
// %s has changed. Undo check out and lose changes?
//
// SourceSafe questions answered in the negative.
//
// A deleted file of the same name already exists in this SourceSafe project.\nDo you want to recover the deleted file instead of adding your local %s?
// %s is writable, replace?
// %s is checked out, replace?

