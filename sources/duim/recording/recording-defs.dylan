Module:       DUIM-Recording-Internals
Synopsis:     DUIM output recording
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// New implementation of output recording

// Composite output records will have a transform (TX, TY) and a full bounding
// box (L, T, R, B).  (TX, TY) is effectively the record's "origin", which doesn't
// change when the region changes.  The region (L, T, R, B) might have a non-(0, 0)
// position; this can happen when a record gets added before (above or to the left)
// the previous value of (L, T).
//
// Creation of a composite record is as follows:
//  1) Initialize the bounding box BL, BT, BR, BB to (0, 0, 0, 0).
//  2) Fetch the current output history position into (RX, RY)
//  3) Create the output record with region = make-box(BL, BT, BR, BB) and
//     transform = make-[mutable-]translation(RX, RY).
//  4) Add the record to the parent.
//
// Adding a child record to a composite:
//  1) Add the new child.
//  2) Extend the region of the composite, if that's enabled.

// Leaf output records will have a transform (TX, TY) and a simple bounding
// box (W, H).  (TX, TY) acts as both the origin and, effectively, as the position.
// We don't need a full LTRB because we can't add a child to a leaf output record,
// so the region never changes.
//
// Output recording of leaf (e.g., graphics) records happens as follows:
//  1) Fetch the sheet's medium state from 'sheet-medium-state'.
//  2) Fetch the medium transform, and apply it to all the positions, distances,
//     and angles supplied by the user.
//  3) Compute the absolute bounding box edges in BL, BT, BR, BB.
//  4) Subtract (BL, BT) from all (x,y) positions, making them all relative to
//     the bounding box.
//  5) Fetch the current output history position into (RX, RY), which is the
//     position of the output record we will be adding the new record to.
//     ('do-with-output-record' continually updates the current history position.)
//  6) Subtract (RX, RY) from BL, BT, BR, BB, giving the bounding box in the
//     coordinates of the parent record.
//  7) Now compute (TX, TY) = (BL, BT) and (WIDTH, HEIGHT) = (BR - BL, BB - BT).
//  8) Create the output record with region = make-box(0, 0, WIDTH, HEIGHT) and
//     transform = make-[mutable-]translation(TX, TY), medium state, and the
//     various user-supplied positions, distances, and angles.
//  9) Add the record to the parent.


/// Output record protocol classes

define open abstract class <abstract-output-record> (<abstract-sheet>) end;


// The protocol class for all output records
define protocol-class output-record (<sheet>, <abstract-output-record>) end;


// The protocol class for output records that store other output records
define protocol-class composite-output-record (<output-record>) end;

// The protocol class for output records that are leaves.
define protocol-class leaf-output-record (<output-record>) end;
