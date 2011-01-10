Module:    internal
Author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//
// CHOOSE
// 

define sealed copy-down-method choose
    (test :: <function>, sequence :: <simple-object-vector>)
 => (result :: <simple-object-vector>);

//
// REMOVE
//

define sealed copy-down-method remove
    (sequence :: <simple-object-vector>, value,
     #key test :: <function> = \==, count :: false-or(<integer>) = #f) 
 => (new-sequence :: <simple-object-vector>);

define sealed copy-down-method remove!
    (sequence :: <simple-object-vector>, value, 
     #key test :: <function> = \==, count :: false-or(<integer>) = #f) 
 => (new-sequence :: <simple-object-vector>);

define sealed copy-down-method member?
    (value, collection :: <simple-object-vector>, #key test = \==)
 => (boolean :: <boolean>);

define sealed copy-down-method add 
    (vector :: <simple-object-vector>, object) 
 => (v :: <simple-object-vector>);

define sealed copy-down-method add-new
    (vector :: <simple-object-vector>, object,
     #key test :: <function> = \==) 
 => (v :: <simple-object-vector>);

define sealed copy-down-method add-new!
    (vector :: <simple-object-vector>, object,
     #key test :: <function> = \==) 
 => (v :: <simple-object-vector>);
