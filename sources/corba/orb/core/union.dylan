Module: orb-core
Author: Clive Tong, Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//define method get-union-type (discriminator, typecode :: <union-typecode>)
//  typecode-native-type(get-union-typecode(discriminator, typecode));
//end method;

//define method get-union-typecode (discriminator, typecode :: <union-typecode>)
//  let branches = typecode-members(typecode);
//  block (return)
//    for (branch in branches)
//      when (member?(discriminator, typecode-branch-tags(branch)))
//	return(typecode-member-typecode(branch));
//      end when;
//    end for;
//    let default = typecode-default-used(typecode);
//    if (default)
//      return(typecode-member-typecode(branches[default]));
//    end if;
//    error(make(<union-branch-error>, discriminator: discriminator, typecode: typecode));
//  end block;
//end method;



