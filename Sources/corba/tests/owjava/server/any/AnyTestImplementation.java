package server.any;

import org.omg.CORBA.SystemException;

import shared._AnyTestOperations;
import shared.AnyTestPackage.*;
import shared.Tree;
import shared.TreeHelper;

/**
 * The AnyTest implementation class.
 */
class AnyTestImplementation implements _AnyTestOperations { 
    Tree[] empty = {};
    Tree _tree_3 = new Tree("3", empty);
    Tree _tree_4 = new Tree("4", empty);
    Tree[] _tree_2_children = {_tree_3, _tree_4};
    Tree _tree_2  = new Tree("2",_tree_2_children);
    Tree _tree_6 = new Tree("4", empty);
    Tree _tree_7 = new Tree("4", empty);
    Tree[] _tree_5_children = {_tree_6, _tree_7};
    Tree _tree_5  = new Tree("5",_tree_5_children);
    Tree[] _tree_1_children = {_tree_2, _tree_5};
    Tree _tree = new Tree("1",_tree_1_children);

  /*Tree _tree = Tree("1",
                      {Tree("2", {Tree("3",{}), Tree("4",{})}),
                       Tree("5",{Tree("6",{}), Tree("7",{})})
                      }); */

    public AnyTestImplementation () throws SystemException{
      super();
    };

    org.omg.CORBA.Any _any_attribute;

    public org.omg.CORBA.Any any_attribute() {
        return _any_attribute;
    };

    public void any_attribute(org.omg.CORBA.Any value) {
        _any_attribute = value;
        return;
    };
 
    org.omg.CORBA.Any _any_tree_attribute;

    public org.omg.CORBA.Any any_tree_attribute() {
       return _any_tree_attribute;
    };

    public void any_tree_attribute(org.omg.CORBA.Any value){
        _any_tree_attribute = value;
        return;
    };

    public void check_any_attribute()  throws failure {
       org.omg.CORBA.Any expected = new IE.Iona.OrbixWeb.CORBA.Any();
       expected.insert_short((short) 0);
       if (!(_any_attribute.equal(expected))) throw new failure();
       //if ((_any_attribute.extract_short()) != 0) throw new failure();

       return;       
    }

    public void check_any_tree_attribute() throws failure {
       org.omg.CORBA.Any expected = new IE.Iona.OrbixWeb.CORBA.Any();
       TreeHelper.insert(expected, _tree);
       if (!(_any_attribute.equal(expected))) throw new failure();
       return;       
    }

    public org.omg.CORBA.Any any_operation(org.omg.CORBA.Any one,org.omg.CORBA.AnyHolder two,org.omg.CORBA.AnyHolder three) {
        two.value = two.value;
        three.value = one;
        return one; 
    } ;
}
