package server.tree;

import org.omg.CORBA.SystemException;
import shared._TreeTestOperations;
import shared.Tree;
import shared.TreeU;
import shared.Tree_wrapper;

/**
 * The tree implementation class.
 */
class TreeTestImplementation implements _TreeTestOperations { 
   
    public TreeTestImplementation () throws SystemException{
      super();
    };

    public short depth(Tree tree) {
      return (short) 0;
    };

    public Tree identity(Tree tree) {
      return tree;
    };

    public Tree identityB(Tree tree) {
      return tree;
    };

    public TreeU identityU(TreeU tree) {
      //        throw new org.omg.CORBA.BAD_OPERATION();
      return tree;
    };

    public Tree extract_Tree(Tree_wrapper wrapper) {
      return (wrapper.real_tree);
    };
};
