package server.chat;

import java.util.Vector;
import org.omg.CORBA.SystemException;

import shared._ChatOperations;
import shared.CallBack;

/**
 * The chat implementation class.
 */
class ChatImplementation implements _ChatOperations { 
    Vector _ChatImplementationClients = new Vector();
    public ChatImplementation () throws SystemException{
      super();
    };

    public void SendMessage(String Mess) {
       for(int i = 0; i < (_ChatImplementationClients.size()); i++) {
          ((CallBack) (_ChatImplementationClients.elementAt(i))).NewMessage(Mess);
       }; 
       return;
    }; 

    public void RegisterClient(CallBack obj,String Name) {
        _ChatImplementationClients.addElement(obj);
        return;
    };


    public void RemoveClient(CallBack obj,String Name) {
       for(int i = 0; i < (_ChatImplementationClients.size()); i++) {
          if (((CallBack) (_ChatImplementationClients.elementAt(i)))._is_equivalent(obj))
               _ChatImplementationClients.removeElementAt(i);
          return;
       };
    };  
};
