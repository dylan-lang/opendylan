package server;

import java.io.*;
import IE.Iona.OrbixWeb._CORBA;
import IE.Iona.OrbixWeb.Features.Config;
import org.omg.CORBA.SystemException;

/**
 * The Server
 */
public class Server {

  public static void main(String args[]) {


      org.omg.CORBA.ORB orb = org.omg.CORBA.ORB.init();
      Config.setConfigItem("IT_IIOP_LISTEN_PORT",String.valueOf(7892));

      _CORBA.Orbix.setDiagnostics(2);

     try {
	   server.any.Server.init_server(orb);
	   server.array.Server.init_server(orb);
	   server.bank.Server.init_server(orb);
	   server.chat.Server.init_server(orb);
	   server.enum.Server.init_server(orb);
	   server.grid.Server.init_server(orb);
           server.pseudoobjects.Server.init_server(orb);
           server.sequence.Server.init_server(orb);
           server.struct.Server.init_server(orb);
           server.tree.Server.init_server(orb);
	   server.union.Server.init_server(orb);
           try  {
             System.out.println("-- Standalone OrbixWeb server started on port " + Config.get_IIOP_LISTEN_PORT());
             _CORBA.Orbix.processEvents(10000*60);
             System.out.println("-- Server Timedout");
           }
           catch(SystemException se) {
             System.out.println("Exception during creation of implementation : " + se.toString());
             System.exit(1);
           }
     }
     catch (SystemException se) {
       System.out.println("Exception when starting server: ");
       System.out.println(se.toString());
       System.exit(1); 
     }
    } // main
}



