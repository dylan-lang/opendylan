package server.struct;

import java.io.*;
import IE.Iona.OrbixWeb._CORBA;
import IE.Iona.OrbixWeb.Features.Config;
import org.omg.CORBA.SystemException;

import shared._tie_StructTest;

/**
 * The StructTest server.
 */
public class Server {

  public static void init_server (org.omg.CORBA.ORB orb) {
    _tie_StructTest structtestImpl = null;
    structtestImpl  = new _tie_StructTest( new StructTestImplementation() );

    FileOutputStream structtest_ior_file;
    try {
      structtest_ior_file = new FileOutputStream("C:\\Temp\\structtest.ior");
      String ior_string = orb.object_to_string(structtestImpl);
      PrintStream print_stream = new PrintStream(structtest_ior_file);
      print_stream.print(ior_string);
      print_stream.close();}
    catch (IOException e) {System.err.println(e.getMessage());}
  };

  public static void main(String args[]) {

      org.omg.CORBA.ORB orb = org.omg.CORBA.ORB.init();
      Config.setConfigItem("IT_IIOP_LISTEN_PORT",String.valueOf(7897));

      _CORBA.Orbix.setDiagnostics(2);

      try {
	init_server(orb);
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
	System.out.println("Exception in new StructTestImplementation: ");
	System.out.println(se.toString());
	System.exit(1); 
      }
  } // main
}
