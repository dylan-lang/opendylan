package server.bank;

import java.io.*;
import IE.Iona.OrbixWeb._CORBA;
import IE.Iona.OrbixWeb.Features.Config;
import org.omg.CORBA.SystemException;

import shared._tie_bank;

/**
 * The bank server.
 */
public class Server {

  private static void init_one_server (org.omg.CORBA.ORB orb, String filename) {
      _tie_bank bankImpl = new _tie_bank( new bankImplementation() );
      FileOutputStream bank_ior_file;
      try {
	bank_ior_file = new FileOutputStream(filename);
	String ior_string = orb.object_to_string(bankImpl);
	PrintStream print_stream = new PrintStream(bank_ior_file);
	print_stream.print(ior_string);
	print_stream.close();}
      catch (IOException e) {System.err.println(e.getMessage());}
  };

  public static void init_server (org.omg.CORBA.ORB orb) {
    init_one_server(orb, "C:\\Temp\\bank-0.ior");
    init_one_server(orb, "C:\\Temp\\bank.ior");
    init_one_server(orb, "C:\\Temp\\bank-2.ior");
    init_one_server(orb, "C:\\Temp\\bank-3.ior");
    init_one_server(orb, "C:\\Temp\\bank-4.ior");
  };

  public static void main(String args[]) {
      org.omg.CORBA.ORB orb = org.omg.CORBA.ORB.init();
      Config.setConfigItem("IT_IIOP_LISTEN_PORT",String.valueOf(7894));

      _CORBA.Orbix.setDiagnostics(2);

      // create 4 bank objects
      try {
	init_server(orb);
	try {
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
	System.out.println("Exception in new bankImplementation: ");
	System.out.println(se.toString());
	System.exit(1); 
      }
  } // main
}
