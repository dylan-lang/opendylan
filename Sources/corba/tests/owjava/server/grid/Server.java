package server.grid;

import java.io.*;
import IE.Iona.OrbixWeb.Features.Config;
import org.omg.CORBA.ORB;
import IE.Iona.OrbixWeb._CORBA;
import org.omg.CORBA.SystemException;

import shared.grid;
import shared._tie_grid;

/**
 * The grid demo server.
 */
public class Server {

  public static void init_server (org.omg.CORBA.ORB orb) {
    grid gridImpl = null;
    gridImpl = new _tie_grid (new GridImplementation(100,100));
    
    FileOutputStream grid_ior_file;
    try {
      grid_ior_file = new FileOutputStream("C:\\Temp\\grid.ior");
      String ior_string = orb.object_to_string(gridImpl);
      PrintStream print_stream = new PrintStream(grid_ior_file);
      print_stream.print(ior_string);
      print_stream.close();}
    catch (IOException e) {System.err.println(e.getMessage());};
   
    //  mock request fowarding 
    FileOutputStream wrong_grid_ior_file;
    try {
      wrong_grid_ior_file = new FileOutputStream("C:\\Temp\\wrong-grid.ior");
      String ior_string = orb.object_to_string(gridImpl);
      PrintStream print_stream = new PrintStream(wrong_grid_ior_file);
      print_stream.print(ior_string);
      print_stream.close();}
    catch (IOException e) {System.err.println(e.getMessage());}
  };

  public static void main(String args[]) { 

    org.omg.CORBA.ORB orb = org.omg.CORBA.ORB.init(args,null); 
    Config.setConfigItem("IT_IIOP_LISTEN_PORT",String.valueOf(7893));

     _CORBA.Orbix.setDiagnostics(2);

    try { 
      init_server(orb);
    }
    catch(SystemException se) {
      System.out.println("Exception raised during creation of Grid_Implementation" + se.toString());
      return;
    };
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
}

