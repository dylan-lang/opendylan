package server.enum;

import java.util.Vector;
import java.io.*;
import IE.Iona.OrbixWeb._CORBA;
import IE.Iona.OrbixWeb.Features.Config;
import IE.Iona.OrbixWeb._OrbixWeb;
import org.omg.CORBA.SystemException;
import shared._EnumTestOperations;
import shared.Planet;

/**
 * The EnumTest implementation class.
 */
class EnumTestImplementation implements _EnumTestOperations { 
   
    private Planet succ(Planet p) {
        switch (p.value()) {
            case p._Mercury : return Planet.Venus;
            case p._Venus : return Planet.Earth;
            case p._Earth : return Planet.Mars;
            case p._Mars : return Planet.Jupiter;
            case p._Jupiter : return Planet.Saturn; 
            case p._Saturn : return Planet.Uranus; 
            case p._Uranus : return Planet.Neptune;
            case p._Neptune : return Planet.Pluto;
            case p._Pluto : return Planet.Mercury;
            default : 
               throw new org.omg.CORBA.BAD_PARAM("Enum out of range");
        }
    };
    
    public EnumTestImplementation () throws SystemException{
      super();
    };
    
    public Planet next_in_parameter = Planet.Mercury;
    public Planet next_result = Planet.Mercury;

    public void reset_in_parameter () {
      next_in_parameter = Planet.Mercury;
    };
 
    public boolean in_parameter (Planet symbol) {
      boolean ok = symbol.equals(next_in_parameter);
      next_in_parameter = succ(next_in_parameter);
      return ok;
    };
   
    public void reset_result () {
      next_result = Planet.Mercury;
    };

    public Planet result () {
      Planet symbol = next_result;
      next_result = succ(symbol);
      return symbol;
    };
}
