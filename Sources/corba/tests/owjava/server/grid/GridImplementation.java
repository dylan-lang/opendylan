package server.grid;

import org.omg.CORBA.SystemException;

import shared._gridOperations;

class GridImplementation implements _gridOperations {

  public int m_height;  // store the height
  public int m_width;   // store the width
  public long m_a[][];  // a 2D array to hold the grid data

  public GridImplementation(int height, int width) 
  {
    m_a = new long[height][width]; // allocate the 2D array
    m_height = (short)height; // set up height
    m_width = (short)width;   // set up width
  }

  // implementation of the function which reads the height attribute
  public short height() {
    return (short)m_height;
  }
  // implementation of the function which sets the height attribute
  public void height(short h){
    m_height = (int)h;
    m_a = new long[m_height][m_width];
    return;
  }

  // implementation of the function which reads the width attribute
  public short width(){
    return (short)m_width;
  }
  // implementation of the function which sets the width attribute
  public void width(short w){
    m_width = (int)w;
    m_a = new long[m_height][m_width];
    return;
  }

  
  // implementation of the set operation
  public void set(short n, short m, int value) {
    m_a[n][m] = value;
  }

  // implementation of the get operation
  public int get(short n , short m) {
    return (short)m_a[n][m];
  }

  // Java can't directly throw exceptions from within expressions (terms) so we need to define method that raises the exception.
  public short short_throw (java.lang.RuntimeException e) throws java.lang.RuntimeException {
      throw e;
  };

  public short context_get_width(org.omg.CORBA.Context context) {
    org.omg.CORBA.NVList extras = context.get_values("", 0, "Extra");
    try {String extra = (extras.count()>0)?
                         extras.item(0).value().extract_string()
                        :null;
         int summand = (extra == null)? 
                         0
	               : ((extra.equals("No"))? 
                           0
                          : ((extra.equals("Yes"))?
                              1
			    : short_throw(new org.omg.CORBA.BAD_OPERATION())));
         return (short) (width() + summand);
    }
    catch (org.omg.CORBA.Bounds e) {
        throw new org.omg.CORBA.BAD_OPERATION();
    };
  };
}
