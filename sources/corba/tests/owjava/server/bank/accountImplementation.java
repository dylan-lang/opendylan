package server.bank;

import org.omg.CORBA.SystemException;

import shared._accountOperations;

/**
 * The account implementation class.
 */
class accountImplementation implements _accountOperations {

    public accountImplementation() {
      m_balance = 0;
      m_name = "";
    }

    public accountImplementation(float initialBalance, String name)
         throws SystemException {
      m_balance = initialBalance;
      m_name = name;
    }

    public String get_name() {
      return m_name;
    }

    public float balance() {
      return m_balance;
    }

    public void makeLodgement(float f) {
      m_balance +=f;
    }

    public void makeWithdrawal(float f) {
      if (f > m_balance) 
         throw new ArithmeticException("Warning: Account "+m_name+" overdrawn.");
      m_balance -=f;
    }

  String m_name;
  float m_balance;
}
