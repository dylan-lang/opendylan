package server.bank;

import org.omg.CORBA.SystemException;

import shared._currentAccountOperations;

/**
 * The current account implementation class.
 * Current Account inherits from account.
 */
class currentAccountImpl extends accountImplementation
        implements _currentAccountOperations {

    public currentAccountImpl (float initialBalance, String name, float limit)
         throws SystemException {
      super(initialBalance,name);
      m_limit = limit;
    }

    public float overdraftLimit() {
      return m_limit;
    }

    public void makeWithdrawal(float f) {
      if (f > (m_balance + m_limit))
         throw new ArithmeticException("ALERT: Account " + m_name + " over agreed overdraft limit.");
      m_balance -=f;
    }

  float m_limit;
}
