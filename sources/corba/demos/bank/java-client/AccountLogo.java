// Name: AccountLogo
// Author: Jason Trenouth
// Copyright: (c) 1999 Functional Objects, Inc. All rights reserved.

package BankingDemo;

import java.awt.*;
import java.awt.event.*;

public class AccountLogo extends Panel {
  Image image = null;

  public AccountLogo() {
    image = getToolkit().getImage("DOLLARCLR_BF24.gif");
  };
  
  public void destroy() {
    image.flush();
  }

  public void paint (Graphics g) {
    g.clipRect(0, 0, image.getWidth(this), image.getHeight(this));
    g.drawImage(image, 0, 0, image.getWidth(this), image.getHeight(this), Color.lightGray, this);
  };

  // NB override to avoid default clearing to background for smoother animation
  public void update (Graphics g) {
    paint(g);
  };
};
