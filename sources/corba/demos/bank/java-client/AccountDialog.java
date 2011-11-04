// Name: AccountDialog
// Author: Jason Trenouth
// Copyright: (c) 1999 Functional Objects, Inc. All rights reserved.

package BankingDemo;

import java.awt.*;
import java.awt.event.*;

public class AccountDialog extends Dialog {
  protected AccountManager dialog_frame;
  protected Label label;
  protected TextField field;
  protected Button ok_button;
  protected Button cancel_button;

  public AccountDialog(AccountManager parent, String title, String prompt) {

    super(parent, title, true);

    dialog_frame = parent;

    BorderLayout overall = new BorderLayout();
    setLayout(overall);

    label = new Label(prompt);
    field = new TextField(20);
    field.setEditable(true);
    Panel field_panel = new Panel();
    field_panel.setLayout(new FlowLayout(FlowLayout.CENTER, 15, 15));
    field_panel.add(label);
    field_panel.add(field);
    add("North", field_panel);

    ok_button = new Button("OK");
    cancel_button = new Button("Cancel");
    Panel button_panel = new Panel();
    button_panel.setLayout(new FlowLayout(FlowLayout.CENTER, 15, 15));
    button_panel.add(ok_button);
    button_panel.add(cancel_button);
    add("South", button_panel);

    Point pt = parent.location();
    move(pt.x + 20, pt.y + 20);
    setResizable(false);
    pack();

  };

  public boolean action(Event e, Object arg) {
    if (e.target instanceof Button) {
      hide();
      dispose();
      if (e.target == ok_button) {
	notify_ok(field.getText());
      } else if (e.target == cancel_button) {
	notify_cancel();
      };
      return true;
    } else {
      return false;
    };
  };

  protected void notify_ok(String entered_value) {
    dialog_frame.set_dialog_value(entered_value);
  };

  protected void notify_cancel() {
    dialog_frame.set_dialog_value(null);
  };


};
