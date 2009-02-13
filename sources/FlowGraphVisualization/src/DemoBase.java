/****************************************************************************
 **
 ** This file is part of yFiles-2.6.0.1. 
 ** 
 ** yWorks proprietary/confidential. Use is subject to license terms.
 **
 ** Redistribution of this file or of an unauthorized byte-code version
 ** of this file is strictly forbidden.
 **
 ** Copyright (c) 2000-2009 by yWorks GmbH, Vor dem Kreuzberg 28, 
 ** 72070 Tuebingen, Germany. All rights reserved.
 **
 ***************************************************************************/

import java.awt.BorderLayout;
import java.awt.Cursor;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.print.PageFormat;
import java.awt.print.PrinterJob;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ActionMap;
import javax.swing.ImageIcon;
import javax.swing.InputMap;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JRootPane;
import javax.swing.JSlider;
import javax.swing.JTextArea;
import javax.swing.JToolBar;
import javax.swing.UIManager;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import y.anim.AnimationFactory;
import y.anim.AnimationPlayer;
import y.base.DataProvider;
import y.base.Node;
import y.base.NodeCursor;
import y.base.NodeList;
import y.io.GMLIOHandler;
import y.io.YGFIOHandler;
import y.layout.BufferedLayouter;
import y.layout.GraphLayout;
import y.option.OptionHandler;
import y.util.D;
import y.view.AreaZoomMode;
import y.view.AutoDragViewMode;
import y.view.EditMode;
import y.view.Graph2D;
import y.view.Graph2DPrinter;
import y.view.Graph2DView;
import y.view.Graph2DViewActions;
import y.view.Graph2DViewMouseWheelZoomListener;
import y.view.LayoutMorpher;
import y.view.PopupMode;
import y.view.Selections;
import y.view.ViewMode;

public class DemoBase extends Thread {
  /**
   * Initializes to a "nice" look and feel.
   */
  public static void initLnF() {
    try {
      if ( !"com.sun.java.swing.plaf.motif.MotifLookAndFeel".equals(
              UIManager.getSystemLookAndFeelClassName()) &&
           !"com.sun.java.swing.plaf.gtk.GTKLookAndFeel".equals(
              UIManager.getSystemLookAndFeelClassName()) &&
           !UIManager.getSystemLookAndFeelClassName().equals(
              UIManager.getLookAndFeel().getClass().getName() ) ) {
        UIManager.setLookAndFeel( UIManager.getSystemLookAndFeelClassName() );
      }
    }
    catch ( Exception e ) {
      e.printStackTrace();
    }
  }


  /**
   * The view component of this demo.
   */
  protected Graph2DView view;
  protected final JPanel contentPane;
  protected IncrementalHierarchicLayout incrementallayouter;
  private String name;
  private LayouterClient client;
  protected JComboBox project_chooser;
  protected JComboBox graph_chooser;
  protected JSlider slider = new JSlider(JSlider.VERTICAL);
  public boolean updatingslider = false;
  protected HashMap<String, String> string_source_map = new HashMap<String, String>();
  
  protected JTextArea text;
  
  /**
   * This constructor creates the {@link #view}
   * and calls,
   * {@link #createToolBar()}
   * {@link #registerViewModes()}, {@link #registerViewActions()},
   * and {@link #registerViewListeners()}
   */
  protected DemoBase(String nam, LayouterClient cl) {
	name = nam;
	client = cl;
	
    view = new Graph2DView();
    view.setAntialiasedPainting( true );

    contentPane = new JPanel();
    contentPane.setLayout( new BorderLayout() );

    JPanel left = new JPanel();
    left.setLayout( new BorderLayout() );

    registerViewModes();
    registerViewActions();

    left.add( view, BorderLayout.CENTER );

    graph_chooser = new JComboBox(new SortedListComboBoxModel());
    graph_chooser.addItem(new ListElement(-1, "new..."));
    graph_chooser.setSelectedIndex(0);
    graph_chooser.setMaximumRowCount(50);
    graph_chooser.addActionListener(new ChangeGraphAction());

    project_chooser = new JComboBox(new SortedListComboBoxModel());
    project_chooser.setMaximumRowCount(50);
    project_chooser.addActionListener(new ChangeProjectAction());
    
    
    final JToolBar jtb = createToolBar();
    if ( jtb != null ) {
      left.add( jtb, BorderLayout.NORTH );
    }

    contentPane.add(left, BorderLayout.CENTER);
    
    JPanel right = new JPanel();
    right.setLayout( new BorderLayout() );
    
    JPanel textok = new JPanel();
    textok.setLayout( new BorderLayout() );
    
    JPanel choosers = new JPanel();
    choosers.setLayout( new BorderLayout() );
    
    //choosers.add(project_chooser, BorderLayout.NORTH);
    choosers.add(graph_chooser, BorderLayout.SOUTH);
    
    textok.add(choosers, BorderLayout.NORTH);
    

    text = new JTextArea("Choose code example or type code!", 8, 40);
    string_source_map.put("new...", text.getText());
    text.setEditable(true);
    textok.add(text, BorderLayout.CENTER );

    JButton send = new JButton("send");
    textok.add(send, BorderLayout.SOUTH );
    send.addActionListener(new SendAction());

    right.add(textok, BorderLayout.NORTH );
    
    slider.setPaintLabels(true);
    slider.setSnapToTicks(true);
    slider.setMinimum(0);
    slider.setMaximum(0);
    slider.addChangeListener(new ChangeSlider());
    right.add(slider, BorderLayout.CENTER );
    contentPane.add( right, BorderLayout.EAST );
    
    registerViewListeners();
  }

  public String methodName () {
	  name = text.getText();
	  String def = "define method ";
	  if (name.startsWith(def))
		  name = name.substring(def.length(), name.indexOf(' ', def.length() + 1)).trim();
	  return name;
  }
  
  public boolean containsMethodHeader () {
	  String name = text.getText();
	  String def = "define method ";
	  if (name.startsWith(def))
		  return true;
	  return false;
  }
  
  public void graphChanged (IncrementalHierarchicLayout ihl) {
	  incrementallayouter = ihl;
	  updatingslider = true;
	  slider.setLabelTable(ihl.sliderLabels);
	  slider.setMaximum(ihl.lastEntry);
	  slider.setValue(ihl.lastslidervalue);
	  updatingslider = false;
	  calcLayout();
  }
  
  public void dispose() {
  }

  /**
   * Creates an application  frame for this demo
   * and displays it. The class name is the title of
   * the displayed frame.
   */
  public final void run() {
	  JFrame frame = new JFrame( name );
	  frame.setDefaultCloseOperation( JFrame.EXIT_ON_CLOSE );
	  this.addContentTo( frame.getRootPane() );
	  frame.pack();
	  frame.setSize(1400, 1000);
	  frame.setLocationRelativeTo( null );
	  frame.setVisible( true );
  }

  public void addContentTo( final JRootPane rootPane ) {
    //final JMenuBar jmb = createMenuBar();
    //if ( jmb != null ) {
    //  rootPane.setJMenuBar( jmb );
    //}
    rootPane.setContentPane( contentPane );
  }

  protected void registerViewActions() {
    //register keyboard actions
    Graph2DViewActions actions = new Graph2DViewActions( view );
    ActionMap amap = actions.createActionMap();
    InputMap imap = actions.createDefaultInputMap( amap );
    view.getCanvasComponent().setActionMap( amap );
    view.getCanvasComponent().setInputMap( JComponent.WHEN_FOCUSED, imap );
  }

  /**
   * Adds the view modes to the view.
   * This implementation adds a new EditMode created by {@link #createEditMode()}
   * a new {@link AutoDragViewMode}.
   */
  protected void registerViewModes() {
    //edit mode will show tool tips over nodes
    view.addViewMode( new AutoDragViewMode() );
  }

  /**
   * Instantiates and registers the listeners for the view.
   * (e.g. {@link y.view.Graph2DViewMouseWheelZoomListener}
   */
  protected void registerViewListeners() {
    //Note that mouse wheel support requires J2SE 1.4 or higher.
    view.getCanvasComponent().addMouseWheelListener( new Graph2DViewMouseWheelZoomListener() );
  }

  /**
   * Creates a toolbar for this demo.
   */
  protected JToolBar createToolBar() {
    JToolBar toolBar = new JToolBar();
    toolBar.add( new Zoom( 1.2 ) );
    toolBar.add( new Zoom( 0.8 ) );
    toolBar.add( new FitContent( view ) );
	toolBar.add( new LayoutAction() );
	toolBar.add( new Play() );
	toolBar.add( new Step() );

    return toolBar;
  }

  /**
   * Create a menu bar for this demo.
   */
  protected JMenuBar createMenuBar() {
    JMenuBar menuBar = new JMenuBar();
    JMenu menu = new JMenu( "File" );
    menu.add( createSaveAction() );
    menu.add( new SaveSubsetAction() );
    menu.addSeparator();
    menu.add( new PrintAction() );
    menu.addSeparator();
    menu.add( new ExitAction() );
    menuBar.add( menu );
    return menuBar;
  }

  protected Action createSaveAction() {
    return new SaveAction();
  }

  public JPanel getContentPane() {
    return contentPane;
  }

  final class SendAction extends AbstractAction
  {
	public void actionPerformed(ActionEvent ev) {
		if (string_source_map.get(methodName()) == null) {
			System.out.println("new method :" + methodName() + ":");
			string_source_map.put(methodName(), text.getText());
			ListElement newLE = new ListElement(-1, methodName());
			graph_chooser.addItem(newLE);
			graph_chooser.setSelectedItem(newLE);
		} 
		int realindex = ((ListElement)graph_chooser.getSelectedItem()).index;
		if (realindex == -1) {
			ArrayList data = new ArrayList();
			data.add(new Symbol("compile"));
			//data.add(new Symbol(methodName()));
			if (containsMethodHeader())
				data.add(text.getText());
			else
				data.add("define function test" + client.getGraphSize() + " () " + text.getText() + " end;");
			client.printMessage(data);
		}
	}
  }
  
  final class ChangeProjectAction extends AbstractAction
  {
		public void actionPerformed(ActionEvent ev) {
			ArrayList data = new ArrayList();
			data.add(new Symbol("open-project"));
			data.add((String)project_chooser.getSelectedItem());
			client.printMessage(data);					
		} 
  }
  
  final class ChangeGraphAction extends AbstractAction
	{
		public ChangeGraphAction() {
			super("Change Graph");
			this.putValue(Action.SHORT_DESCRIPTION, "Change Graph");
		}
		
		public void actionPerformed(ActionEvent ev) {
			text.setText(string_source_map.get(((ListElement)graph_chooser.getSelectedItem()).toString()));
			int realindex = ((ListElement)graph_chooser.getSelectedItem()).index;
			if (realindex >= 0) {
				IncrementalHierarchicLayout ih = client.getGraph(realindex);
				ih.activateLayouter();
			} else {
				updatingslider = true;
				slider.setLabelTable(null);
				slider.setMaximum(0);
				updatingslider = false;
				view.setGraph2D(new Graph2D());
				view.repaint();
				System.out.println("no graph yet, please wait");
			}
				
		}
	}
  
  final class ChangeSlider implements ChangeListener
  {
	public void stateChanged(ChangeEvent arg0) {
		if (!updatingslider && !slider.getValueIsAdjusting() && incrementallayouter.graphfinished) {
			int step = slider.getValue();
			incrementallayouter.resetGraph(step);
		}
	}
  }
  /**
   * Action that prints the contents of the view
   */
  protected class PrintAction extends AbstractAction {
    PageFormat pageFormat;
    OptionHandler printOptions;

    public PrintAction() {
      super( "Print" );

      //setup option handler
      printOptions = new OptionHandler( "Print Options" );
      printOptions.addInt( "Poster Rows", 1 );
      printOptions.addInt( "Poster Columns", 1 );
      printOptions.addBool( "Add Poster Coords", false );
      final String[] area = {"View", "Graph"};
      printOptions.addEnum( "Clip Area", area, 1 );
    }

    public void actionPerformed( ActionEvent e ) {
      Graph2DPrinter gprinter = new Graph2DPrinter( view );

      //show custom print dialog and adopt values
      if ( !printOptions.showEditor() ) {
        return;
      }
      gprinter.setPosterRows( printOptions.getInt( "Poster Rows" ) );
      gprinter.setPosterColumns( printOptions.getInt( "Poster Columns" ) );
      gprinter.setPrintPosterCoords(
          printOptions.getBool( "Add Poster Coords" ) );
      if ("Graph".equals( printOptions.get( "Clip Area" ) ) ) {
        gprinter.setClipType( Graph2DPrinter.CLIP_GRAPH );
      } else {
        gprinter.setClipType( Graph2DPrinter.CLIP_VIEW );
      }

      //show default print dialogs
      PrinterJob printJob = PrinterJob.getPrinterJob();
      if ( pageFormat == null ) {
        pageFormat = printJob.defaultPage();
      }
      PageFormat pf = printJob.pageDialog( pageFormat );
      if ( pf == pageFormat ) {
        return;
      } else {
        pageFormat = pf;
      }

      //setup printjob.
      //Graph2DPrinter is of type Printable
      printJob.setPrintable( gprinter, pageFormat );

      if ( printJob.printDialog() ) {
        try {
          printJob.print();
        } catch ( Exception ex ) {
          ex.printStackTrace();
        }
      }
    }
  }

  /**
   * Action that terminates the application
   */
  protected static class ExitAction extends AbstractAction {
    public ExitAction() {
      super( "Exit" );
    }

    public void actionPerformed( ActionEvent e ) {
      System.exit( 0 );
    }
  }

  /**
   * Action that saves the current graph to a file in YGF format.
   */
  protected class SaveAction extends AbstractAction {
    JFileChooser chooser;

    public SaveAction() {
      super( "Save..." );
      chooser = null;
    }

    public void actionPerformed( ActionEvent e ) {
      if ( chooser == null ) {
        chooser = new JFileChooser();
      }
      if ( chooser.showSaveDialog( contentPane ) == JFileChooser.APPROVE_OPTION ) {
        String name = chooser.getSelectedFile().toString();
        if ( name.endsWith( ".gml" ) ) {
          GMLIOHandler ioh = new GMLIOHandler();
          try {
            ioh.write( view.getGraph2D(), name );
          } catch ( IOException ioe ) {
            D.show( ioe );
          }
        } else {
          if ( !name.endsWith( ".ygf" ) ) {
            name = name + ".ygf";
          }
          YGFIOHandler ioh = new YGFIOHandler();
          try {
            ioh.write( view.getGraph2D(), name );
          } catch ( IOException ioe ) {
            D.show( ioe );
          }
        }
      }
    }
  }

  /**
   * Action that saves the current subset of the graph to a file in YGF format.
   */
  protected class SaveSubsetAction extends AbstractAction {
    JFileChooser chooser;

    public SaveSubsetAction() {
      super( "Save selection..." );
      chooser = null;
    }

    public void actionPerformed( ActionEvent e ) {
      if ( chooser == null ) {
        chooser = new JFileChooser();
      }
      if ( chooser.showSaveDialog( contentPane ) == JFileChooser.APPROVE_OPTION ) {
        String name = chooser.getSelectedFile().toString();
        if ( !name.endsWith( ".ygf" ) ) {
          name = name + ".ygf";
        }
        YGFIOHandler ioh = new YGFIOHandler();
        try {
          DataProvider dp = Selections.createSelectionDataProvider( view.getGraph2D() );
          ioh.writeSubset( view.getGraph2D(), dp, name );
        } catch ( IOException ioe ) {
          D.show( ioe );
        }
      }
    }
  }

  /**
   * Action that loads the current graph from a file in YGF format.
   */
  protected class LoadAction extends AbstractAction {
    JFileChooser chooser;

    public LoadAction() {
      super( "Load..." );
      chooser = null;
    }

    public void actionPerformed( ActionEvent e ) {
      if ( chooser == null ) {
        chooser = new JFileChooser();
      }
      if ( chooser.showOpenDialog( contentPane ) == JFileChooser.APPROVE_OPTION ) {
        String name = chooser.getSelectedFile().toString();
        if ( name.endsWith( ".gml" ) ) {
          GMLIOHandler ioh = new GMLIOHandler();
          try {
            view.getGraph2D().clear();
            ioh.read( view.getGraph2D(), name );
          } catch ( IOException ioe ) {
            D.show( ioe );
          }
        } else {
          if ( !name.endsWith( ".ygf" ) ) {
            name = name + ".ygf";
          }
          YGFIOHandler ioh = new YGFIOHandler();
          try {
            view.getGraph2D().clear();
            ioh.read( view.getGraph2D(), name );
          } catch ( IOException ioe ) {
            D.show( ioe );
          }
        }
        //force redisplay of view contents
        view.fitContent();
        view.getGraph2D().updateViews();
      }
    }
  }

  /**
   * Action that deletes all graph elements.
   */
  protected static class DeleteAll extends AbstractAction {
    private final Graph2DView view;

    public DeleteAll( final Graph2DView view ) {
      this.view = view;
      URL imageURL = ClassLoader.getSystemResource( "demo/view/resource/New16.gif" );
      if ( imageURL != null ) {
        putValue( Action.SMALL_ICON, new ImageIcon( imageURL ) );
      }
      putValue( Action.SHORT_DESCRIPTION, "Clear Window" );
    }

    public void actionPerformed( ActionEvent e ) {
      view.getGraph2D().clear();
      view.getGraph2D().updateViews();
    }
  }

  /**
   * Action that deletes the selected parts of the graph.
   */
  protected static class DeleteSelection extends AbstractAction {
    private final Graph2DView view;

    public DeleteSelection( final Graph2DView view ) {
      super( "Delete Selection" );
      this.view = view;
      URL imageURL = ClassLoader.getSystemResource( "demo/view/resource/Delete16.gif" );
      if ( imageURL != null ) {
        this.putValue( Action.SMALL_ICON, new ImageIcon( imageURL ) );
      }
      this.putValue( Action.SHORT_DESCRIPTION, "Delete Selection" );
    }

    public void actionPerformed( ActionEvent e ) {
      view.getGraph2D().removeSelection();
      view.getGraph2D().updateViews();
    }
  }

  /**
   * Action that applies a specified zoom level to the view.
   */
  protected class Zoom extends AbstractAction {
    double factor;

    public Zoom( double factor ) {
      super( "Zoom " + ( factor > 1.0 ? "In" : "Out" ) );
      URL imageURL;
      if ( factor > 1.0d ) {
        imageURL = ClassLoader.getSystemResource( "demo/view/resource/ZoomIn16.gif" );
      } else {
        imageURL = ClassLoader.getSystemResource( "demo/view/resource/ZoomOut16.gif" );
      }
      if ( imageURL != null ) {
        this.putValue( Action.SMALL_ICON, new ImageIcon( imageURL ) );
      }
      this.putValue( Action.SHORT_DESCRIPTION, "Zoom " + ( factor > 1.0 ? "In" : "Out" ) );
      this.factor = factor;
    }

    public void actionPerformed( ActionEvent e ) {
      view.setZoom( view.getZoom() * factor );
      //optional code that adjusts the size of the
      //view's world rectangle. The world rectangle
      //defines the region of the canvas that is
      //accessible by using the scrollbars of the view.
      Rectangle box = view.getGraph2D().getBoundingBox();
      view.setWorldRect( box.x - 20, box.y - 20, box.width + 40, box.height + 40 );

      view.updateView();
    }
  }

  /**
   * Action that fits the content nicely inside the view.
   */
  protected static class FitContent extends AbstractAction {
    private final Graph2DView view;

    public FitContent( final Graph2DView view ) {
      super( "Fit Content" );
      this.view = view;
      URL imageURL = ClassLoader.getSystemResource( "demo/view/resource/FitContent16.gif" );
      if ( imageURL != null ) {
        this.putValue( Action.SMALL_ICON, new ImageIcon( imageURL ) );
      }
      this.putValue( Action.SHORT_DESCRIPTION, "Fit Content" );
    }

    public void actionPerformed( ActionEvent e ) {
      view.fitContent();
      view.updateView();
    }
  }

  /**
   * Action that zooms the view to the bounding box of selected nodes.
   */
  public class ZoomArea extends AbstractAction {
    public ZoomArea() {
      super( "Zoom Area" );
      URL imageURL = ClassLoader.getSystemResource( "demo/view/resource/Zoom16.gif" );
      if ( imageURL != null ) {
        this.putValue( Action.SMALL_ICON, new ImageIcon( imageURL ) );
      }
      this.putValue( Action.SHORT_DESCRIPTION, "Zoom Area" );
    }

    public void actionPerformed( ActionEvent e ) {
      Iterator viewModes = view.getViewModes();
      while ( viewModes.hasNext() ) {
        ViewMode viewMode = ( ViewMode ) viewModes.next();
        if ( viewMode instanceof EditMode ) {
          EditMode editMode = ( EditMode ) viewMode;
          editMode.setChild( new AreaZoomMode(), null, null );
        }
      }
    }
  }
  
	/**
	 * Simple Layout action (incremental)
	 */
	final class LayoutAction extends AbstractAction
	{
		LayoutAction()
		{
			super("Layout");
			URL imageURL = ClassLoader.getSystemResource("demo/view/resource/Layout16.gif");
			if (imageURL != null){
				this.putValue(Action.SMALL_ICON, new ImageIcon(imageURL));
			}
			this.putValue( Action.SHORT_DESCRIPTION, "Layout");
		}
		public void actionPerformed(ActionEvent ev)
		{
			incrementallayouter.changed = true;
			calcLayout();
		}
	}
	
	final class Play extends AbstractAction
	{
		Play() {
			super("Play");
			this.putValue( Action.SHORT_DESCRIPTION, "Play");
		}
		
		public void actionPerformed (ActionEvent ev) {
			while (true)
				if (! incrementallayouter.nextStep())
					break;
		}
		
	}
	
	final class Step extends AbstractAction
	{
		Step() {
			super("Step");
			this.putValue( Action.SHORT_DESCRIPTION, "Step");
		}
		
		public void actionPerformed (ActionEvent ev) {
			incrementallayouter.nextStep();
		}
		
	}
	
	
	/**
	 * Provides popups for all kinds of actions
	 */
	final class IncrementalPopupMode extends PopupMode {

		public JPopupMenu getNodePopup(final Node v)
		{
			JPopupMenu pm = new JPopupMenu();
			NodeCursor node = new NodeList(v).nodes();
			addNodeActions(pm, node);
			return pm;
		}

		private void addNodeActions(JPopupMenu pm, NodeCursor node) {
			JMenu fixNodesMenu = new JMenu("Fix nodes");
			pm.add(fixNodesMenu);
		}

		public JPopupMenu getSelectionPopup(double x, double y)
		{
			JPopupMenu pm = new JPopupMenu();
			final NodeCursor snc = getGraph2D().selectedNodes();
			if (snc.ok()){
				addNodeActions(pm, snc);
			} else {
				return null;
			}
			return pm;
		}

	}

	/**
	 * Animated layout assignment
	 */
	public void calcLayout(){
		if (!view.getGraph2D().isEmpty() && incrementallayouter.changed){
		    //System.out.println("calculating layout");
			incrementallayouter.changed = false;
			Cursor oldCursor = view.getCanvasComponent().getCursor();
			try {
				view.getCanvasComponent().setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
				GraphLayout layout = new BufferedLayouter(incrementallayouter.hierarchicLayouter).calcLayout(view.getGraph2D());
				LayoutMorpher morpher = new LayoutMorpher(view, layout);
				morpher.setSmoothViewTransform(true);
				morpher.setPreferredDuration(1000);
				final AnimationPlayer player = new AnimationPlayer();
				player.addAnimationListener(view);
				player.setFps(60);
				player.animate(AnimationFactory.createEasedAnimation(morpher));
			} catch (Exception e) {
				System.out.println("got exception during layouting");
				e.printStackTrace();
			} finally {
				view.getCanvasComponent().setCursor(oldCursor);
			}
		}
		view.updateView();
	}

}
