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

import y.base.DataProvider;
import y.io.GMLIOHandler;
import y.io.IOHandler;
import y.io.YGFIOHandler;
import y.option.OptionHandler;
import y.util.D;
import y.view.AreaZoomMode;
import y.view.AutoDragViewMode;
import y.view.EditMode;
import y.view.Graph2DPrinter;
import y.view.Graph2DView;
import y.view.Graph2DViewActions;
import y.view.Graph2DViewMouseWheelZoomListener;
import y.view.Selections;
import y.view.ViewMode;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ActionMap;
import javax.swing.ImageIcon;
import javax.swing.InputMap;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JPanel;
import javax.swing.JRootPane;
import javax.swing.JToolBar;
import javax.swing.UIManager;
import java.awt.BorderLayout;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.print.PageFormat;
import java.awt.print.PrinterJob;
import java.io.IOException;
import java.net.URL;
import java.util.Iterator;

/**
 * Abstract base class for GUI- and <code>Graph2DView</code>-based demos.
 * Provides useful callback methods.
 * <p/>
 * To avoid problems with "calls to overwritten method in constructor", do not initialize the demo
 * within the constructor of the subclass, use the method {@link #initialize()} instead.
 */
public abstract class DemoBase {
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

  /**
   * This constructor creates the {@link #view}
   * and calls,
   * {@link #createToolBar()}
   * {@link #registerViewModes()}, {@link #registerViewActions()},
   * and {@link #registerViewListeners()}
   */
  protected DemoBase() {
    view = new Graph2DView();
    view.setAntialiasedPainting( true );

    contentPane = new JPanel();
    contentPane.setLayout( new BorderLayout() );

    initialize();

    registerViewModes();
    registerViewActions();

    contentPane.add( view, BorderLayout.CENTER );
    final JToolBar jtb = createToolBar();
    if ( jtb != null ) {
      contentPane.add( jtb, BorderLayout.NORTH );
    }

    registerViewListeners();
  }

  /**
   * This method is called before the view modes and actions are registered and the menu and toolbar is build.
   */
  protected void initialize() {
  }

  public void dispose() {
  }

  protected void loadGraph( Class aClass, String resourceString ) {
    String fqResourceName = aClass.getPackage().getName().replace( '.', '/' ) + '/' + resourceString;

    URL resource = aClass.getResource( resourceString );
    if ( resource == null ) {
      String message = "Resource \"" + fqResourceName + "\" not found in classpath";
      D.showError( message );
      throw new RuntimeException( message );
    }

    try {
      IOHandler ioh;
      if ( resource.getFile().endsWith( "ygf" ) ) {
        ioh = new YGFIOHandler();
      } else {
        ioh = new GMLIOHandler();
      }
      ioh.read( view.getGraph2D(), resource );
    } catch ( Exception e ) {
      String message = "Unexpected error while loading resource \"" + fqResourceName + "\" due to " + e.getMessage();
      D.showError( message );
      throw new RuntimeException( message, e );
    }
    view.fitContent();
  }

  protected void loadGraph( String resourceString ) {
    loadGraph( getClass(), resourceString );
  }

  /**
   * Creates an application  frame for this demo
   * and displays it. The class name is the title of
   * the displayed frame.
   */
  public final void start() {
    start( getClass().getName() );
  }

  /**
   * Creates an application  frame for this demo
   * and displays it. The given string is the title of
   * the displayed frame.
   */
  public final void start( String title ) {
    JFrame frame = new JFrame( title );
    frame.setDefaultCloseOperation( JFrame.EXIT_ON_CLOSE );
    this.addContentTo( frame.getRootPane() );
    frame.pack();
    frame.setLocationRelativeTo( null );
    frame.setVisible( true );
  }

  public void addContentTo( final JRootPane rootPane ) {
    final JMenuBar jmb = createMenuBar();
    if ( jmb != null ) {
      rootPane.setJMenuBar( jmb );
    }
    rootPane.setContentPane( contentPane );
  }

  protected void registerViewActions() {
    //register keyboard actions
    Graph2DViewActions actions = new Graph2DViewActions( view );
    ActionMap amap = actions.createActionMap();
    InputMap imap = actions.createDefaultInputMap( amap );
    if ( !isDeletionEnabled() ) {
      amap.remove( Graph2DViewActions.DELETE_SELECTION );
    }
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
    EditMode editMode = createEditMode();
    if ( editMode != null ) {
      view.addViewMode( editMode );
    }
    view.addViewMode( new AutoDragViewMode() );
  }

  /**
   * Callback used by {@link #registerViewModes()} to create the default EditMode
   *
   * @return an instance of {@link EditMode} with showNodeTips enabled
   */
  protected EditMode createEditMode() {
    EditMode editMode = new EditMode();
    editMode.showNodeTips( true );
    return editMode;
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
   * Determines whether default actions for deletions will be added to the view
   * and toolbar.
   */
  protected boolean isDeletionEnabled() {
    return true;
  }

  /**
   * Creates a toolbar for this demo.
   */
  protected JToolBar createToolBar() {
    JToolBar toolBar = new JToolBar();
    if ( isDeletionEnabled() ) {
      toolBar.add( createDeleteAllAction() );
      toolBar.add( createDeleteSelectionAction() );
    }
    toolBar.add( new Zoom( 1.2 ) );
    toolBar.add( new Zoom( 0.8 ) );
    toolBar.add( new ZoomArea() );
    toolBar.add( new FitContent( view ) );

    return toolBar;
  }

  /**
   * Create a menu bar for this demo.
   */
  protected JMenuBar createMenuBar() {
    JMenuBar menuBar = new JMenuBar();
    JMenu menu = new JMenu( "File" );
    menu.add( createLoadAction() );
    menu.add( createSaveAction() );
    menu.add( new SaveSubsetAction() );
    menu.addSeparator();
    menu.add( new PrintAction() );
    menu.addSeparator();
    menu.add( new ExitAction() );
    menuBar.add( menu );
    return menuBar;
  }

  protected Action createLoadAction() {
    return new LoadAction();
  }

  protected Action createSaveAction() {
    return new SaveAction();
  }

  protected Action createDeleteAllAction() {
    return new DeleteAll( view );
  }

  protected Action createDeleteSelectionAction() {
    return new DeleteSelection( view );
  }

  public JPanel getContentPane() {
    return contentPane;
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
}
