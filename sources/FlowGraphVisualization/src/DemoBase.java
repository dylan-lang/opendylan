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

import y.anim.AnimationFactory;
import y.anim.AnimationPlayer;
import y.base.DataProvider;
import y.base.Edge;
import y.base.EdgeCursor;
import y.base.EdgeList;
import y.base.Node;
import y.base.NodeCursor;
import y.base.NodeList;
import y.base.NodeMap;
import y.geom.YPoint;
import y.io.GMLIOHandler;
import y.io.IOHandler;
import y.io.YGFIOHandler;
import y.layout.BufferedLayouter;
import y.layout.GraphLayout;
import y.layout.NodeLayout;
import y.layout.hierarchic.IncrementalHierarchicLayouter;
import y.layout.hierarchic.incremental.IncrementalHintsFactory;
import y.layout.hierarchic.incremental.IntValueHolderAdapter;
import y.layout.hierarchic.incremental.OldLayererWrapper;
import y.option.OptionHandler;
import y.util.D;
import y.view.AreaZoomMode;
import y.view.AutoDragViewMode;
import y.view.BendCursor;
import y.view.BendList;
import y.view.Drawable;
import y.view.EditMode;
import y.view.Graph2D;
import y.view.Graph2DPrinter;
import y.view.Graph2DView;
import y.view.Graph2DViewActions;
import y.view.Graph2DViewMouseWheelZoomListener;
import y.view.HitInfo;
import y.view.HotSpotMode;
import y.view.LayoutMorpher;
import y.view.LineType;
import y.view.NodeRealizer;
import y.view.PopupMode;
import y.view.PortAssignmentMoveSelectionMode;
import y.view.Selections;
import y.view.ViewMode;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ActionMap;
import javax.swing.ImageIcon;
import javax.swing.InputMap;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JList;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JRootPane;
import javax.swing.JToolBar;
import javax.swing.UIManager;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Cursor;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.Stroke;
import java.awt.event.ActionEvent;
import java.awt.geom.Rectangle2D;
import java.awt.print.PageFormat;
import java.awt.print.PrinterJob;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Random;
import java.util.Set;
import java.util.Vector;

/**
 * Abstract base class for GUI- and <code>Graph2DView</code>-based demos.
 * Provides useful callback methods.
 * <p/>
 * To avoid problems with "calls to overwritten method in constructor", do not initialize the demo
 * within the constructor of the subclass, use the method {@link #initialize()} instead.
 */
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
  private LayerDrawable layerDrawable;
  protected IncrementalHierarchicLayout incrementallayouter;
  private String name;
  private LayouterClient client;
  private JComboBox graph_chooser;
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

    this.layerDrawable = new LayerDrawable();
    view.addBackgroundDrawable(layerDrawable);
    
    registerViewModes();
    registerViewActions();

    contentPane.add( view, BorderLayout.CENTER );

    graph_chooser = new JComboBox(new SortedListComboBoxModel());
    graph_chooser.setMaximumRowCount(50);
    graph_chooser.addActionListener(new ChangeGraphAction());

    final JToolBar jtb = createToolBar();
    if ( jtb != null ) {
      jtb.add(graph_chooser);
      contentPane.add( jtb, BorderLayout.NORTH );
    }

    registerViewListeners();
  }

  public void graphChanged (IncrementalHierarchicLayout ihl, Graph2D gra, NodeMap layerids) {
	  incrementallayouter = ihl;
	  this.layerDrawable.setGraphAndLayerId(gra, layerids);
	  calcLayout();
  }
  
  public void addGraph(Integer index, String string) {
	  graph_chooser.addItem(new ListElement(index, string));
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
  public final void run() {
	  JFrame frame = new JFrame( name );
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
    view.addViewMode( new AutoDragViewMode() );
	EditMode editMode = new IncrementalEditMode();
	//editMode.setMoveSelectionMode(paMode = new IncrementalMoveSelectionMode());
	editMode.setPopupMode(new IncrementalPopupMode());
	//editMode.setCreateEdgeMode(new IncrementalEdgeCreateMode());
	editMode.setHotSpotMode(new IncrementalHotSpotMode());
	editMode.allowNodeCreation(false);
	editMode.allowEdgeCreation(false);
	editMode.allowMoveLabels(false);
	editMode.allowMovePorts(false);
	view.addViewMode( editMode );
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
    return false;
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
	toolBar.add( new LayoutAction() );

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

  final class ChangeGraphAction extends AbstractAction
	{
		public ChangeGraphAction() {
			super("Change Graph");
			this.putValue(Action.SHORT_DESCRIPTION, "Change Graph");
		}
		
		public void actionPerformed(ActionEvent ev) {
			IncrementalHierarchicLayout ih = client.getGraph(((ListElement)graph_chooser.getSelectedItem()).myindex());
			ih.activateLayouter();
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
			calcLayout();
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
	 * Recalculate layout after resizings
	 */
	final class IncrementalHotSpotMode extends HotSpotMode {
		public void mouseReleasedLeft(double x, double y)
		{
			super.mouseReleasedLeft(x, y);
			calcLayout();
		}
	}

	/**
	 * Recalculate layout after node creation
	 */
	final class IncrementalEditMode extends EditMode {
		protected void nodeCreated(Node v)
		{
			super.nodeCreated(v);
			final YPoint center = view.getGraph2D().getCenter(v);
			int layerId = layerDrawable.getLayerId(center.x, center.y);
			setLayers(new NodeList(v).nodes(), layerId, Integer.MAX_VALUE);
			calcLayout();
		}
	}

	/**
	 * Drawable implementation and utility functions
	 */
	static final class LayerDrawable implements Drawable {


		private List layers = new ArrayList(20);
		private static final Color[] colors = new Color[] {new Color(255, 255, 150), new Color(255, 255, 100)};

		private Rectangle bounds = new Rectangle(20,20,200,200);

		private Graph2D graph;
		private NodeMap layerIdMap;

		LayerDrawable(){
			//this.graph = graph;
			//this.layerIdMap = layerIdMap;
		}

		public void setGraphAndLayerId (Graph2D gr, NodeMap idmap) {
			this.graph = gr;
			this.layerIdMap = idmap;
		}
		
		public Rectangle getBounds()
		{
			return bounds;
		}

		public void updateLayers(){
			final double spacing = 20.0d;
			layers.clear();
			if (graph.N() < 1) return;
			double minX = Double.MAX_VALUE, maxX = -Double.MAX_VALUE;
			for (NodeCursor nc = graph.nodes(); nc.ok(); nc.next()){
				final Node node = nc.node();
				final int layer = layerIdMap.getInt(node);
				if (layer < 0) continue;
				while (layers.size() - 1 < layer){
					layers.add(new Rectangle2D.Double(0,0,-1,-1));
				}
				Rectangle2D.Double layerRect = (Rectangle2D.Double) layers.get(layer);
				final NodeLayout nl = graph.getNodeLayout(node);
				if (layerRect.width < 0){
					layerRect.setFrame(nl.getX(), nl.getY(), nl.getWidth(), nl.getHeight());
				} else {
					layerRect.add(nl.getX(), nl.getY());
					layerRect.add(nl.getX() + nl.getWidth(), nl.getY() + nl.getHeight());
				}
				minX = Math.min(nl.getX(), minX);
				maxX = Math.max(nl.getX() + nl.getWidth(), maxX);
			}

			double minY = Double.MAX_VALUE;
			double maxY = -Double.MAX_VALUE;
			for (int i = 0; i < layers.size(); i++){
				Rectangle2D.Double rect = (Rectangle2D.Double) layers.get(i);
				rect.x = minX - spacing;
				rect.width = maxX - minX + spacing * 2;
				if (i == 0){
					rect.y -= spacing;
					rect.height += spacing;
					minY = rect.y;
				}
				if (i == layers.size() - 1){
					rect.height += spacing;
					maxY = rect.height + rect.y;
				} else if (i < layers.size() - 1){
					Rectangle2D.Double nextRect = (Rectangle2D.Double) layers.get(i + 1);
					final double mid = (rect.getY() + rect.getHeight() + nextRect.getY()) * 0.5d;
					rect.height += mid - (rect.y + rect.height);
					final double nextDelta = mid - nextRect.y;
					nextRect.y += nextDelta;
					nextRect.height -= nextDelta;
				}
			}
			bounds.setFrame(minX - spacing, minY, maxX - minX + 2 * spacing, maxY - minY);
			graph.updateViews();
		}

		public final int inset = 8;

		public int getLayerId(double x, double y){
			if (x < bounds.x - outerInsets || x > bounds.x + bounds.width + outerInsets){
				return Integer.MAX_VALUE;
			}
			if (y < bounds.y + inset){
				return -1;
			}
			if (y > bounds.y + bounds.height - inset){
				return layers.size();
			}
			for (int i = 0; i < layers.size(); i++){
				final Rectangle2D.Double rect= (Rectangle2D.Double) layers.get(i);
				if (y >= rect.y + inset && y <= rect.y + rect.height - inset){
					return i;
				} else if (y < rect.y + inset){
					return -(i+1);
				}
			}
			return Integer.MAX_VALUE;
		}

		public static final double outerInsets = 40;

		public Rectangle2D getLayerBounds(int layer){
			if (layer >= 0 && layer < layers.size()){
				Rectangle2D.Double rect = (Rectangle2D.Double) layers.get(layer);
				rect = new Rectangle2D.Double(rect.x, rect.y + inset, rect.width, rect.height - 2 * inset);
				return rect;
			}
			if (layer == -1){
				return new Rectangle2D.Double(bounds.x, bounds.y - outerInsets, bounds.width, outerInsets + inset);
			}
			if (layer >= layers.size() && (layer != Integer.MAX_VALUE)){
				return new Rectangle2D.Double(bounds.x, bounds.y + bounds.height - inset, bounds.width, outerInsets);
			}
			if (layer < 0){
				int beforeLayer = -(layer + 1);
				if (beforeLayer < layers.size()){
					Rectangle2D.Double rect = (Rectangle2D.Double) layers.get(beforeLayer);
					rect = new Rectangle2D.Double(rect.x, rect.y - inset, rect.width, 2 * inset);
					return rect;
				}
			}
			return new Rectangle2D.Double(bounds.x - 2 * outerInsets, bounds.y -  2 * outerInsets, bounds.width + 4.0d * outerInsets, bounds.height + outerInsets * 4.0d);
		}

		public void paint(Graphics2D g)
		{
			for (int i = 0; i < layers.size(); i++){
				Color color = colors[i % colors.length];
				g.setColor(color);
				g.fill((Shape) layers.get(i));
			}
		}
	}

	/**
	 * Animated layout assignment
	 */
	public void calcLayout(){
		if (!view.getGraph2D().isEmpty()){
			incrementallayouter.gll.normalize(view.getGraph2D(), incrementallayouter.layerIdMap, incrementallayouter.layerIdMap);
			Cursor oldCursor = view.getCanvasComponent().getCursor();
			try {
				view.getCanvasComponent().setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
				GraphLayout result = new BufferedLayouter(incrementallayouter.hierarchicLayouter).calcLayout(view.getGraph2D());
				LayoutMorpher morpher = new LayoutMorpher(view, result);
				morpher.setSmoothViewTransform(true);
				morpher.setPreferredDuration(300);
				final AnimationPlayer player = new AnimationPlayer();
				player.addAnimationListener(view);
				player.setFps(120);
				player.animate(AnimationFactory.createEasedAnimation(morpher));
			} finally {
				view.getCanvasComponent().setCursor(oldCursor);
			}
		}
		layerDrawable.updateLayers();
		view.updateView();
	}

	/**
	 * Utility method to assign nodes to a new layer.
	 */
	protected void setLayers(NodeCursor nodes, int newLayer, int previousLayer){
		if (!nodes.ok()) return;
		//calculate number of layers to insert
		int lesserLayers = 0;
		int greaterLayers = 0;
		final Set nodeSet = new HashSet();
		for (nodes.toFirst(); nodes.ok(); nodes.next()){
			nodeSet.add(nodes.node());
		}
		if (previousLayer != Integer.MAX_VALUE){
			for (nodes.toFirst(); nodes.ok(); nodes.next()){
				int pLayer = incrementallayouter.layerIdMap.getInt(nodes.node());
				if (pLayer < previousLayer){
					lesserLayers = Math.max(lesserLayers, previousLayer - pLayer);
				}
				if (pLayer > previousLayer){
					greaterLayers = Math.max(greaterLayers, pLayer - previousLayer);
				}
			}
		} else {
			previousLayer = 0;
		}
		final int newLayerCount = lesserLayers + greaterLayers + 1;
		if (newLayer < 0){
			int beforeLayer = -(newLayer + 1);
			for (NodeCursor nc = view.getGraph2D().nodes(); nc.ok(); nc.next()){
				if (!nodeSet.contains(nc.node())){
					int oldLayer = incrementallayouter.layerIdMap.getInt(nc.node());
					if (oldLayer >= beforeLayer){
						incrementallayouter.layerIdMap.setInt(nc.node(), oldLayer + newLayerCount);
					}
				}
			}
			for (nodes.toFirst(); nodes.ok(); nodes.next()){
				int oldLayer = incrementallayouter.layerIdMap.getInt(nodes.node());
				incrementallayouter.layerIdMap.setInt(nodes.node(), beforeLayer + lesserLayers + oldLayer - previousLayer);
			}
		} else {
			if (newLayer == Integer.MAX_VALUE){
				int maxLayer = -1;
				for (NodeCursor nc = view.getGraph2D().nodes(); nc.ok(); nc.next()){
					if (!nodeSet.contains(nc.node())){
						int layer = incrementallayouter.layerIdMap.getInt(nc.node());
						maxLayer = Math.max(layer, maxLayer);
					}
				}
				newLayer = maxLayer + 1;
			}
			if (lesserLayers > 0 || greaterLayers > 0){
				for (NodeCursor nc = view.getGraph2D().nodes(); nc.ok(); nc.next()){
					if (!nodeSet.contains(nc.node())){
						int layer = incrementallayouter.layerIdMap.getInt(nc.node());
						if (layer == newLayer) {
							incrementallayouter.layerIdMap.setInt(nc.node(), layer + lesserLayers);
						} else if (layer > newLayer){
							incrementallayouter.layerIdMap.setInt(nc.node(), layer + newLayerCount);
						}
					}
				}
			}
			for (nodes.toFirst(); nodes.ok(); nodes.next()){
				int oldLayer = incrementallayouter.layerIdMap.getInt(nodes.node());
				incrementallayouter.layerIdMap.setInt(nodes.node(), newLayer + lesserLayers + oldLayer - previousLayer);
			}
		}
	}

	/**
	 * Recalculate layout after selection move
	 */
	final class IncrementalMoveSelectionMode extends PortAssignmentMoveSelectionMode {
		private boolean firstTime = true;
		private MoveSelectionDrawable drawable;
		private NodeList selectedNodes;
		private BendList selectedBends;

		public IncrementalMoveSelectionMode(){
			super(null, null);
		}

		protected void selectionMovedAction(double dx, double dy, double x, double y)
		{
			super.selectionMovedAction(dx, dy, x, y);
			if (selectedNodes != null){
				view.removeBackgroundDrawable(drawable);
				drawable = null;
				int newLayer = layerDrawable.getLayerId(x, y);
				HitInfo hi = this.getLastHitInfo();
				Node movedNode = hi.getHitNode();
				int originalLayer = movedNode != null ? incrementallayouter.layerIdMap.getInt(movedNode) : Integer.MAX_VALUE;
				if (newLayer != originalLayer){
					setLayers(selectedNodes.nodes(), newLayer, originalLayer);
				}
				if (newLayer != Integer.MAX_VALUE){
					List hints = new ArrayList(128);
					for (NodeCursor nc = selectedNodes.nodes(); nc.ok(); nc.next()){
						for (EdgeCursor edges = nc.node().edges(); edges.ok(); edges.next()){
							hints.add(edges.edge());
							incrementallayouter.hintMap.set(edges.edge(), incrementallayouter.hintsFactory.createSequenceIncrementallyHint(edges.edge()));
						}
					}
					calcLayout();
					for (int i = 0; i < hints.size(); i++){
						incrementallayouter.hintMap.set(hints.get(i), null);
					}
				} else {
					List hints = new ArrayList(128);
					for (NodeCursor nc = selectedNodes.nodes(); nc.ok(); nc.next()){
						hints.add(nc.node());
						incrementallayouter.hintMap.set(nc.node(), incrementallayouter.hintsFactory.createLayerIncrementallyHint(nc.node()));
					}
					calcLayout();
					for (int i = 0; i < hints.size(); i++){
						Node node = (Node) hints.get(i);
						incrementallayouter.hintMap.set(node, null);
					}
					layerDrawable.updateLayers();
				}
				selectedNodes = null;
			} else if (selectedBends != null){
				calcLayout();
			}
			selectedBends = null;
			selectedNodes = null;
			firstTime = true;
		}

		protected void selectionOnMove(double dx, double dy, double x, double y)
		{
			if (firstTime){
				firstTime = false;
				Graph2D g = getGraph2D();
				NodeCursor nc = g.selectedNodes();
				selectedBends = null;
				selectedNodes = null;
				if (nc.ok()){
					selectedNodes = new NodeList(nc);
					drawable = new MoveSelectionDrawable();
					view.addBackgroundDrawable(drawable);
				}
				BendCursor bc = g.selectedBends();
				if (selectedNodes == null && bc.ok()){
					selectedBends = new BendList(bc);
				}
			}
			super.selectionOnMove(dx, dy, x, y);
			if (selectedNodes != null) {
				int layer = layerDrawable.getLayerId(x, y);
				drawable.layer = layer;
				drawable.layerCount = layerDrawable.layers.size();
				drawable.drawable = layerDrawable.getLayerBounds(layer);
			}
		}

		final class MoveSelectionDrawable implements Drawable {
			Shape drawable;
			int layer;
			int layerCount;
			Color color = Color.red;
			Color color2 = Color.orange;
			Color color3 = Color.red.darker();

			public Rectangle getBounds()
			{
				return drawable.getBounds();
			}

			public void paint(Graphics2D g)
			{
				Stroke s = g.getStroke();
				if (layer == Integer.MAX_VALUE){
					g.setColor(color3);
					g.setStroke(LineType.DOTTED_3);
					g.draw(drawable);
				} else {
					if (layer >= 0 && layer < layerCount){
						g.setColor(color);
						g.setStroke(LineType.LINE_3);
						g.draw(drawable);
					} else {
						g.setColor(color2);
						g.fill(drawable);
					}
				}
				g.setStroke(s);
			}
		}
	}


}
