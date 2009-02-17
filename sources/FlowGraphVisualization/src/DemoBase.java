import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Cursor;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JSlider;
import javax.swing.JTextArea;
import javax.swing.JToolBar;
import javax.swing.UIManager;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import y.anim.AnimationFactory;
import y.anim.AnimationPlayer;
import y.base.EdgeCursor;
import y.base.Node;
import y.base.NodeCursor;
import y.base.NodeList;
import y.geom.YRectangle;
import y.layout.BufferedLayouter;
import y.layout.GraphLayout;
import y.view.AreaZoomMode;
import y.view.AutoDragViewMode;
import y.view.EditMode;
import y.view.Graph2D;
import y.view.Graph2DView;
import y.view.Graph2DViewMouseWheelZoomListener;
import y.view.LayoutMorpher;
import y.view.LineType;
import y.view.NavigationMode;
import y.view.NodeRealizer;
import y.view.PopupMode;
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
	  frame.getRootPane().setContentPane( contentPane );
	  frame.pack();
	  frame.setSize(1400, 1000);
	  frame.setLocationRelativeTo( null );
	  frame.setVisible( true );
  }

  /**
   * Adds the view modes to the view.
   * This implementation adds a new EditMode created by {@link #createEditMode()}
   * a new {@link AutoDragViewMode}.
   */
  protected void registerViewModes() {
	  view.getCanvasComponent().addMouseListener(new MyMouseListener());
	  view.addViewMode(new NavigationMode());
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

  final class MyMouseListener implements MouseListener {
	public void mouseClicked(MouseEvent arg0) {
		double xv = view.toWorldCoordX(arg0.getX());
		double yv = view.toWorldCoordY(arg0.getY());
		Node selected = null;
		for (NodeCursor nc = incrementallayouter.graph.nodes(); nc.ok(); nc.next()) {
			YRectangle x = incrementallayouter.graph.getRectangle(nc.node());
			if (incrementallayouter.graph.getRectangle(nc.node()).contains(xv, yv)) {
				selected = nc.node();
				break;
			}
		}
		if (selected == null)
			unselect();
		else
			select(selected);
		view.repaint();
	}

	public void mouseEntered(MouseEvent arg0) {
	}

	public void mouseExited(MouseEvent arg0) {
	}

	public void mousePressed(MouseEvent arg0) {
	}

	public void mouseReleased(MouseEvent arg0) {
	}
	  
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
	
	protected void unselect () {
		if (incrementallayouter != null) {
			Node old = incrementallayouter.selection;
			if  (old != null) {
				incrementallayouter.graph.setSelected(old, false);
				for (EdgeCursor ec = old.edges(); ec.ok(); ec.next())
					if (incrementallayouter.graph.getRealizer(ec.edge()).getLineColor() == Color.pink) {
						incrementallayouter.graph.getRealizer(ec.edge()).setLineType(LineType.LINE_1);
						NodeRealizer o = incrementallayouter.graph.getRealizer(ec.edge().opposite(old)); 
						o.setFillColor(o.getFillColor().brighter());
					}
				incrementallayouter.selection = null;
			}
		}
	}
	
	protected void select (Node s) {
		if (s != null) {
			if (s != incrementallayouter.selection) {
				unselect();
				//System.out.println("selection now " + incrementallayouter.graph.getLabelText(s));
				incrementallayouter.graph.setSelected(s, true);
				for (EdgeCursor ec = s.edges(); ec.ok(); ec.next())
					if (incrementallayouter.graph.getRealizer(ec.edge()).getLineColor() == Color.pink) {
						incrementallayouter.graph.getRealizer(ec.edge()).setLineType(LineType.LINE_3);
						NodeRealizer n = incrementallayouter.graph.getRealizer(ec.edge().opposite(s)); 
						n.setFillColor(n.getFillColor().darker());
					}
				incrementallayouter.selection = s;
			}
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
/*			for (NodeCursor nc = incrementallayouter.graph.nodes(); nc.ok(); nc.next()) {
				Object hint = incrementallayouter.hintMap.get(nc.node()); 
				if ((hint != null) && (hint instanceof Integer) && ((Integer)hint == 42))
					incrementallayouter.hintMap.set(nc.node(), null);
				else {
					boolean data = true;
					for (EdgeCursor ec = nc.node().edges(); ec.ok(); ec.next())
						if (incrementallayouter.graph.getRealizer(ec.edge()).getLineColor() != Color.pink) {
							data = false;
							break;
						}
					if (! data) {
						if (nc.node().inDegree() == 1) {
							System.out.println("found generator " + incrementallayouter.graph.getLabelText(nc.node()));
							incrementallayouter.hintMap.set(nc.node(), incrementallayouter.hintsFactory.createLayerIncrementallyHint(nc.node().firstInEdge().source()));
						} else if (nc.node().outDegree() == 1) {
							System.out.println("found single user " + incrementallayouter.graph.getLabelText(nc.node()));
							incrementallayouter.hintMap.set(nc.node(), incrementallayouter.hintsFactory.createLayerIncrementallyHint(nc.node().firstOutEdge().target()));
						} else {
							System.out.println("don't know what to do");
							incrementallayouter.hintMap.set(nc.node(), incrementallayouter.hintsFactory.createLayerIncrementallyHint(nc.node()));
						}
					else 
						Object newhint = incrementallayouter.hintsFactory.createLayerIncrementallyHint(nc.node());
						incrementallayouter.hintMap.set(nc.node(), newhint);
						for (EdgeCursor ec = nc.node().outEdges(); ec.ok(); ec.next())
							if (incrementallayouter.graph.getRealizer(ec.edge()).getLineColor() == Color.pink) {
								System.out.println("setting hint of " + incrementallayouter.graph.getLabelText(ec.edge().target()));
								incrementallayouter.hintMap.set(ec.edge().target(), newhint);
							}
					}
				}
			} */
			try {
				view.getCanvasComponent().setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
				incrementallayouter.calcSwimLanes();
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
				//incrementallayouter.hierarchicLayouter.setLayoutMode(IncrementalHierarchicLayouter.LAYOUT_MODE_INCREMENTAL);
			}
//			for (NodeCursor nc = incrementallayouter.graph.nodes(); nc.ok(); nc.next())
//				incrementallayouter.hintMap.set(nc.node(), 42);
		}
		view.updateView();
	}

}
