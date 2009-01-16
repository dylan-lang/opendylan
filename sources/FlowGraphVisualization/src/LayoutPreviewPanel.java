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

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Insets;
import java.awt.Rectangle;
import java.awt.font.TextLayout;
import java.awt.geom.AffineTransform;
import java.awt.geom.Line2D;
import java.awt.geom.Rectangle2D;
import javax.swing.JComponent;
import javax.swing.JFrame;

import y.base.DataProvider;
import y.base.Edge;
import y.base.EdgeCursor;
import y.base.Node;
import y.base.NodeCursor;
import y.geom.YPoint;
import y.geom.YRectangle;
import y.layout.EdgeLabelLayout;
import y.layout.EdgeLayout;
import y.layout.LayoutGraph;
import y.layout.Layouter;
import y.layout.NodeLayout;
import y.layout.grouping.GroupingKeys;

/**
 * A simple graph viewer component that can be used to
 * visualize instances of LayoutGraph. This class is meant
 * as a debugging aid for developers that do not have
 * the viewer classes of the yFiles Viewer distribution.
 *
 */
public class LayoutPreviewPanel extends JComponent
{
  private int insets = 5;
  private Rectangle rect;
  private LayoutGraph graph;
  private Layouter layouter;

  /**
   * Instantiates a new LayoutPreviewPanel.
   */
  public LayoutPreviewPanel(){
    this(null);
  }

  /**
   * Instantiates a new LayoutPreviewPanel displaying the given LayoutGraph.
   */
  public LayoutPreviewPanel(LayoutGraph graph){
    this(graph, null);
  }


  /**
   * Instantiates a new LayoutPreviewPanel displaying the given LayoutGraph.
   */
  public LayoutPreviewPanel(LayoutGraph graph, Layouter layouter){
    this.graph = graph;
    this.layouter = layouter;
    setPreferredSize(new Dimension(500,500));
  }

  /**
   * Creates an new LayoutPreviewPanel for the given graph and shows the frame.
   */
  public static void showFrame( LayoutGraph graph){
    new LayoutPreviewPanel( graph ).createFrame( "" ).setVisible( true );
  }

  /**
   * Returns the LayoutGraph instance displayed by this view.
   */
  public LayoutGraph getGraph(){
    return graph;
  }

  /**
   * Updates the contents of this view. This includes
   * invoking the set layout algorithm on the graph and adjusting
   * the zoom and clip of the view. 
   */
  public void update(){
    if (layouter != null && graph != null){
      layouter.doLayout(graph);
    }
    if (graph != null){
      rect = graph.getBoundingBox();
    }
    repaint();
  }

  /**
   * Paint this component. This will paint the associated LayoutGraph. 
   */
  public void paintComponent(Graphics g){
    Graphics2D g2d = (Graphics2D) g;
    g2d.setColor(this.getBackground());
    Insets ins = getInsets();
    g2d.fillRect(ins.left, ins.top, getWidth() - (ins.left + ins.right), getHeight() - (ins.top + ins.bottom));
    if (graph != null && rect != null){
      double width = getWidth() - (ins.left + ins.right);
      double height = getHeight() - (ins.top + ins.bottom);
      if ((width-2*insets) > 0 && (height-2*insets) > 0){
        double scaling = Math.min(1, Math.min((width-2 * insets) /rect.width, (height-2 * insets) /rect.height ));
        AffineTransform tr = g2d.getTransform();
        AffineTransform af = g2d.getTransform();
        af.concatenate(AffineTransform.getTranslateInstance(
          insets + ins.left + 0.5d* ((width - 2 * insets) - rect.width * scaling),
          insets + ins.top + 0.5d* ((height - 2 * insets) - rect.height * scaling)));
        af.concatenate(AffineTransform.getScaleInstance(scaling, scaling));
        af.concatenate(AffineTransform.getTranslateInstance(-rect.x, -rect.y));
        g2d.setTransform(af);
        g2d.setColor(getForeground());
        for (EdgeCursor ec = graph.edges(); ec.ok(); ec.next()){
          paint(g2d, graph, ec.edge());
        }
        g2d.setColor(Color.blue);
        for (NodeCursor nc = graph.nodes(); nc.ok(); nc.next()){
          paint(g2d, graph, nc.node());
        }
        g2d.setTransform(tr);
      }
    }
  }

  private Line2D.Double line = new Line2D.Double();
  protected void paint(Graphics2D g, LayoutGraph lg, Edge e){
    g.setColor(Color.black);
    EdgeLayout el = lg.getEdgeLayout(e);
    YPoint p = lg.getSourcePointAbs(e);
    double lastX = p.x;
    double lastY = p.y;
    for (int i = 0; i < el.pointCount(); i++){
      YPoint next = el.getPoint(i);
      line.x1 = lastX;
      line.y1 = lastY;
      line.x2 = next.x;
      line.y2 = next.y;
      g.draw(line);
      lastX = next.x;
      lastY = next.y;
    }
    YPoint end = lg.getTargetPointAbs(e);
    line.x1 = lastX;
    line.y1 = lastY;
    line.x2 = end.x;
    line.y2 = end.y;
    g.draw(line);
    
    EdgeLabelLayout[] ells = lg.getEdgeLabelLayout(e);
    for(int i = 0; i < ells.length; i++)
    {
      p = getEdgeLabelLocation(lg, e, ells[i]);
      YRectangle box = ells[i].getBox();
      rectangle.setFrame(p.x, p.y, box.width, box.height);
      g.draw(rectangle);
    }
  }

  private YPoint getEdgeLabelLocation(LayoutGraph graph, Edge e, EdgeLabelLayout ell)
  {
    return ell.getLabelModel().getLabelPlacement(
      ell.getBox(),
      graph.getEdgeLayout(e), 
      graph.getNodeLayout(e.source()),
      graph.getNodeLayout(e.target()),
      ell.getModelParameter());
  }
  
  Rectangle2D.Double rectangle = new Rectangle2D.Double();
  protected void paint(Graphics2D g, LayoutGraph graph, Node node){      
    NodeLayout nl = graph.getNodeLayout(node);
    rectangle.setFrame(nl.getX(), nl.getY(), nl.getWidth(), nl.getHeight());
    DataProvider dp = graph.getDataProvider(GroupingKeys.GROUP_DPKEY);
    if (dp != null && dp.getBool(node)) {
      g.draw(rectangle);
    } else {
      g.fill(rectangle);
    }
    Color c = g.getColor();
    g.setColor(Color.white);
    TextLayout tl = new TextLayout(Integer.toString(node.index()), g.getFont(), g.getFontRenderContext());
    Rectangle2D box = tl.getBounds();
    double tx = nl.getX() + (nl.getWidth() - box.getWidth())/2.0;
    double ty = nl.getY() + box.getHeight() + (nl.getHeight() - box.getHeight())/2.0;
    
    tl.draw(g, (float)(tx), (float)(ty));
    g.setColor(c);
  }
  
  /**
   * Sets the graph that is to be displayed.
   */
  public void setGraph(LayoutGraph graph)
  {
    this.graph = graph;
  }
  
  /**
   * Returns the layout algorithm to be applied on the graph
   * when invoking update().
   */
  public Layouter getLayouter()
  {
    return layouter;
  }

  /**
   * Returns the layout algorithm to be applied on the graph
   * when invoking update().
   */
  public void setLayouter(Layouter layouter)
  {
    this.layouter = layouter;
  }
  
  public JFrame createFrame(String title) 
  {
    JFrame frame = new JFrame(title);
    frame.setContentPane(this);
    frame.setDefaultCloseOperation( JFrame.DISPOSE_ON_CLOSE );
    frame.pack();
    update();
    return frame;
  }
  
}