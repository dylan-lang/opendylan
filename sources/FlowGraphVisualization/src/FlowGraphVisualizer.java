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



public class FlowGraphVisualizer {

	public static void main(String[] args) {	
		new TCPListener().listen();
		DemoBase.initLnF();
		IncrementalHierarchicLayout demo = new IncrementalHierarchicLayout();
		demo.start("Incremental Hierarchic Layouter Demo");
	}

}
