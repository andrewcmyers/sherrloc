package unittest;

import org.junit.Test;
import static org.junit.Assert.assertEquals;

import constraint.graph.ConstraintGraph;
import diagnositc.Analysis;


public class TestAll {
	
	@Test
	public void testGraph () {
		testJif();
		testSML();
	}
	
	@Test
	public void testSML() {
		try {
			/* test for SML constraint */
			ConstraintGraph graph = Analysis.getConstraintGraph("src/constraint/tests/test1.con", true);
			assertEquals("test1", 1, graph.getPathNumber());
			
			graph = Analysis.getConstraintGraph("src/constraint/tests/test2.con", true);
			assertEquals("test2", 8, graph.getPathNumber());
			
			graph = Analysis.getConstraintGraph("src/constraint/tests/test3.con", true);
			assertEquals("test3", 4, graph.getPathNumber());
			
			graph = Analysis.getConstraintGraph("src/constraint/tests/test4.con", true);
			assertEquals("test4", 1, graph.getPathNumber());
			
			graph = Analysis.getConstraintGraph("src/constraint/tests/test5.con", true);
			assertEquals("test5", 141, graph.getPathNumber());
		}
		catch (Exception e) {
			e.printStackTrace();
		}

	}

	@Test
	public void testJif () {
		try {
			/* test for Jif constraint */
			ConstraintGraph graph = Analysis.getConstraintGraph("src/constraint/tests/jif.con", false);
			assertEquals("jif", 1, graph.getPathNumber());			
		}
		catch (Exception e) {
			e.printStackTrace();
		}

	}
}
