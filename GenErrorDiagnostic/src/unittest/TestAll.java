package unittest;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

import diagnositc.Analysis;


public class TestAll {
	
	@Test
	public void testGraph () {
		testJif();
		testSML();
		jifTestcases();
	}
	
	public void testOneFile (String filename, int expectedpaths, boolean sym) {
		try {
			Analysis ana = Analysis.getAnalysisInstance(filename, sym);;
			assertEquals(filename, expectedpaths, ana.getPathNumber());
		}
		catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	@Test
	public void testSML() {
		try {
			/* test for SML constraint */
			testOneFile("src/constraint/tests/sml/test1.con", 1, true);
			
			testOneFile("src/constraint/tests/sml/test2.con", 8, true);
			
			testOneFile("src/constraint/tests/sml/test3.con", 4, true);
			
			testOneFile("src/constraint/tests/sml/test4.con", 1, true);
			
			testOneFile("src/constraint/tests/sml/test5.con", 141, true);
		}
		catch (Exception e) {
			e.printStackTrace();
		}

	}

	@Test
	public void testJif () {
		try {
			/* test for Jif constraint */
			testOneFile("src/constraint/tests/jif/test.con", 1, false);

			testOneFile("src/constraint/tests/jif/array.con", 2, false);

			testOneFile("src/constraint/tests/jif/constant.con", 2, false);
			
			testOneFile("src/constraint/tests/jif/Do2.con", 1, false);
			
			testOneFile("src/constraint/tests/jif/Do3.con", 1, false);
			
			testOneFile("src/constraint/tests/jif/field.con", 3, false);
			
			testOneFile("src/constraint/tests/jif/For2.con", 1, false);
			
			testOneFile("src/constraint/tests/jif/For3.con", 1, false);
			
			testOneFile("src/constraint/tests/jif/p3.con", 0, false);
			
			testOneFile("src/constraint/tests/jif/para.con", 1, false);
			
			testOneFile("src/constraint/tests/jif/duplicate.con", 0, false);
			
//			testOneFile("src/constraint/tests/jif/23.con", 41, false);
		}
		catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	@Test
	public void jifTestcases () {
		try {
			testOneFile("src/constraint/tests/jiftestcases/A_1.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/A_3.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/A_4.con", 0, false);
			
			testOneFile("src/constraint/tests/jiftestcases/Account_1.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/Account_2.con", 0, false);
			
			testOneFile("src/constraint/tests/jiftestcases/ArgLabel1_4.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/ArgLabel1_6.con", 0, false);
			
			testOneFile("src/constraint/tests/jiftestcases/ArgLabel2_4.con", 0, false);
			
			testOneFile("src/constraint/tests/jiftestcases/ArgLabelSubst_5.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/ArgLabelSubst_7.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/ArgLabelSubst_9.con", 0, false);
		}
		catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	
}
