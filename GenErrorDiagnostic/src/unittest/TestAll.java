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
			testOneFile("src/constraint/tests/jiftestcases/A_2.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/A_3.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/A_4.con", 0, false);
			
			testOneFile("src/constraint/tests/jiftestcases/Account_1.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/Account_2.con", 0, false);
			
			testOneFile("src/constraint/tests/jiftestcases/ArgLabel1_1.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/ArgLabel1_2.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/ArgLabel1_3.con", 0, false);
			
			testOneFile("src/constraint/tests/jiftestcases/ArgLabel2_1.con", 0, false);
			
			testOneFile("src/constraint/tests/jiftestcases/ArgLabelSubst_1.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/ArgLabelSubst_2.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/ArgLabelSubst_3.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/ArgLabelSubst_4.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/ArgLabelSubst_5.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/ArgLabelSubst_6.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/ArgLabelSubst_7.con", 0, false);
			
			testOneFile("src/constraint/tests/jiftestcases/ArgLabelSubst2_1.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/ArgLabelSubst2_2.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/ArgLabelSubst2_3.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/ArgLabelSubst2_4.con", 0, false);
			
			testOneFile("src/constraint/tests/jiftestcases/ArgLabelSubst3_1.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/ArgLabelSubst3_2.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/ArgLabelSubst3_3.con", 0, false);
			
			testOneFile("src/constraint/tests/jiftestcases/ArgLabelSubst4_1.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/ArgLabelSubst4_2.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/ArgLabelSubst4_3.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/ArgLabelSubst4_4.con", 0, false);
			
			testOneFile("src/constraint/tests/jiftestcases/Array_1.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/Array_2.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/Array_3.con", 0, false);
			
			testOneFile("src/constraint/tests/jiftestcases/Array1_1.con", 2, false);
			testOneFile("src/constraint/tests/jiftestcases/Array1_2.con", 0, false);
			
			testOneFile("src/constraint/tests/jiftestcases/Array2_1.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/Array2_2.con", 0, false);
			
			testOneFile("src/constraint/tests/jiftestcases/Array3_1.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/Array3_2.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/Array3_3.con", 0, false);
			
			testOneFile("src/constraint/tests/jiftestcases/Array4_1.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/Array4_2.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/Array4_3.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/Array4_4.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/Array4_5.con", 0, false);
			
			testOneFile("src/constraint/tests/jiftestcases/Array5_1.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/Array5_2.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/Array5_3.con", 0, false);
			
			testOneFile("src/constraint/tests/jiftestcases/Array6_1.con", 2, false);
			testOneFile("src/constraint/tests/jiftestcases/Array6_2.con", 0, false);
			
			testOneFile("src/constraint/tests/jiftestcases/Array7_1.con", 2, false);
			testOneFile("src/constraint/tests/jiftestcases/Array7_2.con", 0, false);
			
			testOneFile("src/constraint/tests/jiftestcases/Array8_1.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/Array8_2.con", 2, false);
			
			testOneFile("src/constraint/tests/jiftestcases/Array9_1.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/Array9_2.con", 1, false);
			
			testOneFile("src/constraint/tests/jiftestcases/Array10_1.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/Array10_2.con", 0, false);
			
			testOneFile("src/constraint/tests/jiftestcases/Array11_1.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/Array11_2.con", 0, false);
			
			testOneFile("src/constraint/tests/jiftestcases/Array12_1.con", 1, false);
			testOneFile("src/constraint/tests/jiftestcases/Array12_2.con", 0, false);
			
			testOneFile("src/constraint/tests/jiftestcases/Array13_1.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/Array13_2.con", 0, false);
			
			testOneFile("src/constraint/tests/jiftestcases/Array14_1.con", 1, false);
			testOneFile("src/constraint/tests/jiftestcases/Array14_2.con", 0, false);
			
			testOneFile("src/constraint/tests/jiftestcases/Array15_1.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/Array15_2.con", 0, false);
			
			testOneFile("src/constraint/tests/jiftestcases/Array16_1.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/Array16_2.con", 0, false);
			
			testOneFile("src/constraint/tests/jiftestcases/Array17_1.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/Array17_2.con", 0, false);
			
			// Array18 is not constraint related
			
			testOneFile("src/constraint/tests/jiftestcases/Array19_1.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/Array19_2.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/Array19_3.con", 0, false);
			
			testOneFile("src/constraint/tests/jiftestcases/Array20_1.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/Array20_2.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/Array20_3.con", 0, false);
			
			testOneFile("src/constraint/tests/jiftestcases/LabelLeConstraint01_1.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/LabelLeConstraint01_2.con", 0, false);
			
			testOneFile("src/constraint/tests/jiftestcases/LabelLeConstraint02_1.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/LabelLeConstraint02_2.con", 0, false);
			
			testOneFile("src/constraint/tests/jiftestcases/LabelLeConstraint03_1.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/LabelLeConstraint03_2.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/LabelLeConstraint03_3.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/LabelLeConstraint03_4.con", 0, false);
			
			testOneFile("src/constraint/tests/jiftestcases/LabelLeConstraint04_1.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/LabelLeConstraint04_2.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/LabelLeConstraint04_3.con", 0, false);

			testOneFile("src/constraint/tests/jiftestcases/LabelLeConstraint05_1.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/LabelLeConstraint05_2.con", 1, false);
			testOneFile("src/constraint/tests/jiftestcases/LabelLeConstraint05_3.con", 0, false);
			
			testOneFile("src/constraint/tests/jiftestcases/LabelLeConstraint06_1.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/LabelLeConstraint06_2.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/LabelLeConstraint06_3.con", 0, false);
			
			testOneFile("src/constraint/tests/jiftestcases/LabelLeConstraint07_1.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/LabelLeConstraint07_2.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/LabelLeConstraint07_3.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/LabelLeConstraint07_4.con", 0, false);
			
			testOneFile("src/constraint/tests/jiftestcases/LabelLeConstraint08_1.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/LabelLeConstraint08_2.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/LabelLeConstraint08_3.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/LabelLeConstraint08_4.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/LabelLeConstraint08_5.con", 0, false);
			
			testOneFile("src/constraint/tests/jiftestcases/LabelLeConstraint08a_1.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/LabelLeConstraint08a_2.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/LabelLeConstraint08a_3.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/LabelLeConstraint08a_4.con", 1, false);
			testOneFile("src/constraint/tests/jiftestcases/LabelLeConstraint08a_5.con", 0, false);
			
			testOneFile("src/constraint/tests/jiftestcases/LabelLeConstraint09_1.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/LabelLeConstraint09_2.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/LabelLeConstraint09_3.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/LabelLeConstraint09_4.con", 3, false);
			testOneFile("src/constraint/tests/jiftestcases/LabelLeConstraint09_5.con", 0, false);
			
			testOneFile("src/constraint/tests/jiftestcases/LabelLeConstraint10_1.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/LabelLeConstraint10_2.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/LabelLeConstraint10_3.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/LabelLeConstraint10_4.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/LabelLeConstraint10_5.con", 0, false);
			
			testOneFile("src/constraint/tests/jiftestcases/LabelLeConstraint11_1.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/LabelLeConstraint11_2.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/LabelLeConstraint11_3.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/LabelLeConstraint11_4.con", 3, false);
			testOneFile("src/constraint/tests/jiftestcases/LabelLeConstraint11_5.con", 0, false);
			
			testOneFile("src/constraint/tests/jiftestcases/LabelLeConstraint12_1.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/LabelLeConstraint12_2.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/LabelLeConstraint12_3.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/LabelLeConstraint12_4.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/LabelLeConstraint12_5.con", 0, false);
			
			testOneFile("src/constraint/tests/jiftestcases/LabelLeConstraint13_1.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/LabelLeConstraint13_2.con", 0, false);
			
			testOneFile("src/constraint/tests/jiftestcases/LabelLeConstraint14_1.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/LabelLeConstraint14_2.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/LabelLeConstraint14_3.con", 0, false);
			
			testOneFile("src/constraint/tests/jiftestcases/LabelSubst01_1.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/LabelSubst01_2.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/LabelSubst01_3.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/LabelSubst01_4.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/LabelSubst01_5.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/LabelSubst01_6.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/LabelSubst01_7.con", 0, false);
			testOneFile("src/constraint/tests/jiftestcases/LabelSubst01_8.con", 0, false);
		}
		catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	
}
