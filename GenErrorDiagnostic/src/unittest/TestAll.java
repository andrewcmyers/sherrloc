package unittest;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

import diagnostic.Analysis;


public class TestAll {
	
	@Test
	public void testGraph () {
		testJif();
		testSML();
		jifTestcases();
	}
	
	public void testErrorPaths (String filename, int expectedpaths, boolean sym) {
		try {
			Analysis ana = Analysis.getAnalysisInstance(filename, sym);;
			assertEquals(filename, expectedpaths, ana.getPathNumber());
		}
		catch (Exception e) {
			System.out.println(filename);
			e.printStackTrace();
		}
	}
	
	public void testAssumptions (String filename, String expected, boolean sym) {
		try {
			Analysis ana = Analysis.getAnalysisInstance(filename, sym);
			assertEquals(filename, expected, ana.getAssumptionString());
		}
		catch (Exception e) {
			System.out.println(filename);
			e.printStackTrace();
		}
	}
	
	public void testAssumptionsSub (String filename, String expected, boolean sym) {
		try {
			Analysis ana = Analysis.getAnalysisInstance(filename, sym);
			assertTrue(filename, ana.getAssumptionString().contains(expected));
		}
		catch (Exception e) {
			System.out.println(filename);
			e.printStackTrace();
		}
	}
	
	@Test
	public void testHypothesis () {
		/* test for Jif constraint */
		
		/* auction */
		testAssumptions("src/constraint/tests/testhypo/AirlineAgent1.con", "(TheAirline)->(TheAirline) <= C_L;\n", false);// same assumption
		/* AirlineAgent2 secure */
		/* User1 secure */
		testAssumptions("src/constraint/tests/testhypo/AirlineExample1.con", "AirlineA <= airlines;\n", false);// same assumption
		testAssumptions("src/constraint/tests/testhypo/AirlineExample2.con", "AirlineA <= airlines;AirlineB <= airlines;\n", false);// same assumption
		/* jif compiler is too conservative in this case. it's secure */
		testAssumptions("src/constraint/tests/testhypo/AirlineExample3.con", "", false); // secure
		
		/* battleship */
		testAssumptions("src/constraint/tests/testhypo/Board1.con", "(p1)->(p1) <= C_L;(p1)<-(p1) <= I_L;C_L <= (p1)->(p1);I_L <= (p1)<-(p1);\n", false); // same assumption

		/* social */
		testAssumptions("src/constraint/tests/testhypo/Agent1.con", "", false); // same assumption [why this test is here?]
		/* SocialNetwork1 secure */
		testAssumptionsSub("src/constraint/tests/testhypo/SocialNetwork2.con", "bb <= SN;bg <= SN;user <= SN;\n", false); // same

		/* friendmap */
		testAssumptions("src/constraint/tests/testhypo/Location1.con", "C_L <= (C_A)join((_)->(_));\n", false);// same assumption
		testAssumptions("src/constraint/tests/testhypo/Snapp1.con", "owner2 <= store1;\n", false); // same assumption
		/* Snapp2 secure */
		testAssumptions("src/constraint/tests/testhypo/FriendMap1.con", "C_*l <= (*)->(s1);\n", false); // same assumption
		testAssumptionsSub("src/constraint/tests/testhypo/FriendMap2.con", "user.friends <= worker$;\n", false); // weaker assumption
		testAssumptionsSub("src/constraint/tests/testhypo/FriendMap3.con", "C_*map_update <= C_*map_access;\n", false); // weaker assumption
		/* FriendMap4 secure */
		testAssumptions("src/constraint/tests/testhypo/FriendMap5.con", "C_*l <= (*)->(s1);\n", false); // weaker assumption
		/* FriendMap6 secure */
		testAssumptionsSub("src/constraint/tests/testhypo/FriendMap7.con", "C_l <= (*)->(n1);C_n <= (*)->(n1);\n", false); // same assumption
		testAssumptions("src/constraint/tests/testhypo/FriendMap8.con", "C_s <= C_*l;I_s <= I_*l;\n", false); // weaker assumption
		testAssumptions("src/constraint/tests/testhypo/FriendMap9.con", "C_*iterLabel <= (*)->(fn);\n", false); //  weaker assumption, this is a good example
		testAssumptions("src/constraint/tests/testhypo/FriendMap10.con", "C_*lbl <= (*)->(n1);\n", false); // weaker  assumption
		testAssumptionsSub("src/constraint/tests/testhypo/FriendMap11.con", "C_user <= (*)->(n1);\n", false); // weaker  assumption
		/* FriendMap12 secure */
		/* FriendMap13 secure */
		testAssumptions("src/constraint/tests/testhypo/FriendMap14.con", "C_*iterLabel <= (*)->(fn);\n", false); // weaker  assumption
		testAssumptionsSub("src/constraint/tests/testhypo/FriendMap15.con", "C_*iterLabel <= (*)->(fn);\n", false); // same  assumption
		testAssumptions("src/constraint/tests/testhypo/FriendMap16.con", "(*)<-(localStore) <= I_*lbl;C_*lbl <= (*)->(localStore);\n", false); // same  assumption
		testAssumptions("src/constraint/tests/testhypo/FriendMap17.con", "C_*l <= (*)->(s1);\n", false); // same  assumption
		testAssumptions("src/constraint/tests/testhypo/Box1.con", "C_L <= (C_A)join((_)->(_));\n", false); // same assumption
		/* Box2 secure */
		/* Box3 secure */
		testAssumptionsSub("src/constraint/tests/testhypo/Box4.con", "C_*l <= C_*a;I_*l <= I_*a;\n", false); // same assumption
		/* MapImage1 secure */
		testAssumptions("src/constraint/tests/testhypo/MapImage2.con", "C_A <= (*)->(s4);\n", false); // same assumption
		testAssumptions("src/constraint/tests/testhypo/MapImage3.con", "C_*a <= (*)->(s1);\n", false); // same assumption
		testAssumptionsSub("src/constraint/tests/testhypo/MapImage4.con", "C_*bdry_update <= C_*bdry_access;I_*bdry_update <= I_*bdry_access;\n", false); // same assumption
		testAssumptionsSub("src/constraint/tests/testhypo/MapImage5.con", "C_boundary <= C_L;C_data <= C_L;I_boundary <= I_L;I_data <= I_L;\n", false); // weaker assumption
		testAssumptions("src/constraint/tests/testhypo/MapImage6.con", "C_L <= C_*l;C_a <= C_*l;C_l <= C_*l;I_L <= I_*l;I_a <= I_*l;I_l <= I_*l;\n", false); // weaker assumption (6 assumptions with integrity)
		testAssumptionsSub("src/constraint/tests/testhypo/MapServer1.con", "*l <= (*)->(this.store);\n", false); // same assumption
	}
	
	@Test
	public void testOcaml () {
		/* test for OCaml constraint */
//		testErrorPaths("src/constraint/tests/ocaml/test.con", 1, true);
		testErrorPaths("src/constraint/tests/ocaml/test1.con", 3, true);
		testErrorPaths("src/constraint/tests/ocaml/test2.con", 8, true);
		testErrorPaths("src/constraint/tests/ocaml/test3.con", 8, false);
		testErrorPaths("src/constraint/tests/ocaml/option.con", 1, true);
	}
	
	@Test
	public void testSML() {
		/* test for SML constraint */
		testErrorPaths("src/constraint/tests/sml/test1.con", 1, true);

		testErrorPaths("src/constraint/tests/sml/test2.con", 8, true);

		testErrorPaths("src/constraint/tests/sml/test3.con", 8, true);

		testErrorPaths("src/constraint/tests/sml/test4.con", 1, true);

		testErrorPaths("src/constraint/tests/sml/test5.con", 141, true);
	}

	@Test
	public void testJif () {
		/* test for Jif constraint */
		testErrorPaths("src/constraint/tests/jif/inte.con", 1, false);
		
		testErrorPaths("src/constraint/tests/jif/test.con", 2, false);

		testErrorPaths("src/constraint/tests/jif/array.con", 4, false);

		testErrorPaths("src/constraint/tests/jif/constant.con", 2, false);

		testAssumptions("src/constraint/tests/jif/CMUcred.con", "Alice <= p1;\n", false);

		testErrorPaths("src/constraint/tests/jif/Do2.con", 2, false);

		testErrorPaths("src/constraint/tests/jif/Do3.con", 2, false);

		testErrorPaths("src/constraint/tests/jif/field.con", 3, false); 

		testErrorPaths("src/constraint/tests/jif/For2.con", 2, false);

		testErrorPaths("src/constraint/tests/jif/For3.con", 2, false);

		testErrorPaths("src/constraint/tests/jif/p3.con", 2, false);

		testErrorPaths("src/constraint/tests/jif/para.con", 1, false);

		testErrorPaths("src/constraint/tests/jif/duplicate.con", 0, false);
								
		/* currently, these contraints are generated from the snapshot of Mar. 6. 2012 */
//		testErrorPaths("src/constraint/tests/jif/r3122.con", 0, false);
//		testAssumptions("src/constraint/tests/jif/r3122.con", "", false);
//		
//		testErrorPaths("src/constraint/tests/jif/r3141.con", 0, false);
//		testAssumptions("src/constraint/tests/jif/r3141.con", 0, false);
//		
//		testErrorPaths("src/constraint/tests/jif/r3142.con", 9, false);
//		testAssumptions("src/constraint/tests/jif/r3142.con", 2, false);
		
		/* the change from 3142 to 3143 is interesting, since another file is changed */
//		testErrorPaths("src/constraint/tests/jif/r3143.con", 16, false);
//		testAssumptions("src/constraint/tests/jif/r3143.con", "", false);
//		
//		testErrorPaths("src/constraint/tests/jif/r3144.con", 16, false); // or 21?
//		testAssumptions("src/constraint/tests/jif/r3144.con", "", false);
//		testOneFile("src/constraint/tests/jif/r3151.con", 41, false); // or 44?
		
//		testOneFile("src/constraint/tests/jif/r3167.con", 5, false); // or 4?
//		
//		testOneFile("src/constraint/tests/jif/r3176.con", 23, false); // or 15?
//		
//		testAssumptions("src/constraint/tests/jif/FriendMap3192.con", "{s} <= {*->n}", false);
//		testAssumptions("src/constraint/tests/jif/hypoinfer.con", "a <= (c)join(b);\n", false); // or 26
		
//		testOneFile("src/constraint/tests/jif/23.con", 41, false);
	}
	
	@Test
	public void jifTestcases () {
		testErrorPaths("src/constraint/tests/jiftestcases/A_1.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/A_2.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/A_3.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/A_4.con", 0, false);
		
		testErrorPaths("src/constraint/tests/jiftestcases/Account_1.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/Account_2.con", 0, false);
		
		testErrorPaths("src/constraint/tests/jiftestcases/ArgLabel1_1.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/ArgLabel1_2.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/ArgLabel1_3.con", 0, false);
		
		testErrorPaths("src/constraint/tests/jiftestcases/ArgLabel2_1.con", 0, false);
		
		testErrorPaths("src/constraint/tests/jiftestcases/ArgLabelSubst_1.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/ArgLabelSubst_2.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/ArgLabelSubst_3.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/ArgLabelSubst_4.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/ArgLabelSubst_5.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/ArgLabelSubst_6.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/ArgLabelSubst_7.con", 0, false);
		
		testErrorPaths("src/constraint/tests/jiftestcases/ArgLabelSubst2_1.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/ArgLabelSubst2_2.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/ArgLabelSubst2_3.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/ArgLabelSubst2_4.con", 0, false);
		
		testErrorPaths("src/constraint/tests/jiftestcases/ArgLabelSubst3_1.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/ArgLabelSubst3_2.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/ArgLabelSubst3_3.con", 0, false);
		
		testErrorPaths("src/constraint/tests/jiftestcases/ArgLabelSubst4_1.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/ArgLabelSubst4_2.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/ArgLabelSubst4_3.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/ArgLabelSubst4_4.con", 0, false);
		
		testErrorPaths("src/constraint/tests/jiftestcases/Array_1.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/Array_2.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/Array_3.con", 0, false);
		
		testErrorPaths("src/constraint/tests/jiftestcases/Array1_1.con", 7, false);
		testErrorPaths("src/constraint/tests/jiftestcases/Array1_2.con", 0, false);
		
		testErrorPaths("src/constraint/tests/jiftestcases/Array2_1.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/Array2_2.con", 0, false);
		
		testErrorPaths("src/constraint/tests/jiftestcases/Array3_1.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/Array3_2.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/Array3_3.con", 0, false);
		
		testErrorPaths("src/constraint/tests/jiftestcases/Array4_1.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/Array4_2.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/Array4_3.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/Array4_4.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/Array4_5.con", 0, false);
		
		testErrorPaths("src/constraint/tests/jiftestcases/Array5_1.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/Array5_2.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/Array5_3.con", 0, false);
		
		testErrorPaths("src/constraint/tests/jiftestcases/Array6_1.con", 6, false);
		testErrorPaths("src/constraint/tests/jiftestcases/Array6_2.con", 0, false);
		
		testErrorPaths("src/constraint/tests/jiftestcases/Array7_1.con", 3, false);
		testErrorPaths("src/constraint/tests/jiftestcases/Array7_2.con", 0, false);
		
		testErrorPaths("src/constraint/tests/jiftestcases/Array8_1.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/Array8_2.con", 4, false);
		
		testErrorPaths("src/constraint/tests/jiftestcases/Array9_1.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/Array9_2.con", 2, false);
		
		testErrorPaths("src/constraint/tests/jiftestcases/Array10_1.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/Array10_2.con", 0, false);
		
		testErrorPaths("src/constraint/tests/jiftestcases/Array11_1.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/Array11_2.con", 0, false);
		
		testErrorPaths("src/constraint/tests/jiftestcases/Array12_1.con", 2, false);
		testErrorPaths("src/constraint/tests/jiftestcases/Array12_2.con", 0, false);
		
		testErrorPaths("src/constraint/tests/jiftestcases/Array13_1.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/Array13_2.con", 0, false);
		
		testErrorPaths("src/constraint/tests/jiftestcases/Array14_1.con", 2, false);
		testErrorPaths("src/constraint/tests/jiftestcases/Array14_2.con", 0, false);
		
		testErrorPaths("src/constraint/tests/jiftestcases/Array15_1.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/Array15_2.con", 0, false);
		
		testErrorPaths("src/constraint/tests/jiftestcases/Array16_1.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/Array16_2.con", 0, false);
		
		testErrorPaths("src/constraint/tests/jiftestcases/Array17_1.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/Array17_2.con", 0, false);
		
		// Array18 is not constraint related
		
		testErrorPaths("src/constraint/tests/jiftestcases/Array19_1.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/Array19_2.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/Array19_3.con", 0, false);
		
		testErrorPaths("src/constraint/tests/jiftestcases/Array20_1.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/Array20_2.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/Array20_3.con", 0, false);
		
		testErrorPaths("src/constraint/tests/jiftestcases/Dyn1_1.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/Dyn1_2.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/Dyn1_3.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/Dyn1_4.con", 0, false);
		
		testErrorPaths("src/constraint/tests/jiftestcases/Dyn2_1.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/Dyn2_2.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/Dyn2_3.con", 0, false);
		
		testErrorPaths("src/constraint/tests/jiftestcases/Dyn4_1.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/Dyn4_2.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/Dyn4_3.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/Dyn4_4.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/Dyn4_5.con", 0, false);
		
		testErrorPaths("src/constraint/tests/jiftestcases/Dyn5_1.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/Dyn5_2.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/Dyn5_3.con", 0, false);
		
		testErrorPaths("src/constraint/tests/jiftestcases/Dyn6_1.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/Dyn6_2.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/Dyn6_3.con", 0, false);
		
		testErrorPaths("src/constraint/tests/jiftestcases/Dyn7_1.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/Dyn7_2.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/Dyn7_3.con", 0, false);
		
		testErrorPaths("src/constraint/tests/jiftestcases/DynLabel1_1.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/DynLabel1_2.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/DynLabel1_3.con", 0, false);
		
		// DynLabel2-7 fails due to syntactic errors
		
		testErrorPaths("src/constraint/tests/jiftestcases/DynLabel8_1.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/DynLabel8_2.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/DynLabel8_3.con", 0, false);
		
		testErrorPaths("src/constraint/tests/jiftestcases/DynLabel9_1.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/DynLabel9_2.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/DynLabel9_3.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/DynLabel9_4.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/DynLabel9_5.con", 0, false);
	
		testErrorPaths("src/constraint/tests/jiftestcases/DynLabel10_1.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/DynLabel10_2.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/DynLabel10_3.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/DynLabel10_4.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/DynLabel10_5.con", 0, false);
		
		testErrorPaths("src/constraint/tests/jiftestcases/DynLabel11_1.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/DynLabel11_2.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/DynLabel11_3.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/DynLabel11_4.con", 0, false);
		
		testErrorPaths("src/constraint/tests/jiftestcases/DynLabel12_1.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/DynLabel12_2.con", 0, false);
//		testOneFile("src/constraint/tests/jiftestcases/DynLabel12_3.con", 0, false); // fails due to the final access path
		testErrorPaths("src/constraint/tests/jiftestcases/DynLabel12_4.con", 0, false);
		
		testErrorPaths("src/constraint/tests/jiftestcases/DynLabel13_1.con", 1, false);
		testErrorPaths("src/constraint/tests/jiftestcases/DynLabel13_2.con", 0, false);
		
		testErrorPaths("src/constraint/tests/jiftestcases/For1_1.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/For1_2.con", 0, false);
		
		testErrorPaths("src/constraint/tests/jiftestcases/For2_1.con", 2, false);
		testErrorPaths("src/constraint/tests/jiftestcases/For2_2.con", 0, false);
		
		testErrorPaths("src/constraint/tests/jiftestcases/For3_1.con", 2, false);
		testErrorPaths("src/constraint/tests/jiftestcases/For3_2.con", 0, false);
		
		testErrorPaths("src/constraint/tests/jiftestcases/For4_1.con", 2, false);
		testErrorPaths("src/constraint/tests/jiftestcases/For4_2.con", 0, false);
		
		testErrorPaths("src/constraint/tests/jiftestcases/For5_1.con", 2, false);
		testErrorPaths("src/constraint/tests/jiftestcases/For5_2.con", 0, false);
		
		testErrorPaths("src/constraint/tests/jiftestcases/LabelLeConstraint01_1.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/LabelLeConstraint01_2.con", 0, false);
		
		testErrorPaths("src/constraint/tests/jiftestcases/LabelLeConstraint02_1.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/LabelLeConstraint02_2.con", 0, false);
		
		testErrorPaths("src/constraint/tests/jiftestcases/LabelLeConstraint03_1.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/LabelLeConstraint03_2.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/LabelLeConstraint03_3.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/LabelLeConstraint03_4.con", 0, false);
		
		testErrorPaths("src/constraint/tests/jiftestcases/LabelLeConstraint04_1.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/LabelLeConstraint04_2.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/LabelLeConstraint04_3.con", 0, false);
	
		testErrorPaths("src/constraint/tests/jiftestcases/LabelLeConstraint05_1.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/LabelLeConstraint05_2.con", 2, false);
		testErrorPaths("src/constraint/tests/jiftestcases/LabelLeConstraint05_3.con", 0, false);
		
		testErrorPaths("src/constraint/tests/jiftestcases/LabelLeConstraint06_1.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/LabelLeConstraint06_2.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/LabelLeConstraint06_3.con", 0, false);
		
		testErrorPaths("src/constraint/tests/jiftestcases/LabelLeConstraint07_1.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/LabelLeConstraint07_2.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/LabelLeConstraint07_3.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/LabelLeConstraint07_4.con", 0, false);
		
		testErrorPaths("src/constraint/tests/jiftestcases/LabelLeConstraint08_1.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/LabelLeConstraint08_2.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/LabelLeConstraint08_3.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/LabelLeConstraint08_4.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/LabelLeConstraint08_5.con", 0, false);
		
		testErrorPaths("src/constraint/tests/jiftestcases/LabelLeConstraint08a_1.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/LabelLeConstraint08a_2.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/LabelLeConstraint08a_3.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/LabelLeConstraint08a_4.con", 1, false);
		testErrorPaths("src/constraint/tests/jiftestcases/LabelLeConstraint08a_5.con", 0, false);
		
		testErrorPaths("src/constraint/tests/jiftestcases/LabelLeConstraint09_1.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/LabelLeConstraint09_2.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/LabelLeConstraint09_3.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/LabelLeConstraint09_4.con", 4, false);
		testErrorPaths("src/constraint/tests/jiftestcases/LabelLeConstraint09_5.con", 0, false);
		
		testErrorPaths("src/constraint/tests/jiftestcases/LabelLeConstraint10_1.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/LabelLeConstraint10_2.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/LabelLeConstraint10_3.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/LabelLeConstraint10_4.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/LabelLeConstraint10_5.con", 0, false);
		
		testErrorPaths("src/constraint/tests/jiftestcases/LabelLeConstraint11_1.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/LabelLeConstraint11_2.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/LabelLeConstraint11_3.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/LabelLeConstraint11_4.con", 6, false);
		testErrorPaths("src/constraint/tests/jiftestcases/LabelLeConstraint11_5.con", 0, false);
		
		testErrorPaths("src/constraint/tests/jiftestcases/LabelLeConstraint12_1.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/LabelLeConstraint12_2.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/LabelLeConstraint12_3.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/LabelLeConstraint12_4.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/LabelLeConstraint12_5.con", 0, false);
		
		testErrorPaths("src/constraint/tests/jiftestcases/LabelLeConstraint13_1.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/LabelLeConstraint13_2.con", 0, false);
		
		testErrorPaths("src/constraint/tests/jiftestcases/LabelLeConstraint14_1.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/LabelLeConstraint14_2.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/LabelLeConstraint14_3.con", 0, false);
		
		testErrorPaths("src/constraint/tests/jiftestcases/LabelSubst01_1.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/LabelSubst01_2.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/LabelSubst01_3.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/LabelSubst01_4.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/LabelSubst01_5.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/LabelSubst01_6.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/LabelSubst01_7.con", 0, false);
		testErrorPaths("src/constraint/tests/jiftestcases/LabelSubst01_8.con", 0, false);
	}
	
	
}
