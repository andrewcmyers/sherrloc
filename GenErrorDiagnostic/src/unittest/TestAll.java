package unittest;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

import diagnostic.Analysis;


public class TestAll {
	
	@Test
	public void testGraph () {
		testJif();
		jifTestcases();
	}
	
	public void testErrorPaths (String filename, int expectedpaths, boolean sym) {
		try {
			Analysis ana = Analysis.getAnalysisInstance(filename, sym);
			assertEquals(filename, expectedpaths, ana.getPathNumber());
		}
		catch (Exception e) {
			System.out.println(filename);
			e.printStackTrace();
		}
	}
	
	public void testExpression (String filename, String loc, boolean sym) {
		try {
			Analysis ana = Analysis.getAnalysisInstance(filename, sym);
			assertTrue(ana.toString().contains(loc));
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
		testAssumptions("test/hypothesis/constraints/AirlineAgent1.con", "(TheAirline)->(TheAirline) <= C_L;\n", false);// same assumption
		/* AirlineAgent2 secure */
		/* User1 secure */
		testAssumptions("test/hypothesis/constraints/AirlineExample1.con", "AirlineA <= airlines;\n", false);// same assumption
		testAssumptions("test/hypothesis/constraints/AirlineExample2.con", "AirlineA <= airlines;AirlineB <= airlines;\n", false);// same assumption
		/* jif compiler is too conservative in this case. it's secure */
		testAssumptions("test/hypothesis/constraints/AirlineExample3.con", "", false); // secure
		
		/* battleship */
		testAssumptions("test/hypothesis/constraints/Board1.con", "(p1)->(p1) <= C_L;(p1)<-(p1) <= I_L;C_L <= (p1)->(p1);I_L <= (p1)<-(p1);\n", false); // same assumption

		/* social */
		/* SocialNetwork1 secure */
		testAssumptionsSub("test/hypothesis/constraints/SocialNetwork2.con", "bb <= SN;bg <= SN;user <= SN;\n", false); // same

		/* friendmap */
		testAssumptions("test/hypothesis/constraints/Location1.con", "C_L <= (C_A)join((⊥)->(⊥));\n", false);// same assumption
		testAssumptions("test/hypothesis/constraints/Snapp1.con", "owner2 <= store1;\n", false); // same assumption
		/* Snapp2 secure */
		testAssumptions("test/hypothesis/constraints/FriendMap1.con", "C_*l <= (⊤)->(s1);\n", false); // same assumption
		testAssumptionsSub("test/hypothesis/constraints/FriendMap2.con", "user.friends <= worker$;\n", false); // weaker assumption
		testAssumptionsSub("test/hypothesis/constraints/FriendMap3.con", "C_*map_update <= C_*map_access;\n", false); // weaker assumption
		/* FriendMap4 secure */
		testAssumptions("test/hypothesis/constraints/FriendMap5.con", "C_*l <= (⊤)->(s1);\n", false); // weaker assumption
		/* FriendMap6 secure */
		testAssumptionsSub("test/hypothesis/constraints/FriendMap7.con", "C_l <= (⊤)->(n1);C_n <= (⊤)->(n1);\n", false); // same assumption
		testAssumptions("test/hypothesis/constraints/FriendMap8.con", "C_s <= C_*l;I_s <= I_*l;\n", false); // weaker assumption
		testAssumptionsSub("test/hypothesis/constraints/FriendMap9.con", "C_*iterLabel <= (⊤)->(fn);\n", false); //  weaker assumption, this is a good example
		testAssumptions("test/hypothesis/constraints/FriendMap10.con", "C_*lbl <= (⊤)->(n1);\n", false); // weaker  assumption
		testAssumptionsSub("test/hypothesis/constraints/FriendMap11.con", "C_user <= (⊤)->(n1);\n", false); // weaker  assumption
		/* FriendMap12 secure */
		/* FriendMap13 secure */
		testAssumptionsSub("test/hypothesis/constraints/FriendMap14.con", "C_*iterLabel <= (⊤)->(fn);\n", false); // weaker  assumption
		testAssumptionsSub("test/hypothesis/constraints/FriendMap15.con", "C_*iterLabel <= (⊤)->(fn);\n", false); // same  assumption
		testAssumptions("test/hypothesis/constraints/FriendMap16.con", "(⊤)<-(localStore) <= I_*lbl;C_*lbl <= (⊤)->(localStore);\n", false); // same  assumption
		testAssumptions("test/hypothesis/constraints/FriendMap17.con", "C_*l <= (⊤)->(s1);\n", false); // same  assumption
		testAssumptions("test/hypothesis/constraints/Box1.con", "C_L <= (C_A)join((⊥)->(⊥));\n", false); // same assumption
		/* Box2 secure */
		/* Box3 secure */
		testAssumptionsSub("test/hypothesis/constraints/Box4.con", "C_*l <= C_*a;I_*l <= I_*a;\n", false); // same assumption
		/* MapImage1 secure */
		testAssumptions("test/hypothesis/constraints/MapImage2.con", "C_A <= (⊤)->(s4);\n", false); // same assumption
		testAssumptions("test/hypothesis/constraints/MapImage3.con", "C_*a <= (⊤)->(s1);\n", false); // same assumption
		testAssumptionsSub("test/hypothesis/constraints/MapImage4.con", "C_*bdry_update <= C_*bdry_access;I_*bdry_update <= I_*bdry_access;\n", false); // same assumption
		testAssumptionsSub("test/hypothesis/constraints/MapImage5.con", "C_boundary <= C_L;C_data <= C_L;I_boundary <= I_L;I_data <= I_L;\n", false); // weaker assumption
		testAssumptions("test/hypothesis/constraints/MapImage6.con", "C_L <= C_*l;C_a <= C_*l;C_l <= C_*l;I_L <= I_*l;I_a <= I_*l;I_l <= I_*l;\n", false); // weaker assumption (6 assumptions with integrity)
		testAssumptionsSub("test/hypothesis/constraints/MapServer1.con", "*l <= (⊤)->(this.store);\n", false); // same assumption
	}
	
	@Test
	public void testJif () {
		testAssumptionsSub("test/friendmap/FriendMap3108_1.con", "(⊥)<-(⊥) <= I_*map_access", false);
		testAssumptionsSub("test/friendmap/FriendMap3110_1.con", "(⊤)<-(user.p) <= I_*map_update", false);
		testAssumptionsSub("test/friendmap/FriendMap3112_1.con", "(⊤)<-(local) <= I_*box_u", false);
		testAssumptionsSub("test/friendmap/FriendMap3113_1.con", "C_map <= C_*map_update;I_map <= I_*map_update;", false);
		testAssumptionsSub("test/friendmap/FriendMap3114_1.con", "C_*map_update <= (⊥)->(⊥)", false);
		testAssumptionsSub("test/friendmap/FriendMap3115_1.con", "C_*map_update <= (⊥)->(⊥)", false);
		testExpression("test/friendmap/FriendMap3116_1.con", "FriendMap3116.fab:429,32-42", false);
		testExpression("test/friendmap/FriendMap3120_1.con", "FriendMap3120.fab:494,4-5", false);
		testAssumptionsSub("test/friendmap/FriendMap3122_1.con", "(⊤)<-((fo)meet(fn)) <= I_*map_update", false);
		testExpression("test/friendmap/FriendMap3144_1.con", "FriendMap3144.fab:445,32-42", false);
		testAssumptionsSub("test/friendmap/FriendMap3167_1.con", "C_l <= (⊤)->(n1);C_n <= (⊤)->(n1)", false);
		testAssumptionsSub("test/friendmap/FriendMap3192_1.con", "C_s <= C_*friend_access_bound", false);
		testAssumptionsSub("test/friendmap/FriendMap3193_1.con", "(⊤)<-(this.service) <= I_*fetchLabel", false);
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
