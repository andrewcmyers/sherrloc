package sherrloc.test;

import static org.junit.Assert.assertTrue;

import org.junit.Test;

import sherrloc.diagnostic.DiagnosticOptions.Mode;
import sherrloc.diagnostic.ErrorDiagnosis;

/**
 * Tests that an analysis is not affected by earlier analyses with the same assumptions. Each
 * analysis must entail relations from its own elements, such as <code>owner &lt;= owner join
 * spender</code>, even if an earlier analysis with the same assumptions had no such elements.
 */
public class TestHypothesisCache {

    private static final String DIR = "tests/hypothesis-cache/";

    private static boolean hasError(String file) throws Exception {
        return ErrorDiagnosis.getAnalysisInstance(DIR + file, Mode.CONS).getUnsatPathNumber() > 0;
    }

    @Test
    public void laterAnalysisSeesItsOwnElements() throws Exception {
        assertTrue("first.con should have no error", !hasError("first.con"));
        assertTrue("second.con should have no error after first.con", !hasError("second.con"));
    }
}
