package sherrloc.test;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.Test;

import sherrloc.diagnostic.ConstraintCheck;
import sherrloc.diagnostic.DiagnosticOptions;
import sherrloc.diagnostic.DiagnosticOptions.Mode;
import sherrloc.diagnostic.ErrorDiagnosis;

/**
 * Tests of constraints with hypotheses. A flow holds only if it is satisfiable along every path
 * between its ends, each under the hypotheses on that path's edges, so paths with different
 * hypotheses must never be merged.
 * <p>
 * Each file in {@code tests/hypothesis-paths} is checked by both the full analysis and the
 * reducing check. A file whose name ends in {@code _W.con} must be reported as an error, and any
 * other file must not. Each check runs several times, since a result that depends on hash
 * iteration order is a bug.
 */
public class TestHypothesisPaths {

    private static final String DIR = "tests/hypothesis-paths";
    private static final int RUNS = 5;

    @Test
    public void hypothesisPaths() throws Exception {
        File[] files = new File(DIR).listFiles((d, name) -> name.endsWith(".con"));
        assertTrue("no test files in " + DIR, files != null && files.length > 0);
        Arrays.sort(files);
        List<String> failures = new ArrayList<>();
        for (File f : files) {
            boolean expectError = f.getName().endsWith("_W.con");
            for (int run = 0; run < RUNS; run++) {
                boolean fullError = ErrorDiagnosis.getAnalysisInstance(f.getPath(), Mode.CONS)
                        .getUnsatPathNumber() > 0;
                if (fullError != expectError) {
                    failures.add(f.getName() + ": full analysis "
                            + (fullError ? "reports an error" : "reports no error")
                            + " (run " + (run + 1) + ")");
                    break;
                }
                boolean checkError = !ConstraintCheck
                        .getAnalysisInstance(new DiagnosticOptions(f.getPath(), Mode.CONS))
                        .getConstraintResult().success();
                if (checkError != expectError) {
                    failures.add(f.getName() + ": reducing check "
                            + (checkError ? "reports an error" : "reports no error")
                            + " (run " + (run + 1) + ")");
                    break;
                }
            }
        }
        if (!failures.isEmpty()) {
            fail(failures.size() + " case(s) failed:\n" + String.join("\n", failures));
        }
    }
}
