package sherrloc.diagnostic;

import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.Set;
import sherrloc.constraint.analysis.ConstraintAnalysis;
import sherrloc.constraint.analysis.ConstraintAnalysisImpl;
import sherrloc.constraint.ast.Axiom;
import sherrloc.constraint.ast.Constraint;
import sherrloc.constraint.ast.Hypothesis;
import sherrloc.constraint.parse.GrmLexer;
import sherrloc.constraint.parse.parser;
import sherrloc.graph.ConstraintGraph;
import java.util.ArrayList;


/**
 * The entry-point for SHErrLoc modified to include efficient type checking.
 */
public class ConstraintCheck {

    /**
     * The given `DiagnosticOptions`.
     */
    private final DiagnosticOptions options;

    /**
     * The `graph` to reduce.
     */
    private final ConstraintGraph graph;

    /**
     * The `Hypothesis` of this instance.
     */
    private final Hypothesis env;

    /**
     * The set of `Constraint`s of this instance.
     */
    private final Set<Constraint> constraints;

    /**
     * The `Axiom`s of this instance.
     */
    private final List<Axiom> axioms;

    /**
     * Creates a `ConstraintCheck` instance of `options`.
     */
    public ConstraintCheck(DiagnosticOptions options) throws Exception {
        this.options = options;

        InputStream inp = options.getConsFile() == null
                ? System.in
                : new FileInputStream(options.getConsFile());
        parser p = new parser(new GrmLexer(new InputStreamReader(inp, "UTF-8")));
        DiagnosisInput result = (DiagnosisInput) p.parse().value;

        env = result.getEnv();
        constraints = result.getConstraints();
        axioms = result.getAxioms();

        graph = new ConstraintGraph(env, constraints, axioms);
        graph.generateGraph();
    }

    /**
     * Gets the `ConstraintCheck` instance using `options`.
     */
    public static ConstraintCheck getAnalysisInstance(DiagnosticOptions options) throws Exception {
        return new ConstraintCheck(options);
    }

    /**
     * Exports `graph` as a png image file at `filename`.
     */
    private void exportGraph(String filename) {
        try {
            graph.labelAll();

            FileOutputStream fos = new FileOutputStream("graph.dot");
            OutputStreamWriter osw = new OutputStreamWriter(fos, "UTF-8");
            osw.write(graph.toDotString());
            osw.close();

            ProcessBuilder pb = new ProcessBuilder("dot", "-Tpng", "graph.dot", "-o", filename);
            Process process = pb.start();
            int exitCode = process.waitFor();
            if (exitCode != 0) {
                System.out.println("Image conversion failed with exit code " + exitCode);
            } else {
                System.out.println("Graph image produced at " + filename);
            }
            Files.deleteIfExists(Paths.get("graph.dot"));
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * Creates an associated `ErrorDiagnosis` instance for error localization.
     */
    private ErrorDiagnosis errorDiagnosisInstance() {
        return ErrorDiagnosis.getAnalysisInstance(this);
    }

    /**
     * Creates a fresh (un-generated) graph of `options`.
     */
    public ConstraintGraph freshGraph() {
        return new ConstraintGraph(env, constraints, axioms);
    }

    /**
     * Gets the `DiagnosticOptions` of this `ConstraintCheck` instance
     */
    public DiagnosticOptions options() {
        return options;
    }

    /**
     * Reduces `graph` and returns a value indicating the satisfiability of the reduced graph.
     */
    private boolean isSatisfiable() {
        if (options.isVerbose()) {
            System.out.println("size_before: " + graph.getAllNodes().size());
        }
        if (options.isDotFile()) {
            exportGraph("original.png");
        }
        graph.reduce();
        if (options.isVerbose()) {
            System.out.println("size_after:  " + graph.getAllNodes().size());
        }
        if (options.isDotFile()) {
            exportGraph("reduced.png");
        }
        ConstraintAnalysis cana = new ConstraintAnalysisImpl(options.isGenHypothesis(), options.isVerbose(),
                options.isRecursive());
        return cana.genErrorPaths(graph).size() == 0;
    }

    /**
     * The API for type checking
     */
    public DiagnosticConstraintResult getConstraintResult() {
        if (isSatisfiable()) {
            return new DiagnosticConstraintResult(true, new ArrayList<>());
        } else {
            return errorDiagnosisInstance().getConstraintResult();
        }
    }

    /**
     * The command-line interface for type checking
     */
    public static void main(String[] args) throws Exception {
        DiagnosticOptions options = new DiagnosticOptions(args);
        try {
            ConstraintCheck cc = new ConstraintCheck(options);
            if (cc.isSatisfiable()) {
            } else {
                cc.errorDiagnosisInstance().writeToOutput();
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
