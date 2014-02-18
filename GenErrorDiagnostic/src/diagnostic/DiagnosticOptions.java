package diagnostic;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.apache.commons.cli.PosixParser;

/**
 * A configuration of the error diagnostic tool
 */
public class DiagnosticOptions {
	/** a set of options */
	private boolean wholeGraph;
	private boolean genElements;
	private boolean genHypothesis;
	private boolean genBoth;
	private boolean recursive;
	private boolean verbose;
	private boolean symmetric;
	private boolean dotFile;
	private boolean toConsole;

	/** input/output files */
	private String sourceName;
	private String htmlFileName;
	private String consFile;

	/**
	 * Setup configuration without a command line. Used for unit tests
	 * 
	 * @param consFile
	 *            Input constraint file name
	 * @param isExpr
	 *            Set "True" to infer likely wrong expressions in program; set
	 *            "False" to infer likely missing hypothesis
	 * @param isSym
	 *            True if only equalities are used
	 */
	public DiagnosticOptions(String consFile, boolean isExpr, boolean isSym) {
		setDefault();
		this.consFile = consFile;
		this.symmetric = isSym;
		this.genElements = isExpr;
		this.genHypothesis = !isExpr;
		this.toConsole = true;
	}

	/**
	 * Get options from command line
	 * 
	 * @param args
	 *            Command line input
	 */
	public DiagnosticOptions(String[] args) {
		Options options = new Options();
		options.addOption("a", false, "generate missing hypothesis");
		options.addOption("c", false, "generate wrong constraint elements");
		options.addOption("d", false, "output constraint graph as a DOT file");
		options.addOption("f", false, "show full dependency graph (use with -d)");
		options.addOption("i", true,  "source file generating the constraints");
		options.addOption("l", false, "console report");
		options.addOption("o", true,  "output file");
		options.addOption("r", false, "allow recursion (e.g., x = list x)");
		options.addOption("s", false, "symmetric constraints (equalities only)");
		options.addOption("u", false, "combined report with wrong constraint elements and missing hypothesis (experimental)");
		options.addOption("v", false, "verbose mode (for evaluation)");

		CommandLineParser parser = new PosixParser();
		CommandLine cmd = null;
		try {
			cmd = parser.parse(options, args);
		} catch (ParseException e) {
			HelpFormatter formatter = new HelpFormatter();
			formatter.printHelp("diagnostic", options);
			System.exit(-1);
		}

		setDefault();
		if (cmd.hasOption("a"))
			genHypothesis = true;
		if (cmd.hasOption("c"))
			genElements = true;
		if (cmd.hasOption("d"))
			dotFile = true;
		if (cmd.hasOption("f"))
			wholeGraph = true;
		if (cmd.hasOption("i"))
			sourceName = cmd.getOptionValue("i");
		if (cmd.hasOption("l"))
			toConsole = true;
		if (cmd.hasOption("o"))
			htmlFileName = cmd.getOptionValue("o");
		if (cmd.hasOption("r"))
			recursive = true;
		if (cmd.hasOption("s"))
			symmetric = true;
		if (cmd.hasOption("u"))
			genBoth = true;
		if (cmd.hasOption("v"))
			verbose = true;

		if (cmd.getArgs().length == 0) {
			System.out.println("Please privide a constraint file to be analyzed");
			System.exit(0);
		} else if (!genElements && !genHypothesis && !genBoth) {
			System.out.println("Please set at least one of report type: -c -a or -u");
			System.exit(0);
		}
		consFile = cmd.getArgs()[0];
	}

	/**
	 * Set all options to default values
	 */
	private void setDefault() {
		dotFile = false;
		genBoth = false;
		genElements = false;
		genHypothesis = false;
		wholeGraph = false;
		toConsole = false;
		recursive = false;
		symmetric = false;
		verbose = false;
		htmlFileName = "error.html";
	}

	/**
	 * @return Input constraint file
	 */
	public String getConsFile() {
		return consFile;
	}

	/**
	 * @return Output HTML file when provided. The default value "error.html" is
	 *         returned otherwise
	 */
	public String getHtmlFileName() {
		return htmlFileName;
	}

	/**
	 * @return The source file of program analysis (e.g., OCaml, Jif programs)
	 *         for locating error cause in source file in the HTML output format
	 */
	public String getSourceName() {
		return sourceName;
	}

	/**
	 * @return True to output the constraint graph as a DOT file
	 */
	public boolean isDotFile() {
		return dotFile;
	}

	/**
	 * @return True to generate combined explanations (a mix of wrong
	 *         expressions and missing hypothesis in general)
	 */
	public boolean isGenBoth() {
		return genBoth;
	}

	/**
	 * @return True to generate likely wrong expressions
	 */
	public boolean isGenElements() {
		return genElements;
	}

	/**
	 * @return True to generate likely missing hypothesis
	 */
	public boolean isGenHypothesis() {
		return genHypothesis;
	}

	/**
	 * @return True to allow recursion in constraints (e.g., x = list x)
	 */
	public boolean isRecursive() {
		return recursive;
	}

	/**
	 * @return True when only equalities are used in constraints
	 */
	public boolean isSymmetric() {
		return symmetric;
	}

	/**
	 * @return True to show the error report on console
	 */
	public boolean isToConsole() {
		return toConsole;
	}

	/**
	 * @return True to collect data for evaluation
	 */
	public boolean isVerbose() {
		return verbose;
	}

	/**
	 * @return True to output the entire constraint graph when -d is set
	 */
	public boolean isWholeGraph() {
		return wholeGraph;
	}
}