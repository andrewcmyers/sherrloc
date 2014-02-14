package diagnostic;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.apache.commons.cli.PosixParser;

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
	String sourceName;
	String htmlFileName;
	String consFile;
	
	/**
	 * Use default configurations
	 */
	public DiagnosticOptions(String consFile, boolean isSym) {
		setDefault();
		this.consFile = consFile;
		this.symmetric = isSym;
	}
    
    /**
     * Get options from command line
     * 
     * @param args Command line input
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
		CommandLine cmd=null;
		try {
			cmd = parser.parse(options, args);
		}
		catch (ParseException e) {
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
		
		if (cmd.getArgs().length==0) {
			System.out.println("Please privide a constraint file to be analyzed");
			System.exit(0);
		}
		consFile = cmd.getArgs()[0];
    }
    
    private void setDefault () {
    	dotFile = false;
    	genBoth = false;
    	genElements = true;
    	genHypothesis = false;
    	wholeGraph = false;
    	toConsole = true;
    	recursive = false;
    	symmetric = false;
    	verbose = false;
    	htmlFileName = "error.html";
    }
    
    public String getConsFile() {
		return consFile;
	}
    
    public String getHtmlFileName() {
		return htmlFileName;
	}
    
    public String getSourceName() {
		return sourceName;
	}
    
    public boolean isDotFile() {
		return dotFile;
	}
    
    public boolean isGenBoth() {
		return genBoth;
	}
    
    public boolean isGenElements() {
		return genElements;
	}
    
    public boolean isGenHypothesis() {
		return genHypothesis;
	}
    
    public boolean isRecursive() {
		return recursive;
	}
    
    public boolean isSymmetric() {
		return symmetric;
	}
    
    public boolean isToConsole() {
		return toConsole;
	}
    
    public boolean isVerbose() {
		return verbose;
	}
    
    public boolean isWholeGraph() {
		return wholeGraph;
	}
}