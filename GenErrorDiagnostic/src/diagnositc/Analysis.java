package diagnositc;

import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Set;

import constraint.ast.Constraint;
import constraint.ast.Element;
import constraint.ast.Environment;
import constraint.graph.ConstraintGraph;
import constraint.graph.ConstraintPath;
import constraint.graph.Edge;
import constraint.graph.ElementNode;
import constraint.graph.pathfinder.PathFinder;
import constraint.graph.pathfinder.ShortestPathFinder;
import constraint.parse.GrmLexer;
import constraint.parse.parser;

public class Analysis {
	boolean DEBUG = true;
    boolean SHOW_WHOLE_GRAPH=true;
	boolean done = false;
	ConstraintGraph graph;
	List<ConstraintPath> errorPaths;
	HashMap<Environment, Environment> cachedEnv;	// Reuse graph.env join env if the current env is already seen before
	
	public Analysis(ConstraintGraph g) {
		graph = g;
        errorPaths = new ArrayList<ConstraintPath>();
        cachedEnv = new HashMap<Environment, Environment>();
	}
	
	public static void main(String[] args) {
		try {
			Analysis ana = Analysis.getAnalysisInstance("src/constraint/tests/jif/duplicate.con", false);;
			ana.writeToDotFile();
		}
		catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	static public Analysis getAnalysisInstance (String input, boolean symmentric) throws Exception {
	    parser p = new parser(new GrmLexer(new FileReader(input)));
	    DiagnosisInput result = (DiagnosisInput) p.parse().value;
//	    for (Equation e: result) {
//	    	System.out.println( e.toString());
//	    }
	    ConstraintGraph graph = new ConstraintGraph(result.getEnv(), result.getConstraints(), symmentric);
	    return new Analysis(graph);
	}
	
	// this method is used to configure the path finder
	public PathFinder getPathFinder ( ) {
//		return new ExistancePathFinder(this);
		return new ShortestPathFinder(graph);
	}
	
    public void genErrorPaths ( ) {
        if (!graph.generated) 
        	graph.generateGraph();
        
        // only the labels without varables can serve as end nodes
        ArrayList<ElementNode> startNodes = new ArrayList<ElementNode>();
        ArrayList<ElementNode> endNodes = new ArrayList<ElementNode>();
        Set<Element> elements = graph.getAllElements();
        
        System.out.println("Total nodes before path generaton: " + elements.size());        
        
        for (Element element : elements) {
            if (element.isStart())                    
            	startNodes.add(graph.getNode(element));
			if (element.isEnd())
            	endNodes.add(graph.getNode(element));
        }
        
        if (DEBUG) {
        	System.out.println("Total start nodes before path generaton: " + startNodes.size());
        	System.out.println("Total end nodes before path generaton: " + endNodes.size());
        	System.out.println("Total comparison required: " + startNodes.size() * endNodes.size());
        }

        PathFinder finder = getPathFinder( );
		
		for (ElementNode start : startNodes) {
			for (ElementNode end : endNodes) {
				if (graph.getEnv().leq(start.getElement(), end.getElement()))
					continue;
				List<Edge> l = finder.getPath(start, end);
				if ( l!=null && (!graph.isSymmentric() || (graph.getIndex(start) < graph.getIndex(end)))) {
					System.out.println("reporting path between "+start+" "+end);
					ConstraintPath path = new ConstraintPath(l);
					Environment env;
					if (cachedEnv.containsKey(path.getAssumption()))
						env = cachedEnv.get(path.getAssumption());
					else {
						env = new Environment();
						env.addEnv(graph.getEnv());
						env.addEnv(path.getAssumption());
						cachedEnv.put(path.getAssumption(), env);
					}
					if (env.leq(start.getElement(), end.getElement()))
						continue;
					System.out.println(path.toString());
//					path.increaseTotal();
					errorPaths.add(path);
				}
			}
		}
		done = true;
		
		if (DEBUG)
			System.out.println("*** Found "+errorPaths.size() + " in total");
	}
    
    public int getPathNumber () {
    	if (!done) {
    		genErrorPaths();
    	}
    	int ret = errorPaths.size();
    	printRank();
    	return ret;
    }
    
    void printRank () {    	
        Constraint[] all = graph.getConstraints().toArray(new Constraint[graph.getConstraints().size()]);
        Arrays.sort(all); 
        for (Constraint equ : all) {
            if (equ.getRank() >0) 
                System.out.println(equ.getRank() + ": " + equ.toString());
        }
    }
    
    public void showErrorPaths() {
    	for (ConstraintPath path : errorPaths)
    		System.out.println(path.toString());
    }
    
    public void writeToDotFile () {
        String filename;

        filename = "error.dot";
        
        if (!done) 
        	genErrorPaths();
        
        try {
            FileWriter fstream = new FileWriter(filename);
            BufferedWriter out = new BufferedWriter(fstream);
            if (SHOW_WHOLE_GRAPH) 
            	graph.labelAll();
            else
            	graph.slicing();   
            out.write(graph.toDotString());
            out.close();
        } catch (IOException e) {
            System.out.println("Unable to write the DOT file to: " + filename);
        }
        
        printRank();
    }
}
