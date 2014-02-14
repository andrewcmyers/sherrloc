package graph;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import constraint.ast.Constraint;
import constraint.ast.ConstructorApplication;
import constraint.ast.Element;
import constraint.ast.EnumerableElement;
import constraint.ast.Environment;
import constraint.ast.Inequality;
import constraint.ast.JoinElement;
import constraint.ast.MeetElement;
import constraint.ast.Position;
import constraint.ast.Relation;

/**
 * A constraint graph is built on a set of constructors (including user-defined
 * ones), assumptions, as well as constraints as inputs.
 * <p>
 * This class builds a basic graph from the constraints, without saturation.
 * Different <code>PathFinders</code> are used to saturate the graph, which is
 * later used as inputs to infer the most likely error cause.
 */
public class ConstraintGraph extends Graph {
	private Environment env;
    
    private Set<String> files;                                          // source codes involved, only used for DOT files
    private boolean generated;                                   		// if the graph has been generated already, just reuse it
    private final boolean PRINT_SRC = false;                     		// print corresponding source code in DOT files
    private final boolean DEBUG = false;
    private boolean SYMMENTRIC;
    private Map<Element, Node> eleToNode = new HashMap<Element, Node>(); // map from AST elements to graph nodes
    private int varCounter = 1;

    /**
     * @param env Global assumptions
     * @param equations Constraints
     * @param symmentric Set to true when all constraints are equalities
     * 
     */
    public ConstraintGraph (Environment env, Set<Constraint> constraints, boolean symmentric) {
        this(env, symmentric);
		/**
		 * generate the simple links from the constraints. handle constructors,
		 * meet and join later
		 */
		for (Constraint cons : constraints) {
			addOneConstraint(cons);
		}
    }
    
    /**
     * See {@link #ConstraintGraph(Environment, Set, boolean)}
     */
    public ConstraintGraph (Environment env, boolean symmentric) {
        this.env = env;
    	this.files = new HashSet<String>();
        this.generated = false;
        this.SYMMENTRIC = symmentric;
    }
                
    /**
	 * Lookup a node representing element <code>e</code> in graph. Create a
	 * fresh node if no such node exists
	 * 
	 * @param e Element to find
	 * @return A node representing <code>e</code>
	 */
    public Node getNode (Element e) {
    	if (! eleToNode.containsKey(e)) {
            String vid = "v"+varCounter;
            varCounter++;
            Node n = new Node (vid, e, this); 
            eleToNode.put(e, n);
        }
        return eleToNode.get(e);
    }

    /**
     * @param e A constraint element
     * @return True if the graph has a node representing <code>e</code>
     */
    public boolean hasElement (Element e) {
    	return eleToNode.containsKey(e);
    }
    
    /**
	 * Adding a constraint to graph (add edges between nodes representing
	 * constraint elements)
	 * 
	 * @param cons Constraint
	 */
    public void addOneConstraint (Constraint cons) {
		Node source = getNode(cons.getFirstElement());
		Node to = getNode(cons.getSecondElement());

		addEdge(new EquationEdge(cons, source, to));

		if (cons.getRelation() == Relation.EQ)
			addEdge(new EquationEdge(cons, to, source));
    }
    
    /**
	 * Adding an inequality to graph
	 * 
	 * @param cons Constraint
	 */
    public void addOneInequality (Inequality ieq) {
    	addOneConstraint(new Constraint(ieq, null, Position.EmptyPosition()));
    }
    
    /**
     * Generate a constraint graph from constraints
     */
    public void generateGraph ( ) {
        if (generated)
            return;
		
		if (DEBUG)
			System.out.println("Total simple nodes : " + eleToNode.size());
                
        /**
         * generate extra nodes and edges for constructors, join and meet elements
         * 1. Constructor edges between a constructor and its parameters
         * 2. Edges from components to a join element
         * 3. Edges from a meet element to components
         */		
        List<Element> workingList = new ArrayList<Element>(eleToNode.keySet());
        Set<Element> processed = new HashSet<Element>();
        
        while (workingList.size()!=0) {
        	Element e = workingList.get(0);
        	Node currentnode = getNode(e);
            workingList.remove(0);
            processed.add(e);
            
            // generate the source node
            Collection<Element> compset;
            
            if (e instanceof EnumerableElement){
            	EnumerableElement ee = (EnumerableElement) e;
            	compset = ee.getElements();
            	
            	int index=0;
                for (Element element : compset) {
                    Node compnode = getNode(element);
                    index++;
                    // add the component element to the working list if not seen before
                    if (!processed.contains(element) && !workingList.contains(element))
                        workingList.add(element);
                    
                    if (e instanceof MeetElement) {
                    	addEdge(new MeetEdge(currentnode, compnode));
                    }
                    else if (e instanceof JoinElement) {
                    	addEdge(new JoinEdge(compnode, currentnode));
                    }
                    else if (e instanceof ConstructorApplication){
                    	ConstructorApplication ce = (ConstructorApplication)e;
                    	Polarity pol = ce.getCons().isContraVariant()?Polarity.NEG:Polarity.POS;
                    	addEdge(new ConstructorEdge(new EdgeCondition(((ConstructorApplication)e).getCons(), index, false, pol), compnode, currentnode));
                    	addEdge(new ConstructorEdge(new EdgeCondition(((ConstructorApplication)e).getCons(), index, true, pol), currentnode, compnode));
                    }
                }
            }
        }
        generated = true;
        if (DEBUG)
        	System.out.println("Total nodes after static: " + eleToNode.size());
    }
    
    /**
	 * This function is used to filter out characters that can not be prettily
	 * printed in the DOT format such as " and \n
	 * 
	 * @param s A string to output to a DOT file
	 * @return A string without " and \
	 */
    private String sanitaze (String s) {
        if (s!=null)
            return s.replace('"', '\'').replace("\\", "\\\\");
        else
            return s;
    }
    
    /**
     * @return A string in DOT file format which represents the graph
     */
    public String toDotString ( ) {
        String ret = "";
        String nodes = "";
        String links = "";
        
        for (Node n : allNodes) {
			if (!n.shouldprint)
				continue;
			nodes += n.printNodeToDotString();
			links += n.printLinkToDotString();
        }
        
        ret += "digraph G1 {\n";
        // print source code
        if (PRINT_SRC) {
            for (String s : files) {
                try {
                    BufferedReader reader = new BufferedReader(new FileReader(s));
                    String line = reader.readLine();
                    int linenum = 1;
                    ret += "source [shape=box, label=\"";
                    while (line != null) {                           
                    	ret += linenum + ":\t" + sanitaze(line) + "\\l";
                        line = reader.readLine();
                        linenum++;
                    }
                    ret += "\"];\n";
                } catch (IOException e) {
                    continue;
                }
            }
        }
        
        ret += "node [color = grey, style = filled];\n";
        ret += nodes;
        ret += links;
        ret += "}\n";
        return ret;
    }
    
    /** 
     * Mark graph nodes that relates to an error
     */
    public void slicing () {
    	for (Node node : allNodes) {
    		node.shouldprint = node.isCause();
    	}
    }
    
    /**
     * @return All constraint elements
     */
    public Set<Element> getAllElements () {
     return eleToNode.keySet();	
    }
    
    /**
     * @return Global assumptions
     */
    public Environment getEnv() {
		return env;
	}
        
    /**
     * @return True if all constraints are equalities
     */
    public boolean isSymmentric() {
		return SYMMENTRIC;
	}
}
