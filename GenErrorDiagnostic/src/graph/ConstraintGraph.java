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

import constraint.ast.ComplexElement;
import constraint.ast.Constraint;
import constraint.ast.Element;
import constraint.ast.EnumerableElement;
import constraint.ast.Environment;
import constraint.ast.JoinElement;
import constraint.ast.MeetElement;
import constraint.ast.Relation;

/*
 * A constraint graph takes a set of constructors (include user-defined ones), assumptions, as well as constraints as inputs
 * 
 * This class builds a basic graph from the constraints, without any path finding algorithm
 * Different PathFinders are used to find unsatisfiable paths, which is later used as inputs to the analysis
 * 
 */
public class ConstraintGraph extends Graph {
	Environment env;
    Set<Constraint> constraints;
    boolean SYMMENTRIC;
    boolean DEBUG = false;

    Set<String> files;                                          // source codes involved
    public boolean generated;                                   // if the graph has been generated already, just reuse it
    static final boolean PRINT_SRC = false;                     // print corresponding source code in DOT files

    public ConstraintGraph (Environment env, Set<Constraint> equations, boolean symmentric) {
        this.env = env;
    	this.constraints = equations;
    	this.files = new HashSet<String>();
        this.generated = false;
        this.SYMMENTRIC = symmentric;
    }
            
    Map<Element, Node> eleToNode = new HashMap<Element, Node>(); // map from AST elements to graph nodes
    int varCounter = 1;
    
    /* get the corresponding node in graph. Create one if none exists */
    public Node getNode (Element e) {
    	return getNode(e, false);
    }
    
    public Node getNode (Element e, boolean inCons) {
    	if (! eleToNode.containsKey(e)) {
            String vid = "v"+varCounter;
            varCounter++;
            Node n = new Node (vid, e, this, inCons); 
            eleToNode.put(e, n);
        }
        return eleToNode.get(e);
    }
    
    public boolean hasElement (Element e) {
    	return eleToNode.containsKey(e);
    }
    
    
    // claim that first element is leq than the second element because of constraint e
    public boolean addOneConstraint (Element first, Element second, Constraint e) {
		Node source = getNode(first, true);
		Node to = getNode(second, true);

		addEdge(source, to, new EquationEdge(e, source, to));

		if (e.getRelation() == Relation.EQ)
			addEdge(to, source, new EquationEdge(e, to, source));
		return true;    		
    }
    
    public void generateGraph ( ) {
        if (generated || constraints.size() == 0)
            return;
       
        // generate the simple links from the constraints. handle constructors, meet and join later
		for (Constraint cons : constraints) {
			addOneConstraint(cons.getFirstElement(), cons.getSecondElement(), cons);
		}
		
		if (DEBUG)
			System.out.println("Total simple nodes : " + eleToNode.size());
                
        /* 
         * generate inferred links from constructors, join and meet elements
         * 1. flow from components to components of same constructor
         * 2. flow from components to a join label
         * 3. flow from a meet label to components
         * 
         */
		
        // only need to handle nodes in the graph
        List<Element> workingList = new ArrayList<Element>(eleToNode.keySet());
        Set<Element> processed = new HashSet<Element>();
        
        // we need to handle constructors, including two special constructors, join and meet
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
                    if (!processed.contains(element) && !workingList.contains(element))
                        workingList.add(element);
                    
                    if (e instanceof MeetElement) {
                    	addEdge(currentnode, compnode, new MeetEdge(currentnode, compnode));
                    }
                    else if (e instanceof JoinElement) {
                    	addEdge(compnode, currentnode, new JoinEdge(compnode, currentnode));
                    }
                    else if (e instanceof ComplexElement){
                    	ComplexElement ce = (ComplexElement)e;
                    	Polarity pol = ce.getCons().isContraVariant()?Polarity.NEG:Polarity.POS;
                    	addEdge(compnode, currentnode, new ConstructorEdge(new EdgeCondition(((ComplexElement)e).getCons(), index, false, pol), compnode, currentnode));
                    	addEdge(currentnode, compnode, new ConstructorEdge(new EdgeCondition(((ComplexElement)e).getCons(), index, true, pol), currentnode, compnode));
                    }
                }
            }
        }
        generated = true;
        if (DEBUG)
        	System.out.println("Total nodes after static: " + eleToNode.size());
    }
    
    // this function is used to filter out letters that can not be prettily printed in the dot format
    // such as " and \n
    private String sanitaze (String s) {
        if (s!=null)
            return s.replace('"', '\'').replace("\\", "\\\\");
        else
            return s;
    }
    
    
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
    
    public void slicing () {
    	for (Node node : allNodes) {
    		node.shouldprint = node.isCause();
    	}
    }
    
    public Set<Element> getAllElements () {
     return eleToNode.keySet();	
    }
    
    public Environment getEnv() {
		return env;
	}
    
    public Set<Constraint> getConstraints() {
		return constraints;
	}
    
    public void showAllConstraints() {
    	for (Constraint c : constraints) {
    		System.out.println(c);
    	}
    }
    
    public boolean isSymmentric() {
		return SYMMENTRIC;
	}
}
