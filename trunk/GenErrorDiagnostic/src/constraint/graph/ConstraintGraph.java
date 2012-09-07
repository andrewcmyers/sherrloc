package constraint.graph;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import constraint.ast.Constraint;
import constraint.ast.ConstructorElement;
import constraint.ast.Element;
import constraint.ast.EnumerableElement;
import constraint.ast.Environment;
import constraint.ast.JoinElement;
import constraint.ast.MeetElement;
import constraint.ast.Relation;

/*
 * A constraint graph takes a set of constructors (user may define there own constructors in order to define the partial order),
 * assumptions, as well as constraints as inputs
 * 
 * This class builds a basic graph from the constraints, without any path finding mechanism
 * Different PathFinders are used to find unsatisfiable paths, which is later used as inputs to the diagnostic analysis
 * 
 */
public class ConstraintGraph extends Graph {
	Environment env;
    Set<Constraint> constraints;
    boolean SYMMENTRIC;
    boolean DEBUG = false;

    Set<String> files;                                          // source codes involved
    public boolean generated;                                          // if the graph has been generated already, just reuse it
    static final boolean PRINT_SRC = false;                     // print corresponding source code

    public ConstraintGraph (Environment env, Set<Constraint> equations, boolean symmentric) {
        this.env = env;
    	this.constraints = equations;
    	this.files = new HashSet<String>();
        this.generated = false;
        this.SYMMENTRIC = symmentric;
    }
            
    /*
     *  map AST elements to graph nodes
     */
    Map<Element, ElementNode> eleToNode = new HashMap<Element, ElementNode>(); // map from AST elements to graph nodes
    int varCounter = 1;
    
    /* get the corresponding node in graph. Create one if none exists */
    public ElementNode getNode (Element e) {
        if (! eleToNode.containsKey(e)) {
            String vid = "v"+varCounter;
            System.out.println("Creating new node for"+e);
            ElementNode n = new ElementNode(vid, e, this); 
            varCounter++;
            // record the source files involved
//            if (e.position()!=null) {
//                files.add(e.position().path());
//            }
            eleToNode.put(e, n);
        }
        return eleToNode.get(e);
    }
    
    public boolean hasElement (Element e) {
    	return eleToNode.containsKey(e);
    }
    
    
    // claim that first element is leq the second element because of constraint e
    public boolean addOneConstraint (Element first, Element second, Constraint e) {
    	// for constructors
//    	if (first instanceof ConstructorElement && second instanceof ConstructorElement) {
//    		ConstructorElement fst = (ConstructorElement) first;
//    		ConstructorElement snd = (ConstructorElement) second;
//			if (!fst.getCons().equals(snd.getCons())) {
//				System.out.println("unsatisfiable constraint: "+e.toDotString());
//				return false;
//			}
//			else {
//				// TODO: this code assumes that leq relation of the constructor is pairwise, but this should be extensible
//				for (int i=0; i<fst.getElements().size(); i++) {
//					if (!addOneConstraint(fst.getElements().get(i), snd.getElements().get(i), e))
//						return false;
//				}
//				return true;
//			}
//		}
//    	else {
    		ElementNode source = getNode(first);
			ElementNode to = getNode(second);

			addEdge(source, to, new EquationEdge(e, source, to));

			// TODO: refactor this code
			if (e.getRelation() == Relation.EQ)
				addEdge(to, source, new EquationEdge(e, to, source));
			return true;
//    	}
    		
    }
    
    public void generateGraph ( ) {
        if (generated || constraints.size() == 0)
            return;
       
        // generate the simple links from the constraints
		for (Constraint cons : constraints) {
			addOneConstraint(cons.getFirstElement(), cons.getSecondElement(), cons);
		}
		
		if (DEBUG)
			System.out.println("Total simple nodes : " + eleToNode.size());
                
        /* 
         * generate inferred links from constructors, join and meet elements
         * 1. flow from components to a join label
         * 2. flow from a meet label to components
         * 3. special edges recording the position in constructor
         * 
         */
        // only need to handle nodes in the graph
        List<ElementNode> workingList = new ArrayList<ElementNode>(eleToNode.values());
        Set<ElementNode> processed = new HashSet<ElementNode>();
        
        // we need to handle constructors
        // and two special constructors, join and meet
        while (workingList.size()!=0) {
        	ElementNode currentnode = workingList.get(0);
            workingList.remove(0);
            processed.add(currentnode);
            Element e = currentnode.e;
            
            // generate the source node
            Collection<Element> compset;
            
            if (e instanceof EnumerableElement){
            	EnumerableElement ce = (EnumerableElement) e;
            	compset = ce.getElements();
            	
            	int index=0;
                for (Element element : compset) {
                    ElementNode srcnode = getNode(element);
                    index++;
                    if (!processed.contains(element) && !workingList.contains(element))
                        workingList.add(getNode(element));
                    
                    if (e instanceof MeetElement) {
                    	addEdge(currentnode, srcnode, new MeetEdge(currentnode, srcnode));
                    }
                    else if (e instanceof JoinElement) {
                    	addEdge(srcnode, currentnode, new JoinEdge(srcnode, currentnode));
                    }
                    else if (e instanceof ConstructorElement){
                    	addEdge(srcnode, currentnode, new ConstructorEdge(new EdgeCondition(((ConstructorElement)e).getCons(), index, false), srcnode, currentnode));
                    	addEdge(currentnode, srcnode, new ConstructorEdge(new EdgeCondition(((ConstructorElement)e).getCons(), index, true), currentnode, srcnode));
                    }
                }
            }
        }
        generated = true;
        if (DEBUG)
        	System.out.println("Total nodes after static: " + eleToNode.size());
    }
    
    // this function is used to filter out letters that can not pretty print in the dot format
    // such as " and \n
    private String sanitaze (String s) {
        if (s!=null)
            return s.replace('"', '\'').replace("\\", "\\\\");
        else
            return s;
    }
    
    
    public String toDotString ( ) {
        String ret = "";
        
        Set<Integer> sourcePosition = new HashSet<Integer>();
        String nodes = "";
        String links = "";
        
        for (Node node : allNodes) {
            if (node instanceof ElementNode) {
                ElementNode n = (ElementNode) node;
                if (!n.shouldprint)
                    continue;
//                sourcePosition.addAll(n.getPositions());
                nodes += n.printNodeToDotString();
                links += n.printLinkToDotString();
            }
        }
        
        ret += "digraph G1 {\n";
        // print source code
//        if (PRINT_SRC) {
//            for (String s : files) {
//                try {
//                    BufferedReader reader =
//                            new BufferedReader(new FileReader(s));
//                    String line = reader.readLine();
//                    int linenum = 1;
//                    ret += "source [shape=box, label=\"";
//                    while (line != null) {
//                        if (v.getSourcePosition().contains(linenum)) {
//                            ret += linenum + ":\t" + sanitaze(line) + "\\l";
//                        }
//                        line = reader.readLine();
//                        linenum++;
//                    }
//                    ret += "\"];\n";
//                } catch (IOException e) {
//                    continue;
//                }
//            }
//        }
        
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
