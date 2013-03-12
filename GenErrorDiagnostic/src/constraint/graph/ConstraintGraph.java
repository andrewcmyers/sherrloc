package constraint.graph;

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
    	return getNode(e, false);
    }
    
    public ElementNode getNode (Element e, boolean inCons) {
    	if (! eleToNode.containsKey(e)) {
            String vid = "v"+varCounter;
            varCounter++;
            ElementNode n = new ElementNode(vid, e, this, inCons); 
            eleToNode.put(e, n);
        }
        return eleToNode.get(e);
    }
    
    public boolean hasElement (Element e) {
    	return eleToNode.containsKey(e);
    }
    
    
    // claim that first element is leq than the second element because of constraint e
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
//    	System.out.println(first+"<="+second);
    		ElementNode source = getNode(first, true);
			ElementNode to = getNode(second, true);
					
			addEdge(source, to, new EquationEdge(e, source, to));

			if (e.getRelation() == Relation.EQ)
				addEdge(to, source, new EquationEdge(e, to, source));
			return true;
//    	}
    		
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
        
        // we need to handle constructors and two special constructors, join and meet
        while (workingList.size()!=0) {
        	Element e = workingList.get(0);
        	ElementNode currentnode = getNode(e);
            workingList.remove(0);
            processed.add(e);
            
            // generate the source node
            Collection<Element> compset;
            
            if (e instanceof EnumerableElement){
            	EnumerableElement ee = (EnumerableElement) e;
            	compset = ee.getElements();
            	
            	int index=0;
                for (Element element : compset) {
                    ElementNode compnode = getNode(element);
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
