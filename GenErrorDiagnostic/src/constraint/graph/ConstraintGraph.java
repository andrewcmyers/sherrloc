package constraint.graph;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import constraint.ast.CompondElement;
import constraint.ast.Constructor;
import constraint.ast.Element;
import constraint.ast.Equation;
import constraint.ast.Relation;
import constraint.graph.pathfinder.ExistancePathFinder;
import constraint.graph.pathfinder.PathFinder;
import constraint.graph.pathfinder.ShortestPathFinder;
import constraint.graph.visitor.SlicingVisitor;
import constraint.graph.visitor.ToDotVisitor;

/*
 * A general dependency graph takes a set of constraints as inputs
 * and output a dependency graph, where the unsatisfiable paths are correctly labeled
 */
public class ConstraintGraph extends Graph {
    List<Equation> equations;
    List<ConstraintPath> errorPaths;
    boolean SHOW_WHOLE_GRAPH=false;
    boolean SYMMENTRIC = true;

    public ConstraintGraph (List<Equation> equations) {
        this.equations = equations;
        generated = false;
        files = new HashSet<String>();
        this.errorPaths = new ArrayList<ConstraintPath>();
    }
    
    Set<String> files;                                          // source codes involved
    boolean generated;                                          // if the graph has been generated already, just reuse it
    static int count=1;                                         // counter for jif errors. Report information flow paths for each error
    static final boolean PRINT_SRC = false;                     // print corresponding source code
    
    /* 
     * fields for Jif option -report
     */
//    public static final Collection flowgraphtopic = CollectionUtil.list(Topics.labelFlow);
//    // different levels of details
//    public static final int messageOnly         = 1;    // concise path info
//    public static final int detailedMessage     = 2;    // detailed path info, including explanation of each constraint
//    public static final int showSlicedGraph     = 3;    // output the relevant graph (nodes related to the error) into a dot file
//    public static final int showWholeGraph      = 4;    // output the whole graph into a dot file
//    
//    public static boolean shouldReport (int obscurity) {
//        return Report.should_report(flowgraphtopic, obscurity);
//    }
    
    /* this class just modifies the behavior of "equals" on the labels */
//    private class LabelWrapper {
//        Label label;
//        
//        public LabelWrapper(Label lbl) {
//            this.label = lbl;
//        }
//        
//        @Override
//        public boolean equals(Object obj) {
//            if (! (obj instanceof LabelWrapper)) return false;
//            LabelWrapper labelwrap = (LabelWrapper) obj;
//            return this.label==labelwrap.label;
//        }
//        
//        @Override
//        public int hashCode() {
//            return this.label.hashCode();
//        }
//    }
    
    /*
     *  map Jif labels to graph nodes
     */
    Map<Element, ElementNode> eleToNode = new HashMap<Element, ElementNode>(); // map from AST elements to graph nodes
    int varCounter = 1;
    
    /* get the corresponding node in graph. Create one if none exists */
    public ElementNode getNode (Element e) {
        if (! eleToNode.containsKey(e)) {
            String vid = "v"+varCounter;
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
    
    
    public boolean addOneEquation (Element first, Element second, Equation e) {
    	if (first instanceof CompondElement && second instanceof CompondElement) {
			CompondElement fst = (CompondElement) first;
			CompondElement snd = (CompondElement) second;
			if (!fst.getCons().equals(snd.getCons())) {
				System.out.println("unsatisfiable constraint: "+e.toDotString());
				return false;
			}
			else {
				// recursively add equations
				for (int i=0; i<fst.getElements().size(); i++) {
					if (!addOneEquation(fst.getElements().get(i), snd.getElements().get(i), e))
						return false;
				}
				return true;
			}
		}
    	else {
    		ElementNode source = getNode(first);
			ElementNode to = getNode(second);

			addEdge(source, to, new EquationEdge(e, source, to));

			// TODO: refactor this code
			if (e.getRelation() == Relation.EQ)
				addEdge(to, source, new EquationEdge(e, to, source));
			return true;
    	}
    		
    }
    
    public void generateGraph ( ) {
        if (generated || equations.size() == 0)
            return;
       
        // generate the dynamic links
		for (Equation equ : equations) {
			addOneEquation(equ.getFirstElement(), equ.getSecondElement(), equ);
		}
		
		System.out.println("Total nodes after dynamic: " + eleToNode.size());
                
        /* generate static links
         * there are two types of static links:
         * 1. flow from components to a join label
         * 2. flow from a meet label to components
         */
        // only need to handle nodes in the graph
        List<ElementNode> workingList = new ArrayList(eleToNode.values());
        Set<ElementNode> processed = new HashSet<ElementNode>();
        
        // we need to handle constructors
        // and two special constructors, join and meet
        // TODO: handle join and meet later
        while (workingList.size()!=0) {
        	ElementNode currentnode = workingList.get(0);
            workingList.remove(0);
            processed.add(currentnode);
            Element e = currentnode.e;
            
            // generate the source node
            Collection<Element> compset;
//            if (e instanceof JoinLabel) {
//                JoinLabel join = (JoinLabel) e;
//                compset = (join).joinComponents();
//                
//                for (Label complbl : compset) {
//                    LabelNode srcnode = getNode(complbl);
//                    if (!processed.contains(complbl) && !workingList.contains(complbl))
//                        workingList.add(getNode(complbl));
//                    addEdge(srcnode, currentnode, staticEdge);
//                }
//            } else if (e instanceof MeetLabel){
//                MeetLabel meet = (MeetLabel) e;
//                compset = (meet).meetComponents();
//                
//                for (Label complbl : compset) {
//                    LabelNode dstnode = getNode(complbl);
//                    if (!processed.contains(complbl) && !workingList.contains(complbl))
//                        workingList.add(getNode(complbl));
//                    addEdge(currentnode, dstnode, staticEdge);
//                }
//            } else {
//                continue;
//            }

            // next, handle the provable flows
//            for (Label component : sourceset) {
//                if (jiferror.failedConstraint.env().leq(
//                        jiferror.bounds.applyTo(lbl),
//                        jiferror.bounds.applyTo(component))) {
//                    addEdge(getNode(lbl), getNode(component), staticEdge);
//                }
//            }
            if (e instanceof CompondElement) {
            	CompondElement ce = (CompondElement) e;
            	compset = ce.getElements();
              
            	int index=0;
              for (Element element : compset) {
                  ElementNode srcnode = getNode(element);
                  index++;
                  if (!processed.contains(element) && !workingList.contains(element))
                      workingList.add(getNode(element));
                  addEdge(srcnode, currentnode, new ConstructorEdge(new EdgeCondition(ce.getCons(), index, false), srcnode, currentnode));
                  addEdge(currentnode, srcnode, new ConstructorEdge(new EdgeCondition(ce.getCons(), index, true), currentnode, srcnode));
              }
            }
        }
        System.out.println("Total nodes after static: " + eleToNode.size());
        generated = true;
        genErrorPaths();
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
        ToDotVisitor v = new ToDotVisitor();
        List<Node> visited = new ArrayList<Node>();
        acceptForward(v, visited);
        
        ret += "digraph G1 {\n";
        // print source code
        if (PRINT_SRC) {
            for (String s : files) {
                try {
                    BufferedReader reader =
                            new BufferedReader(new FileReader(s));
                    String line = reader.readLine();
                    int linenum = 1;
                    ret += "source [shape=box, label=\"";
                    while (line != null) {
                        if (v.getSourcePosition().contains(linenum)) {
                            ret += linenum + ":\t" + sanitaze(line) + "\\l";
                        }
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
        ret += v.getNodeString();
        ret += v.getLinkString();
        ret += "}\n";
        return ret;
    }
    
    public void slicing () {
        List<Node> visited = new ArrayList<Node>();
        acceptForward(new SlicingVisitor(), visited);
    }		
    
    public void genErrorPaths () {
        if (!generated) generateGraph();
        // only the labels without varables can serve as end nodes
        ArrayList<ElementNode> startNodes = new ArrayList<ElementNode>();
        ArrayList<ElementNode> endNodes = new ArrayList<ElementNode>();
        
        System.out.println("Total nodes before path generaton: " + eleToNode.size());        
        
        for (Element element : eleToNode.keySet()) {
            if (element instanceof Constructor || element instanceof CompondElement) {                    
            	startNodes.add(eleToNode.get(element));
				endNodes.add(eleToNode.get(element));
            }
        }
        
        System.out.println("Total start nodes before path generaton: " + startNodes.size());
        System.out.println("Total end nodes before path generaton: " + endNodes.size());
        
		System.out.println("Total nodes: " + startNodes.size() * endNodes.size());
		PathFinder finder = new ExistancePathFinder(this);
//		PathFinder finder = new ShortestPathFinder(this);
		
		for (ElementNode start : startNodes) {
			for (ElementNode end : endNodes) {
				if (start.e.equals(end.e))
					continue;
				List<Edge> l = finder.getPath(start, end);
				if ( l!=null && SYMMENTRIC && (getIndex(start) < getIndex(end))) {
					System.out.println("reporting path between "+start+" "+end);
					ConstraintPath path = new ConstraintPath(l);
					System.out.println(path.toString());
					path.increaseTotal();
					errorPaths.add(path);
				}
			}
		}
	}
    
    void printRank () {
//    	List<Edge> allEdges = allEdges();
        Edge[] all = allEdges.toArray(new Edge[allEdges.size()]);
        Arrays.sort(all); 
        for (Edge edge : all) {
            if (edge.getCount()>0) 
                System.out.println(edge.getRank() + ": " + edge.toString());
        }
    }
    
    public void showErrorPaths() {
    	for (ConstraintPath path : errorPaths)
    		System.out.println(path.toString());
    }
    
    public void writeToDotFile () {
        String filename;

        filename = "error" + count + ".dot";
        count++;
        
        if (!generated) generateGraph();
//        showErrorPaths();
        
        try {
            FileWriter fstream = new FileWriter(filename);
            BufferedWriter out = new BufferedWriter(fstream);
            if (SHOW_WHOLE_GRAPH) 
            	labelAll();
            else
            	slicing();   
            out.write(toDotString());
            out.close();
        } catch (IOException e) {
            System.out.println("Unable to write the DOT file to: " + filename);
        }
        
        printRank();
    }
}
