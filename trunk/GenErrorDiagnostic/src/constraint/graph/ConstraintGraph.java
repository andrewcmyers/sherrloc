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
	List[][] idPath;
	List[][] leftPath;

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
        saturation();
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
       
    /**
     * This algorithm follows the CFL-Reachablity algorithm described in paper
     * interconvertibility of set constraints and context-free language reachability
     * by David Melski and Thomas Reps
     * The complexity is O (|\Sigma|^3 n^3), where \Sigma is grammar size, n is # of nodes
     * 
     * Here is the grammar we use:
     * id := left right | id id
     * left := left id
     * In order to find the (shortest) reduction path for error diagnostic, we use the
     * dynamic programming algorithm proposed by CHRIS BARRETT, RIKO JACOB, AND MADHAV MARATHE
     * The idea is that we are interested in the shortest path from two nodes, that can be
     * derived from the nonterminal "id" in grammar
     */
	public void saturation() {
		// a working list of all edges
		List<Edge> workingList = new ArrayList<Edge>();
		
		int size = allNodes.size();
		idPath = new List[size][size];
		leftPath = new List[size][size];
		
		// first, add all equation edges as constructor edge "id", constructor edge as left or right edge
		List<Edge> edges = getAllEdges();
		System.out.println(edges.size()+" edges in graph");

		// generate the initial CFG graph
		for (Edge edge : edges) {
			List<Edge> list = new ArrayList<Edge>();
			list.add(edge);
			if (edge instanceof EquationEdge) {
				addReductionEdge(edge.from, edge.to, new IdEdge(edge.from, edge.to, list));
			}
			else if (edge instanceof ConstructorEdge) {
				ConstructorEdge e = (ConstructorEdge) edge;
				if (e.getCondition().isReverse()) {
					addReductionEdge(e.from, e.to, new RightEdge(e.getCondition(), e.from, e.to, list));
				}
				else {
					addReductionEdge(e.from, e.to, new LeftEdge (e.getCondition(), e.from, e.to, list));
				}
			}
		}
		
		List<Edge> alledges = getAllReductionEdges();
		int[][] shortestID = new int[size][size];
		int[][] shortestLeft = new int[size][size];
		
		// Step 1: initialize graph, fix 10000 later
		for (Node start : allNodes) {
			for (Node end : allNodes) {
				idPath[getIndex(start)][getIndex(end)] = new ArrayList<Edge>();
				leftPath[getIndex(start)][getIndex(end)] = new ArrayList<Edge>();
				shortestID[getIndex(start)][getIndex(end)] = 10000;
				shortestLeft[getIndex(start)][getIndex(end)] = 10000;
			}
		}
		
		for (Node n : allNodes) {
			shortestID[getIndex(n)][getIndex(n)] = 0;
		}
		
		for (Edge e : alledges) {
			if (e instanceof IdEdge) {
				shortestID[getIndex(e.from)][getIndex(e.to)] = 1;
				idPath[getIndex(e.from)][getIndex(e.to)].add(e);
			}
			
			if (e instanceof LeftEdge) {
				shortestLeft[getIndex(e.from)][getIndex(e.to)] = 1;
				leftPath[getIndex(e.from)][getIndex(e.to)].add(e);
			}
		}

		// Step 2: relax edges repeatedly
		for (int i = 1; i <= allNodes.size(); i++) {
			for (Node start : allNodes) {
				for (Edge e : alledges) {
					Node from = e.from;
					Node to = e.to;

					if (e instanceof IdEdge) {
						// id = id id
						if (shortestID[getIndex(start)][getIndex(from)] + 1 < shortestID[getIndex(start)][getIndex(to)]) {
							shortestID[getIndex(start)][getIndex(to)] = shortestID[getIndex(start)][getIndex(from)] + 1;
							idPath[getIndex(start)][getIndex(to)].clear();
							idPath[getIndex(start)][getIndex(to)]
									.addAll(idPath[getIndex(start)][getIndex(from)]);
							idPath[getIndex(start)][getIndex(to)].add(e);
							System.out.println(start+"-id-"+from+"-id-"+to+" implies "+start+"-id-"+to);
						}

						// left := left id
						if (shortestLeft[getIndex(start)][getIndex(from)] + 1 < shortestLeft[getIndex(start)][getIndex(to)]) {
							shortestLeft[getIndex(start)][getIndex(to)] = shortestLeft[getIndex(start)][getIndex(from)] + 1;
							leftPath[getIndex(start)][getIndex(to)].clear();
							leftPath[getIndex(start)][getIndex(to)]
									.addAll(leftPath[getIndex(start)][getIndex(from)]);
							leftPath[getIndex(start)][getIndex(to)].add(e);
							System.out.println(start+"-left-"+from+"-id-"+to+" implies "+start+"-left-"+to);
						}
					} else if (e instanceof RightEdge) {
						// id = left right
						if (shortestLeft[getIndex(start)][getIndex(from)] + 1 < shortestID[getIndex(start)][getIndex(to)]) {
							shortestID[getIndex(start)][getIndex(to)] = shortestLeft[getIndex(start)][getIndex(from)] + 1;
							idPath[getIndex(start)][getIndex(to)].clear();
							idPath[getIndex(start)][getIndex(to)]
									.addAll(leftPath[getIndex(start)][getIndex(from)]);
							idPath[getIndex(start)][getIndex(to)].add(e);
							System.out.println(start+"-left-"+from+"-right-"+to+" implies "+start+"-id-"+to);
						}
					}
					// do we need to handle the case when the edges is the left
					// part of reduction rules?
				}
			}
		}
	}
		
//		workingList.addAll(getAllReductionEdges());
//		
//		System.out.println(workingList.size()+" edges in total to handle");
//		
//		while (workingList.size() != 0) {
//			Edge currentedge = workingList.get(0);
//			workingList.remove(0);
//
//			// first, look for production of the form A := B. Skip since no
//			// production is of this form
//
//			// Suppose the current reduction edge has form B<i,j>, look for
//			// productions with form A := B C
//			// for C<j,k> add edge A<i,j>
//			List<ReductionEdge> toAdd = new ArrayList<ReductionEdge>();
//			
//			for (Node to : allNodes) {
//				if (!currentedge.from.equals(to)) {
//					Node from = currentedge.from;
//					for (Edge e : getReductionEdges(currentedge.to, to)) {
//						ReductionEdge newedge = null;
//						
//						// id := id id
//						if (currentedge instanceof IdEdge
//								&& e instanceof IdEdge) {
//							List<Edge> list = new ArrayList<Edge>();
//							list.addAll(((IdEdge)currentedge).edges);
//							list.addAll(((IdEdge)e).edges);
//							newedge = new IdEdge(from, to, list);
//						}
//						
//						// id := left right
//						else if (currentedge instanceof LeftEdge && e instanceof RightEdge
//								&& ((LeftEdge)currentedge).cons.matches(((RightEdge)e).cons)) {
//							List<Edge> list = new ArrayList<Edge>();
//							list.addAll(((LeftEdge)currentedge).edges);
//							list.addAll(((RightEdge)e).edges);
//							newedge = new IdEdge(from, to, list);
//						}
//						
//						// left := left id
//						else if (currentedge instanceof LeftEdge
//								&& e instanceof IdEdge) {
//							List<Edge> list = new ArrayList<Edge>();
//							list.addAll(((LeftEdge)currentedge).edges);
//							list.addAll(((IdEdge)e).edges);
//							newedge = new LeftEdge(
//									((LeftEdge) currentedge).cons, from, to, list);
//						}
//
//						if (newedge != null && !hasReductionEdge(newedge)) {
//							toAdd.add(newedge);
//						}
//					}
//				}
//			}
//				
//			// Suppose the current reduction edge has form B<i,j>, look for
//			// productions with form A := C B
//			// for C<k,i> add edge A<k,j>
////			for (Node from : allNodes) {
////				if (!currentedge.to.equals(from)) {
////					Node to = currentedge.to;
////					for (Edge e : getReductionEdges(from, currentedge.from)) {
////						ReductionEdge newedge = null;
//////						System.out.println ("testing "+from+"->"+currentedge.from+"->"+currentedge.to);
////						
////						// id := id id
////						if (e instanceof IdEdge
////								&& currentedge instanceof IdEdge) {
////							List<Edge> list = new ArrayList<Edge>();
////							list.addAll(((IdEdge)e).edges);
////							list.addAll(((IdEdge)currentedge).edges);
////							newedge = new IdEdge(from, to, list);
////						}
////						
////						// id := left right
////						else if (e instanceof LeftEdge && currentedge instanceof RightEdge 
////								&& ((LeftEdge)e).cons.matches(((RightEdge)currentedge).cons)) {
////							List<Edge> list = new ArrayList<Edge>();
////							list.addAll(((LeftEdge)e).edges);
////							list.addAll(((RightEdge)currentedge).edges);
////							newedge = new IdEdge(from, to, list);
////						}
////						
////						// left := left id
////						else if (e instanceof LeftEdge
////								&& currentedge instanceof IdEdge) {
////							List<Edge> list = new ArrayList<Edge>();
////							list.addAll(((LeftEdge)e).edges);
////							list.addAll(((IdEdge)currentedge).edges);
////							newedge = new LeftEdge(
////									((LeftEdge) e).cons, from, to, list);
////						}
////
////						if (newedge != null && !hasReductionEdge(newedge)) {
////							toAdd.add(newedge);
////						}
////					}
////				}
////			}
////			
////			
////			for (ReductionEdge edge : toAdd) {
////				addReductionEdge(edge.from, edge.to, edge);
////				workingList.add(edge);	
////			}
//	}
    
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
		for (ElementNode start : startNodes) {
			for (ElementNode end : endNodes) {
				if (start.e.equals(end.e))
					continue;
				if ( idPath[getIndex(start)][getIndex(end)].size()!=0 && SYMMENTRIC && (getIndex(start) < getIndex(end))) {
					System.out.println("reporting path between "+start+" "+end);
					ConstraintPath path = new ConstraintPath(idPath[getIndex(start)][getIndex(end)]);
					System.out.println(path.toString(this));
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
