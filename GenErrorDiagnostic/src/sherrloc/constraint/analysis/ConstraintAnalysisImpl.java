package sherrloc.constraint.analysis;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import sherrloc.constraint.ast.Application;
import sherrloc.constraint.ast.Element;
import sherrloc.constraint.ast.Hypothesis;
import sherrloc.constraint.ast.JoinElement;
import sherrloc.constraint.ast.MeetElement;
import sherrloc.constraint.ast.Variable;
import sherrloc.diagnostic.UnsatPaths;
import sherrloc.graph.ConstraintGraph;
import sherrloc.graph.ConstraintPath;
import sherrloc.graph.DummyEdge;
import sherrloc.graph.Edge;
import sherrloc.graph.Node;

/**
 * This class identifies satisfiable and unsatisfiable constraints in a
 * constraint graph.
 */
public class ConstraintAnalysisImpl implements ConstraintAnalysis {
	private boolean isSym;
	private boolean isVerbose;
	private boolean isRec;
	private boolean DEBUG = false;

	/** Reuse saturated hypothesis graph when possible */
	private HashMap<Hypothesis, Hypothesis> cachedEnv;

	/**
	 * @param isSym
	 *            True if all constraints are equalities (to avoid duplication
	 *            in result)
	 * @param isVerbose
	 *            True to collect data for evaluation
	 * @param isRec
	 *            True if recursion is allowed
	 */
	public ConstraintAnalysisImpl(boolean isSym, boolean isVerbose, boolean isRec) {
		this.isSym = isSym;
		this.isVerbose = isVerbose;
		this.isRec = isRec;
		cachedEnv = new HashMap<Hypothesis, Hypothesis>();
	}

	/**
	 * Return an instance of constraint analysis. Currently, the only analysis
	 * implemented is {@link ShortestPathFinder}
	 * 
	 * @return An constraint analysis algorithm
	 */
	private PathFinder getPathFinder(ConstraintGraph graph) {
		return new ShortestPathFinder(graph, isVerbose, false);
	}

	@Override
	public UnsatPaths genErrorPaths(ConstraintGraph graph) {
		ArrayList<Node> startNodes = new ArrayList<Node>();
		ArrayList<Node> endNodes = new ArrayList<Node>();
		UnsatPaths unsatPaths = new UnsatPaths();

		// saturate constraint graph
		PathFinder finder = getPathFinder(graph);
		
		/** only search for informative paths */
		for (Node node : graph.getAllNodes()) {
			if (!(node.getElement() instanceof JoinElement))
				startNodes.add(node);
			if (!(node.getElement() instanceof MeetElement))
				endNodes.add(node);
		}

		if (DEBUG) {
			System.out.println("Total start nodes before path generaton: "
					+ startNodes.size());
			System.out.println("Total end nodes before path generaton: "
					+ endNodes.size());
			System.out.println("Total comparison required: "
					+ startNodes.size() * endNodes.size());
		}

		if (isVerbose)
			System.out.println("graph_size: " + graph.getAllNodes().size());
		
		if (!isRec) {
		for (Node node : graph.getAllNodes()) {
			// when recursion is not allowed, constraints such as "x = list x" is unsatisfiable
			if (finder.hasLeftEdge(node, node)) {
				List<List<Edge>> paths = finder.getLeftPaths(node, node);
				for (List<Edge> l : paths) {
					ConstraintPath path = new ConstraintPath(l, finder,
							graph.getEnv(), cachedEnv);
					unsatPaths.addUnsatPath(path);
					if (DEBUG) {
						System.out.println("****** Infinite path ******");
						System.out.println(path);
					}
				}
				continue;
			}
			// go one step ahead
			else {
				for (Node m : graph.getAllNodes()) {
					if (finder.hasLeftEdge(node, m) && finder.hasLeftEdge(m, node)) {
						List<List<Edge>> paths = finder.getLeftPaths(node, m);
						for (List<Edge> l1 : paths) {
							for (List<Edge> l2 : finder.getLeftPaths(m, node)) {
								List<Edge> lst = new ArrayList<Edge>();
								lst.addAll(l1);
								lst.addAll(l2);
								ConstraintPath path = new ConstraintPath(lst, finder, graph.getEnv(), cachedEnv);
								unsatPaths.addUnsatPath(path);
								if (DEBUG) {
									System.out.println("****** Infinite path ******");
									System.out.println(path);
								}
							}
						}
					}
				}
			}
			// TODO: need to generalize the algorithm to more general cases
		}
		}
		
		for (Node start : startNodes) {
			for (Node end : endNodes) {
				// avoid returning duplicated edges when only equalities are used
				if (isSym && (start.getIndex() <= end.getIndex()))
					continue;
				
				// test if a partial ordering can be inferred
				if (!finder.hasLeqEdge(start, end))
					continue;
				
				List<Edge> l = finder.getPath(start, end);
				testConsistency(start.getElement(), end.getElement(), l, graph, finder, unsatPaths);
			}
		}

		return unsatPaths;
	}
	
	void testConsistency (Element e1, Element e2, List<Edge> l, ConstraintGraph graph, PathFinder finder, UnsatPaths unsatPaths) {
		// ignore trivial cases
		if (e1.trivialEnd() || e2.trivialEnd()) {
			return;
		}

		// less interesting paths
		if (e1.isBottom() || e2.isTop())
			return;

		ConstraintPath path = new ConstraintPath(l, finder, graph.getEnv(), cachedEnv);

		if (path.isSatPath()) {
			path.incSuccCounter();
			// need to replace variable elements to identify more potential failures
//			expandGraph(e1, e2, l, graph, finder, unsatPaths);
			return;
		} else if (path.isUnsatPath()) {
			if (DEBUG) {
				System.out.println("****** Unsatisfiable path ******");
				System.out.println(path);
			}
			unsatPaths.addUnsatPath(path);
    		path.setCause();
		}
	}
	
	void expandGraph (Element e1, Element e2,  List<Edge> l, ConstraintGraph graph, PathFinder finder, UnsatPaths unsatPaths) {
		if (e1.hasVars() && e1 instanceof Application && !(e2 instanceof Application)) {
			Application app = (Application) e1;
			for (Variable var : e1.getVars()) {
				Node varnode = graph.getNode(var);
				for (Node n : graph.getNeighbors(varnode)) {
					if (finder.hasLeqEdge(varnode, n)) {
						Element newfrom = app.replace(var, n.getElement());
						if (!graph.hasElement(newfrom)) {
							List<Edge> edgessofar = new ArrayList<Edge>();
							Set<Element> eset = new HashSet<Element>();
							eset.add(newfrom);
							graph.getEnv().addElements(eset);
							edgessofar.add(new DummyEdge(
									graph.getNode(newfrom), varnode));
							edgessofar.addAll(l);
							edgessofar.add(new DummyEdge(n, graph.getNode(e2)));
							testConsistency(newfrom, e2, edgessofar, graph,
									finder, unsatPaths);
						}
					}
				}
			}
		}
	}
}
