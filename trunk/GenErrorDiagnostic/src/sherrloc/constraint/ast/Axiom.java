package sherrloc.constraint.ast;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import sherrloc.constraint.analysis.PathFinder;
import sherrloc.graph.LeqEdge;
import sherrloc.graph.Node;

/**
 * Axioms have the form of: \forall x,y. C_1 => C_2, where C is a conjunction of
 * inequalities
 * 
 */
public class Axiom {
	List<QuantifiedVariable> qvars;		// quantified variables
	Set<Inequality> promise;
	Set<Inequality> conclusion;
	
	public Axiom(List<QuantifiedVariable> vars, Set<Inequality> ieq1, Set<Inequality> ieq2) {
		qvars = vars;
		promise = ieq1;
		conclusion = ieq2;
	}
	
	public void addQVar (QuantifiedVariable v) {
		qvars.add(v);
	}
	
	public void addPromise (Inequality ieq) {
		promise.add(ieq);
	}
	
	public void addConclusion (Inequality ieq) {
		conclusion.add(ieq);
	}
	
	@Override
	public String toString() {
		StringBuffer sb = new StringBuffer();
		if (!qvars.isEmpty()) {
			sb.append("\\");
			for (QuantifiedVariable var : qvars) {
				sb.append(var + " ");
			}
			sb.append(". ");
		}
		int count = 1;
		for (Inequality ieq : promise) {
			if (count > 1)
				sb.append(" ∧ ");
			sb.append(ieq);
			count ++;
		}
		sb.append(" => ");
		count = 1;
		for (Inequality ieq : conclusion) {
			if (count > 1)
				sb.append(" ∧ ");
			sb.append(ieq);
			count ++;
		}
		return sb.toString();
	}
	
	/**
	 * A quick test if a new LEQ edge in the constraint graph would match the axiom
	 * 
	 * @return True if edge matches at least one promise of the axiom
	 */
	public boolean mayMatch (LeqEdge edge) {
		for (Inequality ieq : promise) {
			Map<QuantifiedVariable, Element> map1 = new HashMap<QuantifiedVariable, Element>();
			Map<QuantifiedVariable, Element> map2 = new HashMap<QuantifiedVariable, Element>();
			if (ieq.e1.unifyWith(edge.getFrom().getElement(), map1) && 
					ieq.e2.unifyWith(edge.getTo().getElement(), map2))
				return true;
		}
		return false;
	}
		
	/**
	 * Return true if all inequalities in the promise are applicable to in a
	 * constraint graph. Update substitutions made in the unification when
	 * necessary.
	 * 
	 * @param finder
	 *            A saturated constraint graph where extra edges are to be
	 *            inferred from axioms
	 * @param maps
	 *            All possible substitutions so far
	 * @return True if all inequalities in the promise present in the saturated
	 *         graph. Parameter maps is updated to remove infeasible
	 *         substitutions, and make new substitutions for the axiom when
	 *         necessary
	 */
	public boolean findMatches (PathFinder finder, List<Map<QuantifiedVariable, Element>> maps) {
		Set<Node> nodes = finder.getGraph().getAllNodes();

		// try to unify one promise at one time
		for (Inequality ieq : promise) {
			List<Map<QuantifiedVariable, Element>> ret = new ArrayList<Map<QuantifiedVariable,Element>>();
			while (!maps.isEmpty()) {
				Map<QuantifiedVariable, Element> map = maps.remove(0);

				// get a pair of nodes, and see if they matches the promise
				for (Node n1 : nodes) {
					Map<QuantifiedVariable, Element> n1map = new HashMap<QuantifiedVariable, Element>();
					n1map.putAll(map);
					if (ieq.e1.unifyWith(n1.getElement(), n1map)) {
						Map<QuantifiedVariable, Element> n2map = new HashMap<QuantifiedVariable, Element>();
						n2map.putAll(n1map);
						for (Node n2 : nodes) {
							if (ieq.e2.unifyWith(n2.getElement(), n2map)) {
								if (finder.hasLeqEdge(n1, n2)) {
									ret.add(n2map);
								}
							}
						}
					}
				}
			}
			maps.addAll(ret);
		}
		return true;
	}
	
	/**
	 * Find a match of the axiom on saturated constraint graph, return a list of
	 * Instantiated inequalities on RHS
	 * 
	 * @return
	 */
	public List<Inequality> substRHS (Map<QuantifiedVariable, Element> map) {
		List<Inequality> ret = new ArrayList<Inequality>();
		for (Inequality ieq : conclusion) {
			ret.add(new Inequality(ieq.e1.subst(map), ieq.e2.subst(map), ieq.r));
		}
		return ret;
	}
}
