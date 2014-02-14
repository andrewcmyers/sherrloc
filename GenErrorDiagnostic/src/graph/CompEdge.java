package graph;

import java.util.HashSet;
import java.util.Set;

import constraint.ast.Inequality;

/* this edge is inferred only when all components of an Constructor element flow into another */
public class CompEdge extends Edge {
		String info;
	
		public CompEdge(Node from, Node to, String info) {
			super(from, to);
			this.info = info;
		}
		
		@Override
		public String toString() {
			return "comp("+info+")";
		}
		
		@Override
		public String toDotString() {
			return "comp("+info+")";
		}
		
		@Override
		public boolean isDirected() {
			return true;
		}
		
		@Override
		public Set<Inequality> getInequalities() {
			return new HashSet<Inequality>();
		}
		
		@Override
		public int getLength() {
			return 1;
		}

		@Override
		public Edge getReverse() {
			return new CompEdge(to, from, info);
		}
}
