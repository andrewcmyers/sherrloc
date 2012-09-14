package constraint.graph;

import constraint.ast.Environment;

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
		public void setCause() {
			// do nothing
		}
		
		@Override
		public Environment getAssumption() {
			return new Environment();
		}

}
