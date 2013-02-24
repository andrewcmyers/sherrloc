package constraint.graph;

import constraint.ast.Environment;

/* this edge is inferred only when all components of an Constructor element flow into another */
public class CompEdge extends Edge {
		String info;
		Environment env;
	
		public CompEdge(Node from, Node to, Environment env, String info) {
			super(from, to);
			this.info = info;
			this.env = env;
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
		public void incSuccCounter() {
			// do nothing
		}
		
		@Override
		public Environment getAssumption() {
			return env;
		}
		
		@Override
		public int getLength() {
			return 1;
		}

}
