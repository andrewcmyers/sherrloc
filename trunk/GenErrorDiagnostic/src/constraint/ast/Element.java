package constraint.ast;

import java.util.List;

public abstract class Element {
	String name;
	String info;
	
	public Element(String name, String info) {
		this.name = name;
		this.info = info;
	}
	
	public String toString () {
		return name+pos();
	}
	
	public String getName () {
		return name;
	}
		
	public String pos () {
		return info.equals("")?"":(" @"+info);
	}
	
	abstract public String toDotString ();
	
	abstract public List<Variable> getVars ();
	
	abstract public boolean hasVars ();
	
	abstract public boolean equals (Object o);
		
	abstract public boolean isStart();
	
	abstract public boolean isEnd();
	
}
