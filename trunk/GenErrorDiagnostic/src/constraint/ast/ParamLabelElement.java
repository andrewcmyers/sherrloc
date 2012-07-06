package constraint.ast;

import java.util.List;

public class ParamLabelElement extends Constructor {
	
	public ParamLabelElement(String name) {
		super(name, 0);
	}
	
	@Override
	public boolean equals(Object o) {
		return this==o;
	}
	
	@Override
	public List<Variable> getVars() {
		return null;
	}
	
	@Override
	public boolean hasVars() {
		return false;
	}
	
	@Override
	public boolean isStart() {
		return true;
	}
	
	@Override
	public boolean isEnd() {
		return true;
	}
	
	@Override
	public boolean leq_(Object o) {
		// only leq if equal to this parameter, which is checked in the env
		return false;
	}

}
