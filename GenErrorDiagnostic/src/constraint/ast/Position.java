package constraint.ast;

public class Position {
	int line;
	int colStart;
	int colEnd;
	
	public Position(int line, int colStart, int colEnd) {
		this.line = line;
		this.colStart = colStart;
		this.colEnd = colEnd;
	}
	
	// a workaround function that parses a position
	public Position(String s) {
		if (s.contains("line")) {
			int index = s.indexOf("line")+5;
			String[] comp = s.substring(index).split(",");
			this.line = Integer.parseInt(comp[0]);
			index = comp[1].indexOf("characters")+11;
			String range = comp[1].substring(index);
			this.colStart = Integer.parseInt(range.split("-")[0]);
			this.colEnd = Integer.parseInt(range.split("-")[1]);
		}
		else if (s.contains(",")) {
			String info;
			if (s.split(":").length > 1)
				info = s.split(":")[1];
			else
				info = s;
			String[] comp = info.split(",");
			// ignore a constraint for a segment of code
			if (comp[1].contains(","))
				return;
			this.line = Integer.parseInt(comp[0]);
			String range = comp[1];
			comp = range.split("-");
			if (comp.length == 1) {
				this.colStart = Integer.parseInt(comp[0]);
				this.colEnd = Integer.parseInt(comp[0]);
			} else {
				this.colStart = Integer.parseInt(range.split("-")[0]);
				this.colEnd = Integer.parseInt(range.split("-")[1]);
			}
		}
	}
	
	public int getLine() {
		return line;
	}
	
	public int getColStart() {
		return colStart;
	}
	
	public int getColEnd() {
		return colEnd;
	}
	
	public static void main(String[] args) {
		new Position("jif:21,11-39");
	}
}
