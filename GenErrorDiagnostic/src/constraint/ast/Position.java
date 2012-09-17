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
		new Position("line 4, characters 4-5");
	}
}
