package constraint.ast;

import java.util.HashSet;


public class Position {
	int lineStart;
	int colStart;
	int lineEnd;
	int colEnd;
	String fileName;
	private static Position emptyPosition=null;
	
	public Position(String name, int lStart, int colStart, int lEnd, int colEnd) {
		this.fileName = name;
		this.lineStart = lStart;
		this.lineEnd = lEnd;
		this.colStart = colStart;
		this.colEnd = colEnd;
	}
	
	// a workaround function that parses a position
//	public Position(String s) {
//		if (s.contains("line")) {
//			int index = s.indexOf("line")+5;
//			String[] comp = s.substring(index).split(",");
//			this.lineStart = this.lineEnd = Integer.parseInt(comp[0]);
//			index = comp[1].indexOf("characters")+11;
//			String range = comp[1].substring(index);
//			this.colStart = Integer.parseInt(range.split("-")[0]);
//			this.colEnd = Integer.parseInt(range.split("-")[1]);
//		}
//		else if (s.contains(",")) {
//			String info;
//			if (s.split(":").length > 1)
//				info = s.split(":")[1];
//			else
//				info = s;
//			String[] comp = info.split(",");
//			// ignore a constraint for a segment of code
//			if (comp.length>2)
//				return;
//			this.lineStart = this.lineEnd = Integer.parseInt(comp[0]);
//			String range = comp[1];
//			comp = range.split("-");
//			if (comp.length == 1) {
//				this.colStart = Integer.parseInt(comp[0]);
//				this.colEnd = Integer.parseInt(comp[0]);
//			} else {
//				this.colStart = Integer.parseInt(range.split("-")[0]);
//				this.colEnd = Integer.parseInt(range.split("-")[1]);
//			}
//		}
//	}
	
	public static Position EmptyPosition () {
		if (emptyPosition==null)
			emptyPosition = new Position("", -1, -1, -1, -1);
		return emptyPosition;
	}
	
	public int getLineStart() {
		return lineStart;
	}
	
	public int getLineEnd() {
		return lineEnd;
	}
	
	public int getColStart() {
		return colStart;
	}
	
	public int getColEnd() {
		return colEnd;
	}
	
	public String getFileName () {
		return fileName;
	}
	
	public void setFileName(String fileName) {
		this.fileName = fileName;
	}
	
	public boolean sameline () {
		return lineStart==lineEnd;
	}
	
	public boolean isEmpty () {
		return lineStart==-1;
	}
	
	@Override
	public String toString() {
		if (isEmpty())
			return "";
		
		if (sameline())
			return lineStart+","+colStart+"-"+colEnd;
		else
			return lineStart+","+colStart+"-"+lineEnd+","+colEnd;
	}
	
	
	@Override
	public boolean equals(Object obj) {
		if (obj instanceof Position) {
			return toString().equals(obj.toString());
		}
		return false;
	}
	
	@Override
	public int hashCode() {
		return toString().hashCode();
	}

}
