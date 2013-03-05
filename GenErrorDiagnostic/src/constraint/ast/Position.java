package constraint.ast;



public class Position {
	int lineStart;
	int colStart;
	int lineEnd;
	int colEnd;
	String snippet="";
	private static Position emptyPosition=null;
	
	public Position(String snippet, int lStart, int colStart, int lEnd, int colEnd) {
		this.snippet = snippet;
		this.lineStart = lStart;
		this.lineEnd = lEnd;
		if (colStart<colEnd) {
			this.colStart = colStart;
			this.colEnd = colEnd;
		}
		else {
			this.colEnd = colStart;
			this.colStart = colEnd;
		}
	}
	
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
	
	public String getSnippet () {
		return snippet;
	}
	
	public void setSnippet(String snippet) {
		this.snippet = snippet;
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
