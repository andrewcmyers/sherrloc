package constraint.ast;

public class Position {
	int lineStart;
	int colStart;
	int lineEnd;
	int colEnd;
	String snippet;
	String file;
	private static Position emptyPosition=null;
	
	public Position(String snippet, String file, int lStart, int colStart, int lEnd, int colEnd) {
		this.snippet = snippet;
		this.file = file;
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
			emptyPosition = new Position("", "", -1, -1, -1, -1);
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
	
	public String getFile() {
		return file;
	}
	
	public void setFile(String file) {
		this.file = file;
	}
	
	public boolean sameline () {
		return lineStart==lineEnd;
	}
	
	public boolean isEmpty () {
		return lineStart==-1 || snippet.equals("");
	}
	
	@Override
	public String toString() {
		if (lineStart==-1)
			return "";
		
		String ret="";
		if (!file.equals(""))
			ret += file+":";
		
		if (sameline())
			return ret + lineStart+","+colStart+"-"+colEnd;
		else
			return ret + lineStart+","+colStart+"-"+lineEnd+","+colEnd;
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
