package util;

import graph.ConstraintPath;
import graph.Edge;
import graph.ElementNode;
import graph.EquationEdge;
import graph.Node;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import constraint.ast.Position;
import diagnostic.UnsatPaths;

public class HTTPUtil {

    public static void setShowHideActions(boolean isPath, StringBuffer sb, String loc, int id) {
		String num = isPath?"true":"false"; 
		sb.append(" onmouseover=\"show_elements("+num+", [");
		sb.append(loc+"]) ");
		if (!isPath)
			sb.append("; show_cut("+id+") ");
		sb.append("\"");
		sb.append(" onmouseout=\"hide_elements("+num+", [");
		sb.append(loc+"]) ");
		if (!isPath)
			sb.append("; hide_cut("+id+") ");
		sb.append("\"");
	}
    
    // this class is used to ease the ordering of <line, column> pair
    private class LineColumnPair implements Comparable<LineColumnPair> {
    	int line;
    	int column;
    	// the following two field are only used to break ties
    	int endline;
    	int endcol;
    	String id; 
    	
    	public LineColumnPair(int line, int column, int endline, int endcol, String id) {
    		this.line = line;
    		this.column = column;
    		this.endline = endline;
    		this.endcol = endcol;
    		this.id = id;
		}
    	
    	public int compareTo(LineColumnPair p) {
    		if (line!=p.line)
    			return new Integer(line).compareTo(p.line);
    		if (column!=p.column)
    			return new Integer(column).compareTo(p.column);
    		// order is reverse
    		if (endline!=p.endline)
    			return new Integer(p.endline).compareTo(endline);
    		if (endcol!=p.endcol)
    			return new Integer(p.endcol).compareTo(endcol);
    		return 0;
    	}
    	    	
    	@Override
    	public int hashCode() {
    		return id.hashCode();
    	}
    	
    }
    
    // find all locations involved in the unsat paths, and then wrap the corresponding code with <span> </span> notations
    public String genAnnotatedCode (UnsatPaths unsatPaths, String sourceName) {
    	StringBuffer sb = new StringBuffer();
        sb.append("<button onclick=\"hide_all()\">hide all highlights</button><br>\n");
    	sb.append("\n<pre class=\"code\" id=\"code\">\n");
    	
    	// collect all position information, and sort them
    	List<LineColumnPair> startList = new ArrayList<LineColumnPair>();
    	List<LineColumnPair> endList = new ArrayList<LineColumnPair>();
    	List<LineColumnPair> emptyList = new ArrayList<LineColumnPair>(); // the set where start=end
    	Set<Position> posSet = new HashSet<Position>();
    	for (ConstraintPath path : unsatPaths.getPaths()) {
    		Set<Node> nodes = path.getAllNodes();
    		for (Node node : nodes) {
    			posSet.add(((ElementNode)node).getElement().getPosition());
    		}
    		
    		List<Edge> edges = path.getEdges();
    		for (Edge edge : edges) {
    			if(edge instanceof EquationEdge)
    				posSet.add(((EquationEdge)edge).getEquation().getPos());
    		}
    	}
    	
    	for (Position pos : posSet) {
			if (!pos.isEmpty()) {
				if (pos.getLineStart()==pos.getLineEnd() && pos.getColStart() == pos.getColEnd())
					emptyList.add(new LineColumnPair(pos.getLineStart(), pos.getColStart(), pos.getLineEnd(), pos.getColEnd(), pos.toString()));
				else {
					startList.add(new LineColumnPair(pos.getLineStart(), pos.getColStart(), pos.getLineEnd(), pos.getColEnd(), pos.toString()));
					endList.add(new LineColumnPair(pos.getLineEnd(), pos.getColEnd(), pos.getLineEnd(), pos.getColEnd(), pos.toString()));
				}
			}
    	}
    	
    	Collections.sort(startList);
    	Collections.sort(endList);
    	Collections.sort(emptyList);
    	
    	int startIndex = 0;
    	int endIndex = 0;
    	int emptyIndex = 0;
		LineColumnPair start = startList.get(startIndex++);
		LineColumnPair end = endList.get(endIndex++);
		LineColumnPair empty;
		if (emptyList.isEmpty())
			empty = new LineColumnPair(-1, -1, -1, -1, "");
		else
			empty = emptyList.get(emptyIndex++);
		
    	// add annotations to the source
    	try {
			FileInputStream fstream = new FileInputStream(sourceName);
			BufferedReader in = new BufferedReader(new InputStreamReader(fstream, "UTF-8"));
			String current;
			int currentline = 0;
			while ((current = in.readLine()) != null) {
				currentline++;
				sb.append("<span class=lineno>" + currentline);
				sb.append(".</span>");

				if (end.line != currentline && start.line != currentline && empty.line != currentline) {
					sb.append(current);
					sb.append("\n");
					continue;
				}
				
				int col = 0;
				char[] chars = current.toCharArray();
				for ( ; col<chars.length; col++) {
					// handle end annotation first
					while (end.line==currentline && end.column==col) {
						sb.append("</span>");
						if (endIndex<endList.size())
							end = endList.get(endIndex++);
						else
							break;
					}
					// handle the annotations where start = end
					while (empty.line==currentline && empty.column==col) {
						sb.append("<span class=\"moreinfor\" id=\""+empty.id+"\">");
						sb.append("</span>");
						if (emptyIndex<emptyList.size())
							empty = emptyList.get(emptyIndex++);
						else
							break;
					}
					while (start.line==currentline && start.column==col) {
						sb.append("<span class=\"moreinfor\" id=\""+start.id+"\">");
						if (startIndex<startList.size())
							start = startList.get(startIndex++);
						else
							break;
					}
					sb.append(chars[col]);
				}
				// handle the possible end annotation after the last character
				while (end.line==currentline && end.column>=col) {
					sb.append("</span>");
					if (endIndex<endList.size())
						end = endList.get(endIndex++);
					else
						break;
				}
				sb.append("\n");
			}
			in.close();
		} catch (IOException e) {
			sb.append("Failed to read file: " + sourceName);
		}
		sb.append("</pre>\n");
		return sb.toString();
	}
}
