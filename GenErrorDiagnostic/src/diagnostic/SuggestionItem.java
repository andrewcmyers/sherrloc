package diagnostic;

public class SuggestionItem implements Comparable<SuggestionItem> {
	int rank=0;
	
	@Override
	public int compareTo(SuggestionItem o) {
		return new Integer(rank).compareTo(o.rank);
	}
}
