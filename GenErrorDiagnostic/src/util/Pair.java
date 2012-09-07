package util;

public class Pair<L, R> {

	private final L left;
	private final R right;

	public Pair(L left, R right) {
		this.left = left;
		this.right = right;
	}

	public L getLeft() {
		return left;
	}

	public R getRight() {
		return right;
	}
	
	public boolean equals(Object obj) {
		if (obj instanceof Pair)
			return ((Pair) obj).left.equals(left) && ((Pair) obj).right.equals(right);
		else
			return false;
	}
	
	@Override
	public int hashCode() {
		return left.hashCode() + right.hashCode();
	}
}
