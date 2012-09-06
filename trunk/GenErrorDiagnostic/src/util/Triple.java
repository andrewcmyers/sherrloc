package util;

public class Triple <E1, E2, E3> {

	private final E1 e1;
	private final E2 e2;
	private final E3 e3;

	public Triple (E1 e1, E2 e2, E3 e3) {
		this.e1 = e1;
		this.e2 = e2;
		this.e3 = e3;
	}

	public E1 getFirst() {
		return e1;
	}

	public E2 getSecond() {
		return e2;
	}
	
	public E3 getThird() {
		return e3;
	}
	
	@Override
	public boolean equals(Object obj) {
		if (obj instanceof Triple) {
			return ((Triple) obj).e1.equals(e1) && ((Triple) obj).e2.equals(e2) && ((Triple) obj).e3.equals(e3);
		}
		return false;
	}
	
	@Override
	public int hashCode() {
		return e1.hashCode() + e2.hashCode() + e3.hashCode();
	}
}
