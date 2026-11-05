package sherrloc.graph;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import sherrloc.constraint.ast.Inequality;

/**
 * An immutable set of hypotheses: the inequalities assumed by a path in a constraint graph. A
 * path's hypotheses are the union of the hypotheses of its edges, and every one of them is in force
 * along the path. Inequalities are compared without regard to source positions.
 */
public final class HypothesisSet {

    /** The empty set of hypotheses. */
    public static final HypothesisSet EMPTY = new HypothesisSet(Collections.emptySet());

    private final Set<Inequality> inequalities;

    private HypothesisSet(Set<Inequality> inequalities) {
        this.inequalities = inequalities;
    }

    /** The hypotheses {@code ieqs}, or the empty set if {@code ieqs} is null. */
    public static HypothesisSet of(Set<Inequality> ieqs) {
        if (ieqs == null || ieqs.isEmpty()) {
            return EMPTY;
        }
        Set<Inequality> base = new HashSet<>();
        for (Inequality ieq : ieqs) {
            base.add(ieq.baseInequality());
        }
        return new HypothesisSet(Collections.unmodifiableSet(base));
    }

    /** The hypotheses in either this set or {@code other}. */
    public HypothesisSet union(HypothesisSet other) {
        if (other.inequalities.isEmpty() || other == this) {
            return this;
        }
        if (inequalities.isEmpty()) {
            return other;
        }
        Set<Inequality> u = new HashSet<>(inequalities);
        u.addAll(other.inequalities);
        return new HypothesisSet(Collections.unmodifiableSet(u));
    }

    /** Whether every hypothesis in this set is in {@code other}. */
    public boolean isSubsetOf(HypothesisSet other) {
        return other.inequalities.containsAll(inequalities);
    }

    /** The number of hypotheses in this set. */
    public int size() {
        return inequalities.size();
    }

    @Override
    public boolean equals(Object o) {
        return o instanceof HypothesisSet h && inequalities.equals(h.inequalities);
    }

    @Override
    public int hashCode() {
        return inequalities.hashCode();
    }

    @Override
    public String toString() {
        return inequalities.toString();
    }
}
