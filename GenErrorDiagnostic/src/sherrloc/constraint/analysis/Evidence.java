package sherrloc.constraint.analysis;

import sherrloc.graph.EdgeCondition;
import sherrloc.graph.HypothesisSet;
import sherrloc.graph.Node;

/**
 * One step of a derivation: an edge of type {@code ty} from {@code start} to {@code end}. If
 * {@code hypotheses} is non-null, the step is the derivation of that edge with those hypotheses;
 * otherwise it is the shortest derivation of that edge.
 */
public class Evidence {

    public Node start, end;
    public EdgeCondition ty;
    public HypothesisSet hypotheses;

    public Evidence(Node start, Node end, EdgeCondition type) {
        this(start, end, type, null);
    }

    public Evidence(Node start, Node end, EdgeCondition type, HypothesisSet hypotheses) {
        this.start = start;
        this.end = end;
        this.ty = type;
        this.hypotheses = hypotheses;
    }
}
