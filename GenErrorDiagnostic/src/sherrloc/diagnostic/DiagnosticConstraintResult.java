package sherrloc.diagnostic;

import sherrloc.diagnostic.explanation.Explanation;

import java.util.ArrayList;

public class DiagnosticConstraintResult {
    public boolean succ;
    public ArrayList<Explanation> suggestions;

    public DiagnosticConstraintResult(boolean succ, ArrayList<Explanation> suggestions) {
        this.succ = succ;
        this.suggestions = suggestions;
    }
}
