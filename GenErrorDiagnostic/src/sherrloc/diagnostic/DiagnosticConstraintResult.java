package sherrloc.diagnostic;

import sherrloc.diagnostic.explanation.Explanation;

import java.util.ArrayList;

public class DiagnosticConstraintResult {
    boolean succ;
    ArrayList<Explanation> suggestions;

    public DiagnosticConstraintResult(boolean succ, ArrayList<Explanation> suggestions) {
        this.succ = succ;
        this.suggestions = suggestions;
    }

    public boolean success() {
        return succ;
    }

    public ArrayList<Explanation> getSuggestions() {
        return suggestions;
    }
}
