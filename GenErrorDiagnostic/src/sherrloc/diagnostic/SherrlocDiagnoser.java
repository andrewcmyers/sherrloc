package sherrloc.diagnostic;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;
import sherrloc.constraint.analysis.ConstraintAnalysis;
import sherrloc.constraint.analysis.ConstraintAnalysisImpl;
import sherrloc.constraint.ast.Axiom;
import sherrloc.constraint.ast.Bottom;
import sherrloc.constraint.ast.Constraint;
import sherrloc.constraint.ast.Constructor;
import sherrloc.constraint.ast.ConstructorApplication;
import sherrloc.constraint.ast.Element;
import sherrloc.constraint.ast.Function;
import sherrloc.constraint.ast.FunctionApplication;
import sherrloc.constraint.ast.Hypothesis;
import sherrloc.constraint.ast.Inequality;
import sherrloc.constraint.ast.JoinElement;
import sherrloc.constraint.ast.MeetElement;
import sherrloc.constraint.ast.Position;
import sherrloc.constraint.ast.QuantifiedVariable;
import sherrloc.constraint.ast.Relation;
import sherrloc.constraint.ast.Top;
import sherrloc.constraint.ast.Variable;
import sherrloc.constraint.ast.VariableApplication;
import sherrloc.constraint.parse.ElementParser;
import sherrloc.diagnostic.DiagnosticOptions.Mode;
import sherrloc.graph.ConstraintGraph;
import sherrloc.graph.Variance;

public class SherrlocDiagnoser {

    private final Mode mode;
    private final ConstraintAnalysis constraintAnalysis;
    private boolean tryReducing;

    private final Hypothesis env = new Hypothesis();
    private final Set<Constraint> constraints = new HashSet<>();
    private final List<Axiom> axioms = new ArrayList<>();

    private final Map<String, Constructor> constructors = new HashMap<>();
    private final Map<String, Function> functions = new HashMap<>();
    private final Map<String, Variable> variables = new HashMap<>();
    private final Map<String, QuantifiedVariable> qvars = new HashMap<>();

    private boolean constructorsDeclared = false;

    public SherrlocDiagnoser(Mode mode, boolean tryReducing, boolean isRecursive) {
        this.mode = mode;
        constraintAnalysis = new ConstraintAnalysisImpl(mode == Mode.HYPO, false,
                isRecursive);
        this.tryReducing = tryReducing;

        constructors.put("arrow", new Constructor("arrow", 2, 0, Variance.POS, Position.EmptyPosition()));
        constructors.put("larrow", new Constructor("larrow", 2, 0, Variance.NEG, Position.EmptyPosition()));
        constructors.put("pair",  new Constructor("pair", 2, 0, Variance.POS, Position.EmptyPosition()));
        constructors.put("_",  new Bottom(Position.EmptyPosition()));
        constructors.put("*",  new Top(Position.EmptyPosition()));
    }

    public void defineConstructor(String id, int arity) {
        defineConstructor(id, arity, 0);
    }

    public void defineConstructor(String id, int arity, int level) {
        defineConstructor(id, arity, level, Variance.POS);
    }

    public void defineConstructor(String id, int arity, int level, Variance variance) {
        constructors.put(id, new Constructor(id, arity, level, variance, Position.EmptyPosition()));
        constructorsDeclared = true;
    }

    public void defineFunction(String id, int arity) {
        functions.put(id, new Function(id, arity, Position.EmptyPosition()));
    }

    public void defineExplicitVariable(String id) {
        defineExplicitVariable(id, 0);
    }

    public void defineExplicitVariable(String id, int level) {
        variables.put(id, new Variable(id, level));
    }

    public void addAxiom(List<String> qv, Set<Inequality> conclusion) {
        addAxiom(qv, new HashSet<>(), conclusion);
    }

    public void addAxiom(Set<Inequality> premise, Set<Inequality> conclusion) {
        addAxiom(new ArrayList<>(), premise, conclusion);
    }

    public void addAxiom(List<String> vars, Set<Inequality> premise, Set<Inequality> conclusion) {
        List<QuantifiedVariable> qvarsList = new ArrayList<>();
        for (String s : vars) {
            QuantifiedVariable qv = new QuantifiedVariable(s);
            qvars.put(s, qv);
            qvarsList.add(qv);
        }
        axioms.add(new Axiom(qvarsList, premise, conclusion));
        qvars.clear();

    }

    public void addAssumedInequality(Inequality ieq) {
        env.addInequality(ieq);
    }

    public void addConstraint(Inequality ieq, Position pos) {
        constraints.add(new Constraint(ieq, new Hypothesis(), pos));
    }

    public void addConstraint(Inequality ieq, Set<Inequality> hypotheses, Position pos) {
        Hypothesis h = new Hypothesis();
        for (Inequality hyp : hypotheses) {
            h.addInequality(hyp);
        }
        constraints.add(new Constraint(ieq, h, pos));
    }

    public Inequality createEqualityConstraint(Element e, Element o) {
        return new Inequality(e, o, Relation.EQ);
    }

    public Inequality createLessThanConstraint(Element e, Element o) {
        return new Inequality(e, o, Relation.LEQ);
    }

    public Inequality createGreaterThanConstraint(Element e, Element o) {
        return new Inequality(o, e, Relation.LEQ);
    }

    public Position createPosition(String snippet, String file, int weight, int lineStart, int colStart, int lineEnd, int colEnd) {
        Position pos = createPosition(snippet, file, lineStart, colStart, lineEnd, colEnd);
        pos.setWeight(weight);
        return pos;
    }

    public Position createPosition(String snippet, String file, int lineStart, int colStart, int lineEnd, int colEnd) {
        return new Position(snippet, file, lineStart, colStart, lineEnd, colEnd);
    }

    public Element createElement(String id, Position pos) {
        Element e;
        if (qvars.containsKey(id)) {
            e = qvars.get(id);
        } else if (constructors.containsKey(id)) {
            e = constructors.get(id).clone();
        }
        else if (functions.containsKey(id)) {
            e = functions.get(id).clone();
        }
        else if (!constructorsDeclared && !(variables.containsKey(id))) {
            e = new Constructor(id, 0, 0, Variance.POS, Position.EmptyPosition());
            constructors.put(id, (Constructor) e);
        }
        else if (variables.containsKey(id)) {
            e = variables.get(id);
        }
        else {
            e = ElementParser.parse(id);
            if (e instanceof Variable v) {
                variables.put(id, v);
            }
        }
        e.setPosition(pos);
        return e;
    }

    public Element createArrowElement(Element e1, Element e2) {
        return new ConstructorApplication(constructors.get("arrow"), List.of(e1, e2));
    }

    public Element createLArrowElement(Element e1, Element e2) {
        return new ConstructorApplication(constructors.get("larrow"), List.of(e1, e2));
    }

    public Element createPairElement(Element e1, Element e2) {
        return new ConstructorApplication(constructors.get("pair"), List.of(e1, e2));
    }

    public Element createJoinListElement(List<Element> es) {
        return new JoinElement(es);
    }

    public Element createMeetListElement(List<Element> es) {
        return new MeetElement(es);
    }

    public Element createConstructorApplication(String id, List<Element> es) {
        Constructor c = constructors.get(id);
        if (!constructorsDeclared && constructors.get(c.getName()).getArity() == 0) {
            c.setArity(es.size());
            constructors.get(c.getName()).setArity(es.size());
        } if (c.getArity() < es.size()) {
            c.setArity(es.size());
        }
        return new ConstructorApplication(c, es);
    }

    public Element createFunctionApplication(String id, List<Element> es) {
        Function f = functions.get(id);
        if (!constructorsDeclared && functions.get(f.getName()).getArity() == 0) {
            f.setArity(es.size());
            functions.get(f.getName()).setArity(es.size());
        } if (f.getArity() < es.size()) {
            f.setArity(es.size());
        }
        return new FunctionApplication(f, es);
    }

    public Element createVariableApplication(String id, List<Element> es) {
        Variable v = variables.get(id);
        return new VariableApplication(v, es);
    }

    public DiagnosticConstraintResult getConstraintResult() {
        env.addAxioms(axioms);
        ConstraintGraph graph = new ConstraintGraph(env, constraints, axioms);
        graph.generateGraph();

        if (tryReducing) {
            graph.reduce();
            if (constraintAnalysis.genErrorPaths(graph).size() == 0) {
                return new DiagnosticConstraintResult(true, new ArrayList<>()); // TODO: return inferred types
            } else {
                graph = new ConstraintGraph(env, constraints, axioms);
                graph.generateGraph();
            }
        }

        ErrorDiagnosis errorDiagnosis = ErrorDiagnosis.getAnalysisInstance(graph, mode);
        DiagnosticConstraintResult result = errorDiagnosis.getConstraintResult();
        return result;
    }
}
