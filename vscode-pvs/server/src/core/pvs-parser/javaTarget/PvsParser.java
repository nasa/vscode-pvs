import org.antlr.v4.runtime.*;
import java.util.*;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.tree.*;
import org.antlr.v4.runtime.misc.Interval;

/**
 * PvsParser class
 */
public class PvsParser {
    // flags used for debugging purposes
    protected boolean test = false;
    protected boolean outlineRequest = false;

    // current input file
    protected String ifname = null;
    // input file list
    protected ArrayList<String> iflist = new ArrayList<String>();

    // tree created by the parser
    protected ParserRuleContext tree = null;

    // lexer, tokens, parser, listener, handlers/listeners, walker
    protected PvsLanguageLexer lexer = null;
    protected CommonTokenStream tokens = null;
    protected PvsLanguageParser parser = null;
    protected ErrorListener errorListener = null;
    protected ErrorHandler errorHandler = null;
    protected ParseTreeWalker walker = null;
    protected PvsParserListener listener = null;

    // stats
    protected double parseTime = 0; // ms


    // public boolean outlineRequested () {
    //     return outlineRequest;
    // }
    // public String getInputFileName () {
    //     return ifname;
    // }
    /**
     * Diagnostic levels
     */
    public interface DiagnosticSeverity {
        int Error = 1;
        int Warning = 2;
        int Information = 3;
        int Hint = 4;
    }
    /**
     * antlr error listener, stores all detected syntax errors
     */
    public class ErrorListener extends BaseErrorListener {
        ArrayList<String> errors = new ArrayList<String>(); // array of JSON strings in the form { range: { start: { line: number, character: number }, stop: { line: number, character: number } }, message: string } 

        @Override
        public void syntaxError(Recognizer<?, ?> recognizer,
                                Object offendingSymbol,
                                int line, int col,
                                String message,
                                RecognitionException e) {
            String start = "{ \"line\": " + line +", \"character\": " + col + " }";
            Token sym = (Token) offendingSymbol;
            int len = sym.getStopIndex() - sym.getStartIndex(); //offendingSymbol.stop - offendingSymbol.start;
            String end = "{ \"line\": " + line + ", \"character\": " + (col + 1 + len) + "}";
            // String end = "{ line: " + line + ", character: " + len + "}";
            String range = "{ \"start\": " + start + ", \"end\": " + end + "}";
            String diag = "{ \"range\": " + range + ", \"message\": \"" + message.replaceAll("\"", "\\\\\"") + "\", \"severity\": " + DiagnosticSeverity.Error + " }";
            this.errors.add(diag);
        }

    }
    /**
     * Utility function, returns parse/typecheck errors
     */
    public ArrayList<String> getErrors () {
        return this.errorListener.errors;
    }
    /**
     * antlr error handler strategy
     */
    protected class ErrorHandler extends DefaultErrorStrategy {
        // @Override public void reportNoViableAlternative(Parser parser, NoViableAltException e) {
        //     parser.notifyErrorListeners(e.getOffendingToken(), "Syntax error", e);
        // }
    }
    
    /**
     * Utility function, loads an input file to be parsed by PvsParser
     */
    public void loadInputFile (String ifname) {
        this.ifname = ifname;
    }
    /**
     * Utility function, parses a given input file
     */
    public void parseFile (String ifname) throws java.io.IOException {
        this.ifname = ifname;
        parse();
    }
    /**
     * Utility function, parses the input file already loaded by PvsParser
     */
    public void parse () throws java.io.IOException {
        double parseStart = System.currentTimeMillis();

        CharStream input = CharStreams.fromFileName(ifname);
        // System.err.println("------INPUT START");
        // System.err.println(input.getText(new Interval(0, input.size())));
        // System.err.println("INPUT END-------");
        lexer = new PvsLanguageLexer(input);
        tokens = new CommonTokenStream(lexer);
        parser = new PvsLanguageParser(tokens);
        parser.removeErrorListeners(); // remove default error listener
        errorListener = new ErrorListener();
        errorHandler = new ErrorHandler();
        parser.addErrorListener(errorListener); // add new error listener
        parser.setErrorHandler(errorHandler);
        
        // parser.setBuildParseTree(false); // disable tree creation? This doesn't seem to have any effect on parsing speed
        tree = parser.parse();
        walker = new ParseTreeWalker();
        listener = new PvsParserListener(tokens, ifname, false);
        walker.walk(listener, tree);

        parseTime = System.currentTimeMillis() - parseStart;
    }
    /**
     * Utility function, returns a string representing a JSON object with brief stats for the parsed pvs files
     * An example is { "types": 1, "definitions": 15, "lemmas": 0 }
     */
    public String getStats () {
        return this.listener.getStats();
    }
    /**
     * Utility function, returns the total parsing time, in millis
     */
    public double getParseTime () {
        return this.parseTime;
    }
    /**
     * Utility function, returns a string representing a JSON object outlining the content of the parsed file
     * An example is:
        {
            "contextFolder": "....",
            "fileName": "pvsio_checker",
            "fileExtension": ".pvs",
            "math-objects": { "types": 1, "definitions": 15, "lemmas": 0 },
            "declarations": {
                    "types": [ { "line": 0, "character": 0, "identifier": "nat" }, { "line": 0, "character": 0, "identifier": "rat" }, { "line": 0, "character": 0, "identifier": "bool" }, { "line": 0, "character": 0, "identifier": "string" }, { "line": 9, "character": 2, "identifier": "RoundingMode" }, { "line": 0, "character": 0, "identifier": "int" } ],
                    "functions": [ { "line": 31, "character": 2, "identifier": "real2str" }, { "line": 33, "character": 2, "identifier": "to_str" } ],
                    "formulas": [  ],
                    "locals": [  ]
            },
            "parse-time": { "ms": 714.0 },
            "errors": [{ "range": { "start": { "line": 47, "character": 36 }, "end": { "line": 47, "character": 37}}, "message": "mismatched input ',' expecting K_END", "severity": 1 }]
        }
     */
    public String getOutline () {
        String outline = "{" 
            + "\n \"contextFolder\": \"" + ParserUtils.getContextFolder(ifname) + "\""
            + ",\n \"fileName\": \"" + ParserUtils.getFileName(ifname) + "\""
            + ",\n \"fileExtension\": \"" + ParserUtils.getFileExtension(ifname) + "\""
            + ",\n \"math-objects\": " + getStats()
            + ",\n \"declarations\": {"
                + "\n\t \"theories\": [ " + listener.printTheories() + " ]"
                + ",\n\t \"types\": [ " + listener.printTypes() + " ]"
                + ",\n\t \"functions\": [ " + listener.printFunctions() + " ]"
                + ",\n\t \"formulas\": [ " + listener.printFormulas() + " ]"
                + ",\n\t \"locals\": [ " + listener.printLocals() + " ]"
            + "\n}"
            + ",\n \"parse-time\": { \"ms\": " + parseTime + " }";
        if (this.errorListener.errors.size() > 0) {
            outline += ",\n \"errors\": " + getErrors();
        }
        outline += "\n}";
        return outline;
    }

    /**
     * Utility class PvsContext for storing context information necessary for typechecking
     * This is a preliminary implementation, the code needs to be completed
     */
    public class PvsContext {
        // TODO: load information from JSON file
        String[] builtinTypes = { "int", "nat", "rat", "bool", "string" };

        HashMap<String, ParserUtils.DeclDescriptor> theoryDeclarations = new HashMap<String, ParserUtils.DeclDescriptor>();
        HashMap<String, ParserUtils.DeclDescriptor> typeDeclarations = new HashMap<String, ParserUtils.DeclDescriptor>();
        HashMap<String, ParserUtils.DeclDescriptor> formulaDeclarations = new HashMap<String, ParserUtils.DeclDescriptor>();
        HashMap<String, ParserUtils.DeclDescriptor> functionDeclarations = new HashMap<String, ParserUtils.DeclDescriptor>();
        HashMap<String, ParserUtils.DeclDescriptor> localBindingDeclarations = new HashMap<String, ParserUtils.DeclDescriptor>();
        HashMap<String, ParserUtils.DeclDescriptor> constantDeclarations = new HashMap<String, ParserUtils.DeclDescriptor>();

        // current theory
        // a LIFO stack (elem in position 0 is the current theory) is used to keep track of nested theories
        ArrayList<PvsLanguageParser.TheoryContext> currentTheory = new ArrayList<PvsLanguageParser.TheoryContext>();

        /**
         * Utility function, stores information about the given built-in type
         */
        protected void addBuiltInTypes () {
            for (int i = 0; i < builtinTypes.length; i++) {
                typeDeclarations.put(builtinTypes[i], 
                    new ParserUtils.DeclDescriptor(
                        builtinTypes[i],
                        "", // FIXME: indicate the actual theory name in the prelude
                        0, // FIXME: indicate the actual line in the prelude
                        0, // FIXME: indicate the actual col in the prelude
                        "", // FIXME: indicate the actual declaration in the prelude
                        null,
                        "prelude.pvs",
                        null
                    )
                );
            }
        }
        /**
         * constructor
         */
        PvsContext (boolean include_builtin) {
            // add built-in types
            if (include_builtin) { addBuiltInTypes(); }
        }

        /**
         * Utility function, returns the name of the current theory
         */
        String getCurrentTheoryName () {
            if (currentTheory.size() > 0) {
                PvsLanguageParser.TheoryContext ctx = currentTheory.get(0);
                Token start = ctx.getStart();
                Token stop = ctx.getStop();
                String id = ctx.theoryBegin().identifier().getText();
                return id;
            }
            return "";
        }

        /**
        * utility functions to get information from the current pvs context
        */
        ParserUtils.DeclDescriptor getTerm (String id) {
            if (constantDeclarations != null) {
                ParserUtils.DeclDescriptor desc = constantDeclarations.get(id);
                if (desc != null) { return desc; }
            }
            if (functionDeclarations != null) {
                ParserUtils.DeclDescriptor desc = functionDeclarations.get(id);
                if (desc != null) { return desc; }
            }
            return null;
        }
        public ParserUtils.DeclDescriptor getFormula (String id) {
            if (formulaDeclarations != null) {
                return formulaDeclarations.get(id);
            }
            return null;
        }
        public ParserUtils.DeclDescriptor getType (String id) {
            if (typeDeclarations != null) {
                return typeDeclarations.get(id);
            }
            return null;
        }
    }

    /**
     * Utility class PvsParserListener for storing the outline of the parsed pvs file
     */
    public class PvsParserListener extends PvsLanguageBaseListener {
        protected BufferedTokenStream tokens = null;
        protected TokenStreamRewriter rewriter = null;
        protected String ifname = null;

        // stats
        protected int nTypes = 0;
        protected int nDefinitions = 0;
        protected int nFormulas = 0;
        
        // pvs context
        PvsContext context;

        /**
         * Constructor
         */
        PvsParserListener (BufferedTokenStream tokens, String ifname, boolean include_builtin) {
            super();
            this.tokens = tokens;
            rewriter = new TokenStreamRewriter(tokens);
            this.ifname = ifname;
            this.context = new PvsContext(include_builtin);
        }

        /**
         * Utility function, prints the content of a hashmap table in JSON format
         */
        String printHashMap (HashMap<String, ParserUtils.DeclDescriptor> hs) {
            String ans = "";
            Set<String> elems = hs.keySet();
            Iterator<String> it = elems.iterator();
            while (it.hasNext()) {
                String key = it.next();
                ans += hs.get(key).outline();
                if (it.hasNext()) {
                    ans += ", ";
                }
            }
            return ans;
        }

        /**
         * Utility function, prints the name of the parsed theories in JSON format
         */
        String printTheories () { return this.printHashMap(this.context.theoryDeclarations); }
        /**
         * Utility function, prints the name of the parsed types in JSON format
         */
        String printTypes () { return this.printHashMap(this.context.typeDeclarations); }
        /**
         * Utility function, prints the name of the parsed formulas in JSON format
         */
        String printFormulas () { return this.printHashMap(this.context.formulaDeclarations); }
        /**
         * Utility function, prints the name of the parsed functions in JSON format
         */
        String printFunctions () { return this.printHashMap(this.context.functionDeclarations); }
        /**
         * Utility function, prints the name of the parsed local bindings in JSON format
         */
        String printLocals () { return this.printHashMap(this.context.localBindingDeclarations); }
        /**
         * Utility function, finds a declaration in the parsed file
         */
        ParserUtils.DeclDescriptor findDeclaration (String name) {//, int line, int character) {
            System.out.println("Finding declaration for " + name);
            ParserUtils.DeclDescriptor candidate = context.typeDeclarations.get(name);
            if (candidate != null) {
                return candidate;
            }
            candidate = context.formulaDeclarations.get(name);
            if (candidate != null) {
                return candidate;
            }
            candidate = context.functionDeclarations.get(name);
            if (candidate != null) {
                return candidate;
            }
            candidate = context.localBindingDeclarations.get(name);
            if (candidate != null) {
                return candidate;
            }
            return null;
        }
        /**
         * Utility function, stores information about a type declaration
         */
        void saveTypeDeclaration (PvsLanguageParser.TypeDeclarationContext ctx, PvsLanguageParser.TypeNameContext ictx) {
            Token start = ictx.getStart();
            Token stop = ictx.getStop();
            String id = ictx.getText();
            ParserUtils.Range scope = ParserUtils.findScope(id, ictx);
            this.context.typeDeclarations.put(id, 
                new ParserUtils.DeclDescriptor(
                    id,
                    context.getCurrentTheoryName(),
                    start.getLine(),
                    start.getCharPositionInLine(),
                    ParserUtils.getSource(ctx),
                    scope,
                    this.ifname,
                    ctx
                )
            );
        }
        /**
         * Utility function, stores information about a formula declaration
         */
        void saveFormulaDeclaration (PvsLanguageParser.FormulaDeclarationContext ctx) {
            Token start = ctx.getStart();
            Token stop = ctx.getStop();
            String id = ctx.identifier().getText();
            ParserUtils.Range scope = ParserUtils.findScope(id, ctx);
            this.context.formulaDeclarations.put(id, 
                new ParserUtils.DeclDescriptor(
                    id,
                    context.getCurrentTheoryName(),
                    start.getLine(), 
                    start.getCharPositionInLine(),
                    ParserUtils.getSource(ctx),
                    scope,
                    this.ifname,
                    ctx
                )
            );
        }
        /**
         * Utility function, stores information about a constant declaration
         */
        void saveConstantDeclaration (PvsLanguageParser.ConstantDeclarationContext ctx, PvsLanguageParser.ConstantNameContext ictx) {
            Token start = ictx.getStart();
            Token stop = ictx.getStop();
            String id = ictx.identifierOrOperator().getText();
            ParserUtils.Range scope = ParserUtils.findScope(id, ictx);
            this.context.constantDeclarations.put(id, 
                new ParserUtils.DeclDescriptor(
                    id,
                    context.getCurrentTheoryName(),
                    start.getLine(), 
                    start.getCharPositionInLine(),
                    ParserUtils.getSource(ictx),
                    scope,
                    this.ifname,
                    ctx
                )
            );
        }
        /**
         * Utility function, stores information about a function declaration
         */
        void saveFunctionDeclaration (PvsLanguageParser.FunctionDeclarationContext ctx) {
            Token start = ctx.getStart();
            Token stop = ctx.getStop();
            String id = ctx.functionName().getText();
            ParserUtils.Range scope = ParserUtils.findScope(id, ctx);
            this.context.functionDeclarations.put(id, 
                new ParserUtils.DeclDescriptor(
                    id,
                    context.getCurrentTheoryName(),
                    start.getLine(), 
                    start.getCharPositionInLine(),
                    ParserUtils.getSource(ctx),
                    scope,
                    this.ifname,
                    ctx
                )
            );
        }
        /**
         * Utility function, stores information about a theory declaration
         */
        void saveTheoryDeclaration (PvsLanguageParser.TheoryContext ctx) {
            Token start = ctx.getStart();
            Token stop = ctx.getStop();
            String id = ctx.theoryBegin().identifier().getText();
            ParserUtils.Range scope = ParserUtils.findScope(id, ctx);
            this.context.theoryDeclarations.put(id, 
                new ParserUtils.DeclDescriptor(
                    id,
                    id,
                    start.getLine(), 
                    start.getCharPositionInLine(),
                    ParserUtils.getSource(ctx),
                    scope,
                    this.ifname,
                    ctx
                )
            );
            // update current theory, the most recent theory is in position 0
            this.context.currentTheory.add(0,ctx);
        }
        /**
         * Utility function, stores information about a local binding
         */
        void saveLocalBinding (PvsLanguageParser.TypeIdContext ctx) {
            Token start = ctx.getStart();
            Token stop = ctx.getStop();
            String id = ctx.localName().getText();
            ParserUtils.Range scope = ParserUtils.findScope(id, ctx);
            this.context.localBindingDeclarations.put(id, 
                new ParserUtils.DeclDescriptor(
                    id,
                    context.getCurrentTheoryName(),
                    start.getLine(), 
                    start.getCharPositionInLine(),
                    ParserUtils.getSource(ctx),
                    scope,
                    this.ifname,
                    ctx
                )
            );
        }
        /**
         * antlr handlers
         */
        @Override public void enterTheory(PvsLanguageParser.TheoryContext ctx) {
            saveTheoryDeclaration(ctx);
        }
        @Override public void exitTheory(PvsLanguageParser.TheoryContext ctx) {
            this.context.currentTheory.remove(0);
            if (test) {
                if (context.typeDeclarations != null) {
                    int n = context.typeDeclarations.size();
                    System.out.println("------------------------------");
                    System.out.println(n + " type declarations");
                    System.out.println("------------------------------");
                    for (String id: context.typeDeclarations.keySet()){
                        System.out.println(id + " " + context.typeDeclarations.get(id));
                    }
                }
                if (context.formulaDeclarations != null) {
                    int n = context.formulaDeclarations.size();
                    System.out.println("------------------------------");
                    System.out.println(n + " formula declarations");
                    System.out.println("------------------------------");
                    for (String id: context.formulaDeclarations.keySet()){
                        System.out.println(id + " " + context.formulaDeclarations.get(id));
                    }
                }
                if (context.functionDeclarations != null) {
                    int n = context.functionDeclarations.size();
                    System.out.println("------------------------------");
                    System.out.println(n + " function declarations");
                    System.out.println("------------------------------");
                    for (String id: context.functionDeclarations.keySet()){
                        System.out.println(id + " " + context.functionDeclarations.get(id));
                    }
                }
                if (context.localBindingDeclarations != null) {
                    int n = context.localBindingDeclarations.size();
                    System.out.println("------------------------------");
                    System.out.println(n + " local bindings");
                    System.out.println("------------------------------");
                    for (String id: context.localBindingDeclarations.keySet()){
                        System.out.println(id + " " + context.localBindingDeclarations.get(id));
                    }
                }
            }
        }
        @Override public void enterTypeDeclaration(PvsLanguageParser.TypeDeclarationContext ctx) {
            ListIterator<PvsLanguageParser.TypeNameContext> it = ctx.typeName().listIterator();
            while (it.hasNext()) {
                nTypes++;
                PvsLanguageParser.TypeNameContext ictx = it.next();
                saveTypeDeclaration(ctx, ictx); // this will overwrite any previous type decl with the same name -- do we want this?
            }
        }
        @Override public void enterFormulaDeclaration(PvsLanguageParser.FormulaDeclarationContext ctx) {
            nFormulas++;
            saveFormulaDeclaration(ctx); // this will overwrite any previous formula with the same name -- do we want this?
        }
        @Override public void enterVarDeclaration(PvsLanguageParser.VarDeclarationContext ctx) {
            // nDefinitions++;
        }
        @Override public void enterConstantDeclaration(PvsLanguageParser.ConstantDeclarationContext ctx) {
            ListIterator<PvsLanguageParser.ConstantNameContext> it = ctx.constantName().listIterator();
            while (it.hasNext()) {
                nDefinitions++;
                PvsLanguageParser.ConstantNameContext ictx = it.next();
                saveConstantDeclaration(ctx, ictx);
            }
        }
        @Override public void enterFunctionDeclaration(PvsLanguageParser.FunctionDeclarationContext ctx) {
            nDefinitions++;
            saveFunctionDeclaration(ctx);
        }
        @Override public void enterAssumption(PvsLanguageParser.AssumptionContext ctx) {
            nFormulas++;
        }
        @Override public void enterJudgementDeclaration(PvsLanguageParser.JudgementDeclarationContext ctx) {
            nFormulas++;
        }
        @Override public void enterConversionDeclaration(PvsLanguageParser.ConversionDeclarationContext ctx) {
            nDefinitions++;
        }
        @Override public void enterAutorewriteDeclaration(PvsLanguageParser.AutorewriteDeclarationContext ctx) {
            nDefinitions++;
        }
        @Override public void enterTypeId(PvsLanguageParser.TypeIdContext ctx) {
            saveLocalBinding(ctx);
        }
        /**
         * Utility function, prints brief stats for the parsed file
         */
        String getStats () {
            return "{"
                    + " \"types\": " + this.nTypes + ","
                    + " \"definitions\": " + this.nDefinitions + ","
                    + " \"lemmas\": " + this.nFormulas
                    + " }";
        }
    }

    /**
     * Utility function, parses command line arguments
     */
    protected void parseCliArgs (String[] args) {
        // System.out.println(args.toString());
        for (int a = 0; a < args.length; a++) {
            if (args[a].equals("--test") || args[a].equals("-test")) {
                test = true;
            }
            // else if (args[a].equals("--outline") || args[a].equals("-outline") || args[a].equals("-decls")) {
            //     outlineRequest = true;
            // } 
            else {
                ifname = args[a];
                iflist.add(args[a]);
            }
        }
    }

    /**
     * command-line entry point
     */
    public static void main(String[] args) throws Exception {
        // open file
        if (args != null && args.length > 0) {
            PvsParser parser = new PvsParser();
            parser.parseCliArgs(args);
            int nFiles = parser.iflist.size();
            if (nFiles > 0) {
                System.err.println("Parsing " + nFiles + " files");
                System.out.println("[");
                for (int i = 0; i < nFiles; i++) {
                    String fname = parser.iflist.get(i);
                    if (parser.test) { System.err.println("Parsing file " + parser.ifname); }
                    parser.parseFile(fname);
                    System.out.println(parser.getOutline());
                    if (i < nFiles - 1) {
                        System.out.println(",");
                    }
                }
                System.out.println("]");
                System.err.println("Done with parsing " + nFiles + " files!");
            } else {
                System.out.println("Please specify file name to be parsed");
            }
        }
    }
}