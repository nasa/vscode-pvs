import org.antlr.v4.runtime.*;
import java.util.*;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.tree.*;
import org.antlr.v4.runtime.misc.Interval;

public class PvsParser {
    protected boolean test = false;
    protected String ifname = null;
    protected boolean outlineRequest = false;

    protected ParserRuleContext tree = null;

    protected PvsLanguageLexer lexer = null;
    protected CommonTokenStream tokens = null;
    protected PvsLanguageParser parser = null;
    protected ErrorListener errorListener = null;
    protected ErrorHandler errorHandler = null;
    protected ParseTreeWalker walker = null;
    protected PvsParserListener listener = null;
    protected double parseTime = 0; // ms


    // public boolean outlineRequested () {
    //     return outlineRequest;
    // }
    // public String getInputFileName () {
    //     return ifname;
    // }
    public interface DiagnosticSeverity {
        int Error = 1;
        int Warning = 2;
        int Information = 3;
        int Hint = 4;
    }
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
    public ArrayList<String> getErrors () {
        return this.errorListener.errors;
    }
    protected class ErrorHandler extends DefaultErrorStrategy {
        // @Override public void reportNoViableAlternative(Parser parser, NoViableAltException e) {
        //     parser.notifyErrorListeners(e.getOffendingToken(), "Syntax error", e);
        // }
    }
    
    /**
     * parse a given input file
     */
    public void parseFile (String ifname) throws java.io.IOException {
        this.ifname = ifname;
        parse();
    }
    public void parse () throws java.io.IOException {
        double parseStart = System.currentTimeMillis();

        CharStream input = CharStreams.fromFileName(ifname);
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
        listener = new PvsParserListener(tokens, ifname);
        walker.walk(listener, tree);

        parseTime = System.currentTimeMillis() - parseStart;
    }

    public String getStats () {
        return this.listener.getStats();
    }

    public double getParseTime () {
        return this.parseTime;
    }

    public String getOutline () {
        String outline = "{" 
            + "\n \"contextFolder\": \"" + ParserUtils.getContextFolder(ifname) + "\""
            + ",\n \"fileName\": \"" + ParserUtils.getFileName(ifname) + "\""
            + ",\n \"fileExtension\": \"" + ParserUtils.getFileExtension(ifname) + "\""
            + ",\n \"math-objects\": " + getStats()
            + ",\n \"declarations\": {"
                + "\n\t \"types\": [ " + listener.printTypes() + " ]"
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

    public class PvsContext {
        // TODO: load information from JSON file
        String[] builtinTypes = { "int", "nat", "rat", "bool", "string" };

        HashMap<String, ParserUtils.DeclDescriptor> typeDeclarations = new HashMap<String, ParserUtils.DeclDescriptor>();
        HashMap<String, ParserUtils.DeclDescriptor> formulaDeclarations = new HashMap<String, ParserUtils.DeclDescriptor>();
        HashMap<String, ParserUtils.DeclDescriptor> functionDeclarations = new HashMap<String, ParserUtils.DeclDescriptor>();
        HashMap<String, ParserUtils.DeclDescriptor> localBindingDeclarations = new HashMap<String, ParserUtils.DeclDescriptor>();
        HashMap<String, ParserUtils.DeclDescriptor> constantDeclarations = new HashMap<String, ParserUtils.DeclDescriptor>();

        protected void addBuiltInTypes () {
            for (int i = 0; i < builtinTypes.length; i++) {
                typeDeclarations.put(builtinTypes[i], 
                    new ParserUtils.DeclDescriptor(
                        builtinTypes[i],
                        0, // FIXME: indicate the actual line in the prelude
                        0, // FIXME: indicate the actual col in the prelude
                        "", // FIXME: indicate the actual declaration
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
        PvsContext () {
            // add built-in types to the prelude
            addBuiltInTypes();
        }

        /**
        * utility functions to get information from the current pvs context
        */
        public ParserUtils.DeclDescriptor getTerm (String id) {
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

    public class PvsParserListener extends PvsLanguageBaseListener {
        protected BufferedTokenStream tokens = null;
        protected TokenStreamRewriter rewriter = null;
        protected String ifname = null;

        // stats
        protected int nTypes = 0;
        protected int nDefinitions = 0;
        protected int nFormulas = 0;
        
        // pvs context
        PvsContext context = new PvsContext();

        PvsParserListener (BufferedTokenStream tokens, String ifname) {
            super();
            this.tokens = tokens;
            rewriter = new TokenStreamRewriter(tokens);
            this.ifname = ifname;
        }

        protected String printHashMap (HashMap<String, ParserUtils.DeclDescriptor> hs) {
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

        String printTypes () { return this.printHashMap(this.context.typeDeclarations); }
        String printFormulas () { return this.printHashMap(this.context.formulaDeclarations); }
        String printFunctions () { return this.printHashMap(this.context.functionDeclarations); }
        String printLocals () { return this.printHashMap(this.context.localBindingDeclarations); }

        public ParserUtils.DeclDescriptor findDeclaration (String name) {//, int line, int character) {
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

        void saveTypeDeclaration (PvsLanguageParser.TypeDeclarationContext ctx, PvsLanguageParser.TypeNameContext ictx) {
            Token start = ictx.getStart();
            Token stop = ictx.getStop();
            String id = ictx.getText();
            ParserUtils.Range scope = ParserUtils.findScope(id, ictx);
            this.context.typeDeclarations.put(id, 
                new ParserUtils.DeclDescriptor(
                    id,
                    start.getLine(),
                    start.getCharPositionInLine(),
                    ParserUtils.getSource(ctx),
                    scope,
                    this.ifname,
                    ctx
                )
            );
        }
        void saveFormulaDeclaration (PvsLanguageParser.FormulaDeclarationContext ctx) {
            Token start = ctx.getStart();
            Token stop = ctx.getStop();
            String id = ctx.identifier().getText();
            ParserUtils.Range scope = ParserUtils.findScope(id, ctx);
            this.context.formulaDeclarations.put(id, 
                new ParserUtils.DeclDescriptor(
                    id,
                    start.getLine(), 
                    start.getCharPositionInLine(),
                    ParserUtils.getSource(ctx),
                    scope,
                    this.ifname,
                    ctx
                )
            );
        }
        void saveConstantDeclaration (PvsLanguageParser.ConstantDeclarationContext ctx, PvsLanguageParser.ConstantNameContext ictx) {
            Token start = ictx.getStart();
            Token stop = ictx.getStop();
            String id = ictx.identifierOrOperator().getText();
            ParserUtils.Range scope = ParserUtils.findScope(id, ictx);
            this.context.constantDeclarations.put(id, 
                new ParserUtils.DeclDescriptor(
                    id,
                    start.getLine(), 
                    start.getCharPositionInLine(),
                    ParserUtils.getSource(ictx),
                    scope,
                    this.ifname,
                    ctx
                )
            );
        }
        void saveFunctionDeclaration (PvsLanguageParser.FunctionDeclarationContext ctx) {
            Token start = ctx.getStart();
            Token stop = ctx.getStop();
            String id = ctx.functionName().getText();
            ParserUtils.Range scope = ParserUtils.findScope(id, ctx);
            this.context.functionDeclarations.put(id, 
                new ParserUtils.DeclDescriptor(
                    id,
                    start.getLine(), 
                    start.getCharPositionInLine(),
                    ParserUtils.getSource(ctx),
                    scope,
                    this.ifname,
                    ctx
                )
            );
        }
        void saveLocalBinding (PvsLanguageParser.TypeIdContext ctx) {
            Token start = ctx.getStart();
            Token stop = ctx.getStop();
            String id = ctx.localName().getText();
            ParserUtils.Range scope = ParserUtils.findScope(id, ctx);
            this.context.localBindingDeclarations.put(id, 
                new ParserUtils.DeclDescriptor(
                    id,
                    start.getLine(), 
                    start.getCharPositionInLine(),
                    ParserUtils.getSource(ctx),
                    scope,
                    this.ifname,
                    ctx
                )
            );
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

        public String getStats () {
            return "{"
                    + " \"types\": " + this.nTypes + ","
                    + " \"definitions\": " + this.nDefinitions + ","
                    + " \"lemmas\": " + this.nFormulas
                    + " }";
        }

        @Override public void exitTheory(PvsLanguageParser.TheoryContext ctx) {
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
    }


    protected void parseCliArgs (String[] args) {
        // System.out.println(args.toString());
        for (int a = 0; a < args.length; a++) {
            if (args[a].equals("--test") || args[a].equals("-test")) {
                test = true;
            } else if (args[a].equals("--outline") || args[a].equals("-outline") || args[a].equals("-decls")) {
                outlineRequest = true;
            } else {
                ifname = args[a];
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
            if (parser.ifname != null) {
                if (parser.test) { System.out.println("Parsing file " + parser.ifname); }
                parser.parse();
                System.out.println(parser.getOutline());
            } else {
                System.out.println("Please specify file name to be parsed");
            }
        }
    }
}