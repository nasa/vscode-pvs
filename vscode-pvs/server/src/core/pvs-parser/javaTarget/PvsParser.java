import org.antlr.v4.runtime.*;
import java.util.*;
import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.tree.*;
import org.antlr.v4.runtime.misc.Interval;

public class PvsParser {
    protected static Boolean test = false;
    protected static String ifname = null;

    public static interface DiagnosticSeverity {
        int Error = 1;
        int Warning = 2;
        int Information = 3;
        int Hint = 4;
    }
    public static class ErrorListener extends BaseErrorListener {
        protected ArrayList<String> errors = new ArrayList<String>(); // array of JSON strings in the form { range: { start: { line: number, character: number }, stop: { line: number, character: number } }, message: string } 

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
    public static class ErrorHandler extends DefaultErrorStrategy {
        // @Override public void reportNoViableAlternative(Parser parser, NoViableAltException e) {
        //     parser.notifyErrorListeners(e.getOffendingToken(), "Syntax error", e);
        // }
    }
    protected static void parseCliArgs (String[] args) {
        // System.out.println(args.toString());
        for (int a = 0; a < args.length; a++) {
            if (args[a].equals("--test") || args[a].equals("-test")) {
                test = true;
            } else {
                ifname = args[a];
            }
        }
    }
    

    public static void main(String[] args) throws Exception {
        // open file
        if (args != null && args.length > 0) {
            parseCliArgs(args);
            if (test) {
                System.out.println("Parsing file " + ifname);
            }
            double parseStart = System.currentTimeMillis();

            CharStream input = CharStreams.fromFileName(ifname);
            PvsLanguageLexer lexer = new PvsLanguageLexer(input);
            CommonTokenStream tokens = new CommonTokenStream(lexer);
            PvsLanguageParser parser = new PvsLanguageParser(tokens);
            parser.removeErrorListeners(); // remove ConsoleErrorListener
            ErrorListener el = new ErrorListener();
            parser.addErrorListener(el); // add new error listener
            ErrorHandler eh = new ErrorHandler();
            parser.setErrorHandler(eh);
            // parser.setBuildParseTree(false); // disable parse tree creation, to speed up parsing
            ParserRuleContext tree = parser.parse(); // parse as usual
            // walk the tree
            ParseTreeWalker walker = new ParseTreeWalker();
            PvsParserListener listener = new PvsParserListener(tokens);
            walker.walk(listener, tree);

            double parseTime = System.currentTimeMillis() - parseStart;

            String ans = "{"
                        + "\n \"filename\": \"" + ifname + "\""
                        + ",\n \"math-objects\": " + listener.getStats()
                        + ",\n \"parse-time\": { \"ms\": " + parseTime + " }";
            if (el.errors.size() > 0) {
                ans += ",\n \"errors\": " + el.errors;
            }
            ans += "\n}";
            System.out.println(ans);
        }
    }
    public static class PvsParserListener extends PvsLanguageBaseListener {
        protected BufferedTokenStream tokens = null;
        protected TokenStreamRewriter rewriter = null;

        // stats
        protected int nTypes = 0;
        protected int nDefs = 0;
        protected int nFormulas = 0;
        
        protected HashMap<String, ParserUtils.DeclDescriptor> typeDeclarations = new HashMap<String, ParserUtils.DeclDescriptor>();
        protected HashMap<String, ParserUtils.DeclDescriptor> formulaDeclarations = new HashMap<String, ParserUtils.DeclDescriptor>();
        protected HashMap<String, ParserUtils.DeclDescriptor> functionDeclarations = new HashMap<String, ParserUtils.DeclDescriptor>();
        protected HashMap<String, ParserUtils.DeclDescriptor> localBindingDeclarations = new HashMap<String, ParserUtils.DeclDescriptor>();

        PvsParserListener (BufferedTokenStream tokens) {
            super();
            this.tokens = tokens;
            rewriter = new TokenStreamRewriter(tokens);
        }

        public ParserUtils.DeclDescriptor findDeclaration (String name) {//, int line, int character) {
            System.out.println("Finding declaration for " + name);
            ParserUtils.DeclDescriptor candidate = typeDeclarations.get(name);
            if (candidate != null) {
                return candidate;
            }
            candidate = formulaDeclarations.get(name);
            if (candidate != null) {
                return candidate;
            }
            candidate = functionDeclarations.get(name);
            if (candidate != null) {
                return candidate;
            }
            candidate = localBindingDeclarations.get(name);
            if (candidate != null) {
                return candidate;
            }
            return null;
        }

        @Override public void enterTypeDeclaration(PvsLanguageParser.TypeDeclarationContext ctx) {
            nTypes++;
            ListIterator<PvsLanguageParser.IdentifierContext> it = ctx.identifier().listIterator();
            while (it.hasNext()) {
                PvsLanguageParser.IdentifierContext ictx = it.next();
                Token start = ictx.getStart();
                Token stop = ictx.getStop();
                String id = ictx.getText();
                ParserUtils.Range scope = ParserUtils.findScope(id, ictx);
                this.typeDeclarations.put(id, 
                    new ParserUtils.DeclDescriptor(
                        id,
                        start.getLine(),
                        start.getCharPositionInLine(),
                        ParserUtils.getSource(ctx),
                        scope
                    )
                );
            }
        }
        @Override public void enterFormulaDeclaration(PvsLanguageParser.FormulaDeclarationContext ctx) {
            nFormulas++;
            Token start = ctx.getStart();
            Token stop = ctx.getStop();
            String id = ctx.identifier().getText();
            ParserUtils.Range scope = ParserUtils.findScope(id, ctx);
            this.formulaDeclarations.put(id, 
                new ParserUtils.DeclDescriptor(
                    id,
                    start.getLine(), 
                    start.getCharPositionInLine(), 
                    ParserUtils.getSource(ctx),
                    scope
                )
            );
        }
        @Override public void enterVarDeclaration(PvsLanguageParser.VarDeclarationContext ctx) {
            nDefs++;
        }
        @Override public void enterConstantDeclaration(PvsLanguageParser.ConstantDeclarationContext ctx) {
            nDefs++;
        }
        @Override public void enterFunctionDeclaration(PvsLanguageParser.FunctionDeclarationContext ctx) {
            nDefs++;
            Token start = ctx.getStart();
            Token stop = ctx.getStop();
            String id = ctx.functionName().getText();
            ParserUtils.Range scope = ParserUtils.findScope(id, ctx);
            this.functionDeclarations.put(id, 
                new ParserUtils.DeclDescriptor(
                    id,
                    start.getLine(), 
                    start.getCharPositionInLine(),
                    ParserUtils.getSource(ctx),
                    scope
                )
            );
        }
        @Override public void enterAssumption(PvsLanguageParser.AssumptionContext ctx) {
            nFormulas++;
        }
        @Override public void enterJudgementDeclaration(PvsLanguageParser.JudgementDeclarationContext ctx) {
            nFormulas++;
        }
        @Override public void enterConversionDeclaration(PvsLanguageParser.ConversionDeclarationContext ctx) {
            nDefs++;
        }
        @Override public void enterAutorewriteDeclaration(PvsLanguageParser.AutorewriteDeclarationContext ctx) {
            nDefs++;
        }
        @Override public void enterTypeId(PvsLanguageParser.TypeIdContext ctx) {
            Token start = ctx.getStart();
            Token stop = ctx.getStop();
            String id = ctx.localName().getText();
            ParserUtils.Range scope = ParserUtils.findScope(id, ctx);
            this.localBindingDeclarations.put(id, 
                new ParserUtils.DeclDescriptor(
                    id,
                    start.getLine(), 
                    start.getCharPositionInLine(),
                    ParserUtils.getSource(ctx),
                    scope
                )
            );
        }

        public String getStats () {
            return "{"
                    + " \"types\": " + this.nTypes + ","
                    + " \"definitions\": " + this.nDefs + ","
                    + " \"lemmas\": " + this.nFormulas
                    + " }";
        }

        @Override public void exitTheory(PvsLanguageParser.TheoryContext ctx) {
            if (test) {
                if (this.typeDeclarations != null) {
                    int n = this.typeDeclarations.size();
                    System.out.println("------------------------------");
                    System.out.println(n + " type declarations");
                    System.out.println("------------------------------");
                    for (String id: this.typeDeclarations.keySet()){
                        System.out.println(id + " " + this.typeDeclarations.get(id));
                    }
                }
                if (this.formulaDeclarations != null) {
                    int n = this.formulaDeclarations.size();
                    System.out.println("------------------------------");
                    System.out.println(n + " formula declarations");
                    System.out.println("------------------------------");
                    for (String id: this.formulaDeclarations.keySet()){
                        System.out.println(id + " " + this.formulaDeclarations.get(id));
                    }
                }
                if (this.functionDeclarations != null) {
                    int n = this.functionDeclarations.size();
                    System.out.println("------------------------------");
                    System.out.println(n + " function declarations");
                    System.out.println("------------------------------");
                    for (String id: this.functionDeclarations.keySet()){
                        System.out.println(id + " " + this.functionDeclarations.get(id));
                    }
                }
                if (this.localBindingDeclarations != null) {
                    int n = this.localBindingDeclarations.size();
                    System.out.println("------------------------------");
                    System.out.println(n + " local bindings");
                    System.out.println("------------------------------");
                    for (String id: this.localBindingDeclarations.keySet()){
                        System.out.println(id + " " + this.localBindingDeclarations.get(id));
                    }
                }
            }
        }
    }
}