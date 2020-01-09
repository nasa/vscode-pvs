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
            String diag = "{ \"range\": " + range + ", \"message\": \"" + message + "\", \"severity\": " + DiagnosticSeverity.Error + " }";
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
    public static class DeclDescriptor {
        int start_line;
        int start_character;
        int stop_line;
        int stop_character;
        DeclDescriptor (int start_line, int start_character, int stop_line, int stop_character) {
            this.start_line = start_line;
            this.start_character = start_character;
            this.stop_line = stop_line;
            this.stop_character = stop_character;
        }
        public String toString () {
            return "{ \"start\": "
                + "{ \"line\": " + this.start_line + ", \"character\": " + this.start_character + "}"
                + ", \"stop\": "
                + "{ \"line\": " + this.stop_line + ", \"character\": " + this.stop_character + "}"
                + " }";
        }
    }
    public static void main(String[] args) throws Exception {
        // open file
        if (args != null && args.length > 0) {
            parseCliArgs(args);
            if (test) {
                System.out.println("Parsing file " + ifname);
            }
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
            if (el.errors.size() > 0) {
                System.out.println(el.errors);
            } else {
                if (test) {
                    System.out.println(ifname + " parsed successfully!");
                }
                // walk the tree
                ParseTreeWalker walker = new ParseTreeWalker();
                PvsParserListener listener = new PvsParserListener(tokens);
                walker.walk(listener, tree);
            }
        }
    }
    public static class PvsParserListener extends PvsLanguageBaseListener {
        protected BufferedTokenStream tokens = null;
        protected TokenStreamRewriter rewriter = null;
        protected HashMap<String, DeclDescriptor> typeDeclarations = new HashMap<String, DeclDescriptor>();
        protected HashMap<String, DeclDescriptor> formulaDeclarations = new HashMap<String, DeclDescriptor>();
        PvsParserListener (BufferedTokenStream tokens) {
            super();
            this.tokens = tokens;
            rewriter = new TokenStreamRewriter(tokens);
        }

        @Override public void enterTypeDeclaration(PvsLanguageParser.TypeDeclarationContext ctx) {
            ListIterator<PvsLanguageParser.IdentifierContext> it = ctx.identifier().listIterator();
            while (it.hasNext()) {
                PvsLanguageParser.IdentifierContext ictx = it.next();
                Token start = ictx.getStart();
                Token stop = ictx.getStop();
                String id = ictx.getText();
                this.typeDeclarations.put(
                    id, 
                    new DeclDescriptor(start.getLine(), start.getCharPositionInLine(), stop.getLine(), stop.getCharPositionInLine())
                );
            }
        }
        @Override public void enterFormulaDeclaration(PvsLanguageParser.FormulaDeclarationContext ctx) {
            Token start = ctx.getStart();
            Token stop = ctx.getStop();
            String id = ctx.identifier().getText();
            this.formulaDeclarations.put(
                id, 
                new DeclDescriptor(start.getLine(), start.getCharPositionInLine(), stop.getLine(), stop.getCharPositionInLine())
            );
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
            }
        }
    }
}