import org.antlr.v4.runtime.*;
import java.util.*;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.tree.*;

import org.antlr.v4.runtime.misc.Interval;
import java.io.PrintWriter;
import java.io.BufferedWriter;
import java.io.FileWriter;

public class DdlPrettyPrinter {
    protected static Boolean test = false;
    protected static String ifname = null;
    protected static String ofname = null;
    protected static String istr = null;

    // hidden channels
    protected static final int SPACE = 1;
    protected static final int TAB = 2;
    protected static final int CR = 3;

    public static void main(String[] args) throws Exception {
        // open file
        if (args != null && args.length > 0) {
            parseCliArgs(args);
            if (test) {
                System.out.println("Parsing file " + ifname);
            }
            CharStream input = (ifname != null) ? CharStreams.fromFileName(ifname) : CharStreams.fromString(istr);
            DdlPrettyPrinterLexer lexer = new DdlPrettyPrinterLexer(input);
            CommonTokenStream tokens = new CommonTokenStream(lexer);
            DdlPrettyPrinterParser parser = new DdlPrettyPrinterParser(tokens);
            parser.removeErrorListeners(); // remove ConsoleErrorListener
            ErrorListener el = new ErrorListener();
            parser.addErrorListener(el); // add new error listener
            ParserRuleContext tree = parser.prettyprintDdl(); // parse ddl expression
            if (el.errors.size() > 0) {
                System.out.println(el.errors);
            } else {
                // System.out.println("Done!");
                // walk the tree
                ParseTreeWalker walker = new ParseTreeWalker();
                Pvs2Ddl listener = new Pvs2Ddl(tokens);
                walker.walk(listener, tree);

                System.out.println(listener.getDdlSpec());

                // if (ofname != null) {
                //     // System.out.println("Writing file " + ofname);
                //     PrintWriter printWriter = new PrintWriter(new BufferedWriter(new FileWriter(ofname)), true);
                //     printWriter.println(listener.getPvsSpec());
                //     printWriter.close();
                // }
                // if (test) {
                //     System.out.println(listener.getPvsSpec());
                // }
            }
        }
    }

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
    protected static void parseCliArgs (String[] args) {
        // System.out.println(args.toString());
        for (int a = 0; a < args.length; a++) {
            if (args[a].equals("--test") || args[a].equals("-test")) {
                test = true;
            } else if (args[a].equals("--output") || args[a].equals("-output") 
                        || args[a].equals("--out") || args[a].equals("-out")
                        || args[a].equals("-o")) {
                if (a + 1 < args.length) {
                    a++;
                    ofname = args[a];
                } else {
                    System.err.println("[ddl-pretty-printer.jar] Warning: cli arg -output detected but output file name was not provided");
                }
            } else if (args[a].equals("--translate") || args[a].equals("-translate") 
                        || args[a].equals("--tran") || args[a].equals("-tran")
                        || args[a].equals("-t")) {
                if (a + 1 < args.length) {
                    a++;
                    istr = args[a];
                } else {
                    System.err.println("[ddl-pretty-printer.jar] Warning: cli arg -input detected but input string was not provided");
                }
            } else {
                ifname = args[a];
            }
        }
    }
    public static String getFileName (String fname) {
        if (fname != null) {
            String fileName = fname.replace("file://", "");
            fileName = (fileName.indexOf("/") >= 0) ? fileName.substring(fileName.lastIndexOf("/") + 1, fileName.length() - 1) : fileName;
            fileName = (fileName.indexOf(".") >= 0) ? fileName.substring(0, fileName.lastIndexOf(".")) : fileName;
            return fileName;
        }
        return null;
    }
    public static String getContextFolder (String fname) {
        if (fname != null) {
            String folder = fname.replace("file://", "");
            folder = folder.substring(0, folder.lastIndexOf("/"));
            return folder;
        }
        return null;
    }


    public static class Pvs2Ddl extends DdlPrettyPrinterBaseListener {
        protected String hpComment = "";
        protected BufferedTokenStream tokens = null;
        protected TokenStreamRewriter rewriter = null;
        protected String padding = "";

        protected HashSet<String> variablesMap = null; 
        protected HashMap<String, String> localBindingsMap = null; 

        Pvs2Ddl (BufferedTokenStream tokens) {
            super();
            this.tokens = tokens;
            rewriter = new TokenStreamRewriter(tokens);
            variablesMap = new HashSet<String>();
            localBindingsMap = new HashMap<String, String>();
        }

        public String getSource (ParserRuleContext ctx) {
            Token start = ctx.getStart();
            Token stop = ctx.getStop();
            CharStream cs = start.getInputStream();
            Interval interval = new Interval(start.getStartIndex(), stop.getStopIndex());
            String src = cs.getText(interval);
            return src;
        }
        public String getVariableDeclarations () {
            String ans = "";
            int len = this.variablesMap.size();
            if (len > 0) {
                Iterator<String> it = this.variablesMap.iterator();
                int i = 0;
                ans += "\n  %-- Definition of syntactic sugar for variables and assignments\n";
                while (it.hasNext()) {
                    ans += "  " + it.next() + ": nat = " + i++ + "\n";
                }
                ans += "  %--\n\n";
            }
            return ans;
        }
        public String getDdlSpec () {
            return rewriter.getText();
        }
        protected void setPadding (Token token) {
            List<Token> whites = tokens.getHiddenTokensToLeft(token.getTokenIndex(), SPACE);
            List<Token> tabs = tokens.getHiddenTokensToLeft(token.getTokenIndex(), TAB);
            if (whites != null) {
                for (int i = 0; i < whites.size(); i++) {
                    padding += " ";
                }
                // padding = " ".repeat(whites.size());
                // System.out.println(whites.size());
            }
            if (tabs != null) {
                for (int i = 0; i < tabs.size(); i++) {
                    padding += "\t";
                }
                // padding = "\t".repeat(tabs.size());
                // System.out.println(tabs.size());
            }
        }
        @Override public void enterConstFunction(DdlPrettyPrinterParser.ConstFunctionContext ctx) {
            rewriter.replace(ctx.constFunctionName().getStart(), "");
            rewriter.replace(ctx.parL().getStart(), "");
            rewriter.replace(ctx.parR().getStart(), "");
        }
        @Override public void enterValueFunction(DdlPrettyPrinterParser.ValueFunctionContext ctx) {
            rewriter.replace(ctx.valueFunctionName().getStart(), "");
            rewriter.replace(ctx.parL().getStart(), "");
            rewriter.replace(ctx.parR().getStart(), "");
        }
        @Override public void enterParallelAssignFunction(DdlPrettyPrinterParser.ParallelAssignFunctionContext ctx) {
            rewriter.replace(ctx.assignFunctionName().getStart(), "");
            // leave parL and parR so the series of assignments is grouped together
            rewriter.replace(ctx.parCL().getStart(), "");
            rewriter.replace(ctx.parCR().getStart(), "");
        }
        @Override public void enterAssignFunction(DdlPrettyPrinterParser.AssignFunctionContext ctx) {
            rewriter.replace(ctx.assignFunctionName().getStart(), "");
            rewriter.replace(ctx.parL().getStart(), "");
            rewriter.replace(ctx.parR().getStart(), "");
            rewriter.replace(ctx.parCL().getStart(), "");
            rewriter.replace(ctx.parCR().getStart(), "");
        }
        @Override public void enterAssign(DdlPrettyPrinterParser.AssignContext ctx) {
            rewriter.replace(ctx.parL().getStart(), "");
            rewriter.replace(ctx.parR().getStart(), "");
            rewriter.replace(ctx.comma().getStart(), " :=");
        }
        @Override public void enterParallelDiffFunction(DdlPrettyPrinterParser.ParallelDiffFunctionContext ctx) {
            rewriter.replace(ctx.diffFunctionName().getStart(), "");
            // leave parL and parR so the series of assignments is grouped together
            rewriter.replace(ctx.parCL().getStart(), "");
            rewriter.replace(ctx.parCR().getStart(), "");
            if (ctx.diffInvariant() != null) {
                if (ctx.diffInvariant().getText().equals("DLTRUE")) {
                    rewriter.replace(ctx.commaInv().getStart(), "");
                } else {
                    rewriter.replace(ctx.commaInv().getStart(), "AND");
                }
            }
        }
        @Override public void enterDiffFunction(DdlPrettyPrinterParser.DiffFunctionContext ctx) {
            rewriter.replace(ctx.diffFunctionName().getStart(), "");
            rewriter.replace(ctx.parL().getStart(), "");
            rewriter.replace(ctx.parR().getStart(), "");
            rewriter.replace(ctx.parCL().getStart(), "");
            rewriter.replace(ctx.parCR().getStart(), "");
            if (ctx.diffInvariant() != null) {
                if (ctx.diffInvariant().getText().equals("DLTRUE")) {
                    rewriter.replace(ctx.commaInv().getStart(), "");
                } else {
                    rewriter.replace(ctx.commaInv().getStart(), "AND");
                }
            }
        }
        @Override public void enterDiffAssignment(DdlPrettyPrinterParser.DiffAssignmentContext ctx) {
            rewriter.replace(ctx.identifier().getStart(), ctx.identifier().getText() + "'");
            rewriter.replace(ctx.comma().getStart(), "=");
            rewriter.replace(ctx.parL().getStart(), "");
            rewriter.replace(ctx.parR().getStart(), "");
        }
        @Override public void enterDiffInvariant(DdlPrettyPrinterParser.DiffInvariantContext ctx) {
            rewriter.replace(ctx.dltrue().getStart(), "");
        }
        @Override public void enterAnyFunction(DdlPrettyPrinterParser.AnyFunctionContext ctx) {
            rewriter.replace(ctx.anyFunctionName().getStart(), "");
            rewriter.replace(ctx.comma().getStart(), " :=");
            rewriter.replace(ctx.dlrandom().getStart(), "*");
            rewriter.replace(ctx.parL().getStart(), "");
            rewriter.replace(ctx.parR().getStart(), "");
        }
        @Override public void enterSeqFunction(DdlPrettyPrinterParser.SeqFunctionContext ctx) {
            rewriter.replace(ctx.seqFunctionName().getStart(), "");
            rewriter.replace(ctx.parL().getStart(), "");
            rewriter.replace(ctx.comma().getStart(), ";");
            rewriter.replace(ctx.parR().getStart(), "");
        }
        @Override public void enterAllRunsFunction(DdlPrettyPrinterParser.AllRunsFunctionContext ctx) {
            rewriter.replace(ctx.allRunsFunctionName().getStart(), "");
            rewriter.replace(ctx.parL().getStart(), "[");
            rewriter.replace(ctx.comma().getStart(), "]");
            rewriter.replace(ctx.parR().getStart(), "");
        }
        @Override public void enterSomeRunsFunction(DdlPrettyPrinterParser.SomeRunsFunctionContext ctx) {
            rewriter.replace(ctx.someRunsFunctionName().getStart(), "");
            rewriter.replace(ctx.parL().getStart(), "< ");
            rewriter.replace(ctx.comma().getStart(), " >");
            rewriter.replace(ctx.parR().getStart(), "");
        }
        @Override public void exitInvariant(DdlPrettyPrinterParser.InvariantContext ctx) {
            if (ctx.getText().startsWith("(") == false) {
                rewriter.insertBefore(ctx.getStart(), "(");
                rewriter.insertAfter(ctx.getStop(), ")");
            }
        }
        @Override public void enterTestFunction(DdlPrettyPrinterParser.TestFunctionContext ctx) { 
            rewriter.replace(ctx.testFunctionName().getStart(), "?");
            // leave parentheses for clarity
            // rewriter.replace(ctx.parL().getStart(), "");
            // rewriter.replace(ctx.parR().getStart(), "");
        }
        @Override public void enterUnionFunction(DdlPrettyPrinterParser.UnionFunctionContext ctx) { 
            rewriter.replace(ctx.unionFunctionName().getStart(), "");
            rewriter.replace(ctx.parL().getStart(), "");
            rewriter.replace(ctx.comma().getStart(), "++");
            rewriter.replace(ctx.parR().getStart(), "");
        }
        @Override public void enterStarFunction(DdlPrettyPrinterParser.StarFunctionContext ctx) { 
            rewriter.replace(ctx.starFunctionName().getStart(), "");
            rewriter.replace(ctx.parR().getStart(), ")*");
        }
        @Override public void enterDlForallFunction(DdlPrettyPrinterParser.DlForallFunctionContext ctx) { 
            rewriter.replace(ctx.dlforall().getStart(), "FORALL");
            // the parenthesis are introduced by the bindings
            rewriter.replace(ctx.parL().getStart(), "");
            rewriter.replace(ctx.parR().getStart(), "");
        }
        @Override public void enterDlExistsFunction(DdlPrettyPrinterParser.DlExistsFunctionContext ctx) { 
            rewriter.replace(ctx.dlexists().getStart(), "EXISTS");
            // the parenthesis are introduced by the bindings
            rewriter.replace(ctx.parL().getStart(), "");
            rewriter.replace(ctx.parR().getStart(), "");
        }
        @Override public void enterBindingPart(DdlPrettyPrinterParser.BindingPartContext ctx) {
            // List<DdlPrettyPrinterParser.BindingNameContext> names = ctx.lambdaBindings().bindingName();
            // Iterator<DdlPrettyPrinterParser.BindingNameContext> it = names.iterator();
            // String bindingNames = "";
            // while (it.hasNext()) {
            //     if (it.next().identifierOrOperator().identifier() != null) {
            //         bindingNames += it.next().identifierOrOperator().identifier().toString();
            //     }
            //     if (it.hasNext() && !bindingNames.equals("")) { bindingNames += ", "; }
            // }
            // rewriter.replace(ctx.getStart(), ctx.getStop(), bindingNames + "): ");

            rewriter.replace(ctx.getStart(), ctx.getStop(), ctx.lambdaBindings().getText() + ": ");
        }
        @Override public void enterEntailEmptyInit(DdlPrettyPrinterParser.EntailEmptyInitContext ctx) { 
            rewriter.replace(ctx.emptyInit().getStart(), ctx.emptyInit().getStop(), "");
        }
    }
}