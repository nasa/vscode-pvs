import org.antlr.v4.runtime.*;
import java.util.*;
import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.tree.*;
import org.antlr.v4.runtime.misc.Interval;
import java.io.PrintWriter;
import java.io.BufferedWriter;
import java.io.FileWriter;

public class DdlParser {
    protected static Boolean test = false;
    protected static String ifname = null;
    protected static String ofname = null;

    // hidden channels
    protected static final int SPACE = 1;
    protected static final int TAB = 2;
    protected static final int CR = 3;

    public static void main(String[] args) throws Exception {
        // open file
        if (args != null && args.length > 0) {
            parseCliArgs(args);
            // System.out.println("Parsing file " + ifname);
            CharStream input = CharStreams.fromFileName(ifname);
            DdlEmbeddingLexer lexer = new DdlEmbeddingLexer(input);
            CommonTokenStream tokens = new CommonTokenStream(lexer);
            DdlEmbeddingParser parser = new DdlEmbeddingParser(tokens);
            parser.removeErrorListeners(); // remove ConsoleErrorListener
            ErrorListener el = new ErrorListener();
            parser.addErrorListener(el); // add new error listener
            ParserRuleContext tree = parser.parse(); // parse as usual
            if (el.errors.size() > 0) {
                System.out.println(el.errors);
            } else {
                // System.out.println("Done!");
                // walk the tree
                ParseTreeWalker walker = new ParseTreeWalker();
                Ddl2Pvs listener = new Ddl2Pvs(tokens);
                walker.walk(listener, tree);

                if (ofname != null) {
                    // System.out.println("Writing file " + ofname);
                    PrintWriter printWriter = new PrintWriter(new BufferedWriter(new FileWriter(ofname)), true);
                    printWriter.println(listener.getPvsSpec());
                }
                if (test) {
                    System.out.println(listener.getPvsSpec());
                }
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
            } else if (args[a].equals("--out") || args[a].equals("-out") || args[a].equals("-o")) {
                if (a + 1 < args.length) {
                    a++;
                    ofname = args[a];
                } else {
                    System.err.println("[ddlparser.jar] Warning: cli arg -out detected but output file name was not provided");
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


    public static class Ddl2Pvs extends DdlEmbeddingBaseListener {
        protected String pvsSpec = "";
        protected String hpComment = "";
        protected BufferedTokenStream tokens = null;
        protected TokenStreamRewriter rewriter = null;
        protected String padding = "";

        Ddl2Pvs (BufferedTokenStream tokens) {
            super();
            this.tokens = tokens;
            rewriter = new TokenStreamRewriter(tokens);
        }

        public String getSource (ParserRuleContext ctx) {
            Token start = ctx.getStart();
            Token stop = ctx.getStop();
            CharStream cs = start.getInputStream();
            Interval interval = new Interval(start.getStartIndex(), stop.getStopIndex());
            String src = cs.getText(interval);
            return src;
        }
        public String getPvsSpec () {
            return rewriter.getText();
        }
        protected void setPadding (Token token) {
            List<Token> whites = tokens.getHiddenTokensToLeft(token.getTokenIndex(), SPACE);
            List<Token> tabs = tokens.getHiddenTokensToLeft(token.getTokenIndex(), TAB);
            if (whites != null) {
                padding = " ".repeat(whites.size());
                // System.out.println(whites.size());
            }
            if (tabs != null) {
                padding = "\t".repeat(tabs.size());
                // System.out.println(tabs.size());
            }
        }
        @Override public void enterHpEmbedding(DdlEmbeddingParser.HpEmbeddingContext ctx) {
            // grab the original ddl problem and place it as comment in the pvs file
            String src = getSource(ctx);

            DdlEmbeddingParser.DlProblemContext problem = ctx.dlProblem();
            setPadding(problem.getStart());

            hpComment = "%---------------------------------\n  % ";
            hpComment += src.replaceAll("\n", "\n  % ");;
            hpComment += "\n  %---------------------------------\n  ";
            // System.out.print(comment);

            pvsSpec += ctx.identifier().getText() + ": LEMMA\n";
            // System.out.print(pvsSpec);
            // System.out.println(ctx.getText());
        }
        @Override public void exitHpEmbedding(DdlEmbeddingParser.HpEmbeddingContext ctx) {
            // rewrite hp embedding in the original .hpvs file with pvsSpec so as to produce the .pvs file
            Token start = ctx.getStart();
            Token stop = ctx.getStop();
            rewriter.insertBefore(start, hpComment + pvsSpec.replace("\n", "\n" + padding));
            rewriter.replace(start, stop, "\n");
            // System.out.println(pvsSpec);
        }
        @Override public void enterDlInvariant(DdlEmbeddingParser.DlInvariantContext ctx) {
            pvsSpec += ",";
            // System.out.println(pvsSpec);
        }
        @Override public void enterDlAllRunsProgram(DdlEmbeddingParser.DlAllRunsProgramContext ctx) {
            pvsSpec += "ALLRUNS(";
            // System.out.print(pvsSpec);
        }
        @Override public void exitDlAllRunsProgram(DdlEmbeddingParser.DlAllRunsProgramContext ctx) {
            pvsSpec += ")";
            // System.out.print(pvsSpec);
        }
        @Override public void enterDlSequentialAssignment(DdlEmbeddingParser.DlSequentialAssignmentContext ctx) {
            pvsSpec += "SEQ(";
            // System.out.print(pvsSpec);  
        }
        @Override public void exitDlSequentialAssignment(DdlEmbeddingParser.DlSequentialAssignmentContext ctx) {
            pvsSpec += ")";
            // System.out.print(pvsSpec);
        }
        @Override public void enterDlDiffAssignment(DdlEmbeddingParser.DlDiffAssignmentContext ctx) {
            pvsSpec += "DIFF((: ";
            // System.out.print(pvsSpec);  
        }
        @Override public void exitDlDiffAssignment(DdlEmbeddingParser.DlDiffAssignmentContext ctx) {
            pvsSpec += " :)";
            if (ctx.dlDiffInvariant() == null) {
                pvsSpec += ", DLTRUE";
            }
            pvsSpec += ")";
            // System.out.print(pvsSpec);  
        }
        @Override public void enterDlDiffAssignmentElem(DdlEmbeddingParser.DlDiffAssignmentElemContext ctx) {
            pvsSpec += "(";
            // System.out.print(pvsSpec);
        }
        @Override public void exitDlDiffAssignmentElem(DdlEmbeddingParser.DlDiffAssignmentElemContext ctx) {
            pvsSpec += ")";
            // System.out.print(pvsSpec);
        }
        @Override public void enterDlSimpleAssignment(DdlEmbeddingParser.DlSimpleAssignmentContext ctx) {
            pvsSpec += "ASSIGN((: (";
            // System.out.print(pvsSpec); 
        }
        @Override public void exitDlSimpleAssignment(DdlEmbeddingParser.DlSimpleAssignmentContext ctx) {
            pvsSpec += ") :))";
            // System.out.print(pvsSpec); 
        }
        @Override public void exitOperatorColon(DdlEmbeddingParser.OperatorColonContext ctx) {
            // separates two sequential assignments
            pvsSpec += ", ";
            // System.out.print(pvsSpec); 
        }
        @Override public void enterDlStarProgram(DdlEmbeddingParser.DlStarProgramContext ctx) {
            pvsSpec += "STAR(";
            // System.out.print(pvsSpec);  
        }
        @Override public void exitDlStarProgram(DdlEmbeddingParser.DlStarProgramContext ctx) {
            pvsSpec += ")";
            // System.out.print(pvsSpec);
        }
        @Override public void enterParenLeft(DdlEmbeddingParser.ParenLeftContext ctx) {
            pvsSpec += ctx.getText();
            // System.out.print(pvsSpec);
        }
        @Override public void enterParenRight(DdlEmbeddingParser.ParenRightContext ctx) {
            pvsSpec += ctx.getText();
            // System.out.print(pvsSpec);
        }
        @Override public void enterOperatorEntail(DdlEmbeddingParser.OperatorEntailContext ctx) {
            pvsSpec += "\n" + ctx.getText() + " ";
            // System.out.print(pvsSpec);
        }
        @Override public void enterOperatorAssign(DdlEmbeddingParser.OperatorAssignContext ctx) {
            pvsSpec += ", ";
            // System.out.print(pvsSpec);
        }
        @Override public void enterDlConst(DdlEmbeddingParser.DlConstContext ctx) {
            pvsSpec += "cnst(" + ctx.getText() + ")";
            // System.out.print(pvsSpec);             
        }
        @Override public void enterDlDiffIdentifier(DdlEmbeddingParser.DlDiffIdentifierContext ctx) {
            pvsSpec += ctx.getText().replace("'", "");
            // System.out.print(pvsSpec); 
        }
        @Override public void enterDlIdentifier(DdlEmbeddingParser.DlIdentifierContext ctx) {
            pvsSpec += ctx.getText();
            // System.out.print(pvsSpec); 
        }
        @Override public void enterOperatorNOT(DdlEmbeddingParser.OperatorNOTContext ctx) {
            pvsSpec += " " + ctx.getText() + " ";
            // System.out.print(pvsSpec);            
        }
        @Override public void enterOperatorOR(DdlEmbeddingParser.OperatorORContext ctx) {
            pvsSpec += " " + ctx.getText() + " ";
            // System.out.print(pvsSpec);
        }
        @Override public void enterOperatorAND(DdlEmbeddingParser.OperatorANDContext ctx) {
            pvsSpec += " " + ctx.getText() + " ";
            // System.out.print(pvsSpec);
        }
        @Override public void enterOperatorCMP(DdlEmbeddingParser.OperatorCMPContext ctx) {
            pvsSpec += " " + ctx.getText() + " ";
            // System.out.print(pvsSpec);
        }
        @Override public void enterOperatorPlusMinus(DdlEmbeddingParser.OperatorPlusMinusContext ctx) {
            pvsSpec += " " + ctx.getText() + " ";
            // System.out.print(pvsSpec);
        }
    	@Override public void enterOperatorMulDiv(DdlEmbeddingParser.OperatorMulDivContext ctx) {
            pvsSpec += " " + ctx.getText() + " ";
            // System.out.print(pvsSpec);
        }
        @Override public void enterOperatorExp(DdlEmbeddingParser.OperatorExpContext ctx) {
            pvsSpec += ctx.getText();
            // System.out.print(pvsSpec);
        }
        @Override public void enterDlValue(DdlEmbeddingParser.DlValueContext ctx) {
            pvsSpec += "val(" + ctx.getText() + ")";
            // System.out.print(pvsSpec);
        }
        @Override public void enterDlFunction(DdlEmbeddingParser.DlFunctionContext ctx) {
            pvsSpec += ctx.dlFunctionName().getText() + "(";
        }
        @Override public void exitDlFunction(DdlEmbeddingParser.DlFunctionContext ctx) {
            pvsSpec += ")";
        }
    }
}