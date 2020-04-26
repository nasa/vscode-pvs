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

public class DdlParser {
    protected static Boolean test = false;
    protected static String ifname = null;
    protected static String ofname = null;

    // hidden channels
    protected static final int SPACE = 1;
    protected static final int TAB = 2;
    protected static final int CR = 3;

    // utils
    public static String getFileName (String fname) {
        if (fname != null) {
            String fileName = fname.replace("file://", "");
            fileName = (fileName.indexOf("/") >= 0) ? fileName.substring(fileName.lastIndexOf("/") + 1, fileName.length() - 1) : fileName;
            fileName = (fileName.indexOf(".") >= 0) ? fileName.substring(0, fileName.lastIndexOf(".")) : fileName;
            return fileName;
        }
        return "";
    }
    public static String getFileExtension (String fname) {
        if (fname != null) {
            String fileExtension = fname.replace("file://", "");
            fileExtension = fileExtension.substring(fileExtension.lastIndexOf("."), fileExtension.length());
            return fileExtension;
        }
        return "";
    }
    public static String getContextFolder (String fname) {
        if (fname != null) {
            String folder = fname.replace("file://", "");
            folder = folder.substring(0, folder.lastIndexOf("/"));
            return folder;
        }
        return "";
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
            DdlEmbeddingLexer lexer = new DdlEmbeddingLexer(input);
            CommonTokenStream tokens = new CommonTokenStream(lexer);
            DdlEmbeddingParser parser = new DdlEmbeddingParser(tokens);
            parser.removeErrorListeners(); // remove ConsoleErrorListener
            ErrorListener el = new ErrorListener();
            parser.addErrorListener(el); // add new error listener
            ParserRuleContext tree = parser.parse(); // parse as usual

            String ans = "{"
                + "\n \"contextFolder\": \"" + getContextFolder(ifname) + "\""
                + ",\n \"fileName\": \"" + getFileName(ifname) + "\""
                + ",\n \"fileExtension\": \"" + getFileExtension(ifname) + "\"";
            if (el.errors.size() > 0) {
                ans += ",\n \"errors\": " + el.errors;
            } else {
                // walk the tree
                ParseTreeWalker walker = new ParseTreeWalker();
                Ddl2Pvs listener = new Ddl2Pvs(tokens);
                walker.walk(listener, tree);

                ans += ",\n \"math-objects\": " + listener.getStats();


                if (ofname != null) {
                    // System.out.println("Writing file " + ofname);
                    PrintWriter printWriter = new PrintWriter(new BufferedWriter(new FileWriter(ofname)), true);
                    printWriter.println(listener.getPvsSpec());
                    printWriter.close();
                }
                if (test) {
                    System.out.println(listener.getPvsSpec());
                }
            }
            double parseTime = System.currentTimeMillis() - parseStart;

            // + ",\n \"filename\": \"" + ifname + "\""
            ans += ",\n \"parse-time\": { \"ms\": " + parseTime + " }";
            ans += "\n}";
            System.out.println(ans);
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


    public static class Ddl2Pvs extends DdlEmbeddingBaseListener {
        // stats
        protected int nTypes = 0;
        protected int nDefinitions = 0;
        protected int nFormulas = 0;
        
        protected String pvsSpec = "";
        protected String hpComment = "";
        protected BufferedTokenStream tokens = null;
        protected TokenStreamRewriter rewriter = null;
        protected String padding = "";

        protected HashSet<String> variablesMap = null; 
        protected HashMap<String, String> localBindingsMap = null; 
        protected DdlEmbeddingParser.TheoryBeginContext theoryBeginContext = null;

        Ddl2Pvs (BufferedTokenStream tokens) {
            super();
            this.tokens = tokens;
            rewriter = new TokenStreamRewriter(tokens);
            variablesMap = new HashSet<String>();
            localBindingsMap = new HashMap<String, String>();
        }

        public String getStats () {
            return "{"
                    + " \"types\": " + this.nTypes + ","
                    + " \"definitions\": " + this.nDefinitions + ","
                    + " \"lemmas\": " + this.nFormulas
                    + " }";
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
        public String getPvsSpec () {
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
        @Override public void enterTheoryBegin(DdlEmbeddingParser.TheoryBeginContext ctx) {
            this.theoryBeginContext = ctx;
        }
        @Override public void enterTheoryEnd(DdlEmbeddingParser.TheoryEndContext ctx) {
            if (this.theoryBeginContext != null) {
                // place variable declarations and importing dynamic_logic at the beginning of the theory
                Token stop = this.theoryBeginContext.getStop();
                String importing = "\n\n  IMPORTING dynamic_logic\n";
                String decls = getVariableDeclarations();
                rewriter.insertAfter(stop, importing + decls);
            }
        }
        @Override public void exitHpEmbedding(DdlEmbeddingParser.HpEmbeddingContext ctx) {
            // rewrite hp embedding in the original .hpvs file with pvsSpec so as to produce the .pvs file
            Token start = ctx.getStart();
            Token stop = ctx.getStop();
            // replace PROBLEMS with LEMMAS
            rewriter.insertBefore(start, hpComment + pvsSpec.replace("\n", "\n" + padding));
            rewriter.replace(start, stop, "\n");
            // reset pvsSpec and comment
            this.pvsSpec = "";
            this.hpComment = "";
            // System.out.println(pvsSpec);
        }
        @Override public void enterDlTimesExpression(DdlEmbeddingParser.DlTimesExpressionContext ctx) {
            String number_times_id = ctx.NUMBER_TIMES_ID().getText();
            java.util.regex.Matcher match_number = java.util.regex.Pattern.compile("\\d+").matcher(number_times_id);
            if (match_number.find()) {
                String number = match_number.group(0);
                // replace 2x with 2 * x
                number_times_id = number_times_id.replace(number, "cnst(" + number + ") * ");
            }
            pvsSpec += number_times_id;
        }
        @Override public void enterDlInvariant(DdlEmbeddingParser.DlInvariantContext ctx) {
            pvsSpec += ", ";
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
        @Override public void enterDlSequentialStatement(DdlEmbeddingParser.DlSequentialStatementContext ctx) {
            pvsSpec += "SEQ(";
            // System.out.print(pvsSpec);  
        }
        @Override public void exitDlSequentialStatement(DdlEmbeddingParser.DlSequentialStatementContext ctx) {
            pvsSpec += ")";
            // System.out.print(pvsSpec);
        }
        @Override public void enterDlDiffStatement(DdlEmbeddingParser.DlDiffStatementContext ctx) {
            pvsSpec += "DIFF((: ";
            // System.out.print(pvsSpec);  
        }
        @Override public void exitDlDiffStatement(DdlEmbeddingParser.DlDiffStatementContext ctx) {
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
        @Override public void enterDlSimpleAssignmentStatement(DdlEmbeddingParser.DlSimpleAssignmentStatementContext ctx) {
            pvsSpec += "ASSIGN((: ";
            // System.out.print(pvsSpec); 
        }
        @Override public void exitDlSimpleAssignmentStatement(DdlEmbeddingParser.DlSimpleAssignmentStatementContext ctx) {
            pvsSpec += " :))";
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
            this.variablesMap.add(ctx.getText());
            pvsSpec += ctx.getText();
            // System.out.print(pvsSpec); 
        }
        @Override public void enterOperatorDiffEqual(DdlEmbeddingParser.OperatorDiffEqualContext ctx) {
            pvsSpec += ", ";
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
        @Override public void enterOperatorPlusMinusUnary(DdlEmbeddingParser.OperatorPlusMinusUnaryContext ctx) {
            pvsSpec += " " + ctx.getText();
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
            String id = ctx.getText();
            if (this.localBindingsMap.get(id) != null) {
                pvsSpec += id;
            } else {
                this.variablesMap.add(id);
                pvsSpec += "val(" + id + ")";
            }
            // System.out.print(pvsSpec);
        }
        @Override public void enterDlFunction(DdlEmbeddingParser.DlFunctionContext ctx) {
            pvsSpec += ctx.dlFunctionName().getText() + "(";
        }
        @Override public void exitDlFunction(DdlEmbeddingParser.DlFunctionContext ctx) {
            pvsSpec += ")";
        }
        @Override public void enterVarDeclaration(DdlEmbeddingParser.VarDeclarationContext ctx) {
            this.localBindingsMap.put(ctx.varIdentifiers().getText(), ctx.typeExpression().getText());
        }
        @Override public void enterDlAnyAssignmentStatement(DdlEmbeddingParser.DlAnyAssignmentStatementContext ctx) {
            String id = ctx.dlAnyAssignmentIdentifier().getText();
            this.variablesMap.add(id);
            // String type = this.localBindingsMap.get(id);
            // if (type == null) {
            //     // assume it's a real
            //     type = "real";
            //     this.localBindingsMap.put(id, type);
            // }
            pvsSpec += "ANY(" + id + ", DLRANDOM)";
        }
        @Override public void enterOperatorComma(DdlEmbeddingParser.OperatorCommaContext ctx) {
            pvsSpec += ", ";
        }
        @Override public void enterDlUnionStatement(DdlEmbeddingParser.DlUnionStatementContext ctx) {
            pvsSpec += "UNION(";
        }
        @Override public void exitDlUnionStatement(DdlEmbeddingParser.DlUnionStatementContext ctx) {
            pvsSpec += ")";
        }
        @Override public void enterOperatorPlusPlus(DdlEmbeddingParser.OperatorPlusPlusContext ctx) {
            pvsSpec += ", ";
        }
        @Override public void enterDlUnionElem(DdlEmbeddingParser.DlUnionElemContext ctx) {
            pvsSpec += "ASSIGN((: (";
        }
        @Override public void exitDlUnionElem(DdlEmbeddingParser.DlUnionElemContext ctx) {
            pvsSpec += ") :))";
        }
        @Override public void enterDlTestStatement(DdlEmbeddingParser.DlTestStatementContext ctx) {
            pvsSpec += "TEST(";
        }
        @Override public void exitDlTestStatement(DdlEmbeddingParser.DlTestStatementContext ctx) {
            pvsSpec += ")";
        }
        @Override public void enterDlParallelDiffStatement(DdlEmbeddingParser.DlParallelDiffStatementContext ctx) {
            pvsSpec += "DIFF((: ";
            // System.out.print(pvsSpec);  
        }
        @Override public void exitDlParallelDiffStatement(DdlEmbeddingParser.DlParallelDiffStatementContext ctx) {
            pvsSpec += " :))";
        }
        @Override public void enterDlParallelAssignmentStatement(DdlEmbeddingParser.DlParallelAssignmentStatementContext ctx) {
            pvsSpec += "ASSIGN((: ";
        }
        @Override public void exitDlParallelAssignmentStatement(DdlEmbeddingParser.DlParallelAssignmentStatementContext ctx) {
            pvsSpec += " :))";
        }
        @Override public void enterDlAssignmentElem(DdlEmbeddingParser.DlAssignmentElemContext ctx) {
            pvsSpec += "(";
        }
        @Override public void exitDlAssignmentElem(DdlEmbeddingParser.DlAssignmentElemContext ctx) {
            pvsSpec += ")";
        }
        @Override public void enterFormulaDeclaration(DdlEmbeddingParser.FormulaDeclarationContext ctx) { 
            nFormulas++;
        }
    }
}