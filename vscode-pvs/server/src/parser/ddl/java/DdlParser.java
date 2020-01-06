import org.antlr.v4.runtime.*;
import java.util.*;
import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.tree.*;
import org.antlr.v4.runtime.misc.Interval;

public class DdlParser {
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
    public static class Ddl2Pvs extends DdlEmbeddingBaseListener {        
        public String getSource (ParserRuleContext ctx) {
            Token start = ctx.getStart();
            Token stop = ctx.getStop();
            CharStream cs = start.getInputStream();
            Interval interval = new Interval(start.getStartIndex(), stop.getStopIndex());
            String src = cs.getText(interval);
            return src;
        }
        @Override public void enterHpEmbedding(DdlEmbeddingParser.HpEmbeddingContext ctx) {
            // grab the original ddl problem and place it as comment in the pvs file
            DdlEmbeddingParser.DlProblemContext problem = ctx.dlProblem();
            String src = getSource(ctx);

            String comment = "%---------------------------------\n% ";
            // comment += problem.initCondition().getText().replaceAll("\n", "\n %");
            // comment += "\n% |- " + problem.dlProgram().getText().replaceAll("\n", "\n %");
            comment += src.replaceAll("\n", "\n% ");;
            comment += "\n%---------------------------------\n";
            System.out.print(comment);

            String pvsSpec = ctx.identifier().getText() + ": LEMMA\n";
            System.out.print(pvsSpec);
            // System.out.println(ctx.getText());
        }
        @Override public void exitHpEmbedding(DdlEmbeddingParser.HpEmbeddingContext ctx) {
            System.out.println("\n");
        }
        @Override public void enterDlBooleanExpression(DdlEmbeddingParser.DlBooleanExpressionContext ctx) {
            // String pvsSpec = ctx.getText();
            // System.out.print(pvsSpec);
        }
        @Override public void enterDlInvariant(DdlEmbeddingParser.DlInvariantContext ctx) {
            System.out.println(",");
        }
        @Override public void enterDlAllRunsProgram(DdlEmbeddingParser.DlAllRunsProgramContext ctx) {
            String pvsSpec = "ALLRUNS(";
            System.out.print(pvsSpec);
        }
        @Override public void exitDlAllRunsProgram(DdlEmbeddingParser.DlAllRunsProgramContext ctx) {
            String pvsSpec = ")";
            System.out.print(pvsSpec);
        }
        @Override public void enterDlSequentialAssignment(DdlEmbeddingParser.DlSequentialAssignmentContext ctx) {
            String pvsSpec = "SEQ(";
            System.out.print(pvsSpec);  
        }
        @Override public void exitDlSequentialAssignment(DdlEmbeddingParser.DlSequentialAssignmentContext ctx) {
            String pvsSpec = ")";
            System.out.print(pvsSpec);
        }
        @Override public void enterDlDiffAssignment(DdlEmbeddingParser.DlDiffAssignmentContext ctx) {
            String pvsSpec = "DIFF((: ";
            System.out.print(pvsSpec);  
        }
        @Override public void exitDlDiffAssignment(DdlEmbeddingParser.DlDiffAssignmentContext ctx) {
            String pvsSpec = " :)";
            if (ctx.dlDiffInvariant() == null) {
                pvsSpec += ", DLTRUE";
            }
            pvsSpec += ")";
            System.out.print(pvsSpec);  
        }
        @Override public void enterDlDiffAssignmentElem(DdlEmbeddingParser.DlDiffAssignmentElemContext ctx) {
            String pvsSpec = "(";
            System.out.print(pvsSpec);
        }
        @Override public void exitDlDiffAssignmentElem(DdlEmbeddingParser.DlDiffAssignmentElemContext ctx) {
            String pvsSpec = ")";
            System.out.print(pvsSpec);
        }
        @Override public void enterDlSimpleAssignment(DdlEmbeddingParser.DlSimpleAssignmentContext ctx) {
            String pvsSpec = "ASSIGN((: (";
            System.out.print(pvsSpec); 
        }
        @Override public void exitDlSimpleAssignment(DdlEmbeddingParser.DlSimpleAssignmentContext ctx) {
            String pvsSpec = ") :))";
            System.out.print(pvsSpec); 
        }
        @Override public void exitOperatorColon(DdlEmbeddingParser.OperatorColonContext ctx) {
            // separates two sequential assignments
            String pvsSpec = ", ";
            System.out.print(pvsSpec); 
        }
        @Override public void enterDlStarProgram(DdlEmbeddingParser.DlStarProgramContext ctx) {
            String pvsSpec = "STAR(";
            System.out.print(pvsSpec);  
        }
        @Override public void exitDlStarProgram(DdlEmbeddingParser.DlStarProgramContext ctx) {
            String pvsSpec = ")";
            System.out.print(pvsSpec);
        }
        @Override public void enterParenLeft(DdlEmbeddingParser.ParenLeftContext ctx) {
            String pvsSpec = ctx.getText();
            System.out.print(pvsSpec);
        }
        @Override public void enterParenRight(DdlEmbeddingParser.ParenRightContext ctx) {
            String pvsSpec = ctx.getText();
            System.out.print(pvsSpec);
        }
        @Override public void enterOperatorEntail(DdlEmbeddingParser.OperatorEntailContext ctx) {
            String pvsSpec = "\n" + ctx.getText() + " ";
            System.out.print(pvsSpec);
        }
        @Override public void enterOperatorAssign(DdlEmbeddingParser.OperatorAssignContext ctx) {
            String pvsSpec = ", ";
            System.out.print(pvsSpec);
        }
        @Override public void enterDlConst(DdlEmbeddingParser.DlConstContext ctx) {
            String pvsSpec = "cnst(" + ctx.getText() + ")";
            System.out.print(pvsSpec);             
        }
        @Override public void enterDlDiffIdentifier(DdlEmbeddingParser.DlDiffIdentifierContext ctx) {
            String pvsSpec = ctx.getText().replace("'", "");
            System.out.print(pvsSpec); 
        }
        @Override public void enterDlIdentifier(DdlEmbeddingParser.DlIdentifierContext ctx) {
            String pvsSpec = ctx.getText();
            System.out.print(pvsSpec); 
        }
        @Override public void enterOperatorNOT(DdlEmbeddingParser.OperatorNOTContext ctx) {
            String pvsSpec = " " + ctx.getText() + " ";
            System.out.print(pvsSpec);            
        }
        @Override public void enterOperatorOR(DdlEmbeddingParser.OperatorORContext ctx) {
            String pvsSpec = " " + ctx.getText() + " ";
            System.out.print(pvsSpec);
        }
        @Override public void enterOperatorAND(DdlEmbeddingParser.OperatorANDContext ctx) {
            String pvsSpec = " " + ctx.getText() + " ";
            System.out.print(pvsSpec);
        }
        @Override public void enterOperatorCMP(DdlEmbeddingParser.OperatorCMPContext ctx) {
            String pvsSpec = " " + ctx.getText() + " ";
            System.out.print(pvsSpec);
        }
        @Override public void enterOperatorPlusMinus(DdlEmbeddingParser.OperatorPlusMinusContext ctx) {
            String pvsSpec = " " + ctx.getText() + " ";
            System.out.print(pvsSpec);
        }
    	@Override public void enterOperatorMulDiv(DdlEmbeddingParser.OperatorMulDivContext ctx) {
            String pvsSpec = " " + ctx.getText() + " ";
            System.out.print(pvsSpec);
        }
        @Override public void enterOperatorExp(DdlEmbeddingParser.OperatorExpContext ctx) {
            String pvsSpec = ctx.getText();
            System.out.print(pvsSpec);
        }
        @Override public void enterDlValue(DdlEmbeddingParser.DlValueContext ctx) {
            String pvsSpec = "val(" + ctx.getText() + ")";
            System.out.print(pvsSpec);
        }
    }
    public static void main(String[] args) throws Exception {
        // open file
        if (args != null && args.length > 0) {
            System.out.println("Parsing file " + args[0]);
            CharStream input = CharStreams.fromFileName(args[0]);
            DdlEmbeddingLexer lexer = new DdlEmbeddingLexer(input);
            CommonTokenStream tokens = new CommonTokenStream(lexer);
            DdlEmbeddingParser parser = new DdlEmbeddingParser(tokens);
            parser.removeErrorListeners(); // remove ConsoleErrorListener
            ErrorListener el = new ErrorListener();
            parser.addErrorListener(el); // add new error listener
            ParserRuleContext tree = parser.parse(); // parse as usual
            if (el.errors.size() > 0) {
                System.out.println(el.errors);
            }
            System.out.println("Done!");
            // walk the tree
            ParseTreeWalker walker = new ParseTreeWalker();
            Ddl2Pvs listener = new Ddl2Pvs();
            walker.walk(listener, tree);
        }
    }
}