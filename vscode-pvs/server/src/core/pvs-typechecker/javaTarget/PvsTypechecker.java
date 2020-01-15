import org.antlr.v4.runtime.*;
import java.util.*;
import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.tree.*;
import org.antlr.v4.runtime.misc.Interval;

public class PvsTypechecker {
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
            String diag = "{ \"range\": " + range + ", \"message\": \"" + message + "\", \"severity\": " + PvsParser.DiagnosticSeverity.Error + " }";
            this.errors.add(diag);
        }

    }
    public static class ErrorHandler extends DefaultErrorStrategy {
        // @Override public void reportNoViableAlternative(Parser parser, NoViableAltException e) {
        //     parser.notifyErrorListeners(e.getOffendingToken(), "Syntax error", e);
        // }
    }
    public static class TccDescriptor {
        int line;
        int character;
        String tccType;
        String id;
        String tccBody;
        TccDescriptor (String id, int line, int character, String tccType, String tccBody) {
            this.id = id;
            this.line = line;
            this.character = character;
            this.tccType = tccType;
            this.tccBody = tccBody;
        }
        public String toString () {
            return "{ \"line\": " + this.line
                + ", \"character\": " + this.character
                + ", \"tcc-id\": \"" + this.id + "\""
                + ", \"tcc-type\": \"" + this.tccType + "\""
                + ", \"tcc-tccBody\": \"" + this.tccBody + "\""
                + " }";
        }
    }
    public static void main(String[] args) throws Exception {
        // open file
        if (args != null && args.length > 0) {
            PvsParser.parseCliArgs(args);
            if (PvsParser.test) {
                System.out.println("Typechecking file " + PvsParser.ifname);
            }
            CharStream input = CharStreams.fromFileName(PvsParser.ifname);
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
                if (PvsParser.test) {
                    System.out.println(PvsParser.ifname + " typechecked successfully!");
                }
                // walk the tree
                ParseTreeWalker walker = new ParseTreeWalker();
                PvsTypecheckerListener listener = new PvsTypecheckerListener(tokens);
                walker.walk(listener, tree);
            }
        }
    }
    public static class PvsTypecheckerListener extends PvsParser.PvsParserListener {
        protected ArrayList<TccDescriptor> tccs = new ArrayList<TccDescriptor>();

        PvsTypecheckerListener (BufferedTokenStream tokens) {
            super(tokens);
        }

        @Override public void enterOperatorDiv(PvsLanguageParser.OperatorDivContext ctx) {
            // new subtype tcc (check division by zero)
            String tccContextName = ParserUtils.findScopeName(ctx);
            
            // get operand on the right of operatorDIV
            PvsLanguageParser.BinaryOpExprContext exprContext = (PvsLanguageParser.BinaryOpExprContext) ctx.parent.parent;
            PvsLanguageParser.ExprContext divisorContext = exprContext.expr().get(1);

            // find position of the divisor
            Token start = divisorContext.getStart();
            int line = start.getLine();
            int character = start.getCharPositionInLine();

            if (tccContextName != null) {
                // find name of all terms in the expression
                ArrayList<String> terms = ParserUtils.getTerms(divisorContext);

                // create tcc name
                String tccName = tccContextName + "_TCC" + (this.tccs.size() + 1);

                // create tcc body
                String tccBody = divisorContext.getText() + " /= 0";
                if (terms != null) {
                    // find the declaration of each term in the expression
                    ArrayList<String> termsDecl = new ArrayList<String>();
                    for (String term: terms) {
                        ParserUtils.DeclDescriptor desc = findDeclaration(term);
                        if (desc != null) {
                            termsDecl.add(desc.declaration);
                        }
                    }
                    tccBody = ParserUtils.makeForall(termsDecl) + "  " + tccBody;
                }

                // generate tcc declaration and add it to the list of tccs
                String tccDecl = ParserUtils.makeTccDeclaration(tccName, tccBody);
                this.tccs.add(
                    new TccDescriptor(
                        tccName,
                        line,
                        character,
                        "SUBTYPE_TCC",
                        tccDecl
                    )
                );
            } else {
                System.err.println("[pvs-typechecker] Warning: Operator DIV could not find tcc context name at ln " + line + " col " + character);
            }
            // ListIterator<PvsLanguageParser.BinaryOpContext> it = ctx.binaryOp().listIterator();
            // while (it.hasNext()) {
            //     PvsLanguageParser.BinaryOpContext bctx = it.next();
            //     // System.out.println(bctx.getText());
            //     if (bctx.getText().equals("/")) {
            //         // subtype tcc
            //         Token start = bctx.getStart();
            //         this.tccs.put(
            //             "TCC_" + this.tccCount++,
            //             new TccDescriptor(
            //                 start.getLine(),
            //                 start.getCharPositionInLine(),
            //                 "SUBTYPE_TCC"
            //             )
            //         );
            //     }
            // }
        }

        @Override public void exitTheory(PvsLanguageParser.TheoryContext ctx) {
            if (PvsParser.test) {
                super.exitTheory(ctx);
                if (this.tccs != null) {
                    int n = this.tccs.size();
                    System.out.println("------------------------------");
                    System.out.println(n + " tccs generated");
                    System.out.println("------------------------------");
                    for (TccDescriptor tcc: this.tccs){
                        System.out.println(tcc);
                    }
                }
            }
        }


        // @Override public void enterTypeDeclaration(PvsLanguageParser.TypeDeclarationContext ctx) {
        //     ListIterator<PvsLanguageParser.IdentifierContext> it = ctx.identifier().listIterator();
        //     while (it.hasNext()) {
        //         PvsLanguageParser.IdentifierContext ictx = it.next();
        //         Token start = ictx.getStart();
        //         Token stop = ictx.getStop();
        //         String id = ictx.getText();
        //         this.typeDeclarations.put(id, 
        //             new DeclDescriptor(
        //                 id,
        //                 start.getLine(),
        //                 start.getCharPositionInLine(),
        //                 this.getSource(ctx)
        //             )
        //         );
        //     }
        // }
        // @Override public void enterFormulaDeclaration(PvsLanguageParser.FormulaDeclarationContext ctx) {
        //     Token start = ctx.getStart();
        //     Token stop = ctx.getStop();
        //     String id = ctx.identifier().getText();
        //     this.formulaDeclarations.put(id, 
        //         new DeclDescriptor(
        //             id,
        //             start.getLine(), 
        //             start.getCharPositionInLine(), 
        //             this.getSource(ctx)
        //         )
        //     );
        // }
        // @Override public void exitTheory(PvsLanguageParser.TheoryContext ctx) {
        //     if (test) {
        //         if (this.typeDeclarations != null) {
        //             int n = this.typeDeclarations.size();
        //             System.out.println("------------------------------");
        //             System.out.println(n + " type declarations");
        //             System.out.println("------------------------------");
        //             for (String id: this.typeDeclarations.keySet()){
        //                 System.out.println(id + " " + this.typeDeclarations.get(id));
        //             }
        //         }
        //         if (this.formulaDeclarations != null) {
        //             int n = this.formulaDeclarations.size();
        //             System.out.println("------------------------------");
        //             System.out.println(n + " formula declarations");
        //             System.out.println("------------------------------");
        //             for (String id: this.formulaDeclarations.keySet()){
        //                 System.out.println(id + " " + this.formulaDeclarations.get(id));
        //             }
        //         }
        //     }
        // }
    }
}