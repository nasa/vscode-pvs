import org.antlr.v4.runtime.*;
import java.util.*;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.tree.*;
import org.antlr.v4.runtime.misc.Interval;

public class PvsTypechecker {
    public static class TypecheckErrorListener extends PvsParser.ErrorListener {
        // protected ArrayList<String> errors = new ArrayList<String>(); // array of JSON strings in the form { range: { start: { line: number, character: number }, stop: { line: number, character: number } }, message: string } 

        public void typecheckError(Recognizer<?, ?> recognizer,
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
            String ifname = PvsParser.getInputFileName();
            if (PvsParser.test) {
                System.out.println("Typechecking file " + ifname);
            }
            double parseStart = System.currentTimeMillis();

            CharStream input = CharStreams.fromFileName(ifname);
            PvsLanguageLexer lexer = new PvsLanguageLexer(input);
            CommonTokenStream tokens = new CommonTokenStream(lexer);
            PvsLanguageParser parser = new PvsLanguageParser(tokens);
            parser.removeErrorListeners(); // remove ConsoleErrorListener
            TypecheckErrorListener tel = new TypecheckErrorListener();
            parser.addErrorListener(tel); // add new error listener
            PvsParser.ErrorHandler eh = new PvsParser.ErrorHandler();
            parser.setErrorHandler(eh);
            // parser.setBuildParseTree(false); // disable parse tree creation, to speed up parsing
            ParserRuleContext tree = parser.parse(); // parse as usual
            double parseEnd = System.currentTimeMillis();
            double parseTime = parseEnd - parseStart;

            if (tel.errors.size() > 0) {
                // these are parse errors
                System.out.println(tel.errors);
            } else {
                // walk the tree
                ParseTreeWalker walker = new ParseTreeWalker();
                PvsTypecheckerListener listener = new PvsTypecheckerListener(tokens, ifname);
                listener.addErrorListener(tel);
                walker.walk(listener, tree);
                double typecheckTime = System.currentTimeMillis() - parseEnd;
                // if (tel.errors.size() > 0) {
                //     // after the walker, these are typecheck errors
                //     System.out.println(tel.errors);
                // }

                if (PvsParser.outlineRequested()) {
                    String outline = "{" 
                        + "\n \"contextFolder\": \"" + ParserUtils.getContextFolder(ifname) + "\""
                        + ",\n \"fileName\": \"" + ParserUtils.getFileName(ifname) + "\""
                        + ",\n \"fileExtension\": \"" + ParserUtils.getFileExtension(ifname) + "\""
                        + ",\n \"declarations\": {"
                            + "\n\t \"types\": [ " + listener.printTypes() + " ]"
                            + ",\n\t \"functions\": [ " + listener.printFunctions() + " ]"
                            + ",\n\t \"formulas\": [ " + listener.printFormulas() + " ]"
                            + ",\n\t \"locals\": [ " + listener.printLocals() + " ]"
                        + "\n}"
                        + ",\n \"parse-time\": { \"ms\": " + parseTime + " }"
                        + ",\n \"typecheck-time\": { \"ms\": " + typecheckTime + " }";
                    if (tel.errors.size() > 0) {
                        outline += ",\n \"typecheck-errors\":" + tel.errors;
                    }
                    System.out.println(outline);
                    return;
                }

            }
        }
    }
    public static class PvsTypecheckerListener extends PvsParser.PvsParserListener {
        // the data structures representing the context are in PvsParser.PvsParserListener:
        // typeDeclarations, formulaDeclarations, functionDeclarations, localBindingDeclarations

        protected TypecheckErrorListener tel;

        protected ArrayList<TccDescriptor> tccs = new ArrayList<TccDescriptor>();

        PvsTypecheckerListener (BufferedTokenStream tokens, String ifname) {
            super(tokens, ifname);
        }

        void addErrorListener (TypecheckErrorListener tel) {
            this.tel = tel;
        }

        /**
         * type-rule
         * Types are pre-types that typecheck correctly in a given context. 
         * A pre-type T is said to be fresh in a context Γ if it does not appear in the left-hand side 
         * of any type declaration in Γ.
         */
        @Override public void enterTypeDeclaration(PvsLanguageParser.TypeDeclarationContext ctx) {            
            ListIterator<PvsLanguageParser.TypeNameContext> it = ctx.typeName().listIterator();
            while (it.hasNext()) {
                PvsLanguageParser.TypeNameContext ictx = it.next();
                Token start = ctx.getStart();
                Token stop = ctx.getStop();
                String id = ictx.getText();

                // check if declaration is fresh
                ParserUtils.DeclDescriptor p = typeDeclarations.get(id);
                if (p == null) { // TODO: use full signature?
                    // fresh declaration -> add declaration to the context
                    saveTypeDeclaration(ctx, ictx);
                } else {
                    // typecheck error: duplicate identifier
                    int line = start.getLine();
                    int col = start.getCharPositionInLine();
                    String msg = "Duplicate type identifier '" + id + "' \n" +
                        "'" + id + "' was also declared at " + p.fname + " (Ln " + p.line + ", Col " + p.character + ")";
                    tel.typecheckError(null, start, line, col, msg, null);
                }
            }
        }
        @Override public void enterFormulaDeclaration(PvsLanguageParser.FormulaDeclarationContext ctx) {
            Token start = ctx.getStart();
            Token stop = ctx.getStop();
            String id = ctx.identifier().getText();
            // check if declaration is fresh
            ParserUtils.DeclDescriptor p = formulaDeclarations.get(id);
            if (p == null) {
                saveFormulaDeclaration(ctx);
            } else {
                // typecheck error: duplicate identifier
                int line = start.getLine();
                int col = start.getCharPositionInLine();
                String msg = "Duplicate formula '" + id + "' \n" +
                    "'" + id + "' was also declared at " + p.fname + " (Ln " + p.line + ", Col " + p.character + ")";
                tel.typecheckError(null, start, line, col, msg, null);
            }
        }
        @Override public void enterVarDeclaration(PvsLanguageParser.VarDeclarationContext ctx) {
            // TODO: issue warning for var declarations
        }
        @Override public void enterConstantDeclaration(PvsLanguageParser.ConstantDeclarationContext ctx) {
            ListIterator<PvsLanguageParser.ConstantNameContext> it = ctx.constantName().listIterator();
            while (it.hasNext()) {
                PvsLanguageParser.ConstantNameContext ictx = it.next();
                Token start = ictx.getStart();
                Token stop = ictx.getStop();
                String id = ictx.identifierOrOperator().getText();
                // check if declaration is fresh
                ParserUtils.DeclDescriptor p = constantDeclarations.get(id);
                if (p == null) {
                    saveConstantDeclaration(ctx, ictx);
                } else {
                    // typecheck error: duplicate identifier
                    int line = start.getLine();
                    int col = start.getCharPositionInLine();
                    String msg = "Duplicate constant declaration '" + id + "' \n" +
                        "'" + id + "' was also declared at " + p.fname + " (Ln " + p.line + ", Col " + p.character + ")";
                    tel.typecheckError(null, start, line, col, msg, null);
                }
            }
        }

        @Override public void enterFunctionDeclaration(PvsLanguageParser.FunctionDeclarationContext ctx) {
            Token start = ctx.getStart();
            Token stop = ctx.getStop();
            String id = ctx.functionName().getText();
            // check if declaration is fresh
            ParserUtils.DeclDescriptor p = functionDeclarations.get(id);
            if (p == null) {
                saveFunctionDeclaration(ctx);
            } else {
                // typecheck error: duplicate identifier
                int line = start.getLine();
                int col = start.getCharPositionInLine();
                String msg = "Duplicate function declaration '" + id + "' \n" +
                    "'" + id + "' was also declared at " + p.fname + " (Ln " + p.line + ", Col " + p.character + ")";
                tel.typecheckError(null, start, line, col, msg, null);
            }
        }


        @Override public void enterOperatorDiv(PvsLanguageParser.OperatorDivContext ctx) {
            // new subtype tcc (check division by zero)
            // String tccContextName = ParserUtils.findScopeName(ctx);
            
            // // get operand on the right of operatorDIV
            // PvsLanguageParser.BinaryOpExprContext exprContext = (PvsLanguageParser.BinaryOpExprContext) ctx.parent.parent;
            // PvsLanguageParser.ExprContext divisorContext = exprContext.expr().get(1);

            // // find position of the divisor
            // Token start = divisorContext.getStart();
            // int line = start.getLine();
            // int character = start.getCharPositionInLine();

            // if (tccContextName != null) {
            //     // find name of all terms in the expression
            //     ArrayList<String> terms = null;//ParserUtils.getTerms(divisorContext);

            //     // create tcc name
            //     String tccName = tccContextName + "_TCC" + (this.tccs.size() + 1);

            //     // create tcc body
            //     String tccBody = divisorContext.getText() + " /= 0";
            //     if (terms != null) {
            //         // find the declaration of each term in the expression
            //         ArrayList<String> termsDecl = new ArrayList<String>();
            //         for (String term: terms) {
            //             ParserUtils.DeclDescriptor desc = findDeclaration(term);
            //             if (desc != null) {
            //                 termsDecl.add(desc.declaration);
            //             }
            //         }
            //         tccBody = ParserUtils.makeForall(termsDecl) + "  " + tccBody;
            //     }

            //     // generate tcc declaration and add it to the list of tccs
            //     String tccDecl = ParserUtils.makeTccDeclaration(tccName, tccBody);
            //     this.tccs.add(
            //         new TccDescriptor(
            //             tccName,
            //             line,
            //             character,
            //             "SUBTYPE_TCC",
            //             tccDecl
            //         )
            //     );
            // } else {
            //     System.err.println("[pvs-typechecker] Warning: Operator DIV could not find tcc context name at ln " + line + " col " + character);
            // }

            
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
            // if (PvsParser.test) {
            //     super.exitTheory(ctx);
            //     if (this.tccs != null) {
            //         int n = this.tccs.size();
            //         System.out.println("------------------------------");
            //         System.out.println(n + " tccs generated");
            //         System.out.println("------------------------------");
            //         for (TccDescriptor tcc: this.tccs){
            //             System.out.println(tcc);
            //         }
            //     }
            // }
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