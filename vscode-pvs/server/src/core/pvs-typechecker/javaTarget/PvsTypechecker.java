import org.antlr.v4.runtime.*;
import java.util.*;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.tree.*;
import org.antlr.v4.runtime.misc.Interval;


public class PvsTypechecker extends PvsParser {
    class TypecheckerRules {
        // error listener
        protected TypecheckErrorListener tel = null;

        /**
        * constructor
        */ 
        TypecheckerRules (TypecheckErrorListener tel, PvsParser.PvsContext initialContext) {
            this.tel = tel;
            this.context = initialContext;
        }
        /**
        * data structures for storing context information
        */
        protected PvsParser.PvsContext context;

        /** 
        * utility function for setting the current context
        */
        public void updateContext (PvsParser.PvsContext context) {
            this.context = context;
        }

        //--------------------------------
        // Typechecker rules
        //--------------------------------

        /**
        * Implementation of 'type-rule'
        * Types are pre-types that typecheck correctly in a given context. 
        * A pre-type T is said to be fresh in a context Γ if it does not appear in the left-hand side 
        * of any type declaration in Γ.
        */
        boolean checkTypeRule (String id, Token token) {
            ParserUtils.DeclDescriptor p = context.getType(id);
            boolean success = p == null;
            if (!success) {
                // typecheck error: duplicate identifier
                int line = token.getLine();
                int col = token.getCharPositionInLine();
                String msg = "Duplicate type declaration '" + id + "'\\n" +
                    "'" + id + "' was also declared in " + p.fname + " (Ln " + p.line + ", Col " + p.character + ")";
                tel.typecheckError(null, token, line, col, msg, null);
            }
            return success;
        }

        /**
        * Implementation of 'term-rule'
        * Terms are pre-terms that typecheck correctly in a given context. 
        * A pre-term a is said to be fresh in a context Γ, where Γ = (∆′, A : TYPE, ∆), 
        * if it does not appear in the left-hand side of any term declaration in Γ.
        */
        boolean checkTermRule (String id, Token token) {
            ParserUtils.DeclDescriptor p = context.getTerm(id);
            boolean success = p == null;
            if (!success) {
                int line = token.getLine();
                int col = token.getCharPositionInLine();
                // typecheck error: duplicate identifier
                String msg = "Duplicate term declaration '" + id + "'\\n" +
                    "'" + id + "' was also declared in " + p.fname + " (Ln " + p.line + ", Col " + p.character + ")";
                tel.typecheckError(null, token, line, col, msg, null);
            }
            return success;
        }

        /**
         * builtin-type-rule
         * Built-in types int, nat, rat, bool, string are type
         */
        // boolean checkBuiltInRule (String id, Token token) {

        // }

    }


    public class TccDescriptor {
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


    class TypecheckErrorListener extends PvsParser.ErrorListener {
        public void typecheckError(Recognizer<?, ?> recognizer,
                                Token offendingSymbol,
                                int line, int col,
                                String message,
                                RecognitionException e) {
            String start = "{ \"line\": " + line +", \"character\": " + col + " }";
            int len = offendingSymbol.getStopIndex() - offendingSymbol.getStartIndex();
            String end = "{ \"line\": " + line + ", \"character\": " + (col + 1 + len) + "}";
            // String end = "{ line: " + line + ", character: " + len + "}";
            String range = "{ \"start\": " + start + ", \"end\": " + end + "}";
            String diag = "{ \"range\": " + range + ", \"message\": \"" + message + "\", \"severity\": " + PvsParser.DiagnosticSeverity.Error + " }";
            this.errors.add(diag);
        }

    }



    protected PvsTypecheckerListener typecheckerListener = null;
    protected double typecheckTime = 0;
    protected PvsTypecheckerListener tclistener = null;
    protected TypecheckErrorListener tel = null;

    /**
     * typecheck a given input file
     */
    public void typecheckFile (String ifname) throws java.io.IOException {
        this.ifname = ifname;
        typecheck();
    }
    public void typecheck () throws java.io.IOException {
        double typecheckStart = System.currentTimeMillis();

        CharStream input = CharStreams.fromFileName(ifname);
        lexer = new PvsLanguageLexer(input);
        tokens = new CommonTokenStream(lexer);
        parser = new PvsLanguageParser(tokens);
        parser.removeErrorListeners(); // remove default error listener
        errorListener = new ErrorListener();
        errorHandler = new ErrorHandler();
        tel = new TypecheckErrorListener();
        parser.addErrorListener(errorListener); // add new error listener
        parser.addErrorListener(tel); // add new error listener
        parser.setErrorHandler(errorHandler);
        
        // parser.setBuildParseTree(false); // disable tree creation? This doesn't seem to have any effect on parsing speed
        tree = parser.parse();
        walker = new ParseTreeWalker();
        listener = new PvsTypecheckerListener(tokens, ifname, tel);
        walker.walk(listener, tree);

        typecheckTime = System.currentTimeMillis() - typecheckStart;
    }

    @Override
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
            + ",\n \"parse-time\": { \"ms\": " + parseTime + " }"
            + ",\n \"typecheck-time\": { \"ms\": " + typecheckTime + " }";
        if (this.tel.errors.size() > 0) {
            outline += ",\n \"typecheck-errors\":" + tel.errors;
        }
        outline += "\n}";
        return outline;
    }

    public class PvsTypecheckerListener extends PvsParser.PvsParserListener {
        protected ArrayList<TccDescriptor> tccs = new ArrayList<TccDescriptor>();

        protected TypecheckerRules rules;

        PvsTypecheckerListener (BufferedTokenStream tokens, String ifname, TypecheckErrorListener tel) {
            super(tokens, ifname);
            rules = new TypecheckerRules(tel, this.context);
        }

        @Override public void enterTypeDeclaration(PvsLanguageParser.TypeDeclarationContext ctx) {            
            ListIterator<PvsLanguageParser.TypeNameContext> it = ctx.typeName().listIterator();
            while (it.hasNext()) {
                PvsLanguageParser.TypeNameContext ictx = it.next();
                String id = ictx.getText();
                Token token = ictx.getStart();

                if (
                    rules.checkTypeRule(id, token) // check type-rule
                ) {
                    saveTypeDeclaration(ctx, ictx);
                    rules.updateContext(context);
                }
            }
        }
        // @Override public void enterFormulaDeclaration(PvsLanguageParser.FormulaDeclarationContext ctx) {
        //     Token start = ctx.getStart();
        //     Token stop = ctx.getStop();
        //     String id = ctx.identifier().getText();
        //     // check if declaration is fresh
        //     ParserUtils.DeclDescriptor p = context.getFormula(id);
        //     if (p == null) {
        //         // formula is fresh
        //         saveFormulaDeclaration(ctx);
        //         rules.updateContext(context);
        //     } else {
        //         // typecheck error: duplicate identifier
        //         int line = start.getLine();
        //         int col = start.getCharPositionInLine();
        //         String msg = "Duplicate formula '" + id + "'\\n" +
        //             "'" + id + "' was also declared at " + p.fname + " (Ln " + p.line + ", Col " + p.character + ")";
        //         tel.typecheckError(null, start, line, col, msg, null);
        //     }
        // }
        @Override public void enterVarDeclaration(PvsLanguageParser.VarDeclarationContext ctx) {
            // TODO: issue warning for var declarations
        }
        @Override public void enterConstantDeclaration(PvsLanguageParser.ConstantDeclarationContext ctx) {
            ListIterator<PvsLanguageParser.ConstantNameContext> it = ctx.constantName().listIterator();
            while (it.hasNext()) {
                PvsLanguageParser.ConstantNameContext ictx = it.next();
                String id = ictx.getText();
                Token token = ictx.getStart();

                if (
                    rules.checkTermRule(id, token) // check type-rule
                ) {
                    saveConstantDeclaration(ctx, ictx);
                    rules.updateContext(context);
                }
            }
        }

        @Override public void enterFunctionDeclaration(PvsLanguageParser.FunctionDeclarationContext ctx) {
            String id = ctx.getText();
            Token token = ctx.getStart();

            if (
                rules.checkTermRule(id, token) // check type-rule
            ) {
                saveFunctionDeclaration(ctx);
                rules.updateContext(context);
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

    /**
     * command-line entry point
     */
    public static void main(String[] args) throws Exception {
        // open file
        if (args != null && args.length > 0) {
            PvsTypechecker typechecker = new PvsTypechecker();
            typechecker.parseCliArgs(args);
            if (typechecker.ifname != null) {
                if (typechecker.test) { System.out.println("Typechecking file " + typechecker.ifname); }
                typechecker.typecheck();
                System.out.println(typechecker.getOutline());
            } else {
                System.out.println("Please specify file name to be parsed");
            }
        }
    }
}