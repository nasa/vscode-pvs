import org.antlr.v4.runtime.*;
import java.util.*;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.tree.*;
import org.antlr.v4.runtime.misc.Interval;


public class ParserUtils {
    /**
     * Scope of a declaration, in the form of a range in the document { start: { line: NUMBER, character: NUMBER }, end: { line: NUMBER, character: NUMBER } }.
     */
    public static class Range {
        int start_line;
        int start_character;
        int stop_line;
        int stop_character;
        Range (int start_line, int start_character, int stop_line, int stop_character) {
            this.start_line = start_line;
            this.start_character = start_character;
            this.stop_line = stop_line;
            this.stop_character = stop_character;
        }
        public String toString () {
            return "{ \"start\": "
                + "{ \"line\": " + this.start_line
                + ", \"character\": " + this.start_character
                + " }, \"end\": "
                + "{ \"line\": " + this.stop_line
                + ", \"character\": " + this.stop_character
                + " } }";
        }
    }
    /**
     * Declaration descriptor. 
     * Includes the following information: position (line, character), name of the identifier, full declaration of the identifier
     */
    public static class DeclDescriptor {
        int line;
        int character;
        String theoryName;
        String identifier;
        String declaration;
        Range scope;
        String fname;
        ParserRuleContext ctx; // this is a pointer to the original token created by the lexer
        DeclDescriptor (String identifier, String theoryName, int line, int character, String declaration, Range scope, String fname, ParserRuleContext ctx) {
            this.line = line;
            this.character = character;
            this.identifier = identifier;
            this.theoryName = theoryName;
            this.declaration = declaration;
            this.scope = scope;
            this.fname = fname;
            this.ctx = ctx;
        }
        public String toString () {
            return "{ \"line\": " + this.line
                + ", \"character\": " + this.character
                + ", \"identifier\": \"" + this.identifier + "\""
                + ", \"theoryName\": \"" + this.theoryName + "\""
                + ", \"declaration\": \"" + this.declaration + "\""
                + ", \"scope\": " + this.scope
                + " }";
        }
        public String outline () {
            return "{ \"line\": " + this.line
                + ", \"character\": " + this.character
                + ", \"identifier\": \"" + this.identifier + "\""
                + ", \"theoryName\": \"" + this.theoryName + "\""
                + " }";
        }
    }
    /**
     * Find the scope of an identifier
     * @param identifier (String) Name of the identifier
     * @param ctx (ParserRuleContext) ANTLR context of the identifier
     */
    public static Range findScope (String identifier, ParserRuleContext ctx) {
        ParserRuleContext candidate = ctx.getParent();
        while (candidate != null) {
            if (candidate instanceof PvsLanguageParser.TypeDeclarationContext
                    || candidate instanceof PvsLanguageParser.FormulaDeclarationContext
                    // || candidate instanceof PvsLanguageParser.VarDeclarationContext
                    || candidate instanceof PvsLanguageParser.FunctionDeclarationContext
                    || candidate instanceof PvsLanguageParser.JudgementDeclarationContext
                    || candidate instanceof PvsLanguageParser.ConversionDeclarationContext
                    || candidate instanceof PvsLanguageParser.AutorewriteDeclarationContext
                    || candidate instanceof PvsLanguageParser.LambdaBodyContext
                    || candidate instanceof PvsLanguageParser.TheoryContext) {
                // scope goes from the end of the declaration to the end of the parent node
                Token start = ctx.getStop();
                Token stop = candidate.getStop();
                int start_line = start.getLine();
                int from_col = start.getCharPositionInLine() + 1 + start.getText().length();
                int stop_line = stop.getLine();
                int to_col = stop.getCharPositionInLine() + 1 + stop.getText().length();
                // System.out.println("\nSCOPE of " + identifier + " is " + ((candidate instanceof PvsLanguageParser.TheoryContext) ? "THEORY" : getSource(candidate)));
                // System.out.println("start " + start_line + "(" + from_col + ")");
                // System.out.println("stop " + stop_line + "(" + to_col + ")");
                return new Range(start_line, from_col, stop_line, to_col);
            }
            candidate = candidate.getParent();
        }
        return null;
    }
    public static String findScopeName (ParserRuleContext ctx) {
        String scopeName = null;
        RuleContext candidate = ctx.parent;
        while (candidate != null) {
            if (candidate instanceof PvsLanguageParser.FormulaDeclarationContext) {
                PvsLanguageParser.FormulaDeclarationContext c = (PvsLanguageParser.FormulaDeclarationContext) candidate;
                scopeName = c.identifier().getText();
                break;
            }
            if (candidate instanceof PvsLanguageParser.FunctionDeclarationContext) {
                PvsLanguageParser.FunctionDeclarationContext c = (PvsLanguageParser.FunctionDeclarationContext) candidate;
                if (c.functionName() != null) {
                    scopeName =  c.functionName().getText();
                }
            }
            candidate = candidate.parent;
        }
        return scopeName;
    }
    /**
     * Returns the fragment of source code associated to a given context
     * @param ctx (ParserRuleContex) context for which the source code should be returned
     */
    public static String getSource (ParserRuleContext ctx) {
        Token start = ctx.getStart();
        Token stop = ctx.getStop();
        CharStream cs = start.getInputStream();
        Interval interval = new Interval(start.getStartIndex(), stop.getStopIndex());
        String src = cs.getText(interval);
        return src;
    }

    // /**
    //  * Returns the name of the terms in a given expression context
    //  * @param ctx (ExprContext) the expression context
    //  */
    // public static ArrayList<String> getTerms (PvsLanguageParser.ExprContext ctx) {
    //     if (ctx != null) {
    //         if (ctx instanceof PvsLanguageParser.TermExprContext) {
    //             ArrayList<String> ans = new ArrayList<String>();
    //             ans.add(((PvsLanguageParser.TermExprContext) ctx).term().getText());
    //             System.out.println("[getTerms] " + ans.get(0));
    //             return ans;
    //         }
    //         if (ctx instanceof PvsLanguageParser.BinaryOpExprContext) {
    //             ArrayList<String> left = getTerms(((PvsLanguageParser.BinaryOpExprContext) ctx).expr().get(0));
    //             ArrayList<String> right = getTerms(((PvsLanguageParser.BinaryOpExprContext) ctx).expr().get(1));
    //             if (left != null) {
    //                 left.addAll(right);
    //                 return left;
    //             }
    //             return right;
    //         }

    //         if (ctx instanceof PvsLanguageParser.ParenExprContext) {
    //             return getTerms(((PvsLanguageParser.ParenExprContext) ctx).expr());
    //         }
    //     }
    //     return null;
    // }

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

    public static String makeForall (ArrayList<String> terms) {
        String ans = "";
        if (terms != null && terms.size() > 0) {
            ans += "FORALL (";
            int n = terms.size();
            for (int i = 0; i < n; i++) {
                ans += terms.get(i);
                if (i < n - 1) { ans += ", "; };
            }
            ans += "):\n  ";
        }
        return ans;
    }

    public static String makeTccDeclaration (String tccName, String tccDecl) {
        return tccName + ": OBLIGATION\n  " + tccDecl;
    }
}