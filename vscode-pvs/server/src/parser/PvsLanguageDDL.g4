grammar PvsLanguageDDL;
import PvsDDL, PvsLanguage; // IMPORTANT: PvsDDL must be imported before PvsLanguage because antlr checks the rules sequentially and we want to override some PvsLanguage rules with those in PvsDDL

// override
formulaDeclaration
	: identifier ':' K_FORMULA formulaDefinition
    | identifier ':' K_PROGRAM dlProgram
	;