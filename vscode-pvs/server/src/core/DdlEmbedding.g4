grammar DdlEmbedding;
import DdlLanguage, PvsLanguage; // IMPORTANT: PvsDDL must be imported before PvsLanguage because antlr checks the rules sequentially and we want to override some PvsLanguage rules with those in PvsDDL

// override
formulaDeclaration
	: identifier ':' K_FORMULA formulaDefinition
    | hpEmbedding
	;

hpEmbedding
	: identifier ':' K_PROBLEM dlProblem
	;

// hpTheoryPart
// 	: (importing | declaration)*
// 	;