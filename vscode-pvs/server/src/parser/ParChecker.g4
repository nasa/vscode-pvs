grammar ParChecker;

check
    : parExpr* EOF
    ;

parExpr
    : '(' ')'
    | '(' parExpr+ ')'
    ;

//------------------------
// Whitespace and comments
//------------------------
WS: [ \t\r\n\u000C]+ -> skip;
COMMENT: '%' ~[\r\n]* -> skip;
NON_PAR: ~('(' | ')') -> skip;
UnrecognizedChar: .;
