// Generated from PvsLanguage.g4 by ANTLR 4.5
// jshint ignore: start
var antlr4 = require('antlr4/index');

// This class defines a complete listener for a parse tree produced by PvsLanguageParser.
function PvsLanguageListener() {
	antlr4.tree.ParseTreeListener.call(this);
	return this;
}

PvsLanguageListener.prototype = Object.create(antlr4.tree.ParseTreeListener.prototype);
PvsLanguageListener.prototype.constructor = PvsLanguageListener;

// Enter a parse tree produced by PvsLanguageParser#parse.
PvsLanguageListener.prototype.enterParse = function(ctx) {
};

// Exit a parse tree produced by PvsLanguageParser#parse.
PvsLanguageListener.prototype.exitParse = function(ctx) {
};


// Enter a parse tree produced by PvsLanguageParser#theory.
PvsLanguageListener.prototype.enterTheory = function(ctx) {
};

// Exit a parse tree produced by PvsLanguageParser#theory.
PvsLanguageListener.prototype.exitTheory = function(ctx) {
};


// Enter a parse tree produced by PvsLanguageParser#theoryFormal.
PvsLanguageListener.prototype.enterTheoryFormal = function(ctx) {
};

// Exit a parse tree produced by PvsLanguageParser#theoryFormal.
PvsLanguageListener.prototype.exitTheoryFormal = function(ctx) {
};


// Enter a parse tree produced by PvsLanguageParser#theoryFormalDeclaration.
PvsLanguageListener.prototype.enterTheoryFormalDeclaration = function(ctx) {
};

// Exit a parse tree produced by PvsLanguageParser#theoryFormalDeclaration.
PvsLanguageListener.prototype.exitTheoryFormalDeclaration = function(ctx) {
};


// Enter a parse tree produced by PvsLanguageParser#theoryFormalType.
PvsLanguageListener.prototype.enterTheoryFormalType = function(ctx) {
};

// Exit a parse tree produced by PvsLanguageParser#theoryFormalType.
PvsLanguageListener.prototype.exitTheoryFormalType = function(ctx) {
};


// Enter a parse tree produced by PvsLanguageParser#theoryFormalConst.
PvsLanguageListener.prototype.enterTheoryFormalConst = function(ctx) {
};

// Exit a parse tree produced by PvsLanguageParser#theoryFormalConst.
PvsLanguageListener.prototype.exitTheoryFormalConst = function(ctx) {
};


// Enter a parse tree produced by PvsLanguageParser#assumingPart.
PvsLanguageListener.prototype.enterAssumingPart = function(ctx) {
};

// Exit a parse tree produced by PvsLanguageParser#assumingPart.
PvsLanguageListener.prototype.exitAssumingPart = function(ctx) {
};


// Enter a parse tree produced by PvsLanguageParser#assumingElement.
PvsLanguageListener.prototype.enterAssumingElement = function(ctx) {
};

// Exit a parse tree produced by PvsLanguageParser#assumingElement.
PvsLanguageListener.prototype.exitAssumingElement = function(ctx) {
};


// Enter a parse tree produced by PvsLanguageParser#assumption.
PvsLanguageListener.prototype.enterAssumption = function(ctx) {
};

// Exit a parse tree produced by PvsLanguageParser#assumption.
PvsLanguageListener.prototype.exitAssumption = function(ctx) {
};


// Enter a parse tree produced by PvsLanguageParser#theoryPart.
PvsLanguageListener.prototype.enterTheoryPart = function(ctx) {
};

// Exit a parse tree produced by PvsLanguageParser#theoryPart.
PvsLanguageListener.prototype.exitTheoryPart = function(ctx) {
};


// Enter a parse tree produced by PvsLanguageParser#theoryElement.
PvsLanguageListener.prototype.enterTheoryElement = function(ctx) {
};

// Exit a parse tree produced by PvsLanguageParser#theoryElement.
PvsLanguageListener.prototype.exitTheoryElement = function(ctx) {
};


// Enter a parse tree produced by PvsLanguageParser#theoryDeclaration.
PvsLanguageListener.prototype.enterTheoryDeclaration = function(ctx) {
};

// Exit a parse tree produced by PvsLanguageParser#theoryDeclaration.
PvsLanguageListener.prototype.exitTheoryDeclaration = function(ctx) {
};


// Enter a parse tree produced by PvsLanguageParser#typeDeclaration.
PvsLanguageListener.prototype.enterTypeDeclaration = function(ctx) {
};

// Exit a parse tree produced by PvsLanguageParser#typeDeclaration.
PvsLanguageListener.prototype.exitTypeDeclaration = function(ctx) {
};


// Enter a parse tree produced by PvsLanguageParser#typeDefinition.
PvsLanguageListener.prototype.enterTypeDefinition = function(ctx) {
};

// Exit a parse tree produced by PvsLanguageParser#typeDefinition.
PvsLanguageListener.prototype.exitTypeDefinition = function(ctx) {
};


// Enter a parse tree produced by PvsLanguageParser#formulaDeclaration.
PvsLanguageListener.prototype.enterFormulaDeclaration = function(ctx) {
};

// Exit a parse tree produced by PvsLanguageParser#formulaDeclaration.
PvsLanguageListener.prototype.exitFormulaDeclaration = function(ctx) {
};


// Enter a parse tree produced by PvsLanguageParser#constantDeclaration.
PvsLanguageListener.prototype.enterConstantDeclaration = function(ctx) {
};

// Exit a parse tree produced by PvsLanguageParser#constantDeclaration.
PvsLanguageListener.prototype.exitConstantDeclaration = function(ctx) {
};


// Enter a parse tree produced by PvsLanguageParser#functionDeclaration.
PvsLanguageListener.prototype.enterFunctionDeclaration = function(ctx) {
};

// Exit a parse tree produced by PvsLanguageParser#functionDeclaration.
PvsLanguageListener.prototype.exitFunctionDeclaration = function(ctx) {
};


// Enter a parse tree produced by PvsLanguageParser#recursiveDeclaration.
PvsLanguageListener.prototype.enterRecursiveDeclaration = function(ctx) {
};

// Exit a parse tree produced by PvsLanguageParser#recursiveDeclaration.
PvsLanguageListener.prototype.exitRecursiveDeclaration = function(ctx) {
};


// Enter a parse tree produced by PvsLanguageParser#conversionDeclaration.
PvsLanguageListener.prototype.enterConversionDeclaration = function(ctx) {
};

// Exit a parse tree produced by PvsLanguageParser#conversionDeclaration.
PvsLanguageListener.prototype.exitConversionDeclaration = function(ctx) {
};


// Enter a parse tree produced by PvsLanguageParser#varDeclaration.
PvsLanguageListener.prototype.enterVarDeclaration = function(ctx) {
};

// Exit a parse tree produced by PvsLanguageParser#varDeclaration.
PvsLanguageListener.prototype.exitVarDeclaration = function(ctx) {
};


// Enter a parse tree produced by PvsLanguageParser#arguments.
PvsLanguageListener.prototype.enterArguments = function(ctx) {
};

// Exit a parse tree produced by PvsLanguageParser#arguments.
PvsLanguageListener.prototype.exitArguments = function(ctx) {
};


// Enter a parse tree produced by PvsLanguageParser#typeExpression.
PvsLanguageListener.prototype.enterTypeExpression = function(ctx) {
};

// Exit a parse tree produced by PvsLanguageParser#typeExpression.
PvsLanguageListener.prototype.exitTypeExpression = function(ctx) {
};


// Enter a parse tree produced by PvsLanguageParser#expr.
PvsLanguageListener.prototype.enterExpr = function(ctx) {
};

// Exit a parse tree produced by PvsLanguageParser#expr.
PvsLanguageListener.prototype.exitExpr = function(ctx) {
};


// Enter a parse tree produced by PvsLanguageParser#ifExpression.
PvsLanguageListener.prototype.enterIfExpression = function(ctx) {
};

// Exit a parse tree produced by PvsLanguageParser#ifExpression.
PvsLanguageListener.prototype.exitIfExpression = function(ctx) {
};


// Enter a parse tree produced by PvsLanguageParser#measureExpression.
PvsLanguageListener.prototype.enterMeasureExpression = function(ctx) {
};

// Exit a parse tree produced by PvsLanguageParser#measureExpression.
PvsLanguageListener.prototype.exitMeasureExpression = function(ctx) {
};


// Enter a parse tree produced by PvsLanguageParser#bindingExpression.
PvsLanguageListener.prototype.enterBindingExpression = function(ctx) {
};

// Exit a parse tree produced by PvsLanguageParser#bindingExpression.
PvsLanguageListener.prototype.exitBindingExpression = function(ctx) {
};


// Enter a parse tree produced by PvsLanguageParser#lambdaBindings.
PvsLanguageListener.prototype.enterLambdaBindings = function(ctx) {
};

// Exit a parse tree produced by PvsLanguageParser#lambdaBindings.
PvsLanguageListener.prototype.exitLambdaBindings = function(ctx) {
};


// Enter a parse tree produced by PvsLanguageParser#lambdaBinding.
PvsLanguageListener.prototype.enterLambdaBinding = function(ctx) {
};

// Exit a parse tree produced by PvsLanguageParser#lambdaBinding.
PvsLanguageListener.prototype.exitLambdaBinding = function(ctx) {
};


// Enter a parse tree produced by PvsLanguageParser#bindings.
PvsLanguageListener.prototype.enterBindings = function(ctx) {
};

// Exit a parse tree produced by PvsLanguageParser#bindings.
PvsLanguageListener.prototype.exitBindings = function(ctx) {
};


// Enter a parse tree produced by PvsLanguageParser#binding.
PvsLanguageListener.prototype.enterBinding = function(ctx) {
};

// Exit a parse tree produced by PvsLanguageParser#binding.
PvsLanguageListener.prototype.exitBinding = function(ctx) {
};


// Enter a parse tree produced by PvsLanguageParser#typeId.
PvsLanguageListener.prototype.enterTypeId = function(ctx) {
};

// Exit a parse tree produced by PvsLanguageParser#typeId.
PvsLanguageListener.prototype.exitTypeId = function(ctx) {
};


// Enter a parse tree produced by PvsLanguageParser#typeIds.
PvsLanguageListener.prototype.enterTypeIds = function(ctx) {
};

// Exit a parse tree produced by PvsLanguageParser#typeIds.
PvsLanguageListener.prototype.exitTypeIds = function(ctx) {
};


// Enter a parse tree produced by PvsLanguageParser#letBindings.
PvsLanguageListener.prototype.enterLetBindings = function(ctx) {
};

// Exit a parse tree produced by PvsLanguageParser#letBindings.
PvsLanguageListener.prototype.exitLetBindings = function(ctx) {
};


// Enter a parse tree produced by PvsLanguageParser#letBinding.
PvsLanguageListener.prototype.enterLetBinding = function(ctx) {
};

// Exit a parse tree produced by PvsLanguageParser#letBinding.
PvsLanguageListener.prototype.exitLetBinding = function(ctx) {
};


// Enter a parse tree produced by PvsLanguageParser#letBind.
PvsLanguageListener.prototype.enterLetBind = function(ctx) {
};

// Exit a parse tree produced by PvsLanguageParser#letBind.
PvsLanguageListener.prototype.exitLetBind = function(ctx) {
};


// Enter a parse tree produced by PvsLanguageParser#functionExpression.
PvsLanguageListener.prototype.enterFunctionExpression = function(ctx) {
};

// Exit a parse tree produced by PvsLanguageParser#functionExpression.
PvsLanguageListener.prototype.exitFunctionExpression = function(ctx) {
};


// Enter a parse tree produced by PvsLanguageParser#assignmentExpression.
PvsLanguageListener.prototype.enterAssignmentExpression = function(ctx) {
};

// Exit a parse tree produced by PvsLanguageParser#assignmentExpression.
PvsLanguageListener.prototype.exitAssignmentExpression = function(ctx) {
};


// Enter a parse tree produced by PvsLanguageParser#bindingDeclaration.
PvsLanguageListener.prototype.enterBindingDeclaration = function(ctx) {
};

// Exit a parse tree produced by PvsLanguageParser#bindingDeclaration.
PvsLanguageListener.prototype.exitBindingDeclaration = function(ctx) {
};


// Enter a parse tree produced by PvsLanguageParser#recordType.
PvsLanguageListener.prototype.enterRecordType = function(ctx) {
};

// Exit a parse tree produced by PvsLanguageParser#recordType.
PvsLanguageListener.prototype.exitRecordType = function(ctx) {
};


// Enter a parse tree produced by PvsLanguageParser#functionType.
PvsLanguageListener.prototype.enterFunctionType = function(ctx) {
};

// Exit a parse tree produced by PvsLanguageParser#functionType.
PvsLanguageListener.prototype.exitFunctionType = function(ctx) {
};


// Enter a parse tree produced by PvsLanguageParser#tupleType.
PvsLanguageListener.prototype.enterTupleType = function(ctx) {
};

// Exit a parse tree produced by PvsLanguageParser#tupleType.
PvsLanguageListener.prototype.exitTupleType = function(ctx) {
};


// Enter a parse tree produced by PvsLanguageParser#subtype.
PvsLanguageListener.prototype.enterSubtype = function(ctx) {
};

// Exit a parse tree produced by PvsLanguageParser#subtype.
PvsLanguageListener.prototype.exitSubtype = function(ctx) {
};


// Enter a parse tree produced by PvsLanguageParser#name.
PvsLanguageListener.prototype.enterName = function(ctx) {
};

// Exit a parse tree produced by PvsLanguageParser#name.
PvsLanguageListener.prototype.exitName = function(ctx) {
};


// Enter a parse tree produced by PvsLanguageParser#actuals.
PvsLanguageListener.prototype.enterActuals = function(ctx) {
};

// Exit a parse tree produced by PvsLanguageParser#actuals.
PvsLanguageListener.prototype.exitActuals = function(ctx) {
};


// Enter a parse tree produced by PvsLanguageParser#enumerationType.
PvsLanguageListener.prototype.enterEnumerationType = function(ctx) {
};

// Exit a parse tree produced by PvsLanguageParser#enumerationType.
PvsLanguageListener.prototype.exitEnumerationType = function(ctx) {
};


// Enter a parse tree produced by PvsLanguageParser#importing.
PvsLanguageListener.prototype.enterImporting = function(ctx) {
};

// Exit a parse tree produced by PvsLanguageParser#importing.
PvsLanguageListener.prototype.exitImporting = function(ctx) {
};


// Enter a parse tree produced by PvsLanguageParser#theoryName.
PvsLanguageListener.prototype.enterTheoryName = function(ctx) {
};

// Exit a parse tree produced by PvsLanguageParser#theoryName.
PvsLanguageListener.prototype.exitTheoryName = function(ctx) {
};


// Enter a parse tree produced by PvsLanguageParser#datatype.
PvsLanguageListener.prototype.enterDatatype = function(ctx) {
};

// Exit a parse tree produced by PvsLanguageParser#datatype.
PvsLanguageListener.prototype.exitDatatype = function(ctx) {
};


// Enter a parse tree produced by PvsLanguageParser#identifier.
PvsLanguageListener.prototype.enterIdentifier = function(ctx) {
};

// Exit a parse tree produced by PvsLanguageParser#identifier.
PvsLanguageListener.prototype.exitIdentifier = function(ctx) {
};


// Enter a parse tree produced by PvsLanguageParser#identifiers.
PvsLanguageListener.prototype.enterIdentifiers = function(ctx) {
};

// Exit a parse tree produced by PvsLanguageParser#identifiers.
PvsLanguageListener.prototype.exitIdentifiers = function(ctx) {
};


// Enter a parse tree produced by PvsLanguageParser#identifierOrOperator.
PvsLanguageListener.prototype.enterIdentifierOrOperator = function(ctx) {
};

// Exit a parse tree produced by PvsLanguageParser#identifierOrOperator.
PvsLanguageListener.prototype.exitIdentifierOrOperator = function(ctx) {
};


// Enter a parse tree produced by PvsLanguageParser#identifierOrOperators.
PvsLanguageListener.prototype.enterIdentifierOrOperators = function(ctx) {
};

// Exit a parse tree produced by PvsLanguageParser#identifierOrOperators.
PvsLanguageListener.prototype.exitIdentifierOrOperators = function(ctx) {
};


// Enter a parse tree produced by PvsLanguageParser#unaryOp.
PvsLanguageListener.prototype.enterUnaryOp = function(ctx) {
};

// Exit a parse tree produced by PvsLanguageParser#unaryOp.
PvsLanguageListener.prototype.exitUnaryOp = function(ctx) {
};


// Enter a parse tree produced by PvsLanguageParser#binaryOp.
PvsLanguageListener.prototype.enterBinaryOp = function(ctx) {
};

// Exit a parse tree produced by PvsLanguageParser#binaryOp.
PvsLanguageListener.prototype.exitBinaryOp = function(ctx) {
};



exports.PvsLanguageListener = PvsLanguageListener;