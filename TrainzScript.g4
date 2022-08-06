grammar TrainzScript;

translationUnit: includeSeq? classSpecifierSeq? EOF;

includeSeq: includeDirective+;

// these aren't actually dependent on newlines
includeDirective: 'include' STRING_LITERAL;

// parser rules
// these expressions should be in the same order as here: https://online.ts2009.com/mediaWiki/index.php/Operators#Operators_.281.29.2C_Grouping_.26_Indexing
// that list seems to follow precedence rules
primaryExpression
    : literal+
	| 'me'
	| '(' expression ')'
	| idExpression;


idExpression: IDENTIFIER;


postfixExpression:
	primaryExpression
	// index
	| postfixExpression '[' (rangeIndexer | expression) ']'
	// function call
	| postfixExpression '(' expressionList? ')'
	// constructor
	| 'new' simpleTypeSpecifier (
		// constructor arguments not supported
		('(' ')')

		// LeftParen expressionList? RightParen
		| '[' constantExpression ']'
	)
	// member access (includes functions)
	| postfixExpression '.' (
		idExpression
		// pseudoDestructorName
	)
	| postfixExpression ('++' | '--')
	// | 'cast' '<' theTypeId '>' expression
	| 'cast' '<' simpleTypeSpecifier '>' expression
	// | typeIdOfTheTypeId LeftParen (expression | theTypeId) RightParen
	;

expressionList: initializerList;


unaryExpression:
	postfixExpression
	| ('++' | '--' | unaryOperator) unaryExpression
	// | Sizeof (
	// 	LeftParen theTypeId RightParen
	// 	| Ellipsis LeftParen Identifier RightParen
	// )
	;

// unaryOperator: '|' | '&' | '+' | '~' | '-' | '!';
unaryOperator: '+' | '~' | '-' | '!';

// c style casting
castExpression:
	unaryExpression
	| '(' theTypeId ')' castExpression;

multiplicativeExpression:
	castExpression (
		('*' | '/' | '%') castExpression
	)*;

additiveExpression:
	multiplicativeExpression (
		('+' | '-') multiplicativeExpression
	)*;

shiftOperator: '>>' | '<<';

shiftExpression:
	additiveExpression (shiftOperator additiveExpression)*;

relationalExpression:
	shiftExpression (
		('<' | '>' | '<=' | '>=') shiftExpression
	)*;

equalityExpression:
	relationalExpression (
		('==' | '!=') relationalExpression
	)*;

andExpression: equalityExpression ('&' equalityExpression)*;

exclusiveOrExpression: andExpression ('^' andExpression)*;

inclusiveOrExpression:
	exclusiveOrExpression ('|' exclusiveOrExpression)*;

logicalAndExpression:
	inclusiveOrExpression ('and' inclusiveOrExpression)*;

logicalOrExpression:
	logicalAndExpression ('or' logicalAndExpression)*;

// no ternary operators in trainzscript :(
// conditionalExpression:
// 	logicalOrExpression (
// 		Question expression Colon assignmentExpression
// 	)?;

assignmentExpression:
	logicalOrExpression
	| logicalOrExpression assignmentOperator initializerClause
	// | throwExpression
	;

expression: assignmentExpression (',' assignmentExpression)*;

// no ternary operators in trainzscript :(
// constantExpression: conditionalExpression;
constantExpression: logicalOrExpression;


assignmentOperator:
	'='
	// no assignment operators in trainzscript :(
	// | StarAssign
	// | DivAssign
	// | ModAssign
	// | PlusAssign
	// | MinusAssign
	// | RightShiftAssign
	// | LeftShiftAssign
	// | AndAssign
	// | XorAssign
	// | OrAssign
	;

rangeIndexer:
	(constantExpression ',' constantExpression?)
	| (constantExpression? ',' constantExpression);

// statements

statement:
	labeledStatement
	| declarationStatement
	| (
		expressionStatement
		| compoundStatement
		| selectionStatement
		| iterationStatement
		| waitStatement
		| jumpStatement
		// | tryBlock
	);

labeledStatement:
	(
		// for goto
		IDENTIFIER
		| 'case' constantExpression
		| 'default'
		| 'on' STRING_LITERAL ',' STRING_LITERAL (',' constantExpression)?
	) ':' statement;

expressionStatement: expression? ';';

compoundStatement: '{' statementSeq? '}';

statementSeq: statement+;

selectionStatement:
	'if' '(' condition ')' statement ('else' statement)?
	| 'switch' '(' condition ')' statement;

condition:
	expression
	// no declaration conditions in TrainzScript :(
	// | declSpecifierSeq declarator (
	// 	Assign initializerClause
	// 	| bracedInitList
	// )
	;

iterationStatement:
	'while' '(' condition ')' statement
	// no do while in TrainzScript :(
	// | Do statement While LeftParen expression RightParen Semi
	| 'for' '(' (
		forInitStatement condition? ';' expression?
		// no range iterators in TrainzScript :(
		// | forRangeDeclaration Colon forRangeInitializer
	) ')' statement;

forInitStatement:
	expressionStatement
	// no declaration init in TrainzScript :(
	// | simpleDeclaration
	;
// forRangeDeclaration:
// 	attributeSpecifierSeq? declSpecifierSeq declarator;

waitStatement:
	// must be compound
	'wait' '(' ')' compoundStatement
	;

jumpStatement:
	(
		'break'
		| 'continue'
		// | 'return' (expression | bracedInitList)?
		| 'return' expression?
		| 'goto' IDENTIFIER
	) ';'
	;

declarationStatement: blockDeclaration;

// declarations

declarationseq: declaration+;

declaration:
	blockDeclaration
	| functionDefinition
	// | templateDeclaration
	// | explicitInstantiation
	// | explicitSpecialization
	// | linkageSpecification
	// | namespaceDefinition
	// this doesn't compile
	// | emptyDeclaration
	// | attributeDeclaration
	;

blockDeclaration:
	simpleDeclaration
	// | opaqueEnumDeclaration
	;


simpleDeclaration:
	declSpecifierSeq? initDeclaratorList? ';'
	// | attributeSpecifierSeq declSpecifierSeq? initDeclaratorList Semi
	;


emptyDeclaration: ';';

accessSpecifier:
	'public';

declSpecifier:
	accessSpecifier
	| 'define'
	| functionSpecifier
	| simpleTypeSpecifier
	// | Friend
	// | Typedef
	// | Constexpr
	;
	
// declSpecifierSeq: declSpecifier+? attributeSpecifierSeq?;
declSpecifierSeq: declSpecifier+?;


functionSpecifier:
	'mandatory'
	| 'native'
	| 'obsolete'
	| 'thread'
	| 'legacy_compatibility'
	;

// typeSpecifier:
// 	trailingTypeSpecifier
// 	| classSpecifier
// 	// no enums in trainzscript :(
// 	// | enumSpecifier
// 	;

trailingTypeSpecifier:
	simpleTypeSpecifier
	// | elaboratedTypeSpecifier
	// | typeNameSpecifier
	// | cvQualifier
	;

// typeSpecifierSeq: typeSpecifier+;

trailingTypeSpecifierSeq:
	trailingTypeSpecifier+;

simpleTypeSpecifier:
	(className
	| BUILTIN_TYPENAME)
	arraySpecifier?;

arraySpecifier: '[' ']';

// declarators
initDeclaratorList: initDeclarator (',' initDeclarator)*;

initDeclarator: declarator initializer?;


// pointerDeclarator: (pointerOperator Const?)* noPointerDeclarator;

declarator:
	declaratorid
	| declarator (
		parameters
		// | LeftBracket constantExpression? RightBracket attributeSpecifierSeq?
	)
	// | LeftParen pointerDeclarator RightParen
	;

parameters:
	'(' parameterDeclarationClause? ')';

declaratorid: idExpression;

theTypeId: simpleTypeSpecifier;

parameterDeclarationClause:
	parameterDeclarationList;

parameterDeclarationList:
	parameterDeclaration (',' parameterDeclaration)*;

parameterDeclaration:
	declSpecifierSeq (
		// void as an argument
		declarator?
		// no default initializers in TrainzScript :(
		// declarator (
		// 	'=' initializerClause
		// )?
	);

functionDefinition:
	(declSpecifierSeq? declarator functionBody)
	;

functionBody:
	// no constructor initializers in TrainzScript
	// constructorInitializer? compoundStatement
	compoundStatement
	// | functionTryBlock
	// | Assign (Default | Delete) Semi
	;

initializer:
	'=' initializerClause
	// | LeftParen expressionList RightParen
	;

initializerClause: assignmentExpression;

initializerList:
	initializerClause (
		',' initializerClause
	)*;


// classes
className: IDENTIFIER;

classSpecifier:
	classPreSpecifierSeq? classHead '{' memberSpecification? '}' ';';

classSpecifierSeq:
	classSpecifier+;

classPreSpecifier:
	'static'
	| 'game';

classPreSpecifierSeq:
	classPreSpecifier+;

classHead:
	'class' (
		className
	)? baseClause?
	;

memberSpecification:
	memberdeclaration+;

memberdeclaration:
	declSpecifierSeq? memberDeclaratorList? ';'
	| functionDefinition
	// | usingDeclaration
	// | staticAssertDeclaration
	// | templateDeclaration
	// | aliasDeclaration
	// this doesn't compile
	// | emptyDeclaration
	;

memberDeclaratorList:
	memberDeclarator (',' memberDeclarator)*;

memberDeclarator:
	declarator (
		//virtualSpecifierSeq? pureSpecifier?
		initializer?
	)
	// no bitfields in TrainzScript
	// | Identifier? attributeSpecifierSeq? Colon constantExpression
	;

// derived classes
baseClause: 'isclass' baseSpecifierList;

baseSpecifierList:
	baseSpecifier (',' baseSpecifier )*;

baseSpecifier:
	(
		baseTypeSpecifier
		// | Virtual accessSpecifier? baseTypeSpecifier
		// | accessSpecifier Virtual? baseTypeSpecifier
	);

baseTypeSpecifier: className;

literal:
	INTEGER_LITERAL
	| FLOATING_LITERAL
	| STRING_LITERAL
	| BOOLEAN_LITERAL
	| REFERENCE_LITERAL
	;

// Lexer rules
IDENTIFIER: NONDIGIT (NONDIGIT | DIGIT)*;

ASSIGNMENT_OPERATOR: '=';

UNARY_OPERATOR: '++'
	| '--'
	| '+'
	| '-'
	| '!'
	| '~';

INTEGER_LITERAL:
	DECIMAL_LITERAL
	// | OCTAL_LITERAL
	| HEXADECIMAL_LITERAL
	// | BinaryLiteral
	| '\'' C_CHAR '\''
	;

FLOATING_LITERAL:
	// FRACTIONAL_CONSTANT EXPONENT_PART? FLOATING_SUFFIX?
	FRACTIONAL_CONSTANT FLOATING_SUFFIX?
	// | DIGIT+ EXPONENT_PART FLOATING_SUFFIX?;
	;

STRING_LITERAL: '"' S_CHAR* '"';

BOOLEAN_LITERAL: 'false' | 'true';

REFERENCE_LITERAL: 'null';

fragment NONDIGIT: [a-zA-Z_];

fragment DIGIT: [0-9];

DECIMAL_LITERAL: DIGIT+;

fragment HEXADECIMAL_DIGIT: [0-9a-fA-F];

HEXADECIMAL_LITERAL: ('0x' | '0X') HEXADECIMAL_DIGIT (
		'\''? HEXADECIMAL_DIGIT
	)*;

fragment FRACTIONAL_CONSTANT:
	DIGIT* '.' DIGIT+
	| DIGIT+ '.';

// fragment EXPONENT_PART:
// 	'e' SIGN? DIGIT+
// 	| 'E' SIGN? DIGIT+;

fragment SIGN: [+-];

// only lowercase f supported
fragment FLOATING_SUFFIX: 'f';

fragment C_CHAR:
	~ ['\\\r\n]
	| ESCAPE_SEQUENCE
	// | Universalcharactername;
	;

fragment S_CHAR:
	~ ["\\\r\n]
	| ESCAPE_SEQUENCE
	// | Universalcharactername;
	;

fragment ESCAPE_SEQUENCE:
	SIMPLE_ESCAPE_SEQUENCE
	// | Octalescapesequence
	// | Hexadecimalescapesequence
	;

fragment SIMPLE_ESCAPE_SEQUENCE:
	'\\\''
	| '\\"'
	| '\\?'
	| '\\\\'
	| '\\a'
	| '\\b'
	| '\\f'
	| '\\n'
	| '\\r'
	| ('\\' ('\r' '\n'? | '\n'))
	| '\\t'
	| '\\v';

BUILTIN_TYPENAME: 'int'
				| 'float'
				| 'bool'
				| 'string'
				| 'void'
				| 'object';

// https://online.ts2009.com/mediaWiki/index.php/TrainzScript_Keywords
KEYWORD: 'break'
		| 'case'
		| 'isclass'
		| 'class'
		| 'continue'
		| 'default'
		| 'define'
		| 'else'
		| 'final'
		| 'for'
		| 'game'
		| 'goto'
		| 'if'
		| 'include'
		| 'inherited'
		| 'legacy_compatibility'
		| 'mandatory'
		| 'me'
		| 'native'
		| 'obsolete'
		| 'on'
		| 'public'
		| 'return'
		| 'static'
		| 'switch'
		| 'thread'
		| 'wait'
		| 'while'
		| 'cast'
		| 'new';

WHITESPACE
    :   [ \t]+
        -> skip
    ;

NEWLINE
    :   (   '\r' '\n'?
        |   '\n'
        )
        -> skip
    ;

BLOCK_COMMENT
    :   '/*' .*? '*/'
        -> skip
    ;

LINE_COMMENT
    :   '//' ~[\r\n]*
        -> skip
    ;