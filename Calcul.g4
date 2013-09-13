/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

grammar Calcul;

@header {
package io.github.bobhostern.calcul;
}

PLUS   : '+' ;
MINUS  : '-' ;
MULT   : '*' ;
DIV    : '/' ;
RPAREN : ')' ;
LPAREN : '(' ;
ASSIGN : '=' ;
LBRACKET : '{' ;
RBRACKET : '}' ;
END_STATEMENT : ';' ;
PREPROCESSOR_SYMBOL : '!' ;
LANGLE : '<' ;
RANGLE : '>' ;
PIPE : '|' ;

/*
*   TYPES
*/

HEADERS : 'HEADER_START' ;
HEADERE : 'HEADER_END' ;
NEW_DECL : 'new' ;
COMMA : ',' ;
DOT : '.' ;
ARROW : '->' ;

DEFINE : 'define' ;
PREPROCESSOR : 'PREPROCESSOR' ;
IMPORT : 'import' ;
DECLARE : 'declare' ;
FUNCTION : 'function' ;
RETURN : 'return' ;
CLASS : 'class' ;
UNDERSCORE : '_' ;
NAMESPACE : 'namespace' ;
COLON : ':' ;
PACKAGE : 'package' ;
PUBLIC : 'public:' ; 
PROTECTED : 'protected:' ;
PRIVATE : 'private:' ;
VAR : 'var' ;
INIT : 'init' ;
GID    :      ('a'..'z'|'A'..'Z')('a'..'z'|'A'..'Z'|'0'..'9')*;
FUNC : 'func' ;
 
/*----------------
* PARSER RULES
*----------------*/
 
prog :  preprog? stat*;
preprog :  PREPROCESSOR LBRACKET prestat* RBRACKET ;
prestat :  PREPROCESSOR_SYMBOL IMPORT packageListing END_STATEMENT # PPSImportStatement
        |  PREPROCESSOR_SYMBOL PACKAGE packageListing END_STATEMENT # PPSPackageStatement;
packageListing : (pckNames+=GID DOT)* pckName=GID ;
nsIdentifier : (nsName+=GID COLON)* mainName=GID;

stat :       expr END_STATEMENT # Expression
     |      GID ASSIGN DECLARE function_decl_args? END_STATEMENT # FunctionDeclaration
     |      GID ASSIGN FUNCTION function_decl_args? block # FunctionDeclarationAndDefinition
     |      GID ASSIGN expr END_STATEMENT # Assignment
     |      DEFINE nsIdentifier function_decl_args? block # FunctionDefinition
     |      RETURN expr END_STATEMENT # ReturnStatement
     |      HEADERS # HeaderStart
     |      HEADERE # HeaderEnd 
     |      CLASS GID (LPAREN arg1=GID? (COMMA argList+=GID)* RPAREN)?(LANGLE parent1=nsIdentifier? (COMMA parentList+=nsIdentifier)*)? LBRACKET classdef* RBRACKET END_STATEMENT # ClassDeclaration
     |      DEFINE UNDERSCORE CLASS nsIdentifier LBRACKET classstat* RBRACKET # ClassDefinition
     |      NEW_DECL NAMESPACE namespaceName=GID block # NamespaceDeclaration;

classdef :  PUBLIC # PublicDeclarator
         |  PROTECTED # ProtectedDeclarator
         |  PRIVATE # PrivateDeclarator 
         |  VAR GID (ASSIGN expr)? END_STATEMENT # ClassVariableDeclaration
         |  INIT block # InitClassBlock
         |  FUNCTION GID function_decl_args? END_STATEMENT # ClassFunctionDeclaration;

classstat:  DEFINE GID function_decl_args? block # DefineClassFunction;

expr   :     functors+=multExpr (op+=(PLUS | MINUS )functors+=multExpr)*;
 
multExpr:    functors+=atom (op+=(MULT | DIV) functors+=atom )*;

block : LBRACKET stat* RBRACKET ;

function_decl_args : LPAREN var1=GID? (COMMA varList+=GID)* RPAREN ;
 
atom   :      INT # IntegerAtom
       |      DOUBLE # DoubleAtom
       |      GID # IdentifierAtom
       |      LPAREN expr RPAREN # ExpressionAtom
       |      GID LPAREN atom1=atom? (COMMA atomList+=atom)* RPAREN # FunctionCallAtom
       |      NEW_DECL GID (LPAREN atom1=atom? (COMMA atomList+=atom)* RPAREN)? # NewObjectAtom;
 
/*----------------
* LEXER RULES
*----------------*/
INT : ( PLUS | MINUS )? NUMBER ;
DOUBLE : ( PLUS | MINUS )? NUMBER [.e] NUMBER ;
fragment NUMBER  :      '0'..'9'+;
WS     :      (' '|'\t'|'\n'|'\r'|'\u000C')+ -> skip;
OL_COMMENT : '$' .*? '\n' -> skip;
ML_COMMENT : '#{' .*? '}#' -> skip;