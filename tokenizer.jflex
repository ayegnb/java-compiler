
import java_cup.runtime.* ;

%%

%class Lexer
%unicode
%cup
%line
%column
%yylexthrow ScanError

%{
   StringBuffer readstring = new StringBuffer();

   private Symbol symbol( int type ) { 
      return new Symbol( type, yyline, yycolumn ); 
   }

   private Symbol symbol( int type, Object value )
   {
      return new Symbol( type, yyline, yycolumn, value ); 
   }
%}

LineTerminator = \r|\n|\r\n
WhiteSpace = {LineTerminator} | [ \t\f]
BlockComment = \/\*( (\*)+[^/*] | [^*])*(\*)+\/
QuotedString = \"( [^\"\\] | (\\n) | (\\t) | (\\\\) )* \"
digit = [0-9]
double = [-]?{digit}*({digit}\.?|\.{digit}){digit}*([Ee][-]?{digit}+)?
charconst = \'( [^\'\\] | (\\n) | (\\t) | (\\\\) )\'

%%

"if" { return symbol( sym. IF ); }
"then" { return symbol( sym. THEN ); }
"else" { return symbol( sym. ELSE ); }
"while" { return symbol( sym. WHILE ); }
"do" { return symbol( sym. DO ); }
"return" { return symbol( sym. RETURN ); }
"print" { return symbol( sym. PRINT ); }
"pointer" { return symbol( sym. POINTER ); }
"array" { return symbol( sym. ARRAY ); }
"function" { return symbol( sym. FUNCTION ); }
"structdef" { return symbol( sym. STRUCTDEF ); }
"constant" { return symbol( sym. CONSTANT ); }
"void" { return symbol( sym. VOID ); }
"end" { return symbol( sym. END ); }
"begin" { return symbol( sym. BEGIN ); }

"bool" { return symbol( sym. BOOL ); }
"char" { return symbol( sym. CHAR ); }
"integer" { return symbol( sym. INTEGER ); }
"double" { return symbol( sym. DOUBLE ); }

{charconst} { return symbol( sym. CHARCONST, new ast.Char(yytext( ).charAt(1)) ); }
{QuotedString} { return symbol( sym. STRINGCONST, new ast.String(yytext( ).substring(1, yytext().length()-1)) ); }

"null" { return symbol( sym. POINTERCONST, new ast.Pointer(0) ); }

"true" { return symbol( sym. BOOLCONST, new ast.Bool(true) ); }
"false" { return symbol( sym. BOOLCONST, new ast.Bool(false) );}


"["    { return symbol( sym. LSQPAR ); }
"]"    { return symbol( sym. RSQPAR ); }
"("    { return symbol( sym. LPAR ); }
")"    { return symbol( sym. RPAR ); }
","    { return symbol( sym. COMMA ); }
"."    { return symbol( sym. DOT ); }
";"    { return symbol( sym. SEMICOLON ); }
":"    { return symbol( sym. COLON ); }

"="    { return symbol( sym. ASSIGN ); }
"?"    { return symbol( sym. QUESTION ); }

"+"    { return symbol( sym. ADD ); }
"-"    { return symbol( sym. SUB ); }
"*"    { return symbol( sym. MUL ); }
"/"    { return symbol( sym. TRUEDIV ); }
"--"    { return symbol( sym. MINUSMINUS ); }
"++"    { return symbol( sym. PLUSPLUS ); }
"&" { return symbol( sym. AMPERSAND ); }

"!"    { return symbol( sym. NOT ); }
"=="    { return symbol( sym. EQ ); }
"!="    { return symbol( sym. NE ); }
"<"    { return symbol( sym. LT ); }
">"    { return symbol( sym. GT ); }
"<="    { return symbol( sym. LE ); }
">="    { return symbol( sym. GE ); }

"||"    { return symbol( sym. OR ); }
"&&"    { return symbol( sym. AND ); }

"->"     { return symbol( sym. ARROW ); }

[:jletter:]([:jletter:] | [0-9])*
          { return symbol( sym. IDENTIFIER, new ast.Identifier( yytext( ) )); }

"0" | [1-9][0-9]*
          { return symbol( sym.INTEGERCONST,
                     new ast.Integer( new java.lang.Integer( yytext( ) ))); }

{ double }
          { return symbol( sym.DOUBLECONST,
                     new ast.Double( new java.lang.Double( yytext( ) ))); }

{WhiteSpace}     { }
{BlockComment}  { }
"#"              { return symbol( sym. EOF ); }

// Error fallback:

[^]    { throw new ScanError( "Unrecognized character <" + yytext( ) + ">",
                              yyline, yycolumn ); }

