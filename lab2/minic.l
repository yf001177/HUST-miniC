%{
#include "string.h"
#include "def.h"

extern int yycolumn;

#define YYSTYPE yylval
extern "C"
{	
	int yywrap(void);
	int yylex(void);
}

void flexout(std::string name,std::string value){
  std::cout<<name<<"\t:"<<value<<std::endl;
}
//flex为bison提供行号信息
%}
%option yylineno 
INT         [1-9][0-9]*+|0[0-7]*|0[xX][0-9a-fA-F]+|0
FLOAT       [0-9]+"."[0-9]*(e[+-]?[0-9]+)?
ID          [a-zA-Z_][a-zA-Z_0-9]*
SEMI        \;
COMMA       \,
ASSIGNOP    \=
RELOP       "=="
PLUSPLUS    "++"
MINUSMINUS  "--"
PLUSASS     "+="
MINUSASS    "-="
PLUS        \+
MINUS       \-
STAR        \*
DIV         \/
AND         "&&"
OR          "||"
DOT         \.
NOT         \!
LP          \(
RP          \)
LB          \[
RB          \]
LC          \{
RC          \}
STRUCT      "struct"
RETURN      "return"
IF          "if"
ELSE        "else"
WHILE       "while"
%%

"int"       {flexout("TYPE","int");}
"float"     {flexout("TYPE","float");}
"char"      {flexout("TYPE","char");}
{INT}       {flexout("INT",yytext);}
{FLOAT}     {flexout("FLOAT",yytext);}
{STRUCT}    {flexout("STRUCT","struct");}
{RETURN}    {flexout("RETURN","return");}
{IF}        {flexout("IF","if");}
{ELSE}      {flexout("ELSE","else");}
{WHILE}     {flexout("WHILE","while");}
{ID}        {flexout("ID",yytext);}
{SEMI}      {flexout("SEMI",";");}
{COMMA}     {flexout("COMMA",",");}
{ASSIGNOP}  {flexout("ASSIGNOP","=");}
{RELOP}     {flexout("RELOP","==");}
{PLUSPLUS}    {flexout("PLUSPLUS","++");}
{MINUSMINUS}  {flexout("MINUSMINUS","--");}
{PLUSASS}     {flexout("PLUSASS","+=");}
{MINUSASS}    {flexout("MINUSASS","-=");}
{PLUS}      {flexout("PLUS","+");}
{MINUS}     {flexout("MINUS","-");}
{STAR}      {flexout("STAR","*");}
{DIV}       {flexout("DIV","/");}
{AND}       {flexout("AND","&&");}
{OR}        {flexout("OR","||");}
{DOT}       {flexout("DOT",".");}
{NOT}       {flexout("NOT","!");}
{LP}        {flexout("LP","(");}
{RP}        {flexout("RP",")");}
{LB}        {flexout("LB","[");}
{RB}        {flexout("RB","]");}
{LC}        {flexout("LC","{");}
{RC}        {flexout("RC","}");}

[\n]		{yycolumn=1;}
[ \r\t]		{/*printf("过滤空格等字符\n");*/}

0[0-9]*         {printf("Error type A at Line %d: Illegal octal number \'%s\'\n",yylineno,yytext);}
0[xX][0-9a-zA-Z]+       {printf("Error type A at Line %d: Illegal hexadecimal number \'%s\'\n",yylineno,yytext);}
.           {printf("Error type A at Line %d: Mysterious characters \'%s\'\n",yylineno,yytext);}
%%

int yywrap()
{
    return 1;
}