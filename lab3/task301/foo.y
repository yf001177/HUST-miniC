%union
{
	int ival;
	char *sval;
} //**begin***//
%token <ival>NUM
%nterm <ival>exp
%left '-'
%left '+'
%%
exp:
	exp '+' exp
	|exp '-' exp
	|NUM
	;

%% //**end**//