
%{
(******************************************************************************
    Copyright (c) 1996-2005 Synopsys, Inc.    ALL RIGHTS RESERVED

  The contents of this file are subject to the restrictions and limitations
  set forth in the SYNOPSYS Open Source License Version 1.0  (the "License"); 
  you may not use this file except in compliance with such restrictions 
  and limitations. You may obtain instructions on how to receive a copy of 
  the License at

  http://www.synopsys.com/partners/tapin/tapinprogram.html. 

  Software distributed by Original Contributor under the License is 
  distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either 
  expressed or implied. See the License for the specific language governing 
  rights and limitations under the License.

******************************************************************************)
(*
struct xnumber
{
    int type; /* 0=int, 1=float */
    int intnum;
    double floatnum;
};
typedef struct xnumber xnumber;

#include <stdlib.h>
#include <math.h>
#include <unistd.h>
#include <string.h>
#include "liberty_structs.h"
#include "si2dr_liberty.h"
#include "liberty_parser.h"
#include "libhash.h"
#include "libstrtab.h"
#ifdef DMALLOC
#include <dmalloc.h>
#define my_strdup(ptr) dmalloc_strdup(__FILE__, __LINE__, (ptr), 0)

#else
#include "mymalloc.h"
char *my_strdup(char *str);

char *my_strdup(char *str){ char *x = my_malloc(strlen(str)+3 /* just in case of insertions */); strcpy(x,str); return x;}

#endif

int liberty_parser2_lex(void);


extern liberty_strtable *master_string_table;

/* I added this code to avoid a confusion between bus notation and a complex attr with a single integer value. */

typedef struct token_q
{
  int retval;  /* the token value to return */
  int set_num; /* set the yylval to contain the num; -1 is an int; +1 is a float */
  double floatnum;  /* number to put to yylval */
  int intnum;  /* number to put to yylval */
  char *str;   /* if non-zero, put to yylval */
} token_q;

int token_q_front = 9;
int token_q_rear = 9;

token_q token_q_buf[10];

int token_q_empty(void);

int token_q_empty(void)
{
    if (token_q_front == token_q_rear)
        return 1;
    else
        return 0;
}

static void remove_token(int *retval, int *set_num, int *intnum, double *floatnum, char **str)
{
    if( token_q_empty() )
        return;
        
    if( token_q_front == 9 )
        token_q_front = 0;
    else
        token_q_front++;
    *retval = token_q_buf[token_q_front].retval;
    *set_num = token_q_buf[token_q_front].set_num;
    *floatnum = token_q_buf[token_q_front].floatnum;
    *intnum = token_q_buf[token_q_front].floatnum;
    if( token_q_buf[token_q_front].str )
    {
        *str = token_q_buf[token_q_front].str;
    }
}

static void add_token(int retval, int set_num, int intnum, double floatnum, char *str)
{
    if( token_q_rear == 9 )
        token_q_rear = 0;
    else
        token_q_rear++;

    if( token_q_rear == token_q_front )
    {
        printf("Token Queue Stack Overflow-- notify program maintainers!\n");
        return;
    }
    token_q_buf[token_q_rear].str = str;
    token_q_buf[token_q_rear].retval = retval;
    token_q_buf[token_q_rear].floatnum = floatnum;
    token_q_buf[token_q_rear].intnum = intnum;
    token_q_buf[token_q_rear].set_num = set_num;
}

/* this routine is called from liberty_front_lex.c; which is acting
    as a front end to this lexer */

int injected_token(void);

int injected_token(void)
{
    char *str; 
    double num; 
    int  intnum; 
    int retval; 
    int set_num;

    remove_token(&retval,&set_num,&intnum,&num,&str);

    if( set_num == -1)
    { 
        yylval.num.type = 0;
        yylval.num.intnum = intnum;
    }
    else if(set_num == 1)
    {
        yylval.num.type = 1;
        yylval.num.floatnum = num;
    }
    else if(str)
    {
        yylval.str = str;
    }
    return retval;
}


*)
Var
  tok_encountered : Integer = 0;
  lline : Integer;   // "static"
(*
int save_lineno, save_lline;
static char filenamebuf[1024];
*)
  string_buf : String;
(*
char token_comment_buf[102400]; /* the buffer to hold comments in */
char token_comment_buf2[102400]; /* the spare buffer to hold more comments in */
static char *string_buf_ptr;
struct nl
{
    char *fname;
    struct nl *next;
};
struct nl  *file_name_list;

*)

Procedure set_tok;
Begin
//  if token_comment_buf[0] then
    Inc(tok_encountered);
End;

(*
#define MAX_INCLUDE_DEPTH 1
void clean_file_name(char *,char * );
int include_stack_index = 0;
YY_BUFFER_STATE include_stack[MAX_INCLUDE_DEPTH];

char *search_string_for_linefeeds(char *str);
*)
%}
// %option   noyywrap
// %option prefix="liberty_parser2_"
// %option outfile="lex.yy.c"
// %x comment
// %x stringx
// %x include

%%
\+          Begin lline := lineno; set_tok; return(PLUS);  End;
\-          Begin lline := lineno; set_tok; return(MINUS); End;
\*          Begin lline := lineno; set_tok; return(MULT);  End;
\/          Begin lline := lineno; set_tok; return(TDIV);  End;
\,          Begin lline := lineno; set_tok; return(COMMA); End;
\;          Begin lline := lineno; set_tok; return(SEMI);  End;
\(          Begin lline := lineno; set_tok; return(LPAR);  End;
\)          Begin lline := lineno; set_tok; return(RPAR);  End;
\=          Begin lline := lineno; set_tok; return(EQ);    End;
\{          Begin
              if lline <> lineno then
                WriteLn('Warning: line ',lineno,': Opening Curly must be on same line as group declaration!');
              set_tok();
              return(LCURLY);
            End;

\}[ \t]*\;? Begin lline := lineno; set_tok; return(RCURLY); End;
[ \t]?\:    Begin
              lline := lineno;
              if (Length(yytext) = 1) and tight_colon_ok then
                WriteLn('Error: space must precede Colon (:) at line ',lineno);
              set_tok();
              return(COLON);
            End;
include_file[ \t]*\(
            // BEGIN(include);
            yyerror('include_file not supported');


[-+]?([0-9]+\.?[0-9]*([Ee][-+]?[0-9]+)?|[0-9]*\.[0-9]*([Ee][-+]?[0-9]+)?)
            Begin
              lline := lineno;
              if (Pos('.',yytext) > 0) or (Pos('E',yytext) > 0) or (Pos('e',yytext) > 0) then
                Begin
                  yylval.yyTNum.IsDouble  := true;
                  yylval.yyTNum.DoubleNum := StrToFloat(yytext);
                End
              else 
                Begin
                  yylval.yyTNum.IsDouble  := false;
                  yylval.yyTNum.IntNum    := StrToInt(yytext);
                End;
              set_tok;
              return(NUM);
            End;

[A-Za-z!@#$%^&_+\|~\?:][A-Za-z0-9!@#$%^&_+\|~\?:]*[\<\{\[\(][-0-9:]+[\]\}\>\)]
            Begin
              lline := lineno;
  (*              if !strncmp(yytext,"values(",7)
                Begin
                  /* ugh -- a values() with a single unquoted number in it! let's translate it into a values with a single quoted value instead! */
                  char *ident = liberty_strtable_enter_string(master_string_table, "values"); /* OLD WAY: (char* )malloc(7); */
                  char *str /* OLD WAY: := (char* )malloc(strlen(yytext)-4) */;
                  /* strcpy(ident,"values");  OLD allocation method */
                  yylval.str := ident;
                  str: = liberty_strtable_enter_string(master_string_table, yytext+7);
                  /* OLD WAY: strcpy(str,yytext+7); 
                              str[strlen(str)-1] := 0; */
                  add_token(LPAR, 0, 0, 0.0, 0);
                  add_token(STRING, 0, 0, 0.0, str);
                  add_token(RPAR, 0, 0, 0.0, 0);
                  set_tok();
                  return(IDENT);
                End
              else
                Begin
                  /* OLD:  char *str: = (char* )malloc(strlen(yytext)+1);
                           strcpy(str,yytext)*/ ;  
                  yylval.yyTStr := yytext; // /* OLD: str  NEW: */ liberty_strtable_enter_string(master_string_table, yytext);
  *)                  
                  set_tok();
                  return(TSTRING);
  //                End;
            End;

"define"                Begin lline := lineno; set_tok; return(KW_DEFINE);       End;  
"define_group"          Begin lline := lineno; set_tok; return(KW_DEFINE_GROUP); End;  
[Tt][Rr][Uu][Ee]        Begin lline := lineno; set_tok; return(KW_TRUE);         End;  
[Ff][Aa][Ll][Ss][Ee]    Begin lline := lineno; set_tok; return(KW_FALSE);        End;  
\\?\n                   Begin Inc(lineno); End;  
\\[ \t]+\n  Begin
              WriteLn('ERROR-- ',curr_file,':',lineno,' -- Continuation char followed by spaces or tabs!');
              Inc(lineno);
            End;  
\r          Begin End;  
\t          Begin End;  
" "         Begin End;  

[a-zA-Z0-9!@#$%^&_+\|~\?<>\.\-]+
            Begin
              lline := lineno;
              yylval.yyTStr := yytext; // /* OLD: my_strdup(yytext) NEW: */ liberty_strtable_enter_string(master_string_table, yytext);
              set_tok;
              return(IDENT);
            End;  

%{
// plex doesn't support context, so we have to handle comments in a sub-optimal
// way, see e.g.
//   http://flex.sourceforge.net/manual/How-can-I-match-C_002dstyle-comments_003f.html
// and
//   http://stackoverflow.com/questions/2130097/difficulty-getting-c-style-comments-in-flex-lex
(*
"/*"        Begin
              BEGIN(comment);
              if      (!tok_encountered && token_comment_buf[0] ) strcat(token_comment_buf,"\n")
              else if ( tok_encountered && token_comment_buf2[0]) strcat(token_comment_buf2,"\n");
            End;
<comment>[^*\n]*        /* eat anything that's not a '*' */
            Begin
              if( tok_encountered ) strcat(token_comment_buf2,yytext); else strcat(token_comment_buf,yytext);  End;
<comment>"*"+[^*/\n]*   /* eat up '*'s not followed by '/'s */
            Begin  if( tok_encountered ) strcat(token_comment_buf2,yytext); else strcat(token_comment_buf,yytext);  End;
<comment>\n 
            Begin  ++lineno;if( tok_encountered ) strcat(token_comment_buf2,yytext); else strcat(token_comment_buf,"\n");  End;  
<comment>"*"+"/"
            BEGIN(INITIAL);
*)
%}

"/*"        Begin
              repeat
                repeat
                  yyactchar := get_char;
                  // eat up text of comment
                until (yyactchar = '*') or (yyactchar = #0 (* EOF *));
                if yyactchar = '*' then
                  Begin
                    repeat
                       yyactchar := get_char;
                    until yyactchar <> '*';
                    if yyactchar = '/' then
                      break;  // found the end
                  End
                else if yyactchar = #0 (* EOF *) then
                  Begin
                    yyerror('EOF in comment');
                    break;
                  End;
              until false;
            End;

%{
(*
\"        string_buf_ptr := string_buf; BEGIN(stringx);
<stringx>\"      Begin   char *x; BEGIN(INITIAL); *string_buf_ptr := 0;
            yylval.str := liberty_strtable_enter_string(master_string_table, string_buf);/* OLD: my_strdup(string_buf); */ set_tok(); return(STRING);   End;  

<stringx>\n   Begin   char *x; printf("Error, line %d: unterminated string constant-- use a back-slash to continue a string to the next line.\n", lineno++); 
            BEGIN(INITIAL); *string_buf_ptr := 0;
            yylval.str := liberty_strtable_enter_string(master_string_table, string_buf);/* OLD: my_strdup(string_buf);*/ set_tok(); return(STRING);
          End;  
<stringx>\\\n       Begin   *string_buf_ptr++ := '\\'; *string_buf_ptr++ := '\n'; lineno++;  End;  
<stringx>\\.       Begin   *string_buf_ptr++ := '\\'; *string_buf_ptr++ := yytext[1];  End;  
<stringx>[^\\\n\"]+          Begin   char *yptr := yytext; while ( *yptr) *string_buf_ptr++ := *yptr++;   End;  
*)
%}

\"          Begin
              string_buf := '';
              repeat
                yyactchar := get_char;
                if yyactchar in ['"',#10] then
                  Begin
                    if yyactchar = #10 then
                      Begin
                        WriteLn('Error, line ',lineno,': unterminated string constant-- use a back-slash to continue a string to the next line.');
                        Inc(lineno);
                      End;
                    yylval.yyTStr := string_buf; // liberty_strtable_enter_string(master_string_table, string_buf);/* OLD: my_strdup(string_buf); */
                    // WriteLn(curr_file,':',lineno,': Got string ''',string_buf,'''');
                    set_tok;
                    return(TSTRING);
                    break;
                  End
                else if yyactchar = '\' then
                  Begin
                    yyactchar := get_char;
                    if yyactchar = #10 then
                      Begin
                        string_buf += '\'+#10;
                        Inc(lineno);
                      End
                    else
                      string_buf += '\' + yyactchar;
                  End
                else
                  Begin
                    string_buf += yyactchar;
                  End;
              until false;
            End;

%{
// include files not supported
(*
<include>[ \t]*   Begin    End;  
<include>[^ \t\n);]+      Begin    
                 if ( include_stack_index >:= MAX_INCLUDE_DEPTH )
                       Begin  
                     fprintf( stderr, "******Error: Includes nested too deeply! An included file cannot include another file!\n\n" );
                     exit( 1 );
                       End;  
                 clean_file_name(yytext,filenamebuf);
                  End;  
<include>")"          Begin    End;  
<include>";"          Begin   FILE *yyin_save := yyin; struct nl *nlp;
 
                 include_stack[include_stack_index++] := YY_CURRENT_BUFFER;
                    printf("       ... including file %s\n", filenamebuf);
                 yyin := fopen( filenamebuf, "r" );
            
                 if ( ! yyin )
                  Begin  
                    
                    fprintf(stderr,"******Error: Couldn't find the include file: %s; ignoring the Include directive!\n", filenamebuf);
                     include_stack_index--;
                    yyin := yyin_save;
                    BEGIN(INITIAL);
                  End;  
                else
                  Begin  
                 yy_switch_to_buffer(
                     yy_create_buffer( yyin, YY_BUF_SIZE ) );
                 nlp := (struct nl* )calloc(sizeof(struct nl),1);
                 nlp->fname := /*OLD: (char* )malloc(strlen(filenamebuf)+1); NEW:*/ liberty_strtable_enter_string(master_string_table, filenamebuf);
                              /*OLD: strcpy(nlp->fname,filenamebuf); */
                 nlp->next := file_name_list;
                 file_name_list := nlp;
                 curr_file_save := curr_file;
                 curr_file := nlp->fname;
                 save_lineno := lineno;
                 save_lline := lline;
                 lineno := 1;
                 lline := 1;
                 BEGIN(INITIAL);
                  End;  
           End;  


<<EOF>>    Begin  
                 if ( --include_stack_index < 0 )
                       Begin  
                     yyterminate();
                       End;  

                 else
                       Begin  
                         yy_delete_buffer( YY_CURRENT_BUFFER );
                         yy_switch_to_buffer(include_stack[include_stack_index] );
                         lineno := save_lineno;
                         lline := save_lline;
                         curr_file := curr_file_save;
                       End;  
  End;  
*)
%}

%%

(*
void clean_file_name(char *dirty, char *clean)
  Begin  
    /* clean (no quotes, no spaces) the name;
       then make sure it exists in the current dir or the path; if 
       it exists in the path, plug in the path component into the name,
       so the later fopen will find it */
    char cleanbuf[4096],*p,anotherbuf[4096];
    p := dirty;
    
    while( *p && ( *p =:= '"' || *p =:= ' ' || *p =:= '\t' ) )
      Begin  
        *p++;
      End;  
    
    strcpy( cleanbuf, p);
    
    p := cleanbuf + strlen(cleanbuf) -1;

    while ( *p && p > cleanbuf && ( *p =:= '"' || *p =:= ' ' || *p =:= '\t' ) )
        *p-- := 0;
    if( access(cleanbuf, F_OK ) =:= 0 )
      Begin  
        strcpy(clean, cleanbuf);
        return;
      End;  
    else
      Begin  
        /* hmmm, not in the current directory? */
        char enbuf[20256];
        
        char *en := getenv("LIBERTY_PARSER_SEARCH_PATH");
        if( !en )
          Begin  
            strcpy(clean, cleanbuf);
            return;
          End;  
        else
          Begin  
            char *q;
            
            strcpy(enbuf,en);
            strcat(enbuf,":");
            q := enbuf;
            
            while( q && *q && (p := strchr(q,':') ) )
              Begin  
                *p := 0;
                
                if( p>enbuf && ( *(p-1) =:= '/' || *(p-1) =:= '\\'))
                    *(p-1) := 0;
                
                strcpy(anotherbuf,q);
                strcat(anotherbuf,"/");
                strcat(anotherbuf,cleanbuf);
                
                if( access(anotherbuf,F_OK) =:= 0 )
                  Begin  
                    strcpy(clean,anotherbuf);
                    return;
                  End;  
                
                q := p+1;
              End;  
          End;  
      End;  
  End;  

char *search_string_for_linefeeds(char *str)
  Begin  
   char *s;
   s := str;
   while ( *s)  Begin   if( *s++ =:= '\n' )   Begin  lineno++; if( *(s-2) !:= '\\' )  Begin  printf("Warning: line %d: String constant spanning input lines does not use continuation character.\n",lineno);  End;     End;     End;  
   return(str);
  End;  
*)
