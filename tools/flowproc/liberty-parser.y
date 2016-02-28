/* pyacc input file */
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

  2014 Johann Glaser <Johann.Glaser@gmx.at> translated to Pascal

******************************************************************************)
(*
include <stdio.h>
include <alloca.h>
include <stdlib.h>
include <string.h>
include "liberty_structs.h"
include "si2dr_liberty.h"
include "group_enum.h"
include "attr_enum.h"
include "libhash.h"
include "libstrtab.h"
ifdef DMALLOC
include "dmalloc.h"
endif
include "mymalloc.h" // meant to override the my_*alloc calls if dmalloc defined

static si2drGroupIdT gs[1000];
static int gsindex = 0;

static si2drErrorT   err;
static si2drAttrTypeT atype;
static si2drAttrIdT curr_attr;
static si2drDefineIdT curr_def;
si2drValueTypeT convert_vt(char *type);
*)

Var
  lineno : Integer;
  syntax_errors : Integer;

(*
 static char PB[8000]; // so as not to have a bunch of local buffers
*)
  tight_colon_ok : Boolean = false;
  curr_file      : String;
  curr_file_save : String;
  
(*
 extern char token_comment_buf[102400];
 extern char token_comment_buf2[102400];
 static char token_comment_buft[102400];
 extern int tok_encountered;
 extern char *curr_file;
 extern liberty_strtable *master_string_table;
 
struct xnumber
Begin
    int type; // 0=int, 1=float
    int intnum;
    double floatnum;
End;
typedef struct xnumber xnumber;

void make_complex(liberty_head *h);
void make_simple(char *name, liberty_attribute_value *v);
 
void make_complex(liberty_head *h)
Begin
    liberty_attribute_value *v,*vn;
    
    curr_attr=si2drGroupCreateAttr(gs[gsindex-1],h->name,SI2DR_COMPLEX,&err);
    if( token_comment_buf[0] ) Begin si2drAttrSetComment(curr_attr, token_comment_buf,&err); token_comment_buf[0]=0; tok_encountered = 0;End;
    
    si2drObjectSetLineNo(curr_attr,h->lineno, &err);
    si2drObjectSetFileName(curr_attr,h->filename, &err);
    for(v=h->list;v;v=vn)
    Begin
        if( v->type == LIBERTY__VAL_BOOLEAN )
            si2drComplexAttrAddBooleanValue(curr_attr,v->u.bool_val,&err);
        else if( v->type == LIBERTY__VAL_STRING )
            si2drComplexAttrAddStringValue(curr_attr,v->u.string_val,&err);        
        else if( v->type == LIBERTY__VAL_DOUBLE )
            si2drComplexAttrAddFloat64Value(curr_attr,v->u.double_val,&err);
        else
            si2drComplexAttrAddInt32Value(curr_attr,v->u.int_val,&err);

        vn = v->next;
        my_free(v);
    End;
    my_free (h);
End;

void make_simple(char *name, liberty_attribute_value *v)
Begin
    curr_attr=si2drGroupCreateAttr(gs[gsindex-1],name,SI2DR_SIMPLE,&err);
    if( token_comment_buf[0] ) Begin si2drAttrSetComment(curr_attr, token_comment_buf,&err); token_comment_buf[0]=0; tok_encountered = 0; End;
    
    si2drObjectSetLineNo(curr_attr,lineno, &err);
    si2drObjectSetFileName(curr_attr,curr_file, &err);
    if( v->type == LIBERTY__VAL_BOOLEAN )
        si2drSimpleAttrSetBooleanValue(curr_attr,v->u.bool_val,&err);
    else if( v->type == LIBERTY__VAL_EXPR )
        si2drSimpleAttrSetExprValue(curr_attr,v->u.expr_val,&err);
    else if( v->type == LIBERTY__VAL_STRING )
        si2drSimpleAttrSetStringValue(curr_attr,v->u.string_val,&err);
    else if( v->type == LIBERTY__VAL_DOUBLE )
        si2drSimpleAttrSetFloat64Value(curr_attr,v->u.double_val,&err);
    else
        si2drSimpleAttrSetInt32Value(curr_attr,v->u.int_val,&err);
    my_free(v);
End;



si2drValueTypeT convert_vt(char *type)
Begin
    if( !strcmp(type,"string") )
        return SI2DR_STRING;
    if( !strcmp(type,"integer") )
        return SI2DR_INT32;
    if( !strcmp(type,"float") )
        return SI2DR_FLOAT64;
    if( !strcmp(type,"boolean") )
        return SI2DR_BOOLEAN;
    return SI2DR_UNDEFINED_VALUETYPE;
End;

yyerror(char *s)
Begin
    si2drErrorT err;
    
    si2drMessageHandlerT MsgPrinter;

    MsgPrinter = si2drPIGetMessageHandler(&err); // the printer is in another file!

    sprintf(PB,"===\nERROR === %s file: %s, line number %d\nERROR ===", s, curr_file, lineno);
    ( *MsgPrinter)(SI2DR_SEVERITY_ERR, SI2DR_SYNTAX_ERROR, 
                  PB, 
                  &err);
    
    syntax_errors++;
End;
*)
 
(*%union Begin
    char *str;
    xnumber num;
    liberty_group *group;
    liberty_attribute *attr;
    liberty_attribute_value *val;
    liberty_define *def;
    liberty_head *head;
    si2drExprT *expr;
End;*)

Const yymaxdepth = 100;

Type
  TNum   = record
    IsDouble : Boolean;
    case Boolean of
      false: (IntNum    : Integer);
      true:  (DoubleNum : Double);
  End;
  TDef   = Integer;
  TStr   = ShortString;    // YYSType uses this as a record-case-element, which is npt supported for types which require initialization like AnsiString

%}


%token COMMA SEMI LPAR RPAR LCURLY RCURLY COLON KW_DEFINE KW_DEFINE_GROUP KW_TRUE KW_FALSE PLUS MINUS MULT TDIV EQ
%token UNARY

%token <TNum> NUM
%token <TStr> TSTRING IDENT

%left PLUS MINUS 
%left MULT TDIV
%right UNARY
%left LPAR RPAR

%type <TLibertyGroup>   group file statements statement
%type <TDef>       define define_group
%type <TLibertyAttrVal> param_list attr_val attr_val_expr
%type <TStr>    s_or_i
%type <TLibertyHead> head
%type <TLibertyAttr> expr

%%

file    : {lineno := 1; syntax_errors := 0;} group {}
        ;

group   : head LCURLY { push_group($1);} statements RCURLY {pop_group($1);}
        | head LCURLY { push_group($1);}            RCURLY {pop_group($1);}
        ;


statements   : statement {}
             | statements statement  {}
            ;


statement   : simple_attr {}
            | complex_attr {}
            | define {}
            | define_group {}
            | group  {}
            ;

simple_attr : IDENT COLON attr_val_expr { CurrentGroup.AddSimpleAttr($1,$3); } SEMI
            | IDENT COLON attr_val_expr { CurrentGroup.AddSimpleAttr($1,$3); }
            | IDENT EQ    attr_val_expr { CurrentGroup.AddSimpleAttr($1,$3); } SEMI
            ;

complex_attr    : head  SEMI  { CurrentGroup.AddComplexAttr($1); }
                | head        { CurrentGroup.AddComplexAttr($1); }
                ;

head    : IDENT LPAR {tight_colon_ok := true;} param_list RPAR {
           //WriteLn('Head: ',$1);
           $$ := TLibertyHead.Create;
           $$.name     := $1;
           $$.list     := $4;
           $$.lineno   := lineno;
           $$.filename := curr_file;
           tight_colon_ok := false;
           }
        | IDENT LPAR RPAR {
           //WriteLn('Head: ',$1);
           $$ := TLibertyHead.Create;
           $$.name     := $1;
           $$.list     := Nil;
           $$.lineno   := lineno;
           $$.filename := curr_file;
           }
        ;


param_list  : attr_val { $$:=$1; }
            | param_list COMMA attr_val
              {
                  //WriteLn('param_list comma attr');
                  AppendAttrVal($1,$3);
                  $$ := $1;
              }
            | param_list attr_val
              {
                  //WriteLn('param_list attr');
                  AppendAttrVal($1,$2);
                  $$ := $1;
              }
            ;

define  : KW_DEFINE LPAR s_or_i COMMA s_or_i COMMA s_or_i RPAR SEMI  
        { (* curr_def = si2drGroupCreateDefine(gs[gsindex-1],$3,$5,convert_vt($7),&err);si2drObjectSetLineNo(curr_def,lineno,&err);si2drObjectSetFileName(curr_def,curr_file,&err);
        if( token_comment_buf[0] ) Begin si2drDefineSetComment(curr_def, token_comment_buf,&err); token_comment_buf[0]=0;End;
        if( token_comment_buf2[0] )    Begin strcpy(token_comment_buf, token_comment_buf2);token_comment_buf2[0] = 0;End;
        tok_encountered = 0;*)
        }
        ;


define_group : KW_DEFINE_GROUP LPAR s_or_i COMMA s_or_i RPAR SEMI
            { (* curr_def = si2drGroupCreateDefine(gs[gsindex-1],$3,$5,SI2DR_UNDEFINED_VALUETYPE,&err);si2drObjectSetLineNo(curr_def,lineno,&err);si2drObjectSetFileName(curr_def,curr_file,&err);
            if( token_comment_buf[0] ) Begin si2drDefineSetComment(curr_def, token_comment_buf,&err); token_comment_buf[0]=0;End;
            if( token_comment_buf2[0] )    Begin strcpy(token_comment_buf, token_comment_buf2);token_comment_buf2[0] = 0;End;
            tok_encountered = 0; *)
            }
        ;

s_or_i  : TSTRING {$$ := $1;}
        | IDENT   {$$ := $1;}
        ;

attr_val : NUM { 
                   $$ := TLibertyAttrVal.Create;
                   // I get back a floating point number... not a string, and I have to 
                   // tell if it is an integer, without using any math lib funcs?
                   if not $1.IsDouble then
                     Begin
                       //WriteLn('int attr: ',$1.IntNum);
                       $$.ValType   := LIBERTY__VAL_INT;
                       $$.IntVal    := $1.IntNum;
                     End
                   else
                     Begin
                       //WriteLn('double attr: ',$1.DoubleNum);
                       $$.ValType   := LIBERTY__VAL_DOUBLE;
                       $$.DoubleVal := $1.DoubleNum;
                     End;
            }
         |  s_or_i
               {
                   //WriteLn('string attribute: ',$1);
                   $$ := TLibertyAttrVal.Create;
                   $$.ValType   := LIBERTY__VAL_STRING;
                   $$.StringVal := $1;
               }
         |  s_or_i  COLON s_or_i 
               {
                   //WriteLn('string : string attribute: ',$1,' : ',$3);
                   $$ := TLibertyAttrVal.Create;
                   $$.ValType   := LIBERTY__VAL_STRING;
                   $$.StringVal := $1+':'+$3;
               }
         | KW_TRUE
               {
                   //WriteLn('bool true attribute');
                   $$ := TLibertyAttrVal.Create;
                   $$.ValType   := LIBERTY__VAL_BOOLEAN;
                   $$.BoolVal   := true;
               }
         | KW_FALSE
               {
                   //WriteLn('bool false attribute');
                   $$ := TLibertyAttrVal.Create;
                   $$.ValType   := LIBERTY__VAL_BOOLEAN;
                   $$.BoolVal   := false;
               }
         ;

attr_val_expr : /* NUM { $$= (liberty_attribute_value* )my_calloc(sizeof(liberty_attribute_value),1);
                 $$->type = LIBERTY__VAL_DOUBLE;
                 $$->u.double_val = $1;
               }  I am going to put nums thru the expr stuff

               |  */ TSTRING
               {
                   //WriteLn('string attribute: ',$1);
                   $$ := TLibertyAttrVal.Create;
                   $$.ValType   := LIBERTY__VAL_STRING;
                   $$.StringVal := $1;
               }
         | KW_TRUE
               {
                   //WriteLn('bool true attribute');
                   $$ := TLibertyAttrVal.Create;
                   $$.ValType   := LIBERTY__VAL_BOOLEAN;
                   $$.BoolVal   := true;
               }
         | KW_FALSE
               {
                   //WriteLn('bool false attribute');
                   $$ := TLibertyAttrVal.Create;
                   $$.ValType   := LIBERTY__VAL_BOOLEAN;
                   $$.BoolVal   := false;
               }
         | expr
               {
                   // we do not simplify expressions here
                   if $1 is TLibertyAttrVal then
                     $$ := $1 as TLibertyAttrVal
                   else
                     WriteLn('Warning: Expression not supported: ',$1.ClassName);
(*
                   // all the if/else if-s are to reduce the total number of exprs to a minimum
                   if( $1->type == SI2DR_EXPR_VAL && $1->valuetype == SI2DR_FLOAT64 && !$1->left && !$1->right )
                   Begin
                       $$= (liberty_attribute_value* )my_calloc(sizeof(liberty_attribute_value),1);
                       $$->type = LIBERTY__VAL_DOUBLE;
                       $$->u.double_val = $1->u.d;
                       // printf("EXPR->double %g \n", $1->u.d);
                       si2drExprDestroy($1,&err);
                   End
                   else if( $1->type == SI2DR_EXPR_VAL && $1->valuetype == SI2DR_INT32 && !$1->left && !$1->right )
                   Begin
                       $$= (liberty_attribute_value* )my_calloc(sizeof(liberty_attribute_value),1);
                       $$->type = LIBERTY__VAL_INT;
                       $$->u.int_val = $1->u.i;
                       // printf("EXPR->int - %d \n", $1->u.i);
                       si2drExprDestroy($1,&err);
                   End
                   else if( $1->type == SI2DR_EXPR_OP_SUB && $1->left && !$1->right 
                            && $1->left->valuetype == SI2DR_FLOAT64 && !$1->left->left && !$1->left->right )
                   Begin
                       $$= (liberty_attribute_value* )my_calloc(sizeof(liberty_attribute_value),1);
                       $$->type = LIBERTY__VAL_DOUBLE;
                       $$->u.double_val = -$1->left->u.d;
                       // printf("EXPR->double - %g \n", $1->u.d);
                       si2drExprDestroy($1,&err);
                   End
                   else if( $1->type == SI2DR_EXPR_OP_SUB && $1->left && !$1->right 
                            && $1->left->valuetype == SI2DR_INT32 && !$1->left->left && !$1->left->right )
                   Begin
                       $$= (liberty_attribute_value* )my_calloc(sizeof(liberty_attribute_value),1);
                       $$->type = LIBERTY__VAL_INT;
                       $$->u.int_val = -$1->left->u.i;
                       // printf("EXPR->double - %g \n", $1->u.d);
                       si2drExprDestroy($1,&err);
                   End
                   else if( $1->type == SI2DR_EXPR_OP_ADD && $1->left && !$1->right 
                            && $1->left->valuetype == SI2DR_FLOAT64 && !$1->left->left && !$1->left->right )
                   Begin
                       $$= (liberty_attribute_value* )my_calloc(sizeof(liberty_attribute_value),1);
                       $$->type = LIBERTY__VAL_DOUBLE;
                       $$->u.double_val = $1->left->u.d;
                       // printf("EXPR->double + %g \n", $1->u.d);
                       si2drExprDestroy($1,&err);
                   End
                   else if( $1->type == SI2DR_EXPR_OP_ADD && $1->left && !$1->right 
                            && $1->left->valuetype == SI2DR_INT32 && !$1->left->left && !$1->left->right )
                   Begin
                       $$= (liberty_attribute_value* )my_calloc(sizeof(liberty_attribute_value),1);
                       $$->type = LIBERTY__VAL_INT;
                       $$->u.int_val = $1->left->u.i;
                       // printf("EXPR->double + %g \n", $1->u.d);
                       si2drExprDestroy($1,&err);
                   End
                   else if( $1->type == SI2DR_EXPR_VAL && $1->valuetype == SI2DR_STRING && !$1->left && !$1->right 
                        (* && ( strcmp($1->u.s,"VDD") && strcmp($1->u.s,"VSS")  )  I'm getting complaints about excluding VSS and VDD, so.... they'll not be exprs any more it they are all alone *) )
                   Begin  // uh, do we need to exclude all but VSS and VDD ? no!
                    // The only way a string would turned into an expr, is if it were parsed
                    //   as an IDENT -- so no quotes will ever be seen...
                       $$= (liberty_attribute_value* )my_calloc(sizeof(liberty_attribute_value),1);
                       $$->type = LIBERTY__VAL_STRING;
                       $$->u.string_val = $1->u.s;
                       // printf("EXPR->string = %s \n", $1->u.s);
                       si2drExprDestroy($1,&err);
                   End
                   else
                   Begin
                       $$= (liberty_attribute_value* )my_calloc(sizeof(liberty_attribute_value),1);
                       $$->type = LIBERTY__VAL_EXPR;
                       $$->u.expr_val = $1;
                       // printf("left EXPR alone\n");
                   End;
*)
               }
         ;

expr     : expr PLUS expr
           {
               $$ := TLibertyExpr.Create(SI2DR_EXPR_OP_ADD,$1,$3);
           }
         | expr MINUS expr
           {
               $$ := TLibertyExpr.Create(SI2DR_EXPR_OP_SUB,$1,$3);
           }
         | expr MULT expr
           {
               $$ := TLibertyExpr.Create(SI2DR_EXPR_OP_MUL,$1,$3);
           }
         | expr TDIV  expr
           {
               $$ := TLibertyExpr.Create(SI2DR_EXPR_OP_DIV,$1,$3);
           }
         | LPAR expr RPAR
           {
               $$ := TLibertyExpr.Create(SI2DR_EXPR_OP_PAREN,$2,Nil);
           }
         | MINUS expr %prec UNARY
           {
               $$ := TLibertyExpr.Create(SI2DR_EXPR_OP_SUB,$2,Nil);
           }
         | PLUS  expr %prec UNARY
           {
               $$ := TLibertyExpr.Create(SI2DR_EXPR_OP_ADD,$2,Nil);
           }
         | NUM
           {
               $$ := TLibertyAttrVal.Create;
               if not $1.IsDouble then
                 Begin
                   //WriteLn('int attr: ',$1.IntNum);
                   ($$ as TLibertyAttrVal).ValType   := LIBERTY__VAL_INT;
                   ($$ as TLibertyAttrVal).IntVal    := $1.IntNum;
                 End
               else
                 Begin
                   //WriteLn('double attr: ',$1.DoubleNum);
                   ($$ as TLibertyAttrVal).ValType   := LIBERTY__VAL_DOUBLE;
                   ($$ as TLibertyAttrVal).DoubleVal := $1.DoubleNum;
                 End;
           }
         | IDENT
           {
               $$ := TLibertyAttrVal.Create;
               ($$ as TLibertyAttrVal).ValType   := LIBERTY__VAL_STRING;
               ($$ as TLibertyAttrVal).StringVal := $1;
           }
         ;

