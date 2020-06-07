from lexer import tokens
import ply.yacc as yacc
import sys


precedence = (
    ('left', 'OR'),
    ('left', 'AND','XOR'),
    ('left', 'EQ','NE'),
    ('left', 'LT', 'LE', 'GT', 'GE'),
    ('left', 'PLUS', 'MINUS'),
    ('left', 'MULTIPLY', 'DIVIDE'),
    ('left', 'MOD' ),
    ('right', 'NOT','POWER'),
)


def p_compiler(p):
    '''
    compiler : compiler statement
             | statement
             | empty 

    '''
    if len(p)>2:
        p[0]=p[1]+[p[2]]
    else:
        p[0]=[p[1]]

    


def p_statement(p):
    '''
    statement : var_assign SEMICOLON
              | var_declare SEMICOLON
              | output SEMICOLON
              | struct SEMICOLON
              | struct_dec SEMICOLON
              | struct_vf SEMICOLON
              | struct_va SEMICOLON
              | for_loop SEMICOLON
              
    '''
    p[0]=p[1]

#variable declaration and assignment
def p_var_assign(p):
    '''
    var_assign : IDENTIFIER ASSIGN exp
    '''
    p[0] = ('var_assign', p[1], p[3])

def p_var_declare(p):
    '''
    var_declare : TYPE IDENTIFIER 
                | TYPE IDENTIFIER ASSIGN exp
    ''' 
    if(len(p) > 3):
        p[0] = ('var_declare', p[1], p[2], p[4])
    else:
        p[0] = ('var_declare', p[1], p[2])




#for loop
def p_for(p):
    '''
    for_loop : FOR LPARENT dec_statement SEMICOLON con_statement SEMICOLON inc_statement RPARENT forblock
    '''
    p[0]=('for',p[3],p[5],p[7],p[9])
    

def p_dec_statement(p):
    '''
    dec_statement : var_declare
                    | var_assign
                    | empty
    ''' 
    p[0] = p[1]

def p_con_statement(p):
    '''
    con_statement : exp
                     | empty
    '''
    p[0] = p[1]

def p_inc_statement(p):
    '''
    inc_statement : var_assign
                  | exp
                  | empty
    '''
    p[0] = p[1]

def p_forblock(p):
    '''
    forblock : LCURLY forstatements RCURLY
    '''
    p[0] = ('block', p[2])

def p_statements(p):
    '''
    forstatements : forstatements statement
                  | statement
                  | empty
    '''
    if len(p) > 2:
        p[0] = p[1] + [p[2]]
    else:
        p[0] = [p[1]]

#struct
def p_struct(p):
    '''
    struct : STRUCT IDENTIFIER LCURLY vars RCURLY
    '''
    p[0] = ('struct_def', p[2], p[4])

def p_vars(p):
    '''
    vars : vars var_declare SEMICOLON
         | var_declare SEMICOLON
         | empty
    '''
    if len(p) > 3:
        p[0] = p[1] + [p[2]]
    else:
        p[0] = [p[1]]

def p_struct_declare(p):
    '''
    struct_dec : STRUCT IDENTIFIER IDENTIFIER
    '''
    p[0] = ('struct_dec', p[2], p[3])

def p_struct_val_assign(p):
    '''
    struct_va : IDENTIFIER DOT var_assign
    '''
    p[0] = ('struct_va', p[1], p[3])

def p_struct_val_fetch(p):
    '''
    struct_vf : IDENTIFIER DOT IDENTIFIER 
    '''
    p[0] = ('struct_vf', p[1], ('identifier', p[3])) 

# Expressions

def p_exp_identifier(p):
    'exp : IDENTIFIER'
    p[0] = ("identifier",p[1])

def p_exp_parent(p):
    'exp : LPARENT exp RPARENT'
    p[0] = ('exp',p[2])

def p_exp_neg(p):
    'exp : MINUS exp'
    p[0] = ('neg', p[2])

def p_exp_struct_vf(p):
    'exp : struct_vf'
    p[0] = p[1]

def p_stmt_exp(p):
    'statement : exp SEMICOLON'
    p[0] = ('exp', p[1])

#loop conditions
def p_break(p):
    'statement : BREAK SEMICOLON'
    p[0]=("break",p[1])
def p_continue(p):
    'statement : CONTINUE SEMICOLON'
    p[0]=("continue",p[1])

# Types
def p_exp_string(p):
    'exp : STRING'
    p[0] = ("string", p[1])
def p_exp_bool(p):
    'exp : BOOL'
    p[0] = ("bool", p[1])
def p_exp_int(p):
    'exp : INT'
    p[0] = ("int", p[1])
def p_exp_double(p):
    'exp : DOUBLE'
    p[0] = ("double", p[1])
def p_exp_char(p):
    'exp : CHAR'
    p[0] = ("char", p[1])

#binops
def p_exp_plus(p):
    'exp : exp PLUS exp'
    p[0] = ("plus", p[1], p[3])
def p_exp_minus(p):
    'exp : exp MINUS exp'
    p[0] = ("minus", p[1], p[3])
def p_exp_multiply(p):
    'exp : exp MULTIPLY exp'
    p[0] = ("multiply", p[1], p[3])
def p_exp_divide(p):
    'exp : exp DIVIDE exp'
    p[0] = ("divide", p[1], p[3])


#print
def p_stmt_print(p):
    'output : PRINT LPARENT printblock RPARENT'
    p[0] = ("print", p[3])

def p_printblock(p):
    'printblock : params'
    p[0] = p[1]
def p_printblock_empty(p):
    'printblock : '
    p[0] = []
def p_params(p):
    'params : exp COMMA params'
    p[0] = [ p[1] ] + p[3]
def p_params_exp(p):
    'params : exp'
    p[0] = [ p[1] ]


#logical operator
def p_exp_not(p):
    'exp : NOT exp'
    p[0] = ("not", p[2])
def p_exp_and(p):
    'exp : exp AND exp'
    p[0] = ("and", p[1], p[3])
def p_exp_or(p):
    'exp : exp OR exp'
    p[0] = ("or", p[1], p[3])
def p_exp_xor(p):
    'exp : exp XOR exp'
    p[0] = ("xor", p[1], p[3])

#comparison operator
def p_exp_lt(p):
    'exp : exp LT exp'
    p[0] = ('lessthan', p[1], p[3])
def p_exp_gt(p):
    'exp : exp GT exp'
    p[0] = ('greaterthan', p[1], p[3])
def p_exp_le(p):
    'exp : exp LE exp'
    p[0] = ('lessequal', p[1], p[3])
def p_exp_ge(p):
    'exp : exp GE exp'
    p[0] = ('greaterequal', p[1], p[3])
def p_exp_ne(p):
    'exp : exp NE exp'
    p[0] = ('notequal', p[1], p[3])
def p_exp_eq(p):
    'exp : exp EQ exp'
    p[0] = ('equalequal', p[1], p[3])


#power and mod
def p_exp_mod(p):
    'exp : exp MOD exp'
    p[0] = ('mod', p[1], p[3])
def p_exp_power(p):
    'exp : exp POWER exp'
    p[0] = ('power', p[1], p[3])

#oneline operator
def p_exp_plusplus(p):
    'exp : IDENTIFIER PLUSPLUS'
    p[0] = ("plusplus", p[1])

def p_exp_minusminus(p):
    'exp : IDENTIFIER MINUSMINUS'
    p[0] = ("minusminus", p[1])



def p_empty(p):
    '''
    empty :
    '''
    p[0] = ()

def p_error(p):
    print("syntax error")
    print("error at line no: ",str(p.lexer.lineno-1), "or ", str(p.lexer.lineno))
    exit()
