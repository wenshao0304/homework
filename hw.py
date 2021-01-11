import ply.lex as lex
import ply.yacc as yacc
import networkx as nx 
from networkx.drawing.nx_agraph import graphviz_layout
import matplotlib.pyplot as plt

reserved = {
    'if' : 'IF',
    'else' : 'ELSE',
    'for' : 'FOR',
}
tokens = [
    'NAME', 'NUMBER',
    'PLUS', 'MINUS', 'TIMES', 'DIVIDE', 'MODULO', 'EQUALS','POWER',
    'LPAREN', 'RPAREN',
    'EQUAL', 'NOTEQ', 'LARGE', 'SMALL', 'LRGEQ', 'SMLEQ','SQRT'
] + list(reserved.values())
t_PLUS    = r'\+'
t_MINUS   = r'-'
t_TIMES   = r'\*'
t_DIVIDE  = r'/'
t_MODULO  = r'%'
t_EQUALS  = r'='
t_POWER  = r'\^'
t_SQRT   =  r'\*\*'
t_EQUAL   = r'\=\='
t_LPAREN  = r'\('
t_RPAREN  = r'\)'
t_NOTEQ   = r'\!\='
t_LARGE   = r'\>'
t_SMALL   = r'\<'
t_LRGEQ   = r'\>\='
t_SMLEQ   = r'\<\='




def t_NAME(t):

    r'[a-zA-Z_][a-zA-Z_0-9]*'

    t.type = reserved.get(t.value,'NAME')
    return t


def t_NUMBER(t):
    r'\d+'
    t.value = int(t.value)
    return t


t_ignore = " \t"

def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")

def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

lexer = lex.lex()

precedence = (
    ('left', 'PLUS', 'MINUS'),
    ('left', 'TIMES', 'DIVIDE', 'MODULO'),
    ('left','SQRT'),
    ('left','POWER'),
    ('right', 'UMINUS'),
)

names = {}


def p_for(p):
    '''statement    : FOR NAME NUMBER PLUS NUMBER
                    | FOR NAME NUMBER MINUS NUMBER
                    | FOR NAME NUMBER TIMES NUMBER'''
    i=p[2]
    num1=p[3]
    num2=p[5]
    total=num1
    if(p[4]=='+'):
        for i in range(num1+1,num2+1):
            total+=i
    elif(p[4]=='-'):
        for i in range(num1-1,num2-1,-1):
            total-=i
    elif(p[4]=='*'):
        for i in range(num1+1,num2+1):
            total=total*i      
    names[p[2]]=total

def p_if(p):
    '''statement    : IF compare NAME EQUALS expression
                    | IF compare NAME EQUALS expression ELSE NAME EQUALS expression '''

    if p[2]==True:
        names[p[3]] = p[5]
    elif p[2]==False:
        if p[7] is not None:
            names[p[7]]=p[9]

def p__assign(p):
    'statement : NAME EQUALS expression'
    names[p[1]] = p[3]
    
def p_expr(p):
    'statement : expression'
    print(p[1])
def p_comp(p):
    'statement : compare'
    print(p[1])
    

def p_compare(p):
    '''compare : expression EQUAL expression
                          | expression NOTEQ expression
                          | expression LARGE expression
                          | expression SMALL expression
                          | expression LRGEQ expression
                          | expression SMLEQ expression'''
    if p[2] == '==':
        p[0] = p[1] == p[3]
    elif p[2] == '!=':
        p[0] = p[1] != p[3]
    elif p[2] == '>':
        p[0] = p[1] > p[3]
    elif p[2] == '<':
        p[0] = p[1] < p[3]
    elif p[2] == '>=':
        p[0] = p[1] >= p[3]
    elif p[2] == '<=':
        p[0] = p[1] <= p[3]
    print(p[0])

def p_expression_binop(p):
    '''expression : expression PLUS expression
                          | expression MINUS expression
                          | expression TIMES expression
                          | expression DIVIDE expression
                          | expression POWER expression
                          | expression SQRT expression
                          | expression MODULO expression'''
    if p[2] == '+':
        p[0] = p[1] + p[3]
        
    elif p[2] == '-':
        p[0] = p[1] - p[3]
        
    elif p[2] == '*':
        p[0] = p[1] * p[3]
        
    elif p[2] == '/':
        p[0] = p[1] / p[3]
        
    elif p[2] == '^':
        p[0] = p[1] ** p[3]
        
    elif p[2] == '**':
         p[0] = p[1] ** (1/p[3])
         
    elif p[2] == '%':
        p[0] = p[1] % p[3]

def p_expression_uminus(p):
    'expression : MINUS expression %prec UMINUS'
    p[0] = -p[2]


def p_expression_group(p):
    'expression : LPAREN expression RPAREN'
    p[0] = p[2]


def p_expression_number(p):
    'expression : NUMBER'
    p[0] = p[1]


def p_expression_name(p):
    'expression : NAME'
    try:
        p[0] = names[p[1]]
    except LookupError:
        print("Undefined name '%s'" % p[1])
        p[0] = 0


def p_error(p):
    print("Syntax error at '%s'" % p.value)

yacc.yacc()

def find_top_prio(lst): 
    prio_dict = {'-':1,'+':2,'*':3,'/':4,'**':5,'^':6}
    top_prio = 1 
    count_ops = 0 
    for ops in lst: 
        if ops in prio_dict: 
            count_ops += 1 
            if prio_dict[ops] > 1: 
                top_prio = prio_dict[ops] 
    return top_prio, count_ops

def TAC():
    
    prio_dict = {'-':1,'+':2,'*':3,'/':4,'**':5,'^':6}
    op_lst = [] 
    op_lst.append(['op','arg1','arg2','result'])
    top_prio, count_ops = find_top_prio(ip_lst) 
    ip = ip_lst 
    i, res = 0, 0
    while i in range(len(ip)): 
        if ip[i] in prio_dict: 
            op = ip[i] 
            if (prio_dict[op]>=top_prio) and (ip[i+1] in prio_dict): 
                res += 1 
                op_lst.append([ip[i+1],ip[i+2],' ','t'+str(res)]) 
                ip[i+1] = 't'+str(res) 
                ip.pop(i+2) 
                i = 0 
                top_prio, count_ops = find_top_prio(ip) 
            elif prio_dict[op]>=top_prio: 
                res += 1 
                op_lst.append([op,ip[i-1],ip[i+1],'t'+str(res)]) 
                ip[i] = 't'+str(res) 
                ip.pop(i-1) 
                ip.pop(i) 
                i = 0 
                top_prio, count_ops = find_top_prio(ip) 
        if len(ip) == 1:
            op_lst.append(['=',ip[i],' ','a'])
            G = nx.DiGraph()
            G.clear()
            data = op_lst
            for i in range(1,len(data)-1):
                if(data[i][1]==data[i][2]):
                   data[i][1] = "L_" + data[i][1]
                   data[i][2] = "R_" + data[i][2]

                G.add_node("%s" %(data[i][1]))
                G.add_node("%s" %(data[i][2]))
                G.add_node("%s" %(data[i][3]))

                G.add_edge("%s" %(data[i][3]), "%s" %(data[i][1]))
                G.add_edge("%s" %(data[i][3]), "%s" %(data[i][2]))
        
            nx.nx_agraph.write_dot(G,'test.dot')
            plt.title('draw_networkx')
            pos = graphviz_layout(G, prog='dot')
            nx.draw(G, pos, with_labels=True, arrows=False, node_size=600)

            plt.show()
            plt.clf()
            plt.cla()
        
        i+=1
    for i in range(len(op_lst)):
        print(op_lst[i])

while True:
    try:
        s = input('calc > ')
        
        lexer.input(s)
        while True:
            tok = lexer.token()
            if not tok:
                break
            print(tok)
        ip_str = s
        ip_lst = list(map(str,ip_str))
        TAC()
        
    except EOFError:
        break  
    yacc.parse(s)
    print(names)
    
    
