import lexer
import parser
import ply.lex as lex
import ply.yacc as yacc
import sys
import copy

#These four functins are binops. First the left side(ls and right side(rs) expressions are evalued before the actual operation is performed.
def plus(x, y):
        ls_type = x['type']
        ls_val = x['value']
        rs_type = y['type']
        rs_val = y['value']
        if ls_type == 'bool' or rs_type == 'bool':
            print(f"unsupported operand type(s) for +: bool")
            exit()
        elif ls_type == rs_type:
            return {'value': ls_val + rs_val, 'type': ls_type}
        elif ls_type in ['int', 'double'] and rs_type in ['int', 'double']:
            return {'value': ls_val + rs_val, 'type': 'double'}
        elif (ls_type == "char" and rs_type=='int') or (rs_type=="char" and ls_type=='int'):
            return {'value': (ord(ls_val)+rs_val),'type':'char'}
        else:
            print(f"unsupported operand type(s) for +: '{ls_type}' and '{rs_type}'")
            exit()
def minus(x, y):    
        ls_type = x['type']
        ls_val = x['value']
        rs_type = y['type']
        rs_val = y['value']
        if ls_type in ['bool', 'string'] or rs_type in ['bool', 'string']:
            print(f"unsupported operand type(s) for -: bool or string")
            exit()
        elif ls_type == rs_type:
            return {'value': ls_val - rs_val, 'type': ls_type}
        elif ls_type in ['int', 'double'] and rs_type in ['int', 'double']:
            return {'value': ls_val - rs_val, 'type': 'double'}
        elif (ls_type == "char" and rs_type=='int') or (rs_type=="char" and ls_type=='int'):
            return {'value': (ord(ls_val)-rs_val),'type':'char'}
        else:
            print(f"unsupported operand type(s) for -: '{ls_type}' and '{rs_type}'")
            exit()
def multiply(x, y):
        ls_type = x['type']
        rs_type = y['type']
        ls_val = x['value']
        rs_val = y['value']
        if ls_type in ['bool', 'string'] or rs_type in ['bool', 'string']:
            print(f"unsupported operand type(s) for *: bool or string")
            exit()
        elif ls_type == rs_type:
            return {'value': ls_val * rs_val, 'type': ls_type}
        elif ls_type in ['int', 'double'] and rs_type in ['int', 'double']:
            return {'value': ls_val * rs_val, 'type': 'double'}
        else:
            print(f"unsupported operand type(s) for *: '{ls_type}' and '{rs_type}'")
            exit()
def divide(x, y):
        rs_val = y['value']
        rs_type = y['type']
        ls_type = x['type']
        ls_val = x['value']
        if ls_type in ['bool', 'string'] or rs_type in ['bool', 'string']:
            print(f"unsupported operand type(s) for /: bool or string")
            exit()
        elif ls_type in ['int','double'] and rs_type in ['int','double']:
            if rs_val==0:
                print(f"Division by Zero error")
                exit()
            return {'value': ls_val / rs_val, 'type': 'double'}                
        else:
            print(f"unsupported operand type(s) for /: '{ls_type}' and '{rs_type}'")
            exit()
def neg_handler(x):
        ls_type = x['type']
        ls_val = x['value']
        if ls_type in ['int', 'double']:
            return {'value': -ls_val, 'type': ls_type}
        else:
            print(f"unsupported operand type(s) for -: '{ls_type}'")
            exit()

def finder(env, identifier):

        if identifier in env:
            return env
        elif 'parent' in env:
            return finder(env['parent'], identifier)
        else:
            return None  

def lookup(env, identifier):

        var_env = finder(env, identifier)
        if var_env is None:
            print(f"Variable '{identifier}' not declared")
            exit()
        else:
            return var_env[identifier] 
def struct_lookup(env, identifier):
        
        var_env = finder(env, identifier[1])
        if var_env is None:
            print(f"attribute error")
            exit()
        else:
            return var_env[identifier[1]]   

def declare(env, identifier, value):
        var_env = finder(env, identifier)
        if var_env is None:
            #This is converting char to ascii value so that plus and minus op could be performed on the char type expressions
            if value['type'] == 'char':
                value['value'] = ord(value['value'])
            env[identifier] = value
        else:
            print('Redeclaration Error')
            exit()
def update(env, identifier, value):
        var_env = finder(env, identifier)
        if var_env != None:
            #converting 
            if value['type'] == 'char':
                value['value'] = ord(value['value'])
            if var_env[identifier]['type'] == value['type']:
                var_env[identifier]['value'] = value['value']
            elif var_env[identifier]['type'] == 'double' and value['type'] in ['char', 'int']:
                var_env[identifier]['value'] = float(value['value'])
            elif var_env[identifier]['type'] == 'int' and value['type'] in ['char']:
                var_env[identifier]['value'] = int(value['value'])
            else:
                print(f"TypeError: Cannot set type '{var_env[identifier]['type']}' with type '{value['type']}'")
                exit()
            # print(lookup(env,"x"),"Ad")
        else:
            print(f"Variable '{identifier}' not declared")
            exit()

def execute_for(dec_statement, con_statement, inc_statement, block, var_dict):
    run(dec_statement, var_dict)
    if len(con_statement) == 0:
        while True:
            for branch in block:
                result = run(block, var_dict)
                if result != None:
                    if result[0] == "break":
                        return
                    elif result[0]=="continue":
                        break
            run(inc_statement, var_dict)
    else:
        while(True):
            if(not run(con_statement, var_dict)['value']):
                break;
            for branch in block[1]:
                result = run(branch, var_dict)
                if result != None:
                    if result[0] == "break":
                        return
                    elif result[0]=="continue":
                        break

            run(inc_statement, var_dict)
            first=list(var_dict.keys())[1]
            En_Value = var_dict[first]
            parent=var_dict['parent']
            var_dict.clear() 
            var_dict['parent']=parent
            var_dict[first] = En_Value
    return 
def mod(x, y):
        op = '%'
        ls_type = x['type']
        rs_type = y['type']
        ls_val = x['value']
        rs_val = y['value']
        if ls_type == 'int' and rs_type == 'int':
            return {'value': int(ls_val % rs_val), 'type': ls_type}
        elif ls_type in ['int', 'double'] and rs_type in ['int', 'double']:
            return {'value': ls_val % rs_val, 'type': 'double'}
        else:
            print(f"unsupported operand type(s) for %: '{ls_type}' and '{rs_type}'")
            exit()
def power(x, y):
    ls_type = x['type']
    rs_type = y['type']
    ls_val = x['value']
    rs_val = y['value']
    if ls_type == 'int' and rs_type == 'int':
        return {'value': int(ls_val ** rs_val), 'type': ls_type}
    elif ls_type in ['int', 'double'] and rs_type in ['int', 'double']:
        return {'value': ls_val ** rs_val, 'type': 'double'}
    else:
        print(f"unsupported operand type(s) for ^: '{ls_type}' and '{rs_type}'")
        exit()

def run(tree,env):
    binoperands = ['plus', 'minus', 'divide','multiply']
    logicaloperand=['and','or','xor']
    comparsionopernad=["greater","lesser","greaterequal","lesserequal","doubleequal","notequal"]
    otheroperand=['mod','power','plusplus','minusminus']


    DEFAULT = {
        'int': 0,
        'string': "",
        'char': '\0',
        'double': 0.0,
        'bool': False,
    }
    if tree is None or len(tree) ==0:
        return
    elif tree[0]=="break":
        return ["break"]
    elif tree[0]=="continue":
        return ["continue"]  
    elif tree[0] =="exp":
        return run(tree[1],env) 
    elif tree[0] in binoperands:
        if tree[0] == "plus":
            ls = run(tree[1], env)
            rs = run(tree[2], env)
            return plus(ls, rs)
        elif tree[0] == "minus":
            ls = run(tree[1], env)
            rs = run(tree[2], env)
            return minus(ls, rs)
        elif tree[0] == "multiply":
            ls = run(tree[1], env)
            rs = run(tree[2], env)
            return multiply(ls, rs)
        elif tree[0] == "divide":
            ls = run(tree[1], env)
            rs = run(tree[2], env)
            return divide(ls, rs)
    elif tree[0] == "identifier":
        return lookup(env, tree[1])
    elif tree[0] == "double":
        return {'value': float(tree[1]), 'type': 'double'}
    elif tree[0] == "int":
        return {'value': int(tree[1]), 'type': 'int'}
    elif tree[0] == "char":
         return {'value': tree[1], 'type': 'char'}
    elif tree[0] == "bool":
        b = tree[1]
        if b == "true":
            return {'value': True, 'type': 'bool'}
        elif b == "false":
            return {'value': False, 'type': 'bool'}
        else:
            Exception(f'Value not understood for bool type: {b}')
    elif tree[0] == "string":
        return {'value': tree[1], 'type': 'string'}

    elif tree[0] == "var_declare":
        identifier = tree[2]
        dtype = tree[1]
        declare(env, identifier, {'value': DEFAULT[dtype], 'type': dtype})
        if len(tree)>3:
            result = run(tree[3], env)
            update(env, identifier, result)
    elif tree[0] == "var_assign":
        identifier = tree[1]
        result = run(tree[2], env)
        update(env, identifier, result)
    elif tree[0]=="print":
        exps = tree[1]
        for exp in exps:
            result = run(exp, env)
            #converting back char from its ascii value to char 
            if result['type'] == 'char':
                output = chr(result['value'])
            else:
                output = result['value']
            print(output, end=' ')
        print()

    elif tree[0]=="not":
        ls = run(tree[1], env)
        return {'value': not ls['value'], 'type': 'bool'}

    elif tree[0]=="and":
        ls = run(tree[1], env)
        rs = run(tree[2], env)
        return {'value': ls['value'] and rs['value'], 'type': 'bool'}

    elif tree[0]=="xor":
        ls = run(tree[1], env)
        rs = run(tree[2], env)
        if (ls['type'] is "int" and rs['type'] is "int") or (ls['type'] is "bool" and rs['type'] is "bool"):
            print(f"unsupported operand type(s) for xor: '{ls['type']}' and '{rs['type']}'")
            exit()

        return {'value': ls['value']^rs['value'], 'type': 'bool'}

    elif tree[0]=="or":
        ls = run(tree[1], env)
        rs = run(tree[2], env)
        return {'value': ls['value'] or rs['value'], 'type': 'bool'}

    elif tree[0] == "lessthan":
        ls = run(tree[1], env)
        rs = run(tree[2], env)
        return {'value': ls['value'] < rs['value'], 'type': 'bool'}
    elif tree[0] == "greaterthan":
        ls = run(tree[1], env)
        rs = run(tree[2], env)
        return {'value': ls['value'] > rs['value'], 'type': 'bool'}
    elif tree[0] == "lessequal":
        ls = run(tree[1], env)
        rs = run(tree[2], env)
        return {'value': ls['value'] <= rs['value'], 'type': 'bool'}
    elif tree[0] == "greaterequal":
        ls = run(tree[1], env)
        rs = run(tree[2], env)
        return {'value': ls['value'] >= rs['value'], 'type': 'bool'}
    elif tree[0] == "notequal":
        ls = run(tree[1], env)
        rs = run(tree[2], env)
        return {'value': ls['value'] != rs['value'], 'type': 'bool'}
    elif tree[0] == "equalequal":
        ls = run(tree[1], env)
        rs = run(tree[2], env)
        return {'value': ls['value'] == rs['value'], 'type': 'bool'}
    elif tree[0] == "neg":
        rs = run(tree[1], env)
        return neg_handler(rs)
    elif tree[0]=="struct_def":
        identifier=tree[1]
        variables=tree[2]
        struct_temp[identifier]= {'value': {}, 'type': "struct","temp":identifier}
        for var in variables:
            struct_temp[identifier]['value'][var[2]]={'value': DEFAULT[var[1]], 'type': var[1]}
    elif tree[0]=="struct_dec":
        tempelate=tree[1]
        identifier=tree[2]
        temp_dic=copy.deepcopy(struct_temp[tempelate])
        declare(env,identifier,temp_dic)
    elif tree[0]=="struct_va":
        identifier = tree[1]
        result = run(tree[2][2], env)
        update(env[identifier]['value'], tree[2][1], result)

    elif tree[0]=="struct_vf":
        identifier=tree[1]
        return struct_lookup(env[identifier]['value'],tree[2])

    elif tree[0]=="for":
        block = tree[4]
        initial_statement = tree[1]
        con_statement = tree[2]
        third_statement = tree[3]
        new_env={}
        new_env['parent']=env
        ret_val=execute_for(initial_statement,con_statement,third_statement,block,new_env)

    elif tree[0] == "plusplus":
            # print(tree)
            result = lookup(env,tree[1])
            if result['type'] == 'int':
                update(env, tree[1], {'value': result['value'] + 1, 'type': result['type']})
                return lookup(env,tree[1])
            else:
                print(f"unsupported operand type(s) for ++, '{tree[1]}' is type {result['type']}")
                exit()

    elif tree[0] == "minusminus":
        result = lookup(env,tree[1])
        if result['type'] == 'int':
            update(env, tree[1], {'value': result['value'] - 1, 'type': result['type']})
            return lookup(env,tree[1])
        else:
            print(f"unsupported operand type(s) for --, '{tree[1]}' is type {result['type']}")
            exit()

    elif tree[0] == "mod":
            ls = run(tree[1], env)
            rs = run(tree[2], env)
            return mod(ls, rs)
    elif tree[0] == "power":
        ls = run(tree[1], env)
        rs = run(tree[2], env)
        return power(ls, rs)


struct_temp={}
def main(p):
    environment_dict={}
    global struct_temp
    # print(p)
    if p is None:
        pass
    for branch in p:
        run(branch, environment_dict)
if len(sys.argv)<2:
    test=["standard_output","variables","expressions","for_loop","structs"]
    for i in test:
        with open(f"./test_cases/{i}.txt", 'r') as f:
            code = f.read()
            mylexer = lex.lex(module=lexer)
            # Tokenize
            # mylexer.input(code)
            # while True:
            #     tok = mylexer.token()
            #     if not tok: 
            #         break
                # print(tok)
            myparser = yacc.yacc(module=parser)
            parse_tree = myparser.parse(code, lexer=mylexer)
            # print(parse_tree) 
            print(f"<<<<<<<<<<<<<Testing {i} >>>>>>>>>>>>>>>\n")
            print("OUTPUT OF THE INTERPETER: \n")
            try:
                main(parse_tree)
            except:
                print("<<<<<<<<<<<<<<END>>>>>>>>>>>>>>>>>>>\n")
                continue
            print("<<<<<<<<<<<<<<END>>>>>>>>>>>>>>>>>>>\n")

elif sys.argv[1] != "testmode":
    mylexer = lex.lex(module=lexer)

    filename=sys.argv[1]
    with open(f"./test_cases/{filename}", 'r') as f:
        code = f.read()
        # print(code)


    # Tokenize
    # mylexer.input(code)
    # while True:
    #     tok = mylexer.token()
    #     if not tok: 
    #         break
        # print(tok)


    myparser = yacc.yacc(module=parser)
    parse_tree = myparser.parse(code, lexer=mylexer)
    # print(parse_tree) 
    print("OUTPUT OF THE INTERPETER: \n")
    main(parse_tree)
else:
    test=["standard_output","variables","expressions","for_loop","structs"]
    for i in test:
        with open(f"./test_cases/{i}.txt", 'r') as f:
            code = f.read()
            mylexer = lex.lex(module=lexer)
            # Tokenize
            # mylexer.input(code)
            # while True:
            #     tok = mylexer.token()
            #     if not tok: 
            #         break
                # print(tok)
            myparser = yacc.yacc(module=parser)
            parse_tree = myparser.parse(code, lexer=mylexer)
            # print(parse_tree) 
            print(f"<<<<<<<<<<<<<Testing {i} >>>>>>>>>>>>>>>\n")
            print("OUTPUT OF THE INTERPETER: \n")
            try:
                main(parse_tree)
            except:
                print("<<<<<<<<<<<<<<END>>>>>>>>>>>>>>>>>>>\n")
                continue
            print("<<<<<<<<<<<<<<END>>>>>>>>>>>>>>>>>>>\n")
            


