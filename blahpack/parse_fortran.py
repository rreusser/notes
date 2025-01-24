import sys
import json
from fparser.common.readfortran import FortranStringReader
from fparser.two.parser import ParserFactory
from fparser.two.utils import walk, get_child
from fparser.two import Fortran2003

from fparser.two.Fortran2003 import (
    Add_Operand,
    Assignment_Stmt,
    Assumed_Size_Spec,
    Actual_Arg_Spec_List,
    Block_Nonlabel_Do_Construct,
    Comment,
    Dummy_Arg_List,
    Else_Stmt,
    Else_If_Stmt,
    End_Do_Stmt,
    End_If_Stmt,
    End_Function_Stmt,
    End_Subroutine_Stmt,
    Entity_Decl,
    Entity_Decl_List,
    Equiv_Operand,
    Execution_Part,
    Function_Stmt,
    Function_Subprogram,
    If_Stmt,
    If_Then_Stmt,
    If_Construct,
    Implicit_Part,
    Int_Literal_Constant,
    Intrinsic_Function_Reference,
    Intrinsic_Name,
    Intrinsic_Type_Spec,
    Level_1_Expr,
    Level_2_Expr,
    Level_2_Unary_Expr,
    Level_3_Expr,
    Level_4_Expr,
    Logical_Literal_Constant,
    Loop_Control,
    Mult_Operand,
    Name,
    Named_Constant_Def_List,
    Named_Constant_Def,
    Nonlabel_Do_Stmt,
    Or_Operand,
    Parenthesis,
    Part_Ref,
    Program,
    Real_Literal_Constant,
    Return_Stmt,
    Section_Subscript_List,
    Specification_Part,
    Subroutine_Stmt,
    Subroutine_Subprogram,
    Type_Declaration_Stmt,

    #Block_Label_Do_Construct,
    #Continue_Stmt,
    #Label,
    #Label_Do_Stmt,
)

LEVEL_4_OPERATORS = {
    '.OR.': '||',
    '.AND.': '&&',
    '.NOT.': '!',
    '.EQ.': '===',
    '.NE.': '!==',
    '.GT.': '>',
    '.GE.': '>=',
    '.LT.': '<',
    '.LE.': '<=',
    '<': '<',
    '>': '>',
    '<=': '<=',
    '>=': '>=',
    '/=': '!=',
    '==': '==',
}

LEVEL_2_OPERATORS = {
    '+': '+',
    '-': '-',
    '*': '*',
    '/': '/',
    '**': '**',
    'MOD': '%'
}

UNARY_OPERATORS = {
    '+': '+',
    '-': '-',
}

LOGICAL_LITERALS = {
    '.TRUE.': True,
    '.FALSE.': False
}

ASSIGNMENT_OPERATORS = {
    '=': '='
}

INTRINSIC_FUNCTIONS = {
    'sqrt': 'sqrt',
    'abs': 'abs',
    'dabs': 'abs',
    'min': 'min',
    'max': 'max',
    'sign': 'sign',
}

TYPES = {
    'logical': 'bool',
    'real': 'float64',
    'real(wp)': 'float64',
    'double precision': 'float64',
    'integer': 'int32',
}

def index_of (parent, node):
    for idx, child in enumerate(parent.children):
        if child is node:
            return idx
    return -1

def eprint(*args, **kwargs):
    print(*args, file=sys.stderr, **kwargs)

def filt(lst):
    return [item for item in lst if item is not None]

def flatten(S):
    if S == []:
        return S
    if isinstance(S[0], list):
        return flatten(S[0]) + flatten(S[1:])
    return S[:1] + flatten(S[1:])

def get_value(node):
    if isinstance(node, Int_Literal_Constant):
        return int(node.string)
    elif isinstance(node, Real_Literal_Constant):
        return float(node.string.replace('d', 'e').replace('D', 'E'))
    else:
        raise TypeError('Unrecognized node type: %s' % node.__class__.__name__)

def find_all(node, node_type):
    return [node for node in walk(node) if isinstance(node, node_type)]

def collect_parameter_defs (specification):
    def_nodes = find_all(specification, Named_Constant_Def_List)
    defs = []
    for node in def_nodes:
        for defn in find_all(node, Named_Constant_Def):
            name = get_child(defn, Name)
            value_node = defn.items[1]
            defs.append({
                'name': name.string,
                'value': get_value(value_node)
            })
    return defs

def block (body):
    if isinstance(body, list):
        return {
            'type': 'BlockStatement',
            'body': body,
        }
    else:
        return body

class FortranTranslator:
    def __init__(self, code):
        self.code = code

        reader = FortranStringReader(code, ignore_comments=False)
        parser = ParserFactory().create(std="f2003")
        self.ast = parser(reader)
    
    def to_estree(self):
        return self._to_estree(self.ast)

    def parse_loop_control(self, ctrl):
        assert isinstance(ctrl, Loop_Control)
        assert len(ctrl.items) == 3

        if ctrl.items[0] is not None and ctrl.items[1] is None and ctrl.items[2] is None:
            return {
                'type': 'WhileStatement',
                'test': self._to_estree(ctrl.items[0]),
                'body': None
            }
        else:
            var = self._to_estree(ctrl.items[1][0])
            rng = ctrl.items[1][1]
            start = self._to_estree(rng[0])
            end = self._to_estree(rng[1])
            try:
                inc = ctrl.items[1][2]
            except IndexError:
                inc = Int_Literal_Constant('1')

            return {
                'type': 'ForStatement',
                'init': {
                    'type': 'AssignmentExpression',
                    'operator': '=',
                    'left': var,
                    'right': start
                },
                'test': {
                    'type': 'BinaryExpression',
                    'left': var,
                    'operator': '<=',
                    'right': end
                },
                'update': {
                    'type': 'AssignmentExpression',
                    'operator': '+=',
                    'left': var,
                    'right': self._to_estree(inc)
                },
                'body': None
            }

    def collect_specification_vars (self, specification):
        assert isinstance(specification, Specification_Part)

        def recurse(node, depth=0):
            if not node or isinstance(node, str):
                return None
            elif isinstance(node, Comment):
                return None
            elif isinstance(node, Name):
                return None
            elif isinstance(node, Implicit_Part):
                pass
            elif isinstance(node, Type_Declaration_Stmt):
                vars = []
                its = get_child(node, Intrinsic_Type_Spec)
                assert isinstance(its, Intrinsic_Type_Spec)
                type = TYPES[its.string.lower()]

                decl_list = get_child(node, Entity_Decl_List)
                assert isinstance(decl_list, Entity_Decl_List)
                for decl in decl_list.children:
                    assert isinstance(decl, Entity_Decl)

                    name = get_child(decl, Name)
                    assert isinstance(name, Name)

                    spec = get_child(decl, Assumed_Size_Spec)
                    if spec is not None:
                        # Handle spec info
                        pass

                    vars.append({
                        'type': type,
                        'name': name.string
                    })

                return vars

            return filt([recurse(child, depth + 1) for child in node.children])

        vars = filt(flatten([recurse(child) for child in specification.children]))

        vars.sort(key=lambda x: (-len(x['name']), x['name']))

        return vars

    def _to_estree (self, node):
        if isinstance(node, list):
            return filt([self._to_estree(child) for child in node])

        elif node is None:
            raise TypeError('Cannot convert None to ESTree')

        elif isinstance(node, Add_Operand):
            return {
                'type': 'BinaryExpression',
                'left': self._to_estree(node.items[0]),
                'operator': LEVEL_2_OPERATORS[node.items[1]],
                'right': self._to_estree(node.items[2])
            }

        elif isinstance(node, Block_Nonlabel_Do_Construct):
            if get_child(node, Nonlabel_Do_Stmt):
                nonlabel_do_stmt = get_child(node, Nonlabel_Do_Stmt)
                end_do_stmt = get_child(node, End_Do_Stmt)

                nonlabel_do_stmt_idx = index_of(node, nonlabel_do_stmt)
                end_do_stmt_idx = index_of(node, end_do_stmt)

                if nonlabel_do_stmt_idx > -1:
                    body = node.children[nonlabel_do_stmt_idx + 1: end_do_stmt_idx]
                else:
                    raiseError('Unexpected structure in Block_Nonlabel_Do_Construct')

                loop_control = get_child(nonlabel_do_stmt, Loop_Control)
                assert isinstance(loop_control, Loop_Control)

                expr = self.parse_loop_control(loop_control)
                expr['body'] = block(self._to_estree(node.children[nonlabel_do_stmt_idx + 1: end_do_stmt_idx]))
                return expr
            else:
                raise TypeError('Unhandled Block_Nonlabel_Do_Construct')

        elif isinstance(node, Assignment_Stmt):
            return {
                'type': 'ExpressionStatement',
                'expression': {
                    'type': 'AssignmentExpression',
                    'operator': ASSIGNMENT_OPERATORS[node.items[1]],
                    'left': self._to_estree(node.items[0]),
                    'right': self._to_estree(node.items[2]),
                }
            }

        elif isinstance(node, Part_Ref):
            name = get_child(node, Name)
            assert isinstance(name, Name)

            subs = get_child(node, Section_Subscript_List)
            assert isinstance(subs, Section_Subscript_List)

            assert len(subs.items) == 1

            return {
                'type': 'MemberExpression',
                'computed': True,
                'object': self._to_estree(name),
                'property': self._to_estree(subs.items[0])
            }

        elif isinstance(node, Execution_Part):
            return self._to_estree(node.children)

        elif isinstance(node, Comment):
            return None

        elif isinstance(node, Equiv_Operand):
            return {
                'type': 'BinaryExpression',
                'left': self._to_estree(node.items[0]),
                'operator': LEVEL_4_OPERATORS[node.items[1]],
                'right': self._to_estree(node.items[2])
            }


        elif isinstance(node, Function_Subprogram):
            function_stmt = get_child(node, Function_Stmt)
            assert isinstance(function_stmt, Function_Stmt)

            dummy_arg_list = function_stmt.items[2]
            assert isinstance(dummy_arg_list, Dummy_Arg_List)
            arg_names = [arg.string for arg in dummy_arg_list.items]

            specs = find_all(node, Specification_Part)
            vars = flatten([self.collect_specification_vars(spec) for spec in specs])
            var_names = [var['name'] for var in vars]

            defs = collect_parameter_defs(node)

            internal_vars = [var for var in var_names if var not in arg_names]

            execution_part = get_child(node, Execution_Part)
            assert isinstance(execution_part, Execution_Part)

            end_function_stmt = get_child(node, End_Function_Stmt)
            assert isinstance(end_function_stmt, End_Function_Stmt)

            body = []

            body.extend([{
                'type': 'VariableDeclaration',
                'kind': 'var',
                'declarations': [
                    {'type': 'Identifier', 'name': var, "init": None}
                ]
            } for var in internal_vars])

            for defn in defs:
                body.append({
                    'type': 'ExpressionStatement',
                    'expression': {
                        'type': 'AssignmentExpression',
                        'operator': '=',
                        'left': {
                            'type': 'Identifier',
                            'name': defn['name']
                        },
                        'right': {
                            'type': 'Literal',
                            'value': defn['value'],
                            'raw': str(defn['value'])
                        }
                    }
                })

            # Unwrap the execution part from a BlockStatement to avoid extra nesting
            body.extend(self._to_estree(execution_part))

            return {
                "type": "FunctionDeclaration",
                "id": self._to_estree(function_stmt.get_name()),
                "params": filt([{
                    "type": "Identifier",
                    "name": arg.string
                } for arg in dummy_arg_list.items]),
                "body": {
                    "type": "BlockStatement",
                    "body": body,
                }
            }

        elif isinstance(node, If_Stmt):
            assert len(node.items) == 2
            return {
                'type': 'IfStatement',
                'test': self._to_estree(node.items[0]),
                'consequent': block(self._to_estree(node.items[1])),
                'alternate': None
            }

        elif isinstance(node, If_Construct):
            if get_child(node, If_Then_Stmt):
                if_then_stmt = get_child(node, If_Then_Stmt)
                if_stmt_idx = index_of(node, if_then_stmt)

                indices = []
                for index, child in enumerate(node.children):
                    if isinstance(child, (If_Then_Stmt, Else_Stmt, Else_If_Stmt, End_If_Stmt)):
                        indices.append(index)
                
                # Assert the sequence of if construct nodes is valid
                assert isinstance(node.children[indices[0]], If_Then_Stmt)
                assert isinstance(node.children[indices[-1]], End_If_Stmt)
                if len(indices) > 2:
                    for idx in indices[1:-2]:
                        assert isinstance(node.children[idx], Else_If_Stmt)
                    assert isinstance(node.children[indices[-2]], (If_Stmt, Else_Stmt, Else_If_Stmt))

                tree = None
                cur_tree = None

                for i in range(len(indices) - 1):
                    start_idx, end_idx = indices[i], indices[i + 1]
                    ctrl_node = node.children[start_idx]
                    if isinstance(ctrl_node, If_Then_Stmt):
                        # Only one If_Then_Stmt should be at the start
                        assert cur_tree is None
                        tree = {
                            'type': 'IfStatement',
                            'test': self._to_estree(ctrl_node.items[0]),
                            'consequent': block(self._to_estree(node.children[start_idx + 1: end_idx])),
                            'alternate': None,
                        }
                        cur_tree = tree
                    elif isinstance(ctrl_node, Else_If_Stmt):
                        assert cur_tree is not None
                        new_tree = {
                            'type': 'IfStatement',
                            'test': self._to_estree(ctrl_node.items[0]),
                            'consequent': block(self._to_estree(node.children[start_idx + 1: end_idx])),
                            'alternate': None
                        }
                        cur_tree['alternate'] = new_tree
                        cur_tree = new_tree
                    elif isinstance(ctrl_node, Else_Stmt):
                        assert cur_tree is not None
                        cur_tree['alternate'] = block(self._to_estree(node.children[start_idx + 1: end_idx]))
                    else:
                        raiseError('Unexpected structure in If_Construct')
                
                return tree
            else:
                raise TypeError('Unhandled If_Construct')

        elif isinstance(node, If_Then_Stmt):
            eprint(node.__repr__())
            return None

        elif isinstance(node, Int_Literal_Constant):
            return {
                'type': 'Literal',
                'value': int(node.string),
                'raw': node.string
            }

        elif isinstance(node, Intrinsic_Function_Reference):
            intrinsic_name = get_child(node, Intrinsic_Name)
            assert isinstance(intrinsic_name, Intrinsic_Name)
            func = intrinsic_name.string.lower()

            args = get_child(node, Actual_Arg_Spec_List)

            if func == 'mod':
                lhs = args.items[0]
                rhs = args.items[1]
                return {
                    'type': 'BinaryExpression',
                    'left': self._to_estree(lhs),
                    'operator': '%',
                    'right': self._to_estree(rhs)
                }
            if func in INTRINSIC_FUNCTIONS:
                return {
                    'type': 'CallExpression',
                    'callee': {
                        'type': 'Identifier',
                        'name': INTRINSIC_FUNCTIONS[func]
                    },
                    'arguments': filt([self._to_estree(arg) for arg in args.items])
                }
            else:
                raise ValueError('Unhandled intrinsic function: %s' % func)

        elif isinstance(node, Intrinsic_Name):
            eprint('INTRINSIC', node.__repr__())
            return None

        elif isinstance(node, Level_2_Expr):
            return {
                'type': 'BinaryExpression',
                'left': self._to_estree(node.items[0]),
                'operator': LEVEL_2_OPERATORS[node.items[1]],
                'right': self._to_estree(node.items[2])
            }

        elif isinstance(node, Level_4_Expr):
            return {
                'type': 'BinaryExpression',
                'left': self._to_estree(node.items[0]),
                'operator': LEVEL_4_OPERATORS[node.items[1]],
                'right': self._to_estree(node.items[2])
            }
        
        elif isinstance(node, Logical_Literal_Constant):
            value = LOGICAL_LITERALS[node.string.upper()],
            return {
                'type': 'Literal',
                'value': value,
                'raw': str(value).lower(),
            }
        
        elif isinstance(node, Mult_Operand):
            assert len(node.items) == 3
            return {
                'type': 'BinaryExpression',
                'operator': LEVEL_2_OPERATORS[node.items[1]],
                'left': self._to_estree(node.items[0]),
                'right': self._to_estree(node.items[2])
            }

        elif isinstance(node, Name):
            return {
                'type': 'Identifier',
                'name': node.string
            }
        
        elif isinstance(node, Or_Operand):
            return {
                'type': 'BinaryExpression',
                'left': self._to_estree(node.items[0]),
                'operator': LEVEL_4_OPERATORS[node.items[1]],
                'right': self._to_estree(node.items[2])
            }

        elif isinstance(node, Real_Literal_Constant):
            string = node.string.replace('d', 'e').replace('D', 'E')
            return {
                'type': 'Literal',
                'value': float(string),
                'raw': node.string
            }

        elif isinstance(node, Return_Stmt):
            return {
                'type': 'ReturnStatement',
                'argument': None
            }
        
        elif isinstance(node, Parenthesis):
            assert len(node.items) == 3
            assert node.items[0] == '('
            assert node.items[2] == ')'
            return self._to_estree(node.items[1])
        
        elif isinstance(node, Program):
            return {
                'type': 'Program',
                'body': self._to_estree(node.children)
            }

        elif isinstance(node, Subroutine_Subprogram):
            subroutine_stmt = get_child(node, Subroutine_Stmt)
            assert isinstance(subroutine_stmt, Subroutine_Stmt)

            dummy_arg_list = subroutine_stmt.items[2]
            assert isinstance(dummy_arg_list, Dummy_Arg_List)
            arg_names = [arg.string for arg in dummy_arg_list.items]

            specs = find_all(node, Specification_Part)
            vars = flatten([self.collect_specification_vars(spec) for spec in specs])
            var_names = [var['name'] for var in vars]

            defs = collect_parameter_defs(node)

            internal_vars = [var for var in var_names if var not in arg_names]

            execution_part = get_child(node, Execution_Part)
            assert isinstance(execution_part, Execution_Part)

            end_subroutine_stmt = get_child(node, End_Subroutine_Stmt)
            assert isinstance(end_subroutine_stmt, End_Subroutine_Stmt)

            body = []

            body.extend([{
                'type': 'VariableDeclaration',
                'kind': 'var',
                'declarations': [
                    {'type': 'Identifier', 'name': var, "init": None}
                ]
            } for var in internal_vars])

            for defn in defs:
                body.append({
                    'type': 'ExpressionStatement',
                    'expression': {
                        'type': 'AssignmentExpression',
                        'operator': '=',
                        'left': {
                            'type': 'Identifier',
                            'name': defn['name']
                        },
                        'right': {
                            'type': 'Literal',
                            'value': defn['value'],
                            'raw': str(defn['value'])
                        }
                    }
                })

            # Unwrap the execution part from a BlockStatement to avoid extra nesting
            body.extend(self._to_estree(execution_part))

            return {
                "type": "FunctionDeclaration",
                "id": self._to_estree(subroutine_stmt.get_name()),
                "params": filt([{
                    "type": "Identifier",
                    "name": arg.string
                } for arg in dummy_arg_list.items]),
                "body": {
                    "type": "BlockStatement",
                    "body": body,
                }
            }
        
        elif isinstance(node, Level_2_Unary_Expr):
            assert len(node.items) == 2
            return {
                'type': 'UnaryExpression',
                'operator': UNARY_OPERATORS[node.items[0]],
                'argument': self._to_estree(node.items[1]),
                'prefix': True
            }

        else:
            raise TypeError('Unrecognized node type: %s' % node.__class__.__name__)


# Define your Fortran source code
with open(sys.argv[1], 'rt') as f:
    translator = FortranTranslator(f.read())


print(json.dumps( translator.to_estree(), indent=2 ))

    # Convert AST back to Fortran source code
    #transformed_code = "".join(str(item) for item in ast.content)
    #print("Transformed Fortran Code:")
    #print(transformed_code)
