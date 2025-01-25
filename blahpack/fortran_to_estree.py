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
    Data_Stmt,
    Data_Stmt_Object_List,
    Data_Stmt_Set,
    Data_Stmt_Value_List,
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
    Intrinsic_Procedure_Name_List,
    Intrinsic_Stmt,
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
    Prefix,
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

def parse_fortran_float(string):
    return float(string.replace('d', 'e').replace('D', 'E'))

def parse_dummy_arg_list(node):
    assert isinstance(node, Dummy_Arg_List)
    return [arg.string.lower() for arg in node.items]

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
        return {
            'type': 'int32',
            'value': int(node.string)
        }
    elif isinstance(node, Real_Literal_Constant):
        return {
            'type': 'float64',
            'value': parse_fortran_float(node.string)
        }
    else:
        raise TypeError('Unrecognized node type: %s' % node.__class__.__name__)

def assert_child_types(node, type_list, allow_none=True):
    for child in node.children:
        if allow_none and child is None:
            continue
        if not any(isinstance(child, t) for t in type_list):
            raise TypeError('Unexpected child type: %s' % child.__class__.__name__)

def find_one(node, node_type):
    all = [node for node in walk(node) if isinstance(node, node_type)]
    assert len(all) == 1, 'Expected one node of type %s, found %d' % (node_type.__name__, len(all))
    return all[0]

# Use of this function is discouraged, as it silences unhanded types. Instead, iterate
# over all children and raise an error if a node type is unhandled.
def find_all(node, node_type):
    return [node for node in walk(node) if isinstance(node, node_type)]

def get_one_child(node, node_type):
    all = [child for child in node.children if isinstance(child, node_type)]
    assert len(all) == 1, 'Expected one child of type %s, found %d' % (node_type.__name__, len(all))
    return all[0]

def parse_data_stmt(node):
    assert_child_types(node, (Data_Stmt_Set,))
    constants = {}
    for dataset in node.children:
        assert_child_types(dataset, (Data_Stmt_Object_List, Data_Stmt_Value_List))
        obj_list = find_one(node, Data_Stmt_Object_List)
        val_list = find_one(node, Data_Stmt_Value_List)

        assert len(obj_list.items) == len(val_list.items)
        for i in range(len(obj_list.items)):
            name = obj_list.items[i].string.lower()
            constants[name] = get_value(val_list.items[i])

    return constants


def parse_type_declaration_stmt (node):
    assert_child_types(node, (Intrinsic_Type_Spec, Entity_Decl_List))

    variables = {}

    # Implement as needed if not None
    assert node.children[1] == None

    int_type_spec = find_one(node, Intrinsic_Type_Spec)
    entity_decl_list = find_one(node, Entity_Decl_List)

    # Implement as needed if not None
    assert int_type_spec.items[1] == None

    type = TYPES[int_type_spec.string.lower()]

    for decl in entity_decl_list.children:
        if isinstance(decl, Entity_Decl):
            # Recall, Fortran is case-insensitive
            name = decl.get_name().string.lower()
            variables[name] = {
                'name': name,
                'type': type
            }
        else:
            raise TypeError('Unexpected child type in Type_Declaration_Stmt: %s' % entity_decl.__class__.__name__)

    return variables

def merge_dicts (dst, src):
    for key, value in src.items():
        if key in dst:
            raise KeyError(f"Duplicate key '{key}' found while merging dictionaries")
        dst[key] = value
    return dst

def parse_intrinsic_stmt (node):
    intrinsics = {}
    for child in node.children:
        if child == 'INTRINSIC':
            continue
        elif isinstance(child, Intrinsic_Procedure_Name_List):
            for name in child.children:
                _name = name.string.lower()
                intrinsics[_name] = {
                    'name': _name
                }
        else:
            raise TypeError('Unexpected child type in Intrinsic_Stmt: %s' % child.__class__.__name__)
    return intrinsics

def parse_spec (node):
    declarations = {}
    constants = {}
    intrinsics = {}
    assert isinstance(node, Specification_Part)
    for child in node.children:
        if isinstance(child, Type_Declaration_Stmt):
            merge_dicts(declarations, parse_type_declaration_stmt(child))
        elif isinstance(child, Intrinsic_Stmt):
            merge_dicts(intrinsics, parse_intrinsic_stmt(child))
        elif isinstance(child, Data_Stmt):
            merge_dicts(constants, parse_data_stmt(child))
        elif isinstance(child, Implicit_Part):
            # For now, assume implicit_part may only contain comments
            assert_child_types(child, (Comment,))
        else:
            raise TypeError('Unexpected child type in Specification_Part: %s' % child.__class__.__name__)

    return (declarations, constants, intrinsics)

def block (body):
    if isinstance(body, list):
        # Wrap in a BlockStatement if multiple statements are present
        return {
            'type': 'BlockStatement',
            'body': body,
        }
    else:
        # Return the body node directly if it's not a list
        return body

class TypeSpec:
    def __init__(self, name, type=None, size=None):
        self.name = name
        self.size = size
        self.type = type
    
    def __repr__(self):
        return f'TypeSpec(name={self.name}, type={self.type}, size={self.size})'
    
class Scope:
    def __init__(self, ast, parent_context=None):
        self.variables = {}
        self.ast = ast
        self.parent_context = parent_context
        self.type_spec = {}
        self.return_name = None
        self.intrinsics = {}
        self.constants = {}
    
    def is_root_node(self, node):
        return node == self.ast
    
    def root_scope(self):
        if self.parent_context is None:
            return self
        else:
            return self.parent_context.root_scope()
    
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

            if isinstance(rng[0], Int_Literal_Constant):
                # If the start value is a literal, convert it to zero-based index
                start = self._to_estree(Int_Literal_Constant(str(int(rng[0].string) - 1)))
            else:
                # If the start value is an expression, use that expression
                start = self._to_estree(rng[0])

            end = self._to_estree(rng[1])
            try:
                inc = rng[2]
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
                    'operator': '<',
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

                    kwargs = {'type': type}
                    size_spec = get_child(decl, Assumed_Size_Spec)
                    if size_spec is not None:
                        kwargs['size'] = size_spec

                    spec = TypeSpec(name, **kwargs)

                    vars.append({
                        'type': type,
                        'name': name.string
                    })

                return vars

            return filt([recurse(child, depth + 1) for child in node.children])

        vars = filt(flatten([recurse(child) for child in specification.children]))

        vars.sort(key=lambda x: (-len(x['name']), x['name']))

        return vars
    
    def to_estree(self):
        return self._to_estree(self.ast)

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

        elif isinstance(node, Execution_Part):
            return self._to_estree(node.children)

        elif isinstance(node, Comment):
            return None
        
        elif isinstance(node, Data_Stmt_Set):
            raise TypeError('Data_Stmt_Set should not be encountered')

        elif isinstance(node, Equiv_Operand):
            return {
                'type': 'BinaryExpression',
                'left': self._to_estree(node.items[0]),
                'operator': LEVEL_4_OPERATORS[node.items[1]],
                'right': self._to_estree(node.items[2])
            }

        elif isinstance(node, Function_Subprogram):
            if not self.is_root_node(node):
                # Create a new scope for subroutines
                return Scope(node).to_estree()

            assert_child_types(node, (Function_Stmt, Specification_Part, Execution_Part, End_Function_Stmt))

            function_stmt = get_one_child(node, Function_Stmt)
            specification_part = get_one_child(node, Specification_Part)
            execution_part = get_one_child(node, Execution_Part)
            end_function_stmt = get_one_child(node, End_Function_Stmt)

            function_name = function_stmt.get_name()
            assert self.return_name is None
            self.return_name = function_name

            dummy_args = parse_dummy_arg_list(get_one_child(function_stmt, Dummy_Arg_List))
            (decls, consts, intrinsics) = parse_spec(specification_part)

            body = []

            all_vars = list(set(list(decls.keys()) + list(consts.keys())))
            all_vars = [var for var in all_vars if var not in dummy_args]
            all_vars.sort(key=lambda x: (-len(x), x))

            body.extend([{
                'type': 'VariableDeclaration',
                'kind': 'var',
                'declarations': [{'type': 'Identifier', 'name': var, "init": None}]
            } for var in all_vars])

            for name, meta in consts.items():
                body.append({
                    'type': 'ExpressionStatement',
                    'expression': {
                        'type': 'AssignmentExpression',
                        'operator': '=',
                        'left': {
                            'type': 'Identifier',
                            'name': name
                        },
                        'right': {
                            'type': 'Literal',
                            'value': meta['value'],
                            'raw': str(meta['value'])
                        }
                    }
                })

            # Unwrap the execution part from a BlockStatement to avoid extra nesting
            body.extend(self._to_estree(execution_part))

            return {
                "type": "FunctionDeclaration",
                "id": self._to_estree(function_name),
                "params": filt([{
                    "type": "Identifier",
                    "name": arg,
                } for arg in dummy_args]),
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
                #if_stmt_idx = index_of(node, if_then_stmt)

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
            raise TypeError('Intrinsic_Name should not be encountered')
        
        elif isinstance(node, Intrinsic_Stmt):
            raise TypeError('Intrinsic_Stmt should not be encountered')


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
                'name': node.string.lower()
            }
        
        elif isinstance(node, Or_Operand):
            return {
                'type': 'BinaryExpression',
                'left': self._to_estree(node.items[0]),
                'operator': LEVEL_4_OPERATORS[node.items[1]],
                'right': self._to_estree(node.items[2])
            }

        elif isinstance(node, Real_Literal_Constant):
            num = parse_fortran_float(node.string)
            return {
                'type': 'Literal',
                'value': num,
                'raw': str(num)
            }

        elif isinstance(node, Return_Stmt):
            # Fortran, we assume the return value is the function's name
            return {
                'type': 'ReturnStatement',
                'argument': None if self.return_name is None else self._to_estree(self.return_name)
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
        
        elif isinstance(node, Implicit_Part):
            return self._to_estree(node.children)

        elif isinstance(node, Specification_Part):
            return self._to_estree(node.children)
        
        elif isinstance(node, Type_Declaration_Stmt):
            raise TypeError('Type_Declaration_Stmt should not be encountered')

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
            
        elif isinstance(node, Subroutine_Subprogram):
            if not self.is_root_node(node):
                # Create a new scope for subroutines
                return Scope(node).to_estree()

            assert_child_types(node, (Subroutine_Stmt, Specification_Part, Execution_Part, End_Subroutine_Stmt))

            subroutine_stmt = get_one_child(node, Subroutine_Stmt)
            specification_part = get_one_child(node, Specification_Part)
            execution_part = get_one_child(node, Execution_Part)
            end_subroutine_stmt = get_one_child(node, End_Subroutine_Stmt)

            dummy_args = parse_dummy_arg_list(get_one_child(subroutine_stmt, Dummy_Arg_List))
            (decls, consts, intrinsics) = parse_spec(specification_part)

            body = []

            all_vars = list(set(list(decls.keys()) + list(consts.keys())))
            all_vars = [var for var in all_vars if var not in dummy_args]
            all_vars.sort(key=lambda x: (-len(x), x))

            body.extend([{
                'type': 'VariableDeclaration',
                'kind': 'var',
                'declarations': [{'type': 'Identifier', 'name': var, "init": None}]
            } for var in all_vars])

            for name, meta in consts.items():
                body.append({
                    'type': 'ExpressionStatement',
                    'expression': {
                        'type': 'AssignmentExpression',
                        'operator': '=',
                        'left': {
                            'type': 'Identifier',
                            'name': name
                        },
                        'right': {
                            'type': 'Literal',
                            'value': meta['value'],
                            'raw': str(meta['value'])
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
                    "name": arg,
                } for arg in dummy_args]),
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


class FortranTranslator:
    def __init__(self, code):
        self.code = code

        reader = FortranStringReader(code, ignore_comments=False)
        parser = ParserFactory().create(std="f2003")
        self.ast = parser(reader)
    
    def to_estree(self):
        return Scope(self.ast).to_estree()


# Define your Fortran source code
with open(sys.argv[1], 'rt') as f:
    translator = FortranTranslator(f.read())

print(json.dumps( translator.to_estree(), indent=2 ))