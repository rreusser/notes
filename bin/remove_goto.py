#!/usr/bin/env python

import re
import sys
import json
from fparser.common.readfortran import FortranStringReader
from fparser.two.parser import ParserFactory
from fparser.two.utils import walk, get_child
from fparser.two import Fortran2003
import argparse

from fparser.two.Fortran2003 import (
    Goto_Stmt,
    Computed_Goto_Stmt,
    Label,
    Label_List,
    Label_Do_Stmt,
    Nonlabel_Do_Stmt,
    Loop_Control,
    Continue_Stmt,
    End_Do_Stmt,
    If_Stmt,
)

def eprint(*args, **kwargs):
    print(*args, file=sys.stderr, **kwargs)

def get_one_child(node, node_type):
    all = [child for child in node.children if isinstance(child, node_type)]
    assert len(all) == 1, 'Expected one child of type %s, found %d' % (node_type.__name__, len(all))
    return all[0]


class GotoRemover:
    def __init__(self, code, output_comments=True):
        self.code = code
        self.output_comments = output_comments
        reader = FortranStringReader(code, ignore_comments=False)
        parser = ParserFactory().create(std="f2003")
        self.ast = parser(reader)
        self.continue_stmts = dict()
        self.goto_stmts = dict()
        self.do_stmts = dict()
        self.if_stmts = dict()

        self.analyze()

        self.replace_trivial_label_do()
        self.replace_trivial_if_stmts()
      
    def __str__(self):
        return str(self.ast)
      
    def analyze(self):
        for node in walk(self.ast):
            if isinstance(node, Continue_Stmt):
                label = node.item.label
                label = str(label)
                assert label not in self.continue_stmts
                self.continue_stmts[label] = node

            if isinstance(node, Goto_Stmt):
                label = get_one_child(node, Label).string
                if label not in self.goto_stmts:
                    self.goto_stmts[label] = []
                self.goto_stmts[label].append(node)
            
            if isinstance(node, Computed_Goto_Stmt):
                labels = get_one_child(node, Label_List)
                for label_node in labels.items:
                    assert isinstance(label_node, Label)
                    label = label_node.string
                    if label not in self.goto_stmts:
                        self.goto_stmts[label] = []
                    self.goto_stmts[label].append(node)
            
            if isinstance(node, Label_Do_Stmt):
                label = get_one_child(node, Label).string
                assert label not in self.do_stmts
                self.do_stmts[label] = node
            
            if isinstance(node, If_Stmt):
                potential_goto = node.items[1]
                if isinstance(potential_goto, Goto_Stmt):
                    label = get_one_child(potential_goto, Label).string
                    if label not in self.if_stmts:
                        self.if_stmts[label] = node
            
    def replace_trivial_label_do(self):
        orig_cnt = len(self.do_stmts)
        to_delete = []
        # Replace DO/CONTINUE loops with DO/END DO
        for label, labeldo_node in self.do_stmts.items():
            continue_cnt = 1 if label in self.continue_stmts else 0
            goto_cnt = len(self.goto_stmts[label]) if label in self.goto_stmts else 0

            if continue_cnt != 1 or goto_cnt != 0:
                continue

            continue_node = self.continue_stmts[label]
            assert isinstance(continue_node, Continue_Stmt)

            loopctrl_node = labeldo_node.items[2]
            assert isinstance(loopctrl_node, Loop_Control)

            nonlabel_do = Nonlabel_Do_Stmt('DO')
            items = list(nonlabel_do.items)
            items[1] = loopctrl_node
            nonlabel_do.items = tuple(items)

            labeldo_idx = labeldo_node.parent.children.index(labeldo_node)
            labeldo_node.parent.children[labeldo_idx] = nonlabel_do

            end_do_node = End_Do_Stmt('END DO')
            continue_idx = continue_node.parent.children.index(continue_node)
            continue_node.parent.children[continue_idx] = end_do_node

            to_delete.append(label)
        
        for label in to_delete:
            del self.do_stmts[label]
            del self.continue_stmts[label]

        eprint('Replaced %d trivial DO loops' % (orig_cnt - len(self.do_stmts)))
    
    def replace_trivial_if_stmts(self):
        orig_cnt = len(self.if_stmts)
        to_delete = []
        for label, if_node in self.if_stmts.items():
            goto_node = if_node.items[1]
            assert isinstance(goto_node, Goto_Stmt)
            label = get_one_child(goto_node, Label).string

            eprint('Removing trivial IF statement with label %s' % label)

            if label in self.goto_stmts:
                continue
            to_delete.append(label)
        
        for label in to_delete:
            del self.if_stmts[label]
        
        eprint('Replaced %d trivial IF statements' % (orig_cnt - len(self.if_stmts)))

def fix_fortran_formatting(input_string, free_form=True):
    lines = input_string.splitlines()
    formatted_lines = []
    for line in lines:
        stripped = line.strip()
        if stripped.startswith('*'):
            if free_form:
                # Convert fixed-form comments (* in column 1) to free-form (!)
                formatted_lines.append('!' + stripped[1:])
            else:
                formatted_lines.append(stripped)
        elif stripped == '':
            formatted_lines.append('')
        else:
            formatted_lines.append('      ' + stripped)
    return '\n'.join(formatted_lines)
    
if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Convert Fortran code to ESTree')
    parser.add_argument('filename', nargs='?', type=str, help='Fortran source file')
    parser.add_argument('--no-comments', action='store_true', default=False, help='Exclude comments from the output')
    parser.add_argument('--no-output', action='store_true', default=False, help='Suppress output')
    args = parser.parse_args()

    if not args.filename and sys.stdin.isatty():
        eprint('Usage: python fortran_to_estree.py <filename> or pipe input to the script')
        sys.exit(1)
    
    if args.filename:
        with open(args.filename, 'rt') as f:
            code = f.read()
    else:
        code = sys.stdin.read()

    translator = GotoRemover(code, output_comments=not args.no_comments)

    output = fix_fortran_formatting(str(translator))

    if not args.no_output:
        print(output)