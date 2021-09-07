from inspect import getdoc, getfullargspec, getmembers, isfunction
import os
from pathlib import Path
import re

import pharmpy.modeling


def create_functions():
    modeling_functions = getmembers(pharmpy.modeling, isfunction)
    full_str = ''

    for _, func in modeling_functions:
        r_func = create_r_func(func)
        r_doc = create_r_doc(func)
        full_str += f'{r_doc}\n{r_func}\n\n'

    cwd = os.getcwd()
    print(cwd)
    print(os.listdir(cwd))
    pharmr_root = Path(cwd).parent.parent
    print(pharmr_root)
    print(os.listdir(pharmr_root))
    func_path = pharmr_root / 'R' / 'functions_wrapper.R'
    with open(func_path, 'w') as f:
        f.write(full_str)


def create_r_func(func):
    func_name = func.__name__
    argspecs = getfullargspec(func)
    args = argspecs.args
    defaults = argspecs.defaults

    if defaults:
        if len(args) > len(defaults):
            defaults_new = [py_to_r(d, is_arg=True) for d in defaults]
            defaults = [None for _ in range(len(args) - len(defaults_new))] + defaults_new
        args_defaults = {arg: default for arg, default in zip(args, defaults)}
        arg_list = [(f'{arg}={default}' if default is not None else f'{arg}') for arg, default in args_defaults.items()]
    else:
        arg_list = args

    func_args = ', '.join(arg_list)
    args_str = ', '.join(args)

    r_func = f'{func_name} <- function({func_args}) {{\n' \
             f'    return(pharmpy$modeling${func_name}({args_str}))\n' \
             f'}}'

    return r_func


def create_r_doc(func):
    doc = getdoc(func)

    if not doc:
        return f'#\' @title\n' \
               f'#\' {func.__name__}\n' \
               f'#\' \n' \
               f'#\' @export'

    doc_description, doc_parameters, doc_returns = split_doc_to_subtypes(doc)

    doc_str = f'@title\n{func.__name__}\n\n@description\n'

    for row in doc_description:
        is_list = row.startswith('-')
        if is_list:
            row = re.sub('^- ', '* ', row)
        if row.startswith('.. '):
            row = re.sub(r'^\.\. ', '', row)
        doc_str += f'{py_to_r(row, is_str=True)}\n'

    if doc_parameters:
        doc_str += create_r_params_or_returns(doc_parameters, 'param')
    if doc_returns:
        doc_str += create_r_params_or_returns(doc_returns, 'return')

    r_doc = ''
    for row in doc_str.split('\n'):
        r_doc += f'#\' {row}\n'
        
    r_doc += f'#\' @export'
    return r_doc 


def create_r_params_or_returns(doc_list, doc_type):
    doc_str = '\n'
    for row in doc_list:
        type_declare_pattern = re.compile(r'([\w0-9]+) : ([\w0-9]+)')
        if type_declare_pattern.match(row):
            type_declare = row.split(' : ')
            doc_str += f'@{doc_type} {type_declare[0]} ({py_to_r(type_declare[1], is_str=True)})'
        else:
            doc_str += f' {py_to_r(row, is_str=True)}\n'
    return doc_str


def split_doc_to_subtypes(doc_str):
    doc_split = doc_str.split('\n')

    doc_type_current = 'description'
    doc_description = []
    doc_parameters = []
    doc_returns = []

    for row in doc_split:
        if row == 'Parameters':
            doc_type_current = 'parameters'
            continue
        elif row == 'Returns':
            doc_type_current = 'returns'
            continue

        if doc_type_current == 'description':
            doc_description += [row.strip()]
        elif doc_type_current == 'parameters':
            if not row.startswith('--'):
                doc_parameters += [row.strip()]
        elif doc_type_current == 'returns':
            if not row.startswith('--'):
                doc_returns += [row.strip()]

    return doc_description, doc_parameters, doc_returns


py_to_r_dict_basic = {'None': 'NULL',
                      'True': 'TRUE',
                      'False': 'FALSE'}


def py_to_r(arg, is_arg=False, is_str=False):
    if is_arg:
        return py_to_r_arg(arg)
    elif is_str:
        return py_to_r_str(arg)


def py_to_r_arg(arg):
    py_to_r_dict = {**py_to_r_dict_basic,
                    **{'{}': 'list()',
                       '[]': 'c()',
                       '': '\'\''}}

    arg_str = str(arg)

    try:
        return py_to_r_dict[arg_str]
    except KeyError:
        if isinstance(arg, str):
            return f'\'{arg}\''
        else:
            return arg


def py_to_r_str(arg):
    arg_new = arg
    py_to_r_dict = {**py_to_r_dict_basic,
                    **{r'\blist\b': 'vector',
                       r'\bdict\b': 'list',
                       'dictionary': 'list',
                       'bool': 'logical',
                       r'\\mathsf': '',
                       r'\\cdot': '*',
                       r'\\text': '',
                       r'\\frac': 'frac',
                       r'\\log': 'log',
                       r'\\exp': 'exp',
                       r'\\min': 'min',
                       r'\\max': 'max',
                       r'\\epsilon': 'epsilon',
                       r'\[([0-9]+)\]_*': r'(\1)'}
                    }

    for key, value in py_to_r_dict.items():
        arg_new = re.sub(key, value, arg_new)

    return arg_new


if __name__ == '__main__':
    create_functions()
