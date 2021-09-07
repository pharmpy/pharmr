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

    # TODO: more general way to get right directory
    cwd = os.getcwd()
    pharmr_root = Path(cwd).parent
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
            defaults_new = [py_to_r_arg(d) for d in defaults]
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

    doc_dict = split_doc_to_subtypes(doc)

    doc_str = f'@title\n{func.__name__}\n\n@description\n'

    for row in doc_dict['description']:
        is_list = row.startswith('-')
        if is_list:
            row = re.sub('^- ', '* ', row)
        if row.startswith('.. '):
            row = re.sub(r'^\.\. ', '', row)
        doc_str += f'{py_to_r_str(row)}\n'

    if 'params' in doc_dict.keys():
        doc_str += create_r_params_or_returns(doc_dict['params'], 'param')
    if 'returns' in doc_dict.keys():
        doc_str += create_r_params_or_returns(doc_dict['returns'], 'return')
    if 'notes' in doc_dict.keys():
        doc_str += '@note\n'
        doc_str += ''.join(doc_dict['notes']) + '\n'
    if 'see_also' in doc_dict.keys():
        doc_str += '@seealso\n'
        doc_str += '\n\n'.join(doc_dict['see_also']) + '\n'

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
            doc_str += f'@{doc_type} {type_declare[0]} ({py_to_r_str(type_declare[1])})'
        else:
            doc_str += f' {py_to_r_str(row)}\n'
    return doc_str


def split_doc_to_subtypes(doc_str):
    doc_split = doc_str.split('\n')

    # add notes
    doc_titles = {'Parameters': 'params',
                  'Returns': 'returns',
                  'Return': 'returns',
                  'Results': 'returns',
                  'Example': 'example',
                  'Examples': 'example',
                  'Notes': 'notes',
                  'See also': 'see_also',
                  'See Also': 'see_also'}

    doc_type_current = 'description'

    doc_dict = dict()
    doc_dict[doc_type_current] = []

    for row in doc_split:
        if row in doc_titles.keys():
            doc_type_current = doc_titles[row]
            doc_dict[doc_type_current] = []
            continue

        if not row.startswith('--') and doc_type_current != 'example':
            doc_dict[doc_type_current].append(row.strip())

    return doc_dict


def py_to_r_arg(arg):
    py_to_r_dict = {'None': 'NULL',
                    'True': 'TRUE',
                    'False': 'FALSE',
                    '{}': 'list()',
                    '[]': 'c()',
                    '': '\'\''}

    try:
        return py_to_r_dict[str(arg)]
    except KeyError:
        if isinstance(arg, str):
            return f'\'{arg}\''
        else:
            return arg


def py_to_r_str(arg):
    py_to_r_dict = {'None': 'NULL',
                    'True': 'TRUE',
                    'False': 'FALSE',
                    r'\blist\b': 'vector',
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
                    r'\[([0-9]+)\]_*': r'(\1)'
                    }

    arg_sub = arg
    for key, value in py_to_r_dict.items():
        arg_sub = re.sub(key, value, arg_sub)

    return arg_sub


if __name__ == '__main__':
    create_functions()
