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
    arg_names = [str(arg) for arg in args]
    defaults = argspecs.defaults
    varargs, varkw = argspecs.varargs, argspecs.varkw

    if defaults:
        if len(args) > len(defaults):
            defaults_new = [py_to_r_arg(d) for d in defaults]
            defaults = [None for _ in range(len(arg_names) - len(defaults_new))] + defaults_new
        args_defaults = {arg: default for arg, default in zip(arg_names, defaults)}
        arg_list = [(f'{arg}={default}' if default is not None else f'{arg}') for arg, default in args_defaults.items()]
    else:
        arg_list = arg_names.copy()

    if varargs or varkw:
        arg_list += ['...']
        arg_names += ['...']

    func_args = ', '.join(arg_list)
    args_str = ', '.join(arg_names)

    if 'pd.dataframe' in getdoc(func).lower() or 'pd.series' in getdoc(func).lower():
        return f'{func_name} <- function({func_args}) {{\n' \
               f'    df <- pharmpy$modeling${func_name}({args_str})\n' \
               f'    df_reset <- df$reset_index()\n' \
               f'    return(py_to_r(df_reset))\n' \
               f'}}'
    else:
        r_func = f'{func_name} <- function({func_args}) {{\n' \
                 f'    func_out <- pharmpy$modeling${func_name}({args_str})\n' \
                 f'    return(py_to_r(func_out))\n' \
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
        doc_str += create_r_params(doc_dict['params'])
    if 'returns' in doc_dict.keys():
        doc_str += create_r_returns(doc_dict['returns']) + '\n\n'
    if 'examples' in doc_dict.keys():
        doc_str += create_r_example(doc_dict['examples']) + '\n'
    if 'notes' in doc_dict.keys():
        doc_str += '@note\n'
        doc_str += ''.join(doc_dict['notes']) + '\n\n'
    if 'see_also' in doc_dict.keys():
        doc_str += '@seealso\n'
        doc_str += '\n\n'.join(doc_dict['see_also']) + '\n\n'

    r_doc = ''
    for row in doc_str.split('\n'):
        r_doc += f'#\' {row}\n'
        
    r_doc += f'#\' @export'
    return r_doc 


def create_r_params(doc_list):
    doc_str = ''
    skip = False
    for i, row in enumerate(doc_list):
        if skip:
            skip = False
            continue
        type_declare_pattern = re.compile(r'([\w0-9]+) : ([\w0-9]+)')
        if type_declare_pattern.match(row):
            type_declare = row.split(' : ')
            doc_str += f'@param {type_declare[0]} ({py_to_r_str(type_declare[1])})'
        elif row == 'args' or row == 'kwargs':
            doc_str += f'@param ... {doc_list[i+1]}\n'
            skip = True
        else:
            doc_str += f' {py_to_r_str(row)}\n'
    return doc_str


def create_r_returns(doc_list):
    doc_list_trim = list(filter(None, doc_list))
    type_py = doc_list_trim[0]
    type_r = py_to_r_str(type_py)

    doc_str = f'@return ({type_r}) '
    doc_str += ' '.join(py_to_r_str(row) for row in doc_list_trim[1:])

    return doc_str


def create_r_example(doc_list):
    pattern_start = re.compile(r'>>> |^\.\.\. ')
    pattern_methods = re.compile(r'([A-Za-z]\d*)\.([A-Za-z]\d*)')
    pattern_list_idx = re.compile(r'\w\[(\d+)]')
    pattern_list = re.compile(r'\[([\'\"\w(),\s]+)]')
    pattern_dict = re.compile(r'{(([\'\"]*[\w\d()]+[\'\"]*: [\'\"]*[\w\d]+\'*,*\s*)+)}')
    pattern_doctest = re.compile(r'\s+# doctest:.*')

    doc_code = [row for row in doc_list if row.startswith('>>>') or re.match(r'^\.\.\.\s+[$\w\d]', row)]
    doc_code = [row for row in doc_code if ' import ' not in row]

    doc_code_r = ''
    for row in doc_code:
        # Check for rows that starts with ... or >>>
        row_r = re.sub(pattern_start, '', row)
        row_r = py_to_r_str(row_r, example=True)
        row_r = re.sub(' = ', ' <- ', row_r)

        # Substitute any . to $, e.g. model.parameters -> model$parameters
        row_r = re.sub(pattern_methods, r'\1$\2', row_r)

        # Check if row has list subscript
        if re.search(r'\w\[', row_r):
            idx = re.search(pattern_list_idx, row_r)
            if idx:
                # Increase idx by 1, e.g. list[0] -> list[1]
                row_r = re.sub(idx.group(1), f'{int(idx.group(1)) + 1}', row_r)
        else:
            # Substitute any [] to c(), e.g. ['THETA(1)'] -> c('THETA(1)')
            row_r = re.sub(pattern_list, r'c(\1)', row_r)

        # Check if row contains python dict
        dict_py = re.search(pattern_dict, row_r)
        if dict_py:
            # Substitute {} to list(), e.g. {'EONLY': 1} -> list('EONLY'=1)
            dict_r = f'list({dict_py.group(1).replace(": ", "=", )})'
            row_r = row_r.replace('{' + f'{dict_py.group(1)}' + '}', dict_r)

        # Replace len() with length()
        row_r = row_r.replace(r'len(', 'length(')

        # Remove doctest comments
        row_r = re.sub(pattern_doctest, '', row_r)
        doc_code_r += row_r + '\n'

    return '@examples\n\\dontrun{\n' + doc_code_r + '}'


def split_doc_to_subtypes(doc_str):
    doc_split = doc_str.split('\n')

    doc_titles = {'Parameters': 'params',
                  'Returns': 'returns',
                  'Return': 'returns',
                  'Results': 'returns',
                  'Example': 'examples',
                  'Examples': 'examples',
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

        if not row.startswith('--'):
            doc_dict[doc_type_current].append(row.strip())

    return doc_dict


def py_to_r_arg(arg):
    py_to_r_dict = {'None': 'NULL',
                    'True': 'TRUE',
                    'False': 'FALSE',
                    '': '\'\''}

    try:
        return py_to_r_dict[str(arg)]
    except KeyError:
        if isinstance(arg, str):
            return f'\'{arg}\''
        else:
            return arg


def py_to_r_str(arg, example=False):
    args = {'None': 'NULL',
            'True': 'TRUE',
            'False': 'FALSE'}

    types = {r'\bint\b': 'integer',
             'float': 'numeric',
             r'\bbool\b': 'logical',
             r'\blist\b': 'vector',
             r'\bdict\b': 'list',
             'dictionary': 'list',
             'pd.DataFrame': 'data.frame',
             'pd.Series': 'data.frame'}

    latex = {r'\\mathsf': '',
             r'\\cdot': '*',
             r'\\text': '',
             r'\\frac': 'frac',
             r'\\log': 'log',
             r'\\exp': 'exp',
             r'\\min': 'min',
             r'\\max': 'max',
             r'\\epsilon': 'epsilon'}

    py_to_r_dict = {**args, **types, **latex}

    if not example:
        py_to_r_dict = {**py_to_r_dict, **{r'\[([0-9]+)\]_*': r'(\1)'}}

    arg_sub = arg
    for key, value in py_to_r_dict.items():
        arg_sub = re.sub(key, value, arg_sub)

    return arg_sub


if __name__ == '__main__':
    create_functions()
