import inspect
import pandas as pd
import re
import textwrap
from pathlib import Path
from typing import List, Literal, Union, get_args, get_origin, get_type_hints
from collections.abc import Mapping, Sequence

import pharmpy.modeling
import pharmpy.tools
from pharmpy.deps import sympy
from pharmpy.model import DataInfo, Model
from pharmpy.results import ModelfitResults

TYPE_DICT = {
    DataInfo: 'DataInfo',
    Model: 'Model',
    ModelfitResults: 'ModelfitResults',
    str: 'str',
    int: 'integer',
    float: 'numeric',
    sympy.Float: 'numeric',
    bool: 'logical',
    list: 'array',
    None: 'NULL',
    pd.DataFrame: 'data.frame',
    pd.Series: 'array',
}


def create_functions():
    modeling_str = create_module_functions(pharmpy.modeling)
    tool_str = create_module_functions(pharmpy.tools)
    full_str = modeling_str + tool_str

    pharmr_root = Path(inspect.getsourcefile(lambda: 0)).parent.parent
    func_path = pharmr_root / 'R' / 'functions_wrapper.R'

    with open(func_path, 'w') as f:
        f.write(full_str)


def create_module_functions(module):
    funcs = inspect.getmembers(module, inspect.isfunction)
    func_str = ''
    for name, func in funcs:
        if name not in module.__all__:
            continue
        r_func = create_r_func(func, module)
        r_doc = create_r_doc(func)
        func_str += f'{r_doc}\n{r_func}\n\n'
    return func_str


def create_r_func(func, module):
    func_name = func.__name__
    if 'tools' in module.__name__:
        module_name = 'tools'
    elif 'modeling' in module.__name__:
        module_name = 'modeling'
    else:
        raise ValueError(f'Module {module.__name__} not supported')

    wrapper_args, pyfunc_args = [], []

    sig = inspect.signature(func)
    for param in sig.parameters.values():
        if param.kind == param.VAR_KEYWORD or param.kind == param.VAR_POSITIONAL:
            if '...' not in wrapper_args:
                wrapper_args += ['...']
                pyfunc_args += ['...']
        elif param.default is param.empty:
            wrapper_args += [f'{param.name}']
            pyfunc_args += [f'{param.name}']
        else:
            wrapper_args += [f'{param.name}={py_to_r_arg(param.default)}']
            pyfunc_args += [f'{param.name}={param.name}']

    wrapper_arg_str = ', '.join(wrapper_args)
    pyfunc_arg_str = ', '.join(pyfunc_args)

    if not inspect.getdoc(func):
        raise ValueError(f'No documentation available for {func_name}')

    func_def = f'{func_name} <- function({wrapper_arg_str})'
    func_execute = f'func_out <- pharmpy${module_name}${func_name}({pyfunc_arg_str})'
    func_return = 'return(py_to_r(func_out))'

    r_wrapper = [f'{func_def} {{']

    if module_name == 'tools':
        error_msg = [
            'message(cond)',
            'message("Full stack:")',
            'message(reticulate::py_last_error())',
            'message("pharmr version: ", packageVersion("pharmr"))',
            'message("Pharmpy version: ", print_pharmpy_version())',
            'return(NA)'
        ]

        r_trycatch = [
            'tryCatch(',
            '{',
            f'{func_execute}',
            f'{func_return}',
            '},',
            'error=function(cond) {',
            *error_msg,
            '},',
            'warning=function(cond) {',
            *error_msg,
            '}',
            ')',
            '}'
        ]

        r_wrapper.extend(r_trycatch)
    else:
        r_execute = f'{func_execute}'
        r_wrapper.append(r_execute)
        if _has_return_type_pd(inspect.getdoc(func)):
            r_reset_index = [
                'if (func_out$index$nlevels > 1) {',
                'func_out <- func_out$reset_index()',
                '}'
            ]
            r_wrapper.extend(r_reset_index)
        r_return = [
            f'{func_return}',
            '}'
        ]
        r_wrapper.extend(r_return)

    r_wrapper_indented = indent(r_wrapper)
    return '\n'.join(r_wrapper_indented)


def _has_return_type_pd(doc):
    m = re.compile(r'(Returns*|Results*)\n-+\n(.+)')
    return_row = m.search(doc)
    if return_row:
        return_type = return_row.group(2)
    else:
        return False
    if 'series' in return_type.lower() or 'dataframe' in return_type.lower():
        return True
    return False


def indent(r_code: List[str]):
    # FIXME make more general
    indent_cur = 0
    list_indented = []
    for row in r_code:
        if row.count('}'):
            indent_cur -= row.count('}')
        list_indented.append(textwrap.indent(row, '\t'*indent_cur))
        if row.count('{'):
            indent_cur += row.count('{')
    assert indent_cur == 0
    return list_indented


def create_r_doc(func):
    doc = inspect.getdoc(func)

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
        doc_str += create_r_params(doc_dict['params'], func)
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


def create_r_params(doc_list, func):
    type_hints = {key: value for key, value in get_type_hints(func).items() if key != 'return'}
    params = inspect.signature(func).parameters.values()
    params_unbound = [param for param in params if param.kind in (param.VAR_KEYWORD, param.VAR_POSITIONAL)]
    if len(params_unbound) > 2:
        raise ValueError(f'Unexpected number of unbound parameters: {params_unbound}')

    if type_hints and len(type_hints) == len(params) - len(params_unbound):
        try:
            type_dict = _convert_types_from_typehints(type_hints)
            if params_unbound:
                for param in params_unbound:
                    type_dict[param.name] = None
        except NotImplementedError:
            print(f'Could not translate function {func.__name__} with type hints, fall back to docstring')
            type_dict = _convert_types_from_docs(doc_list)
    else:
        type_dict = _convert_types_from_docs(doc_list)

    desc_dict = _get_desc(type_dict.keys(), '\n'.join(doc_list))

    r_params, unbound_added = '', False
    for key, value in type_dict.items():
        if key in ('args', 'kwargs'):
            if unbound_added:
                continue
            r_params += f'@param ... {desc_dict[key]}'
            unbound_added = True
        else:
            r_params += f'@param {key} ({value}) {desc_dict[key]}'

    return r_params + ' \n'


def _convert_types_from_docs(doc_list):
    type_dict = {}
    for i, row in enumerate(doc_list):
        type_declare_pattern = re.compile(r'([\w0-9]+)\s?: ([\w0-9]+)')
        if type_declare_pattern.match(row):
            type_declare = row.split(': ')
            var_name = type_declare[0].strip()
            type_dict[var_name] = py_to_r_str(type_declare[1].strip())
        elif row == 'args' or row == 'kwargs':
            type_dict[row.strip()] = None
    return type_dict


def _convert_types_from_typehints(type_hints):
    type_dict = {}
    for var_name, var_type in type_hints.items():
        try:
            r_type = _translate_type_hints(var_type)
        except NotImplementedError:
            raise
        type_dict[var_name] = r_type

    return type_dict


def _translate_type_hints(var_type):
    skip_args = [sympy.Expr, sympy.Symbol, Path, type(None)]
    skip_origins = [Literal]

    if isinstance(var_type, type):
        return TYPE_DICT[var_type]
    else:
        args, origin = get_args(var_type), get_origin(var_type)
        if origin in skip_origins:
            return 'str'
        args_as_str = [_translate_type_hints(arg) for arg in args if arg not in skip_args]
        # If two args are translated to same type, only write once
        args_as_str = set(args_as_str)
        if origin is Union:
            if type(None) in args:
                return f'{", ".join(args_as_str)} (optional)'
            else:
                return ' or '.join(args_as_str)
        elif origin is list or origin is Sequence:
            return f'array({",".join(args_as_str)})'
        elif origin is Mapping:
            return f'list({"=".join(args_as_str)})'
        else:
            raise NotImplementedError(f'Could not translate {var_type}')


def _get_desc(var_names, docstring):
    # FIXME currently assumes order is same as in typing
    m = re.compile(r'^\S+\s*:\s*.+\s*\n|kwargs\n|args\n', flags=re.MULTILINE)
    descs = m.split(docstring)
    descs = list(filter(None, descs))
    assert len(descs) == len(var_names)
    return {name: py_to_r_str(desc) for name, desc in zip(var_names, descs)}


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

        # Substitute """ or ''' to " for mutliline strings
        row_r = re.sub(r'"""', r'"', row_r)
        row_r = re.sub(r"'''", r'"', row_r)

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
        if row in doc_titles.keys() and doc_titles[row] not in doc_dict.keys():
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
             'pd.Series': 'data.frame',
             r'\w+\[Model\]': 'vector of Model',  # FIXME: more general pattern
             r'\w+\[ModelfitResults\]': 'vector of ModelfitResults'}  # FIXME: more general pattern

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
