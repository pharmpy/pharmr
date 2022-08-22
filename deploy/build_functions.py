from inspect import getdoc, getfullargspec, getmembers, isfunction
import os
from pathlib import Path
import re

import pharmpy.modeling
import pharmpy.tools


def create_functions():
    modeling_str = create_module_functions(pharmpy.modeling)
    tool_str = create_module_functions(pharmpy.tools)
    full_str = modeling_str + tool_str
    # TODO: more general way to get right directory
    cwd = os.getcwd()
    pharmr_root = Path(cwd).parent
    func_path = pharmr_root / 'R' / 'functions_wrapper.R'

    with open(func_path, 'w') as f:
        f.write(full_str)


def create_module_functions(module):
    funcs = getmembers(module, isfunction)
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
    argspecs = getfullargspec(func)
    args = [str(arg) for arg in argspecs.args]
    defaults = argspecs.defaults
    if defaults:
        if len(defaults) > 0:
            defaults_new = [py_to_r_arg(d) for d in defaults]
            defaults = [None for _ in range(len(args) - len(defaults_new))] + defaults_new
        wrapper_args, pyfunc_args = [], []
        for arg, default in zip(args, defaults):
            if default is None:
                wrapper_args += [f'{arg}']
                pyfunc_args += [f'{arg}']
            else:
                wrapper_args += [f'{arg}={default}']
                pyfunc_args += [f'{arg}={arg}']
    else:
        wrapper_args = args.copy()
        pyfunc_args = args.copy()

    if argspecs.varargs or argspecs.varkw:
        wrapper_args += ['...']
        pyfunc_args += ['...']

    wrapper_arg_str = ', '.join(wrapper_args)
    pyfunc_arg_str = ', '.join(pyfunc_args)

    if not getdoc(func):
        raise ValueError(f'No documentation available for {func_name}')

    func_def = f'{func_name} <- function({wrapper_arg_str})'
    func_out = f'func_out <- pharmpy${module_name}${func_name}({pyfunc_arg_str})'
    func_return = 'return(py_to_r(func_out))'

    if module_name == 'tools':
        r_func = f'{func_def} {{\n' \
                 f'\ttryCatch(\n' \
                 f'\t{{\n' \
                 f'\t\t{func_out}\n' \
                 f'\t\t{func_return}\n' \
                 f'\t}},\n' \
                 f'\terror=function(cond) {{\n' \
                 f'\t\tmessage(cond)\n' \
                 f'\t\tmessage(\'Full stack:\')\n' \
                 f'\t\tmessage(reticulate::py_last_error())\n' \
                 f'\t\tmessage("pharmr version: ", packageVersion("pharmr"))\n' \
                 f'\t\tmessage("Pharmpy version: ", print_pharmpy_version())\n' \
                 f'\t\treturn(NA)\n' \
                 f'\t}},\n' \
                 f'\twarning=function(cond) {{\n' \
                 f'\t\tmessage(cond)\n' \
                 f'\t\tmessage(\'Full stack:\')\n' \
                 f'\t\tmessage(reticulate::py_last_error())\n' \
                 f'\t\tmessage("pharmr version: ", packageVersion("pharmr"))\n' \
                 f'\t\tmessage("Pharmpy version: ", print_pharmpy_version())\n' \
                 f'\t\treturn(NA)\n' \
                 f'\t}}\n' \
                 f'\t)\n' \
                 f'}}'
    else:
        if 'pd.dataframe' in getdoc(func).lower() or 'pd.series' in getdoc(func).lower():
            func_reset = f'func_out <- func_out$reset_index()'
            r_func = f'{func_def} {{\n' \
                     f'\t{func_out}\n' \
                     f'\t{func_reset}\n' \
                     f'\t{func_return}\n' \
                     f'}}'
        else:
            r_func = f'{func_def} {{\n' \
                     f'\t{func_out}\n' \
                     f'\t{func_return}\n' \
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
        type_declare_pattern = re.compile(r'([\w0-9]+)\s?: ([\w0-9]+)')
        if type_declare_pattern.match(row):
            type_declare = row.split(': ')
            doc_str += f'@param {type_declare[0].strip()} ({py_to_r_str(type_declare[1].strip())})'
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
             r'List\[Model\]': 'vector of Models'}  # FIXME: more general pattern

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
