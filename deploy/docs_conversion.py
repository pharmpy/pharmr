import inspect
import re
import warnings

from typing import Any, Literal, Union, get_args, get_origin, get_type_hints
from collections.abc import Iterable, Mapping, Sequence

from help_functions import py_to_r_str, SKIP, TYPE_DICT


def create_r_doc(func):
    doc = inspect.getdoc(func)

    if not doc:
        return f'#\' @title\n' \
               f'#\' {func.__name__}\n' \
               f'#\' \n' \
               f'#\' @export'

    doc_dict = _split_doc_to_subtypes(doc)

    doc_str = f'@title\n{func.__name__}\n\n@description\n'

    for row in doc_dict['description']:
        is_list = row.startswith('-')
        if is_list:
            row = re.sub('^- ', '* ', row)
        if row.startswith('.. '):
            row = re.sub(r'^\.\. ', '', row)
        doc_str += f'{py_to_r_str(row)}\n'

    if 'params' in doc_dict.keys():
        doc_str += _create_r_params(doc_dict['params'], func)
    if 'returns' in doc_dict.keys():
        doc_str += _create_r_returns(doc_dict['returns']) + '\n\n'
    if 'examples' in doc_dict.keys():
        doc_str += _create_r_example(doc_dict['examples']) + '\n'
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


def _create_r_params(doc_list, func):
    type_hints = {key: value for key, value in get_type_hints(func).items() if key != 'return'}

    params = inspect.signature(func).parameters.values()
    params_unbound = [param for param in params if param.kind in (param.VAR_KEYWORD, param.VAR_POSITIONAL)]

    if len(params_unbound) > 2:
        raise ValueError(f'Unexpected number of unbound parameters: {func.__name__}')

    if len(type_hints) == len(params) - len(params_unbound):
        type_dict = _convert_types_from_typehints(type_hints)
        if params_unbound:
            for param in params_unbound:
                type_dict[param.name] = None
    else:
        raise ValueError(f'All type hints not represented: {func.__name__}')

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


def _convert_types_from_typehints(type_hints):
    type_dict = {}
    for var_name, var_type in type_hints.items():
        try:
            r_type = _translate_type_hints(var_type)
        except NotImplementedError:
            raise
        if r_type == '':
            raise ValueError('Could not translate')
        type_dict[var_name] = r_type

    return type_dict


def _translate_type_hints(var_type):
    if isinstance(var_type, type):
        # Do not attempt to translate
        if var_type.__module__.startswith('pharmpy'):
            return var_type.__name__
        elif var_type not in TYPE_DICT:
            warnings.warn(f'Could not translate type: {var_type}')
            return var_type
        return TYPE_DICT[var_type]
    elif var_type == Any:
        return 'any'
    else:
        args, origin = get_args(var_type), get_origin(var_type)
        args_trans = [_translate_type_hints(arg) for arg in args if arg not in SKIP]
        # If two args are translated to same type, for union only needs to be written once
        if origin is Union:
            args_trans = list(filter(None, dict.fromkeys(args_trans)))
        if not args_trans:
            return ''
        if origin is Union:
            if type(None) in args:
                return f'{" or ".join(args_trans)} (optional)'
            else:
                return ' or '.join(args_trans)
        elif origin in (list, Iterable, Sequence):
            return f'array({",".join(args_trans)})'
        elif origin in (dict, Mapping):
            return f'list({"=".join(args_trans)})'
        elif origin is tuple:
            return f'list({",".join(args_trans)})'
        elif origin is Literal:
            return 'str'
        else:
            raise NotImplementedError(f'Could not translate origin: {origin}')


def _get_desc(var_names, docstring):
    # FIXME currently assumes order is same as in typing
    m = re.compile(r'^\S+\s*:\s*.+\s*\n|kwargs\n|args\n', flags=re.MULTILINE)
    descs = m.split(docstring)
    descs = list(filter(None, descs))
    assert len(descs) == len(var_names)
    return {name: py_to_r_str(desc) for name, desc in zip(var_names, descs)}


def _create_r_returns(doc_list):
    doc_list_trim = list(filter(None, doc_list))
    type_py = doc_list_trim[0]
    type_r = py_to_r_str(type_py)

    doc_str = f'@return ({type_r}) '
    doc_str += ' '.join(py_to_r_str(row) for row in doc_list_trim[1:])

    return doc_str


# FIXME: use from Pharmpy
def _create_r_example(doc_list):
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


def _split_doc_to_subtypes(doc_str):
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
