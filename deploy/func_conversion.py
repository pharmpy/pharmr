import inspect

import re
import textwrap
from typing import get_args, get_origin, get_type_hints, Union


from help_functions import py_to_r_arg


def create_r_func(func, module):
    func_name = func.__name__
    if 'tools' in module.__name__:
        module_name = 'tools'
    elif 'modeling' in module.__name__:
        module_name = 'modeling'
    else:
        raise ValueError(f'Module {module.__name__} not supported')

    if not inspect.getdoc(func):
        raise ValueError(f'No documentation available for {func_name}')

    params = inspect.signature(func).parameters
    wrapper_arg_str, pyfunc_arg_str = _get_args(params)

    func_def = f'{func_name} <- function({wrapper_arg_str})'
    func_execute = f'func_out <- pharmpy${module_name}${func_name}({pyfunc_arg_str})'

    if module_name == 'tools':
        r_func_body = _create_func_body_tool(func, func_execute)
    else:
        r_func_body = []
        # FIXME remove once model is immutable
        if 'model' in params.keys() and func.__name__ != 'copy_model':
            r_func_body += ['if (\'pharmpy.model.model.Model\' %in% class(model)) {',
                            'model = pharmpy$modeling$copy_model(model)',
                            '}']
        r_func_body += _create_func_body_modeling(func, func_execute)

    r_wrapper = [f'{func_def} {{',
                 *r_func_body,
                 '}']

    r_wrapper_indented = _indent(r_wrapper)
    return '\n'.join(r_wrapper_indented)


def _get_args(params):
    wrapper_args, pyfunc_args = [], []
    for param in params.values():
        if param.kind == param.VAR_KEYWORD or param.kind == param.VAR_POSITIONAL:
            if '...' not in wrapper_args:
                wrapper_args.append('...'), pyfunc_args.append('...')
        elif param.default is param.empty:
            wrapper_args.append(f'{param.name}'), pyfunc_args.append(f'{param.name}')
        else:
            wrapper_args.append(f'{param.name}={py_to_r_arg(param.default)}')
            pyfunc_args.append(f'{param.name}={param.name}')

    return ', '.join(wrapper_args), ', '.join(pyfunc_args)


def _create_func_body_modeling(func, func_execute):
    r_func_body = [*_preprocess_input(func),
                   f'{func_execute}',
                   *_create_func_return(func)]
    return r_func_body


def _create_func_body_tool(func, func_execute):
    error_msg = [
        'message(cond)',
        'message(\'Full stack:\')',
        'message(reticulate::py_last_error())',
        'message("pharmr version: ", packageVersion("pharmr"))',
        'message("Pharmpy version: ", print_pharmpy_version())',
        'return(NA)'
    ]

    r_func_body = [
        'tryCatch(',
        '{',
        *_preprocess_input(func),
        f'{func_execute}',
        *_create_func_return(func),
        '},',
        'error=function(cond) {',
        *error_msg,
        '},',
        'warning=function(cond) {',
        *error_msg,
        '}',
        ')',
    ]

    return r_func_body


# FIXME make more general (handle nested types), combine overlapping functionality with
#  equivalent in docs_conversion.py::_translate_type_hints
def _preprocess_input(func):
    try:
        type_hints = {key: value for key, value in get_type_hints(func).items() if key != 'return'}
    except AttributeError:
        type_hints = None

    if not type_hints:
        return []

    r_preprocess = []
    for key, value in type_hints.items():
        args, origin = get_args(value), get_origin(value)
        if value is Union or origin is Union:
            r_conversion = _get_conversion_str(key, args, origin)
        else:
            r_conversion = _get_conversion_str(key, value, origin)

        if r_conversion is None:
            continue

        if origin is Union and type(None) in get_args(value):
            r_conversion_null = [f'if (!is.null({key}) {{',
                                 r_conversion,
                                 '}']
            r_preprocess.extend(r_conversion_null)
        else:
            r_preprocess.append(r_conversion)

    return r_preprocess


def _get_conversion_str(key, args, origin):
    if args is int:
        return f'{key} = as.integer({key})'
    elif origin is list:
        return f'{key} = as.list({key})'
    # FIXME make more general (handles optional type)
    elif isinstance(args, tuple) and len(args) == 2 and type(None) in args:
        if get_origin(args[0]) is list:
            return f'{key} = as.list({key})'
    return None


def _create_func_return(func):
    func_return = []
    if _has_return_type_pd(inspect.getdoc(func)):
        r_reset_index = [
            'if (func_out$index$nlevels > 1) {',
            'func_out <- func_out$reset_index()',
            '}'
        ]
        func_return.extend(r_reset_index)
    func_return += [
        f'return(py_to_r(func_out))',
    ]
    return func_return


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


def _indent(r_code):
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

