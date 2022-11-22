import inspect

import re
import textwrap


from help_functions import py_to_r_arg


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

    r_wrapper_indented = _indent(r_wrapper)
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

