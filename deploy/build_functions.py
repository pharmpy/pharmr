from inspect import getfullargspec, getmembers, isfunction
from pathlib import Path

import pharmpy.modeling


def create_functions():
    modeling_functions = getmembers(pharmpy.modeling, isfunction)
    full_str = ''

    for _, func in modeling_functions:
        r_func = create_r_func(func)
        full_str += f'{r_func}\n\n'

    pharmr_root = Path(__file__).parent.parent
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
            defaults_new = [py_to_r(str(d)) for d in defaults]
            defaults = [None for _ in range(len(args) - len(defaults_new))] + defaults_new
        args_defaults = {arg: default for arg, default in zip(args, defaults)}
        arg_list = [(f'{arg}={default}' if default else f'{arg}') for arg, default in args_defaults.items()]
    else:
        arg_list = args

    func_args = ', '.join(arg_list)
    args_str = ', '.join(args)

    r_func = f'{func_name} <- function({func_args}) {{\n' \
             f'    return(pharmpy$modeling${func_name}({args_str}))\n' \
             f'}}'

    return r_func


def py_to_r(arg):
    py_to_r_dict = {'None': 'NULL',
                    'True': 'TRUE',
                    'False': 'FALSE',
                    '': '\"\"'}

    try:
        return py_to_r_dict[arg]
    except KeyError:
        if isinstance(arg, str):
            return f'\'{arg}\''
        else:
            return arg


if __name__ == '__main__':
    create_functions()
