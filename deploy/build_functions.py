import inspect
import os

from pathlib import Path

import pharmpy.modeling
import pharmpy.tools

from func_conversion import create_r_func
from docs_conversion import create_r_doc


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
    funcs = inspect.getmembers(module, inspect.isfunction)
    func_str = ''
    for name, func in funcs:
        if name not in module.__all__:
            continue
        r_func = create_r_func(func, module)
        r_doc = create_r_doc(func)
        func_str += f'{r_doc}\n{r_func}\n\n'
    return func_str


if __name__ == '__main__':
    create_functions()
