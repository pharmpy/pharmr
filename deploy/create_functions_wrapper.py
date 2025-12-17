import os

from pathlib import Path

import pharmpy.modeling
import pharmpy.tools
from pywrapr.build_functions import create_module_functions


def create_functions():
    skip = ['sympy', 'symengine', 'numpy']
    modeling_str = create_module_functions(pharmpy.modeling, skip=skip)
    tool_str = create_module_functions(pharmpy.tools, skip=skip)
    full_str = modeling_str + tool_str
    # TODO: more general way to get right directory
    cwd = os.getcwd()
    pharmr_root = Path(cwd).parent
    func_path = pharmr_root / 'R' / 'functions_wrapper.R'

    with open(func_path, 'w') as f:
        f.write(full_str)


if __name__ == '__main__':
    create_functions()
