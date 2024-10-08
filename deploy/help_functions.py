import pandas as pd
import re
from pathlib import Path
from typing import Any
import symengine

from pharmpy.deps import sympy
from pharmpy.deps import numpy as np
from pharmpy.workflows import ModelDatabase

TYPE_DICT = {
    str: 'str',
    int: 'numeric',
    float: 'numeric',
    sympy.Float: 'numeric',
    bool: 'logical',
    list: 'array',
    None: 'NULL',
    pd.DataFrame: 'data.frame',
    pd.Series: 'array',
    Any: 'any'
}

SKIP = [
    sympy.Expr,
    sympy.Symbol,
    symengine.Basic,
    sympy.Eq,
    np.random.Generator,
    Path,
    type(None),
    ModelDatabase,
]


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
        elif isinstance(arg, tuple):
            def _convert(elem):
                if isinstance(elem, str):
                    return f'\'{elem}\''
                return elem

            return f"c({','.join(_convert(elem) for elem in arg)})"
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

    py_to_r_dict = {**args, **types}

    if not example:
        py_to_r_dict = {**py_to_r_dict, **{r'\[([0-9]+)\]_*': r'(\1)'}}
        py_to_r_dict = {**py_to_r_dict, **{r'\[([\'\"\w(),\s]+)]': r'c(\1)'}}
        py_to_r_dict = {**py_to_r_dict, **{r'\{(.+:.+,*)+\}': r'list(\1)'}}
        py_to_r_dict = {**py_to_r_dict, **{
            r'list\(((.+):(.+),*)+\)': lambda x: x.group().replace(':', "=".format(x.group(1)))}
                        }

    arg_sub = arg
    for key, value in py_to_r_dict.items():
        arg_sub = re.sub(key, value, arg_sub)

    return arg_sub
