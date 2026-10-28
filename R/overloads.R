# SPDX-FileCopyrightText: 2021-2026 Rikard Nordgren and Stella Belin
#
# SPDX-License-Identifier: LGPL-3.0-or-later

#' @exportS3Method "$" pharmpy.internals.immutable.Immutable
`$.pharmpy.internals.immutable.Immutable` <- function(x, name)
{
    obj <- reticulate::py_get_attr(x, name)
    convert_output(obj)
}

#' @exportS3Method length pharmpy.model.parameters.Parameters
length.pharmpy.model.parameters.Parameters <- function(x) {
    reticulate::py_len(x)
}
