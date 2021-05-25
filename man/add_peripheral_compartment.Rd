\name{add_peripheral_compartment}
\alias{add_peripheral_compartment}
\title{add_peripheral_compartment}
\description{
Add a peripheral distribution compartment to model.

\bold{Usage}

\code{add_peripheral_compartment(model)}

}
\details{
Link to Python
\href{https://pharmpy.github.io/latest/reference/pharmpy.modeling.html#pharmpy.modeling.add_peripheral_compartment}{API reference} (for correct rendering of equations, tables etc.).
Add a peripheral distribution compartment to model

The rate of flow from the central to the peripheral compartment

will be parameterized as QPn / VC where VC is the volume of the central compartment.

The rate of flow from the peripheral to the central compartment

will be parameterized as QPn / VPn where VPn is the volumne of the added peripheral

compartment.

Initial estimates:

==  ===================================================

n

==  ===================================================

1   :math:`{CL} = {CL'}`, :math:`{VC} = {VC'}`,

:math:`{QP1} = {CL'}` and :math:`{VP1} = {VC'} * 0.05`

2   :math:`{QP1} = {QP1' / 2}`, :math:`{VP1} = {VP1'}`,

:math:`{QP2} = {QP1' / 2}` and :math:`{VP2} = {VP1'}`

==  ===================================================

This documentation was automatically generated from Pharmpy (0.24.2).}
