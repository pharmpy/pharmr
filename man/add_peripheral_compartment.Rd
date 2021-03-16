\name{add_peripheral_compartment}
\alias{add_peripheral_compartment}
\title{add_peripheral_compartment}
\description{
\href{https://pharmpy.github.io/latest/reference/pharmpy.modeling.html?highlight=add_cov#pharmpy.modeling.add_peripheral_compartment}{Link to Python API reference} (e.g. for correct rendering of equations)

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

1   :math:`\mathsf{CL} = \mathsf{CL'}`, :math:`\mathsf{VC} = \mathsf{VC'}`,

:math:`\mathsf{QP1} = \mathsf{CL'}` and :math:`\mathsf{VP1} = \mathsf{VC'} \cdot 0.05`

2   :math:`\mathsf{QP1} = \mathsf{QP1' / 2}`, :math:`\mathsf{VP1} = \mathsf{VP1'}`,

:math:`\mathsf{QP2} = \mathsf{QP1' / 2}` and :math:`\mathsf{VP2} = \mathsf{VP1'}`

==  ===================================================
}
\usage{
add_peripheral_compartment(model)
}
\arguments{

}
\value{

}
