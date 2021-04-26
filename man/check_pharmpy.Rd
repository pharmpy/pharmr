\name{check_pharmpy}
\alias{check_pharmpy}
\title{
Checks Pharmr and Pharmpy version.
}
\description{
Compares Pharmr version to which version of Pharmpy that is used.
}
\usage{
check_pharmpy(pharmpy_version)
}
\arguments{
\item{
pharmpy_version
}{
Pharmpy version (can be called by pharmpy$`__version__`)
}
}
\details{
The documentation of modeling functions in Pharmr is automatically generated from 
the latest release of Pharmpy. This function checks if the two have the same major 
and minor version, otherwise the documentation may be outdated and an update is 
recommended.
}