\name{installQuasiMC}
\alias{installQuasiMC}
\title{
Download and install the QuasiMC executable
}
\description{
Downloads the Windows executable necessary for running \code{\link{runYplant}} and 
\code{\link{YplantDay}}. 

QuasiMC developed by Cieslak et al. (2008), which is part of the Virtual Laboratory plant modelling 
software created at the University of Calgary (\url{http://www.algorithmicbotany.org}).

The executable is \code{QuasiMC.exe} (ca. 7MB), and the settings file is \code{enviro.e} (tiny).

Both files are placed in the directory \code{c:/QuasiMC}, which is created if it does not exist already.

This function can also be used to update an existing installation.
}
\usage{
installQuasiMC(proxy = FALSE)
}
\arguments{
  \item{proxy}{Use TRUE if connecting through a proxy.}
}

\references{
Cieslak, M., C. Lemieux, J. Hanan and P. Prusinkiewicz. 2008. Quasi-Monte Carlo simulation of 
the light environment of plants. Functional Plant Biology. 35:837-849.
}
\author{
QuasiMC by Mik Cieslak, R installation code by Remko Duursma. 
}

\keyword{misc}