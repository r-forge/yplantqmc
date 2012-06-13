\name{readl}
\alias{readl}
\title{
Read a Yplant leaf file
}
\description{
Reads a Yplant leaf file (.l or .lf) into an object of class \code{leaffile}.

NOTE: if there are more than one leaf type type in the leaffile, it only reads the first one.
At the moment, YplantQMC only uses one leaf file for all leaves in the canopy.
}
\usage{
readl(lfile = NA)
}
\arguments{
  \item{lfile}{Name of the leaf file (character string).}
}
\value{
An object of class \code{leaffile}. Currently, only a plot method exists (see \code{\link{plot.leaffile}}).
}
\author{
Remko Duursma
}
\seealso{
\code{\link{plot.leaffile}}
}



\keyword{ misc }
