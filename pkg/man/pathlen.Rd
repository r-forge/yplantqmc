\name{pathlen}
\alias{pathlen}
\title{
Calculates path length from soil to leaf
}
\description{
For an object of class \code{plant3d}, calculates for each leaf the distance to the lowest node, 
through the branching structure.

Problems have been spotted with some plants. Use this function at your own risk!

}
\usage{
pathlen(plant, 
testleaf = NA, 
add = FALSE, 
plotcols = c("orange", "green"))
}
\arguments{
  \item{plant}{
Object of class \code{plant3d}, see \code{\link{constructplant}}.
}
\item{testleaf}{
Optional, if specified, calculates path length only for this leaf, and produces a plot.
}
\item{add}{
If a testleaf was specified, whether or not to add it to a current plot.
}
\item{plotcols}{
Colors for stem and branch segments.
}
}

\value{
So far, a dataframe with one variable ("totlen"), specifying the total path length from each leaf to the lowest node.
}

\note{
This function is called by \code{\link{summary.plant3d}}, to calculate the average path length
for all leaves in a plant. It will be used, eventually, to drive a hydraulic model in YplantQMC.
}

\author{
Remko Duursma
}

\seealso{
\code{\link{constructplant}},\code{\link{summary.plant3d}}
}

% \examples{


% }

\keyword{misc}

