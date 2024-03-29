\name{makeStand}
\alias{makeStand}
\title{
Make a stand of virtual plants
}
\description{
Make a stand of plants, for use in \code{runYplant}, and for visualization. See the example below to get started. Support for \code{runYplant} is somewhat experimental, and \code{YplantDay} is not supported yet. Proceed at your own risk.
}
\usage{
makeStand(plants = list(), xyz = data.frame(x = 0, y = 0, z = 0), plotbox = NULL)
}

\arguments{
  \item{plants}{List of plants to be placed in the stand.}
  \item{xyz}{Data frame (or matrix) with x,y,z locations of the stem positions of the plants.}
  \item{plotbox}{Optional. Plot boundary, used for scaling-up purposes.}
}
\details{

The \code{xyz} argument must be a dataframe or matrix with three columns, and it is assumed to be in the order X,Y,Z.

The \code{plotbox} argument is optional, if it is not provided the plot boundary will be as a rectangle that just fits
around the projected crown area. In some cases, the base of the stem can thus fall outside the plot boundary. For now, the plot boundary is only used to calculate the leaf area index, which has no bearing on any simulation results.

}

\value{
An object of class \code{stand3d}, methods exist for \code{print}, \code{plot}, \code{runYplant}. And soon, \code{YplantDay}.

}
%\references{

%}
\author{
Remko Duursma
}

\examples{

\dontrun{

# Make a stand consisting of three 'toona' plants.
toonastand <- makeStand(list(toona,toona,toona),
                        xyz=data.frame(x=c(0,200,100),
                                       y=c(50,50,300),
                                       z=c(0,0,0)))

# The print method shows a very short summary:
toonastand

# Plot the stand
plot(toonastand)

}

}

\keyword{misc}
