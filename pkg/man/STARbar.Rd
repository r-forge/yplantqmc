\name{STARbar}
\alias{STARbar}
\alias{STARbar.plant3dlist}
\alias{STARbar.plant3d}
\alias{raytrace}
\alias{plot.tracedplant}
\title{Calculate displayed and projected leaf areas of a 3D plant}
\description{From a 3D plant, calculates the area of all leaves facing the viewing angle (from a given azimuth and
altitude), or 'projected area', and the area of the leaves that is visible ('displayed area'). By default, repeats 
calculations from many viewing angles to estimate the hemi-spherically averaged STAR, or \eqn{\overline{STAR}}. 
}
\usage{
\method{STARbar}{plant3d}(object, 
	method=c("gridtracer","exact","QuasiMC","slowquasimc"),
	integration=c("Turtlesky","Yplant","Turtle244"), 
	progressbar=TRUE, 
	returnldr=FALSE, 
	quiet=FALSE,
	npside=50, 
	silhouette=TRUE, 
	azimuth=NA, 
	altitude=NA, \dots)
\method{STARbar}{plant3dlist}(object, 
	quiet=FALSE, \dots)

}
\arguments{
  \item{object}{An object of class 'plant3d', see \code{\link{constructplant}}}
  \item{method}{The method to calculate the displayed area. See Details.}
  \item{integration}{The integration method to calculate the average STAR over the hemisphere. See Details.}
  \item{progressbar}{If TRUE (default), displays a graphical progress bar.}
  \item{npside}{For \code{method = "gridtracer"}, the number of grid cells per side for raytracing.}
  \item{returnldr}{If TRUE, returns a dataframe with results per viewing angle.}
  \item{quiet}{If TRUE, prints no progress to the screen.}
  \item{silhouette}{If TRUE, also calculates the 2D convex hull around the projected plant (see \code{\link{Silhouette}}).}
  \item{azimuth, altitude}{Azimuth and altitude of the viewing direction (Optional, in Radians).}
  \item{\dots}{Further arguments are ignored, for now.}
}
\details{

This function calculates the displayed area (DA) and projected area (PA) of the entire plant, either
over some specified viewing angle(s), or (by default) the hemispherically average values.
From these averages, the \eqn{\overline{STAR}} is estimated (see Duursma et al. 2012).

The \eqn{\overline{STAR}} can also be calculated with the \code{\link{summary.plant3d}} function.

There are four methods for the calculation of the displayed and projected area from a given viewing angle:
\describe{
\item{gridtracer}{A regular grid is placed over the projected plant. Displayed area is calculated from the
	number of intersecting grid points, and the grid size. The argument \code{npside} sets the number of grid points
	on the shorter side of the projected plant. 
	Slow for large plants, but good estimates are still obtained with low values of \code{npside}.}
\item{exact}{A polygon method to calculate the exact displayed area. Useful for testing, but \emph{very} slow.}
\item{QuasiMC}{Runs \code{QuasiMC} over the entire hemisphere - the \code{QuasiMC} model does the averaging. 
	Relatively slow for small plants, but fast for large plants.}
\item{slowquasimc}{Do not use this method (for testing only). Runs \code{QuasiMC} once per viewing angle, then averages the results.
	Painfully slow.}
}

There are three integration methods. Note that if \code{method = "QuasiMC"}, the integration method is ignored.
\describe{
\item{Turtlesky}{Integrates over 58 points, that are placed approx. uniformly over the hemisphere.}
\item{Yplant}{Integrates over 160 angles, distributed over the hemisphere as in the original Yplant. 
Weighing is applied over these angles to yield the average STAR.}
\item{Turtle244}{As Turtlesky, but uses 244 points (slow method).}
}


}
% \value{
% }
\author{
Remko Duursma
}

\references{
Duursma, R.A., D.S. Falster, F. Valladares, F.J. Sterck, R.W. Pearcy, C.H. Lusk, K.M. Sendall, M. Nordenstahl, 
N.C. Houter, B.J. Atwell, N. Kelly, J.W.G. Kelly, M. Liberloo, D.T. Tissue, B.E. Medlyn and D.S. Ellsworth. 2012. 
Light interception efficiency explained by two simple variables: a test using a diversity of small- to medium-sized 
woody plants. New Phytologist. 193:397-408.

See also \url{http://www.remkoduursma/yplantqmc}
}

\seealso{
\code{\link{projectplant}},\code{\link{constructplant}},\code{\link{Silhouette}},
\code{\link{summary.plant3d}}
}

\examples{

# Get STARbar for the built-in Toona plant:
# (Settings are fast, not accurate)
STARbar(toona, method="gridtracer", npside=15)

\dontrun{
# For exact STAR, use:
STARbar(toona, method="exact")

# To produce an LDR file (as in the original Yplant), use:
clidstar <- STARbar(toona, method="gridtracer", npside=30, integration="Yplant", returnldr=T)
write.table(clidstar$ldr, "toona.LDR")
}

}


\keyword{misc}
