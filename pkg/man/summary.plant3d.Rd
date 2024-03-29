\name{summary.plant3d}
\alias{summary.plant3d}
\alias{print.summary.plant3d}
\alias{summary.plant3dlist}
\title{
Summarize 3D plants
}
\description{
Summarize the 3D plant in various useful ways. Requires an object of class \code{plant3d}, 
made with \code{\link{constructplant}}.
}
\usage{
\method{summary}{plant3d}(object, nKErepeat = 10, nsignif = 3, calcSTARbar=FALSE, \dots)
\method{summary}{plant3dlist}(object, writefile=FALSE, \dots)
}
\arguments{
  \item{object}{Object of class 'plant3d' (one 3D plant) or 'plant3dlist' (a list of 3D plants).}
  \item{nKErepeat}{Number of replicates for \code{\link{leafdispersion}}, see its help page.}
  \item{nsignif}{Number of digits for output (only for printing).}
  \item{writefile}{If TRUE, writes a text file with the summary results in the cur. working dir.}
  \item{calcSTARbar}{If TRUE, also calculates STARbar and adds it to the summary result.}
  \item{\dots}{Further arguments passed to \code{\link{STARbar}}.}
}
\details{
The \code{summary.plant3d} prints a number of plant summary variables. They are also stored in a list.
These are the variables that are currently calculated:

\preformatted{  
  LA           -  Total leaf area (m2)
  meanleafsize -  Mean leaf size (cm2)
  nleavesp     -  Number of leaves
  leaflen      -  Mean leaf length (cm)
  meanleafang  -  Mean leaf angle (deg)
  wmeanleafang -  Mean leaf angle weighted by leaf area (deg)
  Xellipsoid   -  Ellipsoidal leaf angle dist. par.
  crownvol     -  Crown volume (convex hull) (m3)
  crownsurf    -  Crown surface area (convex hull) (m2)
  ALAC         -  Crown density (AL/AC) (m2 m-2)
  cw           -  Crown width (m)
  cl           -  Crown length (m)
  htot         -  Total height(m)
  cshape       -  Crown shape index (-)
  stemsurf     -  Stem + branch surface area (cm2)
  stemvol      -  Stem + branch volume (cm3)
  stemdiam     -  Stem base diameter (mm)
  meanpath     -  Mean pipe length (mm)
  sdpath       -  Standard deviation of pipe length (mm)
  totlen       -  Total woody segment length (mm)
  Ek           -  Expected distance to 5 nearest leaves (no edge corr.)
  Ek2          -  Expected distance to 5 nearest leaves (with edge corr.)
  Ok           -  Observed distance to 5 nearest leaves
  disp         -  Dispersion parameter (no edge corr.)
  disp2        -  Dispersion parameter (with edge corr.)
  STARbar      -  (Optional, only when calcSTARbar = TRUE). 
}

Note that when generating a summary on a \code{plant3dlist} object, the above information is written to an outputfile.
The outputfile is tab-delimited, and has the name 'Plant summaries-YYYY-MM-DD.txt', using the current date.

The following functions are called to calculate some of the summary variables:
\code{\link{leafdispersion}} for Ek,Ek2,Ok,disp,disp2; \code{pathlen} (hidden function) for meanpath,sdpath,totlen;
the \code{\link{fitdistribution}} function in the \code{LeafAngle} package for Xellipsoid; \code{\link{getR}} for cw;
and \code{\link{crownhull}} for crownsurf, crownvol and ALAC. The remainder of the variables are trivial calculations 
from the plant input file (the .p file) or the leaf file. 

Optionally, the \eqn{\overline{STAR}} is calculated (when \code{calcSTARbar = TRUE}), by calling \code{\link{STARbar}}. 
See its help page for full details. Note that all options for \code{\link{STARbar}} can also be set with the 
summary function (see Examples).

}
\value{
A list with components described above. See also the Examples below.
}

\author{Remko Duursma}

\seealso{\code{\link{crownhull}}, \code{\link{leafdispersion}}, \code{\link{getR}}}

\examples{

# Print summary (use built-in Toona plant):
summary(toona)

# Or save summary as a list, access single values:
plantsumm <- summary(toona)
plantsumm$meanpath  # mean path length from soil to leaf
# See table above for names of single variables.

\dontrun{
# Summary on a plant3dlist ('myplants' is constructed with 'readplantlist').
summary(myplants, writefile=TRUE)

# Also calculate STARbar (with the exact method).
summary(myplant, calcSTARbar=TRUE, method="exact")
}

}
\keyword{misc}
