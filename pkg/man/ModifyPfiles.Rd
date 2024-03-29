\name{ModifyPfiles}
\alias{replaceangles}
\alias{changeinternodes}
\alias{modifypfile}
\title{ Modify Yplant input files.}
\description{
Reads a plant file (a Yplant input file with extension \code{.p}, known as a \code{pfile}), 
and writes a new pfile, by modifying angles, internode lengths, or segment diameters. 
}
\usage{
modifypfile(pfile = NA, whichvar = NA, outputfile = "tmp.p", 
newvalues = NULL)

replaceangles(whichangle="An.3", pfile=NA, outputfile = NA,
newangles = NULL, distobj = NULL)

changeinternodes(pfile=NA,outputfile = "tmp.p",method=c("perc","constant"),
	changeperc=50, consvalue = NA)
}
\arguments{
  \item{whichangle}{By default the leaf angle, or one of the other angles (see Details).}
  \item{pfile}{Name of the .p file.}
  \item{outputfile}{Name of the new file.}
  \item{distobj}{An object of class \code{\link{angledist}}, 
  that is a leaf angle distribution (See examples).}
  \item{newangles}{A vector of angles to be used in the new p file (optional, in stead of distobj).}
  \item{whichvar}{Name of variable in p file to modify (see \code{\link{readp}} for list of variables.}
  \item{newvalues}{Vector of new values for the variable that is to be replaced.}
  \item{method}{for \code{changeinternodes}, a percentage change ("perc") or a new constant value ("constant").}
  \item{changeperc}{If method="perc", change the internodes by this percentage of their orginal value.}
  \item{consvalue}{If method="constant", change all internodes to this constant value.}
  
}
\details{

 The function 'modifypfile' is a general function that can be used to modify any variable
 in a \code{pfile}. The following variables can be changed in a \code{pfile} :
 
 \describe{
 \item{Az, An}{ Azimuth and angle of stem sections}
 \item{Or}{ Orientation angle of the midrib of the leaf}
 \item{Az.1, An.1}{ Branch sections}
 \item{Az.2, An.2}{ Petioles }
 \item{Az.3, An.3}{ Leaves }
 }
 
 The function \code{replaceangles} is a specialized version for replacing leaf angles 
 (and do some specific error checking, or sampling from leaf angle distributions). The function \code{changeinternodes}
 is a special function for lengths of woody segments (technically not quite the same as internodes!).
 
 For \code{replaceangles}, an object of class \code{angledist} can be constructed with the \code{\link{angledist}} function in 
 package \code{LeafAngle} (See Example below) or from fitting to a sample with \code{\link{fitdistribution}}.  If angles are provided but the vector is 
 too short, the vector will be sampled with replacement.

}
\value{
A new \code{pfile} is created, by default with the name "tmp.p", unless
the argument \code{outputfile} is set.
}

\author{ Remko Duursma }

\seealso{ \code{\link{readp}}, \code{\link{fitdistribution}}, \code{\link{readl}}, \code{\link{constructplant}}}

\examples{

\dontrun{

# Replace angles by sampling from an ellipsoidal distribution:
mydist <- angledist("ellipsoid", distpars=0.7)
replaceangles(pfile="someplant.p", distobj=mydist)

# Make constant angles:
replaceangles(pfile="someplant.p", newangles=45)

# Change new file name:
replaceangles(pfile="someplant.p", outputfile="someplant 45degrees.p", newangles=45)

# Change petiole orientation, choose pfile with dialog box:
replaceangles("Az.2", newangles=runif(300, 0, 360))

# Modify various variables in a pfile, until we end up with an artificial plant,
# useful for testing.
# Order of changes, in this case (although it does not matter!):
# Leaf azimuth, leaf orientation, leaf angle, petiole length, petiole angle.
modifypfile("originalplant.p", whichvar="Az.3", outputfile="testplant.p", newvalues=45)
modifypfile("testplant.p", whichvar="Or", outputfile="testplant.p", newvalues=45)
modifypfile("testplant.p", whichvar="An.3", outputfile="testplant.p", newvalues=-45)
modifypfile("testplant.p", whichvar="L.2", outputfile="testplant.p", newvalues=10)
modifypfile("testplant.p", whichvar="An.2", outputfile="testplant.p", newvalues=45)


}

}
\keyword{ misc }
