\name{runYplant}
\alias{runYplant}
\alias{runYplant.plant3d}
\alias{runYplant.stand3d}

\title{
A single simulation of YplantQMC
}
\description{
Runs the YplantQMC model for one timestep. Runs the QuasiMC raytracer to estimate absorbed PAR
for every leaf on the plant, given diffuse and direct radiation (set by \code{fbeam}, see below),
the position of the sun, and reflectance and transmittance of the foliage material. 

Required is a 3D plant object (see \code{\link{constructplant}}). Optionally, a leaf gas exchange model 
is used (not needed if only light absorption is calculated) to calculate photosynthesis (and optionally,
transpiration rate). Also optionally, a hemiphoto object is used to calculate shading by the overstorey canopy.

Output is not as easy to use as the more user-friendly \code{\link{YplantDay}}. If you are only interested in
diurnal simulations (and plant totals by timestep), use that function. The \code{runYplant} function is available
for programming purposes (and more advanced custom simulations).
}
\usage{
\method{runYplant}{plant3d}(x, phy = NULL, hemi = NULL, reldiff = NULL, 
	reldir = NULL, 	altitude = 90, azimuth = 0, fbeam = 1, VPD = 1.5, 
	PAR0 = 1, PARwhere = c("above", "below"), Ca = 390, Tair = 25, 
	Patm = 101, reflec = c(0.1, 0.1), transmit = c(0.1, 0.1), 
	runphoto = TRUE, intern = TRUE, 
	debug = FALSE, delfiles=TRUE, rewriteplantfile = TRUE, ...)
\method{runYplant}{stand3d}(x,...)
}
\arguments{
  \item{x}{An object of class 'plant3d', see \code{\link{constructplant}}}
  \item{phy}{An object of class 'ypphy', see \code{\link{setPhy}}}
  \item{hemi}{An object of class 'yphemi', see \code{\link{setHemi}}}
  \item{reldiff}{Optional. A vector of relative diffuse absorption, same length as number of leaves. See Details.}
  \item{reldir}{Optional. A vector of relative direct absorption, same length as number of leaves. See Details.}
  \item{altitude,azimuth}{Solar altitude and azimuth (degrees).}
  \item{fbeam}{Beam fraction (0-1). If 0, only diffuse interception is calculated, if 1, only direct.}
  \item{VPD}{Vapor pressure deficit (kPa)}
  \item{PAR0}{Incident PAR on a horizontal surface (mu mol m-2 s-1).}
  \item{PARwhere}{If 'above', \code{PAR0} is given as an above-canopy value. If 'below', it is below the canopy. See Details.}
  \item{Ca}{Atmospheric CO2 concentration (ppm).}
  \item{Tair}{Air temperature (deg C).}
  \item{Patm}{Atmospheric pressure (kPa).}
  \item{reflec}{Leaf reflectance (top, bottom of leaf).}
  \item{transmit}{Leaf transmittance (top, bottom of leaf).}
  \item{runphoto}{Whether to run leaf gas exchange model (default TRUE, or FALSE when no phy object given).}
  \item{intern}{If FALSE, returns output of QuasiMC to the console.}
  \item{debug}{If TRUE, opens the QuasiMC debug window (for testing).}
  \item{delfiles}{If TRUE, deletes intermediate files, and QuasiMC in/output files.}
  \item{rewriteplantfile}{If TRUE, writes the plant QuasiMC input file.}
  \item{\dots}{Further arguments passed to \code{writecfg}.}
}
\details{

The arguments \code{intern}, \code{debug}, \code{delfiles} and \code{rewriteplantfile} should not be set by the user,
unless you really know what you are doing. These arguments exist for testing, and are used by \code{YplantDay}.

The arguments \code{reldiff} and \code{reldir} can be supplied if they are already known (from a previous 
simulation, when the solar angle was the same, in particular). If you are not sure, please do not set these
arguments!

}
\value{
This returns a dataframe with one row per leaf. The variables included are 
(PAR is in units mu mol m-2 s-1):

\describe{
\item{PAR0}{Incident PAR on a horizontal surface *above* the canopy}
\item{PARinc}{Incident PAR on a horizontal surface *below* the canopy}
\item{PARleaf}{Absorbed PAR (for each leaf)}
\item{PARdir}{Absorbed direct PAR}
\item{PARdiff}{Absorbed diffuse PAR}
\item{reldiff}{Relative diffuse absorbed PAR (0 - 1)}
\item{reldir}{Relative direct absorbed PAR (0 - 1)}
\item{LA}{Leaf area (mm2)}
\item{LAproj}{Projected leaf area (mm2)}
\item{LAsunlit}{Sunlit leaf area (mm2)}
\item{A}{CO2 assimilation rate (mu mol m-2 s-1)}
\item{E}{Transpiration rate (mmol m-2 s-1)}
\item{gs}{Stomatal conductance (mol m-2 s-1)}
\item{A0}{CO2 assimilation rate for a horizontal leaf *below* the canopy.}
}

}

\references{
See \url{http://www.remkoduursma/yplantqmc}
}

\author{
Remko Duursma
}



\seealso{
\code{\link{YplantDay}}, \code{\link{setPhy}}, \code{\link{setHemi}}.
}
\examples{

\dontrun{

# Compare diffuse only to direct only
run_dir <- runYplant(pilularis, fbeam=1, altitude=90, azimuth=0, reflec=0.15, transmit=0.1)
run_diff <- runYplant(pilularis, fbeam=0, reflec=0.15, transmit=0.1)

# Compare density functions of absorbed PAR by leaf:
plot(density(run_dir$PARleaf, from=0, to=1), xlim=c(0,1), main="", lwd=2, col="blue",
	xlab="Absorbed PAR (relative units)")
lines(density(run_diff$PARleaf, from=0, to=1), lwd=2, col="red")
legend("topright",c("Diffuse","Direct"), lwd=2, col=c("red","blue"))
}

}

\keyword{misc}
