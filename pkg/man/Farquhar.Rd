\name{Farquhar}
\alias{Farquhar}
\title{
Farquhar-Ball-Berry coupled leaf gas exchange model
}
\description{
Coupled photosynthesis-stomatal conductance model. A full implementation of the photosynthesis model by 
Farquhar et al. (1980), as described by Medlyn et al. (2002) (i.e., following their notation), coupled with
a few choices of Ball-Berry type stomatal conductance models. The default is that of Medlyn et al. (2011).

Many parameters can be set in this model, including all temperature response parameters and stomatal
conductance parameters. See Details for more information.
}
\usage{
Farquhar(PAR, Tair, Ca, VPD, RH = 0, Patm = 101, SWP = 0, Vcmax, Jmax, Rd0, G1, ...)
}
\arguments{
  \item{PAR}{Photosynthetically active radiation (mu mol m-2 s-1).}
  \item{Tair}{Air temperature (deg C)}
  \item{Ca}{Atmospheric CO2 (ppm)}
  \item{VPD}{Vapor pressure deficit (kPa)}
  \item{RH}{Relative humidity (ignored unless MODELGS = 2)}
  \item{Patm}{Atmospheric pressure (kPa)}
  \item{SWP}{Soil water potential (MPa)}
  \item{Vcmax}{Required.}
  \item{Jmax}{Required.}
  \item{Rd0}{Dark respiration at 25 deg C (required).}
  \item{G1}{Slope parameter in stomatal conductance model.}
  \item{\dots}{Other parameters can be set : see Details.}
}
\details{

The minimum required parameters are: \code{Vcmax}, \code{Jmax}, \code{Rd0} and \code{G1}.
However, the model contains many other parameters, each with their own default value. To override the default value,
for example:
\preformatted{
	# Use Default values:
	Farquhar(PAR=1000, Tair=20, Ca=380, VPD=1, Vcmax=100, Jmax=150, Rd0=1)
	
	# Change the shape of the light response curve:
	Farquhar(PAR=1000, Tair=20, Ca=380, VPD=1, Vcmax=100, Jmax=150, Rd0=1, THETA=0.8)
}
Below is a list and explanation of all the parameters and their default values.

\preformatted{
	# Respiration parameters
	Q10F = 0.67		 # logarithm of the Q10 (Equation for respiration : 
					 # RESP =  RD0 * EXP(Q10F * (TLEAF-RTEMP)/10) * DAYRESP
	RTEMP = 25       # Reference temperature (T at which RD0 was measured)
	DAYRESP = 1.0    # Respiration in the light as fraction of that in the dark.   
	TBELOW = -100.0  # No respiration occurs below this temperature (degC).

	# Stomatal conductance parameters
	MODELGS = 4      # model : 4 = Medlyn et al. 2011; 6 = Tuzet et al. 2003.
	EMAXLEAF = 999   # Only used when considering soil water stress and MODELGS is not 6.
	KTOT = 2         # Leaf-specific hydraulic conductance (mmol m-2 s-1 MPa-1)

	G0 = 0.03        # Stomatal leakiness (gs when photosynthesis is zero).

	D0L = 5          # Parameter for the Leuning model (MODELGS=3)
	GAMMA = 0        # Gamma for all Ball-Berry type models
	G1 = 7           # Parameter for all Ball-Berry type models
	GK = 0.3         # Parameter for three-parameter Medlyn et al. 2011 model (MODELGS=5)

	SF = 3.2         # Tuzet model parameters (MODELGS=6)
	PSIV = -1.9

	# Light-response parameters of electron transport rate.
	THETA = 0.4      # Shape parameter of the non-rectangular hyperbola.
	AJQ = 0.324      # Quantum yield of electron transport.
	HMSHAPE = 0.999  # Shape of the hyperbolic minimum function (no need to change)

	# Temperature response parameters.
	# Parameters for Jmax.
	EAVJ = 37259     # Ha in Medlyn et al. (2002)
	EDVJ = 200000    # Hd in Medlyn et al. (2002)
	DELSJ = 640.02   # DELTAS in Medlyn et al. (2002)

	# Parameters for Vcmax.
	EAVC = 47590     # Ha in Medlyn et al. (2002)
	EDVC = 0.0       # Hd in Medlyn et al. (2002)
	DELSC = 0.0      # DELTAS in Medlyn et al. (2002)
}

}

\references{
Farquhar, G.D., S. Caemmerer and J.A. Berry. 1980. A biochemical model of photosynthetic CO2 assimilation 
in leaves of C3 species. Planta. 149:78-90.

Medlyn, B.E., E. Dreyer, D. Ellsworth, M. Forstreuter, P.C. Harley, M.U.F. Kirschbaum, X. Le Roux, P. Montpied, 
J. Strassemeyer, A. Walcroft, K. Wang and D. Loustau. 2002. Temperature response of parameters of a biochemically 
based model of photosynthesis. II. A review of experimental data. Plant Cell and Environment. 25:1167-1179.

Medlyn, B.E., R.A. Duursma, D. Eamus, D.S. Ellsworth, I.C. Prentice, C.V.M. Barton, K.Y. Crous, P. De Angelis, 
M. Freeman and L. Wingate. 2011. Reconciling the optimal and empirical approaches to modelling stomatal conductance. 
Global Change Biology. 17:2134-2144.

}
\author{
Remko Duursma. Original implementation in FORTRAN by Belinda Medlyn (in the Maestra model). 
See : \url{http://bio.mq.edu.au/maestra/}
}
\note{
The \code{Farquhar} function is really just a wrapper for the \code{photosyn} function in
the package \code{GasExchangeR}. In that package, the code is borrowed from the MAESTRA model.
}

\seealso{
\code{\link{lightresponse}},\code{\link{setPhy}}
}
\examples{

# A ypphy object, using the coupled Farquhar model.
eucphy <- setPhy("Farquhar", leafpars=list(Vcmax=50, Jmax=100, G1=8, G0=0.01, Rd0=1))

# The 'print' method reminds you what it is:
eucphy

}
\keyword{misc}

