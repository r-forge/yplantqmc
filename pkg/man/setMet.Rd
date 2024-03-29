\name{setMet}
\alias{setMet}

\title{
Generate a weather object
}
\description{
To run Yplant, a weather object needs to be constructed, that contains solar position data, radiation,
air temperature, and so on. This function generates a daily diurnal weather dataset using a fairly standard
weather generator, or constructs the weather object with user-specified data. See Details.
}
\usage{
setMet(location=NULL,
	metdat=NULL,
	year = 2012, 
	month = NA, 
	day = NA, 
	nsteps = 10, 
	PARday = 22, 
	AtmTrans = 0.76, 
	fbeamday = NA, 
	fbeammethod = c("spitters", "constant"), 
	Tmin = 10, 
	Tmax = 25, 
	VPDmax = NA, 
	maxlag = 0.1, 
	Ca = 390, 
	Patm = 101)
}
\arguments{
  \item{location}{A Yplant location object (class 'yplocation', see \code{\link{setLocation}}).}
  \item{metdat}{Optionally, a dataframe (or name of CSV file) with standard weather variables.}
  \item{year}{Optional (slight effects on solar path).}
  \item{month}{1-12}
  \item{day}{day of month}
  \item{nsteps}{number of steps (will affect number of simulation steps in \code{\link{YplantDay}}.}
  \item{PARday}{Total daily PAR on a horizontal surface (mol m-2 d-1).}
  \item{AtmTrans}{Atmospheric transmission.}
  \item{fbeamday}{Daily beam fraction.}
  \item{fbeammethod}{If 'Spitters', uses the Spitters algorithm to estimate fbeam by timestep, 
	otherwise it is constant (and given by \code{fbeamday}.}
  \item{Tmin,Tmax}{Daily minimum and maximum temperature (deg C).}
  \item{VPDmax}{Optional. Daily maximum VPD (if not given, estimated from \code{Tmin}).}
  \item{maxlag}{Lag of temperature maximum behind solar maximum (fraction of day).}
  \item{Ca}{Atmospheric CO2 concentration (ppm).}
  \item{Patm}{Atmospheric pressure (kPa).}
}
\details{

A built-in weather generator simulates the following variables:
\describe{
\item{altitude,azimuth}{Position of the sun (degrees).}
\item{PAR}{Photosynthetically active radiation (mu mol m-2 s-1).}
\item{fbeam}{Fraction direct beam of PAR (-).}
\item{Tair}{Air temperature (deg C).}
\item{VPD}{Vapor pressure deficit.}
}
The following two variables are user input, and have no within-day variation:
\describe{
\item{Ca}{Atmospheric CO2 concentration (ppm). (Default = 390ppm).}
\item{Patm}{Atmospheric pressure (kPa). (Default = 1.01kPa).}
}


If you are curious about the algorithm, please check the code (type \code{setMet}).

To generate a weather dataset, simply use this command:
\preformatted{
	aprilday <- setMet(richmond, nsteps=12, Tmin=9, Tmax=25, month=6, day=21)
}
Where \code{richmond} is a Yplant location object, generated with \code{\link{setLocation}}.

The weather object can be plotted: the following command produces a simple built-in graph
of PAR, Tair, VPD and fbeam:
\preformatted{
	plot(aprilday)
}

Alternatively, the user can input a dataframe (or CSV file) that contains the weather variables 
(or a subset of them). For example,
\preformatted{
	mymet <- data.frame(Tair=20, PAR0=seq(5,1000,length=10), fbeam=0, Ca=400)
}
The names of the variables need to be *exactly* as described above (and are case-sensitive!).

If solar altitude and azimuth are not provided, they will be calculated from the location object.
In the case that \code{fbeam = 0}, though, the solar position has no effect and is ignored, and
not calculated.
}
\value{
An object of class 'ypmet', a list with the following components:
\describe{
\item{dat}{A dataframe with the weather variables (see Details for a description).}
\item{method}{Either 'generated' (weather generator was used), 
or 'input' when user provided \code{metdata}.}
\item{daylength}{in hours}
\item{sunset,sunrise}{in hours}
\item{location}{A Yplant location object (class 'yplocation', see \code{\link{setLocation}}}
}

}
\references{
For fraction diffuse radiation, uses the 'Spitters algorithm':

Spitters, C.J.T., Toussaint, H.A.J.M., Goudriaan, J., 1986, Separating the diffuse and direct
component of global radiation and its implications for modeling canopy photosynthesis. Part I.
Components of incoming radiation, Ag. For. Meteorol., 38:217-229.

}
\author{
Remko Duursma. 
Solar path and diffuse partitioning code borrowed from Maestra (thanks to Belinda Medlyn).
}


\seealso{
\code{\link{setPhy}},\code{\link{setLocation}}
}
% \examples{}
\keyword{misc}
