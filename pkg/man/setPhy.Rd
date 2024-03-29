\name{setPhy}
\alias{setPhy}
\alias{includePhy.plant3d}
\alias{includePhy}
\alias{includePhy.plant3dlist}
\title{
Make a leaf physiology object
}
\description{
Constructs an object of class 'ypphy', which contains a function that calculates 
leaf photosynthesis and transpiration (and possibly other variables), from weather data
(air temperature, humidity, etc.), and absorbed PAR.

Users can write their own leaf gas exchange functions to be included in a physiology object,
or use one of two built in functions: the Farquhar model (see \code{\link{Farquhar}}), or a simple
non-rectangular light response curve (see \code{\link{lightresponse}}).
}
\usage{
setPhy(leafmodel, leafpars = list())

\method{includePhy}{plant3d}(object, ...)
\method{includePhy}{plant3dlist}(object, phydfr, ...)
}
\arguments{
  \item{leafmodel}{Name of the leaf gas exchange model ('Farquhar', or 'lightresponse', or user-defined).
}
  \item{leafpars}{List of parameters that are passed to the leafmodel 
  (and should be arguments of that function).}
\item{object}{A 'plant3d' object (see \code{\link{constructplant}}), or a 'plant3dlist' object.}
\item{phydfr}{A dataframe with leaf parameters, for batch analyses (see Details).}
\item{\dots}{Further arguments passed to 'setPhy'} 
}
\details{
A typical usage of \code{setPhy} is :
\preformatted{
	eucphy <- setPhy("Farquhar", leafpars=list(Vcmax=80, Jmax=140, Rd=1, G1=7))
}
This object may be used when running Yplant directly (see \code{\link{YplantDay}},
or it may be saved into a plant object (which makes it somewhat easier to organize,
especially for batch processing). This is achieved with the \code{includePhy} function:
\preformatted{
	myplant <- includePhy(myplant, eucphy)
}
To find out whether a plant has a physiology object saved in it, simply type:
\preformatted{
	myplant$phy
}
If there is a physiology object, it will print a summary of its contents, otherwise it is \code{NULL}.

For batch analyses, \code{includePhy} can set the leaf parameters for a list of plants (as constructed with 
\code{\link{readplantlist}}). To do this, construct a dataframe where each row corresponds to a set of parameters
for a plant, and the columns include \code{pfile} (required, to match the parameters to the plants in the list),
\code{leafmodel} (required, the name of the leaf model), and further any parameters that can be accepted by the leafmodel 
(for example, \code{Vcmax} or \code{Amax}, and so on). Then use this command,
\preformatted{
	myplantlist <- includePhy(myplantlist, leafpardataframe)
}

}

\value{
An object of class 'ypphy'.
}

\author{
Remko Duursma
}


\seealso{
\code{\link{Farquhar}},\code{\link{lightresponse}},\code{\link{makereport}}
}
% \examples{}
\keyword{misc}

