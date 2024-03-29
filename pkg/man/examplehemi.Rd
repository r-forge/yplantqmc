\name{examplehemi}
\alias{smallgap}
\alias{largegap}
\alias{examplehemi}
\docType{data}
\title{
Two example hemiphotos in YplantQMC format
}
\description{
A canopy with a small gap, and a canopy with a large gap. Both are objects of class \code{yphemi},
which is normally constructed with the function \code{\link{setHemi}}.
}
\source{
Thanks to Daniel Falster and Bob Pearcy.
}

\examples{

\dontrun{
# Small gap
plot(smallgap)

# Large gap, with solar path in June in the south of france.
southfrance <- setLocation(44)
juneday <- setMet(southfrance, month=6, day=21)
plot(largegap, juneday)
}
}

\keyword{datasets}
