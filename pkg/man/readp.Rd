\name{readp}
\alias{readp}
\title{ Reads the data from a .p file.}
\description{
	Reads all data of the plant input file (\code{.p}) into a dataframe.
}
\usage{
readp(pfile=NA)
}
\arguments{
  \item{pfile}{Name of the p file (a character string).}
}
\value{
Dataframe. The column names correspond to the labels in the .p file, but duplicates 
have a numeric subscript;

\describe{
\item{N}{ - node number}
\item{MN}{ - mother node}
\item{ste}{ - stem (1) or branch (2)}
\item{Lt}{ - leaf type}
\item{Az, Az.1, Az.2}{ - Azimuth of stem, branch, petiole and leaf.}
\item{An, An.1, An.2}{ - Angle of stem, branch, petiole and leaf.}
\item{L, L.1, L.2, L.3}{ - Length of stem, branch, petiole and leaf.}
}
More detail on the leaf angles:

\describe{
\item{Az.3}{ - Leaf azimuth (azimuth angle of the normal to the leaf surface).}
\item{Or}{ - Leaf orientation (azimuth angle of the midrib).}
\item{An.3}{ - Leaf angle (angle between normal to the leaf surface and the z-axis).}

}


}

\author{ Remko Duursma }
\keyword{ misc }
