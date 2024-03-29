\name{randomplant}
\alias{randomplant}
\title{Generate a plant with randomly distributed leaves}
\description{
Generates a plant crown with specified total leaf area or number of leaves, in one of six crown shapes. These include cone, paraboloid, cylinder, and others. Leaves are randomly distributed in the crown shapes.
}
\usage{
randomplant(nleaves = 500, 
radius = 250, 
height = 500, 
shape = c("BOX", "CONE", "HELIP", "PARA", "ELIP", "CYL"), 
crownbase = 0, 
LAD = angledist("ellipsoid", 1), 
leaflen = 30, 
LA = NULL,  
lfile = NULL, 
writefile = FALSE, 
quiet = FALSE)
}
\arguments{
  \item{nleaves}{Number of leaves, can be left unspecified if leaf area (LA) is given.}
  \item{radius}{Crown radius (mm).}
  \item{height}{Crown height (mm).}
  \item{shape}{One of six pre-specified crown shapes. See Details.}
  \item{crownbase}{Height to crownbase (mm).}
  \item{LAD}{Leaf angle distribution.}
  \item{leaflen}{The leaf length (constant value for all leaves).}
  \item{LA}{Total leaf area, optional (m2)}
  \item{lfile}{Name of the leaf file (see \code{\link{readl}}), if left unspecified a triangle is used.}
  \item{writefile}{Logical. If TRUE, writes a Q file to disk.}
  \item{quiet}{Logical. If FALSE, no messages are printed to the console.}
}
\details{

Six crown shapes are implemented, use one of the following abbreviations for the argument \code{crownshape}:
\describe{
\item{BOX}{A box shape; radius in X and Y directions assumed to be the same.}
\item{CONE}{A cone shape.}
\item{HELIP}{A half ellipsoid (the top half of a full ellipsoid).}
\item{PARA}{A paraboloid.}
\item{ELIP}{A full ellipsoid.}
\item{CYL}{Cylinder shape.}
}

The leaf angle distribution (\code{LAD}) must be specified using the \code{\link{angledist}} function in the \code{LeafAngle} package. To assign a constant leaf angle of 45 degrees, use this command:
\preformatted{
LAD = angledist("const", 45)
}
To assign a spherical LAD, use this command:
\preformatted{
LAD = angledist("spherical")
}

}

\value{
Produces a plant in the Q file format, see \code{\link{constructplant}} for details. 
If \code{writefile=TRUE}, the Q file is written to the current working directory. Otherwise,
returns a dataframe that can be used directly in \code{constructplant}, see Details.
}
\author{
Remko Duursma. Uses code from Belinda Medlyn (SURFACE subroutine in MAESTRA).
}
\note{
Currently only random distributions are supported. I have at this time no idea how to produce
clumped or uniform distributions in 3D (if you do, let me know).
}

\seealso{
\code{\link{constructplant}}
}
\examples{
\dontrun{
# An ellipsoid shape crown, write a Q file to disk.
# Specify approximate total leaf area.
randomplant(radius=400, height=2000, shape="ELIP",
leaflen=40, LA=1.5, writefile=TRUE)

# Embed the function in a call to 'constructplant', giving a plant in the 
# YplantQMC format.
# Because no leaf file is specified, it uses a built-in triangle-shaped leaf.
 coneplant <- constructplant(randomplant(radius=250, height=500, shape="CONE",
 leaflen=25, LA=1))

}
}
\keyword{misc}
