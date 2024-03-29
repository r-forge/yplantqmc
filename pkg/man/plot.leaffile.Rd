\name{plot.leaffile}
\alias{plot.leaffile}
\alias{plot3d.leaffile}
\title{
Plots a Yplant leaf file (a file with extension \code{.l} or \code{.lf}) in 2D.
}
\description{
Produces a plot of a Yplant leaf file, read in using \code{\link{readl}}.
}
\usage{
\method{plot}{leaffile}(x, nleaf=1, edgepoints = TRUE, edgecex = 0.8, \dots)
}
\arguments{
  \item{x}{Object of class 'leaffile'.}
  \item{nleaf}{Which leaf to plot in the leaf file (if more than one leaf available in the file).}
  \item{edgepoints}{Logical. If TRUE, plots dots on the leaf edge coordinates.}
  \item{edgecex}{If edgepoint=TRUE, cex (i.e. size) of the leaf edge dots.}
  \item{\dots}{
Further parameters passed to \code{plot.default}.
}
}
\author{
Remko Duursma
}
\seealso{
\code{\link{readl}}
}
\examples{

\dontrun{

# Read and plot a leaf in one go, select a leaf from a menu.
plot(readl())

# Make a pdf of all leaf files in the current working directory:
leaffiles <- list.files(pattern="\\\.l$", ignore.case=TRUE)
pdf("Leaf files.pdf", onefile=TRUE)
for(i in 1:length(leaffiles))plot(readl(leaffiles[i]))
dev.off()


}

}
\keyword{misc}
