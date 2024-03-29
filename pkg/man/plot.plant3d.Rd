\name{plot.plant3d}
\alias{plot.plant3d}
\alias{plot.plant3dlist}
\title{Plots a plant in 3D}
\description{
Based on a plant constructed with \code{\link{constructplant}}, plots the plant in 3D using the rgl package. Optionally
adds the convex hull to the plot.
}
\usage{

\method{plot}{plant3d}(x,    	 
	noleaves=FALSE,    
	squarewidth=250,
	addcrownhull=FALSE,
	hullalpha=0.4,
	leaffill=TRUE,
	leafoutline=TRUE,
	stems=TRUE,
	cylinderstems=stems,
	leafcolor="forestgreen",
	markleaves=NULL,
	markcolor="red",
	stemcol="black",
	branchcol="black",
    petiolecol="brown",
	add=FALSE,
	shiftxyz=c(0,0,0),\dots)
\method{plot}{plant3dlist}(x, 
	whichrows=NA, 
	png=FALSE, 
	pngsuff="", 
	keepopen=!png,
	windowrect=c(50,50,800,800), 
	squarewidth=25, 
	skipexisting=FALSE, 
	addfilename=FALSE, \dots)

}
\arguments{
  \item{x}{Object of class \code{plant3d} or \code{plant3dlist}.}
  \item{noleaves}{Logical. If TRUE, leaves are not plotted. }
  \item{squarewidth}{Width of the square to be plotted at the ground surface. }
  \item{addcrownhull}{Logical. If TRUE, a convex hull is added to the plot. }
  \item{hullalpha}{Opacity of the convex hull.}
  \item{leaffill}{Logical. If TRUE, the default, fills in the leaves.}
  \item{leafoutline}{Logical. If TRUE, plots leaf outlines.}
  \item{stems}{Logical. If TRUE, plots the stems (black) and branches .}
  \item{cylinderstems}{Logical. If TRUE, plots the stems as cylinder sections given the diameter in the plant file.}
  \item{leafcolor}{Color of the filled leaves. Can only specify one color.}
  \item{markleaves,markcolor}{A vector of leaf numbers to 'mark', using 'markcolor'.}
  \item{stemcol,branchcol,petiolecol}{Color of the stems, branches, petioles}
  \item{add}{Logical. If TRUE (defaults to FALSE), adds this plant to an existing scene.}
  \item{shiftxyz}{Vector of x,y,z coordinates to shift the plant (see Examples).}
  \item{whichrows}{Which of the plants in the \code{plant3dlist} object to plot. Defaults to all.}
  \item{png}{If TRUE, saves a PNG snapshot of every plant plotted.}
  \item{pngsuff}{A suffix to add to the filename (defaults to pfile.PNG).}
  \item{keepopen}{Whether to keep all windows open. Max number of windows is 10.}
  \item{windowrect}{Size of the \code{rgl} window (see \code{\link{open3d}}). Affects PNG quality.}
  \item{skipexisting}{If PNG=TRUE, whether to skip plants that are already saved to disk.}
  \item{addfilename}{If TRUE, places a label of the pfile on the plot (currently poorly placed).}
  \item{\dots}{Further arguments passed to \code{\link{plot3d}} in the \code{rgl} package.}
}
\details{
 Some of the hard work, figuring out the X,Y,Z coordinates of the leaf tip and leaf sides, is
 directly taken from the original Yplant Delphi code (in the non-visible function \code{madeleafdirection}.
 
 See the function \code{\link{crownhull}} for details of the convex hull algorithm.
}

\author{ Remko Duursma. Robert Pearcy for the Y-plant code. }

\seealso{ \code{\link{crownhull}} }

\examples{
\dontrun{
# Construct a plant like this:
# myplant <- constructplant("somepfile.p","aleaf.l")

# Standard plot (using built-in sugar maple):
plot(sugarmaple)

# Add a convex hull.
plot(sugarmaple, addcrownhull=TRUE)

# Plot two plants in one scene (second plant is moved 750mm in the x-direction)
plot(sugarmaple)
plot(sugarmaple, shiftxyz=c(750,0,0), add=TRUE)

# Mark the first 10 leaves in red (i.e. first 10 leaves in P file):
plot(sugarmaple, markleaves=1:10)

# Mark all leaves on the plant that have a leaf angle > 45.
plot(toona, markleaves=which(toona$leafdata$ang > 45))

# Plot the stems (and branches) only:
plot(pilularis, noleaves=TRUE)

# Plot many plants.
# First organize a 'leafplantkey', a comma-separated file that links each pfile to an lfile.
# See the online manual for an example, or the help file for 'constructplant'.
myplants <- readplantlist(lpk="leafplantkey.txt")
plot(myplants, png=TRUE, addfilename=TRUE)

}

}
\keyword{ misc }
