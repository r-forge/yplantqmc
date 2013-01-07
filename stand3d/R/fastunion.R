
fastunion <- function(mlist){

	# m is a matrix (1st column X, 2nd column Y coordinates of leaf outline).
	tonumeric <- function(m)c(1, length(m[,1]), 0, as.vector(t(m)))

	vec <- tonumeric(mlist[[1]])
	for (p in mlist[-1]) {
            clip <- tonumeric(p)
            vec <- .Call("Rgpc_polygon_clip", vec, clip, 3, PACKAGE = "gpclib")
    }
	return(as(vec, "gpc.poly"))
}