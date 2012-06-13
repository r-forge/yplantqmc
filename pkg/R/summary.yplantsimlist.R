summary.yplantsimlist <- function(object, ...){

	x <- object
	# Sum these variables.
	vars <- c("PARleaf","PARdir","PARdiff","A","A0","E")
	vars <- intersect(vars, names(x[[1]]$psrdata))
	
	sums <- lapply(x, function(z){
		psr <- z$psrdata
		v <- colSums(psr[,vars] * psr$timestep * psr$LAplant) *10^-6
		names(v) <- paste0("tot",vars)
		return(v)
	})
	sumdfr <- as.data.frame(do.call("rbind",sums))
	pfdfr <- data.frame(pfile=sapply(x, function(x)x$plant$pfile),
	                    lfile=sapply(x, function(x)x$plant$lfile))
	sumdfr <- cbind(pfdfr, sumdfr)
	
return(sumdfr)
}