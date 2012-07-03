summary.yplantsimlist <- function(object, writefile=FALSE, ...){

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

	if(writefile){
	
		filen <- paste0("YplantDayBatchResults-",as.Date(Sys.time()),".txt")
		unlink(filen)
	
		r <- c()
		r[1] <- "pfile - plant file used to construct plant."
		r[2] <- "lfile - leaf file used to construct plant."
		r[3] <- "PARleaftot - total PAR absorption (mol day-1)" 
		r[4] <- "PARdfirtot - total direct PAR absorption (mol day-1)"
		r[5] <- "PARdifftot - total diffuse PAR absorption (mol day-1)"
		r[6] <- "Atot - total CO2 assimilation (mol day-1)"
		r[7] <- "A0tot - total CO2 assimilation by unshaded horizontal leaf (mol day-1)"
		r[8] <- "Etot - total transpiration (mmol day-1)"
		
		r <- c("YplantDay Bath Simulation Result - produced with YplantQMC\n\n",r)
		r <- c(r, "\n\n")

		writeLines(r, filen)
		
		options(warn=-1)
		write.table(sumdfr, filen, append=TRUE, sep="\t", quote=FALSE, row.names=FALSE)
		options(warn=0)

		message("\nSimulation results (plant totals) written to file:\n ", filen)
		return(invisible(sumdfr))
	} else {
		return(sumdfr)
	}
}