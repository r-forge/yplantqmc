readl <- function(lfile=NA){
    options(warn=-1)

	if(is.na(lfile)){
		if(.Platform$OS.type != "windows" || !interactive())
			stop("Please provide a leaf (.L or .LF) file")
		lfile <- file.choose()
	}
	
	r <- readLines(lfile, warn=FALSE)
	r <- r[r != ""]
	
    npoints <- as.numeric(r[2])
    #dfr <- read.table(lfile, skip=2, nrow=npoints)
	writeLines(r[3:(3+npoints-1)], "tmp.txt")
    dfr <- try(read.table("tmp.txt"), silent=TRUE)
	
	if(inherits(dfr, "try-error") && length(grep("did not have 2",dfr)) > 0){
		k <- 1
		while(inherits(dfr, "try-error")){
			dfr <- try(read.table("tmp.txt", nrows=npoints-k),silent=TRUE)
			k <- k + 1
		}
		warning("Number of points in",lfile,"does not match - read in first",nrow(dfr),"points.")
	}
	unlink("tmp.txt")
	
	l <- list()
	if(!sum(dfr[nrow(dfr),] == 0))dfr <- rbind(dfr, c(0,0))
	l$XYZ <- matrix(cbind(dfr[,1],dfr[,2],rep(0,nrow(dfr))), ncol=3)
	colnames(l$XYZ) <- c("X","Y","Z")
	
	
	# Is there a point along the midrib?
	p1 <- 1	# First point always on midrib by convention (0,0).
	i <- 2:(nrow(l$XYZ)-1)  # do not look in first and last points (generally 0,0 both)
	zerox <- l$XYZ[i,1] == 0 & l$XYZ[i,2] > 0
	hasmidrib <- any(zerox)
	if(!hasmidrib)stop("Leaf needs a point on the midrib where X=0")
	# Sometimes more than one x=0 point ('ossaea problem') : pick the one with max Y value.
	p2 <- which(zerox)[which.max(l$XYZ[zerox,2])] + 1 # Add one : we deleted first point!
	l$midribpoints <- c(p1,p2)
	
	if(length(grep("\\.lf$",lfile)) > 0)
		npars <- 9
	else
		npars <- 6
	
	l$leafpars <- as.numeric(r[(length(r)-npars+1):length(r)])
	if(length(l$leafpars) == 6)
		names(l$leafpars) <- c("Amax","Rd","QY","shape","absorp","reflec")

	
	# If missing leaf pars, put NAs at the end (typical?)
	LP <- na.omit(l$leafpars)
	if(length(LP) < length(l$leafpars))
		l$leafpars <- c(LP, rep(NA, length(l$leafpars) - length(LP)))
	
	l$leaftype <- as.numeric(trim(gsub("leaf","",tolower(r[1]))))
	l$nleaftypes <- length(grep("leaf", readLines(lfile, warn=FALSE), ignore.case=TRUE))
		
	# Get leaf area factor
	leafpoly <- as(cbind(l$XYZ[,1],l$XYZ[,2]),"gpc.poly")
	leafarea <- area.poly(leafpoly)
	#len <- max(l$XYZ[,2]) - min(l$XYZ[,2])
	l$midriblen <- unname(abs(l$XYZ[l$midribpoints[2],2] - l$XYZ[l$midribpoints[1],2] ))
	l$leafshape <- leafarea / l$midriblen^2
	
	options(warn=0)
    class(l) <- "leaffile"
    return(l)
}