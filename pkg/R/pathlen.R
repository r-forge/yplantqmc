
pathlen <- function(plant, testleaf=NA, plotit=!is.na(testleaf), add=FALSE, 
	returnwhat=c("totlen","seglist")){
	
	returnwhat <- match.arg(returnwhat)

	
	plotstem <- function(plant,i){

			st <- plant$stems[[i]]
			if(st$diam > 0){
				YplantQMC:::plot3dcylinder(start=st$xyz$from, end=st$xyz$to, radius=st$diam/2)
			}
	}
	plotbranch <- function(plant,i){

			st <- plant$branches[[i]]
			if(st$diam > 0){
				YplantQMC:::plot3dcylinder(start=st$xyz$from, end=st$xyz$to, radius=st$diam/2)
			}
	}
	
	if(!inherits(plant, "plant3d"))stop("Need object of class 'plant3d'")
	p <- plant
	
	nleaves <- p$nleaves
	totlen <- sdlen <- c()
	
	# If 'testleaf' given, test the routine graphically.
	testmode <- if(!is.na(testleaf)) TRUE else FALSE
	theseleaves <- if(!testmode) 1:nleaves else testleaf
	if(testmode & testleaf > nleaves)stop("Pick a leaf that exists.")
	
	# Node numbers, and the nodes they connect to (mothernodes).
	#!!NOTE: first node = 1, not zero as in pfiles.
	leafnodes <- sapply(p$leaves, "[[", "leafnodenumber")

	branchnodes <- sapply(p$branches, "[[", "node")
	branchmothernodes <- sapply(p$branches, "[[", "mothernode")
	
	ste <- p$pdata$ste  # whether attached to a branch or stem node......

	if(testmode && plotit){
			if(!add)plot(p, noleaves=TRUE, cylinderstems=FALSE)
			for(i in theseleaves){
				xyz <- p$leaves[[i]]$XYZ
				plot3d(xyz[,1], xyz[,2], xyz[,3], col="red", 
					type="l", add=T, lwd=2)
			}
	}
	
	for(i in theseleaves){
		
		# List of segment pieces that connect leaf to soil (in that order).
		seglist <- list()
		N <- leafnodes[i]   # node number of this leaf

		# Now find the segments (if ste=2, use the branch segment, otherwise the stem one).
		while(N > 0){

			thisb <- try(which(branchnodes == N))	
			if(inherits(thisb, "try-error"))stop("Fatal error in 'which(branchnodes==N))")
			curste <- ste[thisb]
			N <- branchmothernodes[thisb]
			curn <- branchnodes[thisb]
			
			if(length(curste) == 0 || length(thisb) != 1 ){
				warning("Mother node not in node number; leaf skipped")
				N <- 1  # causes break out of loop.
			} else {

			seglist <- c(seglist, p$stems[thisb])
			if(plotit)plotstem(plant, thisb)

			}
			if(curn == 1)break
			
		}

		
		if(length(seglist) > 0){
		
		# Length of a segment (specifically for p$branches or p$stems).
		seglen <- function(x){
			a <- x$xyz$from
			b <- x$xyz$to
			sqrt((a[1] - b[1])^2 + (a[2] - b[2])^2 + (a[3] - b[3])^2)
		}

		for(k in 1:length(seglist)){
			lens <- c()
			diams <- c()
			lens[k] <- seglen(seglist[[k]])
			diams[k] <- seglist[[k]]$diam
			# --> save as list, to be used already in constructplant?
			# or only when calling the hydraulics routine?
		}
		
		# Total length (used in summary.plant3d)
		totlen[i] <- sum(lens)
		
	} else {
		totlen[i] <- NA
	}
	
	}
	
if(returnwhat=="totlen")return(invisible(data.frame(totlen=totlen[theseleaves])))
if(returnwhat=="seglist")return(invisible(seglist))

}