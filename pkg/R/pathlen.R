

pathlen <- function(plant, testleaf=NA, add=FALSE, plotcols=c("orange","green")){


# plant <- maple
# testleaf <- NA
# add <- F

	if(!inherits(plant, "plant3d"))stop("Need object of class 'plant3d'")
	p <- plant
	
	nleaves <- p$nleaves
	if(length(plotcols) == 1)plotcols <- c(plotcols,plotcols)
	totlen <- sdlen <- c()
	
	# If 'testleaf' given, test the routine graphically.
	testmode <- if(!is.na(testleaf)) TRUE else FALSE
	theseleaves <- if(!testmode) 1:nleaves else testleaf
	if(testmode & testleaf > nleaves)stop("Pick a leaf that exists.")
	
	# Node numbers, and the nodes they connect to (mothernodes).
	#!!NOTE: this is yptools node numbering (first node = 1, not zero as in pfiles).
	leafnodes <- sapply(p$leaves, "[[", "leafnodenumber")

	branchnodes <- sapply(p$branches, "[[", "node")
	branchmothernodes <- sapply(p$branches, "[[", "mothernode")
	ste <- p$pdata$ste  # whether attached to a branch or stem node......

	for(i in theseleaves){
		
		# List of segment pieces that connect leaf to soil (in that order).
		seglist <- list()
		N <- leafnodes[i]   # node number of this leaf
		#ROW <- which(p$pdata$N == N)   # actual row in the pdata file
		
		# First segment ('walking' from leaf to soil):
		# if ste=2, this is a branch segment (which comes off the leaf, not to it).
		# if(is.na(ste[N]))browser()
		# if(ste[N] == 1){
			# seglist[[1]] <- p$stems[N][[1]]
			# seglist[[1]]$type <- "stem"
		# }
		
		# Now find the segments (if ste=2, use the branch segment, otherwise the stem one).
		while(N > 0){

			thisb <- try(which(branchnodes == N))	
			if(inherits(thisb, "try-error"))stop("Fatal error in 'which(branchnodes==N))")
			curste <- ste[thisb]
			N <- branchmothernodes[thisb]
			
			
			if(length(curste) == 0 || length(thisb) != 1 ){
				warning("Mother node not in node number; leaf skipped")
				N <- 1  # causes break out of loop.
			} else {
			
			if(curste == 1){
			 
				seglist <- c(seglist, p$stems[thisb])
				seglist[[length(seglist)]]$type <- "stem"
			} 
			if(curste == 2){
				seglist <- c(seglist, p$branches[thisb])
				seglist[[length(seglist)]]$type <- "branch"
			}
			}
			if(N == 1)break
			
		}
		
		# seglist <- c(seglist, p$stems[1])
		# seglist[[length(seglist)]]$type <- "stem"
		# seglist <- seglist[-1]
		
		
		# browser()
		# # plot leaf, test nodelink
		
		if(length(seglist) > 0){
		
		if(testmode){
			if(!add)plot(p, noleaves=TRUE)
			xyz <- p$leaves[[i]]$XYZ
			plot3d(xyz[,1], xyz[,2], xyz[,3], col="red", 
				type="l", add=T, lwd=3)
		}
			
		# Length of a segment (specifically for p$branches or p$stems).
		seglen <- function(x){
			a <- x$xyz$from
			b <- x$xyz$to
			sqrt((a[1] - b[1])^2 + (a[2] - b[2])^2 + (a[3] - b[3])^2)
		}

		lens <- c()
		for(k in 1:length(seglist)){
			lens[k] <- seglen(seglist[[k]])
			
				if(testmode){
					seg <- seglist[[k]]
					M <- rbind(seg$xyz$from, seg$xyz$to)
		
					plotcol <- if(seg$type == "branch") plotcols[2] else plotcols[1]
					lines3d(M, col = plotcol, lwd=3)
				}
		}
		
		# Total length.
		totlen[i] <- sum(lens)
		# segment diameter go here....
	} else {
		totlen[i] <- NA
	}
	
	
	}

	
if(!testmode)return(data.frame(totlen=totlen))
}
		

# pathlen(p)


# plot(p, noleaves=TRUE)
# for(i in 1:p$nleaves){
	# pathlen(p,i,add=T)
	# Sys.sleep(0.5)
# }




