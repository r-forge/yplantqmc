leafdispersion <- function(plant, kneighbors=5, nreplicate=10, nleaves=NA, crownvol=NA){
	# Nearest neighbors (n=5) : Ok is observed mean distance to 5 nearest neighbor leaf midpoints,
	# Ek is expected (based on numerical simulations).
	
	if(class(plant) != "plant3d")stop("Need object of class 'plant3d'")
	if(any(sapply(plant$leaves, function(x)any(is.na(x$XYZ)))))return(NA)
		
	if(is.na(nleaves)){
		nleaves <- plant$nleaves
	}

	if(nleaves < min(kneighbors))return(NA)

	if(is.na(crownvol)){
		ch <- crownhull(plant, plotit=FALSE)
		crownvol <- ch$crownvolume * 1E-09  # m3
	}
	
	lad <- nleaves / crownvol  # m-3
	Ek_noedge <- lad^(-1/3) * (0.5 -0.08784436 + 0.21374167 * sqrt(5))
		
	if(length(kneighbors) > 1)
		kneighbors <- kneighbors[1]:min(kneighbors[length(kneighbors)],nleaves-1)
	
	l <- list()
	l$Ok <- Kn(plant,kneighbors)
	
	Ek_edge <- replicate(nreplicate, KE(lad, nleaves, kneighbors))
	
	l$Ek_noedge <- Ek_noedge
	
	if(length(kneighbors) == 1){
		l$Ek_edge <- mean(Ek_edge)
		l$Ek_edgeSD <- sd(Ek_edge)
	} else {
		l$Ek_edge <- apply(Ek_edge,1,mean)
		l$Ek_edgeSD <- apply(Ek_edge,1,sd)
	}
	
	l$kneighbors <- kneighbors
	l$disp_edge <- l$Ok / l$Ek_edge
	l$disp_noedge <- l$Ok / l$Ek_noedge
	return(l)
}

