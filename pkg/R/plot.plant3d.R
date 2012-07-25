plot.plant3d <- function(x,
					  #zheight=0.1,     	 # minimum height of plotting canvas, in m.
					  noleaves=FALSE,    # suppress leaf plotting.
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
					  add=FALSE,shiftxyz=c(0,0,0),
					  ...
					  ){  

	plant <- x
	if(noleaves){
		leaffill <- FALSE
		leafoutline <- FALSE
	}
	
	# r <- require(rgl, quietly=TRUE)				  
	# if(!r)stop("Need to install package 'rgl'")
	
	inputformat <- plant$inputformat
	if(stems && inputformat == "Q")stems <- FALSE  # Q format does not support stems.

	# Too bad .... no longer works.
	# # Leaf colors
	# if(leafcolor == "autumn")leafcolor <- "fall"
	# if(leafcolor == "fall"){
		# leafcolor <- c("#FED976","#FEB24C","#FD8D3C","#FC4E2A","#E31A1C","#BD0026")
		# #brewer.pal(9, "YlOrRd")[2:8]
	# }
	
	shiftm <- function(m,v){
		n <- nrow(m)
		plusm <- matrix(rep(v,each=n),nrow=n)
		m + plusm
	}
	
	# Set up the canvas for plotting:
	# zheight <- 1000 * zheight 
	# xradius <- zheight/2
	# yradius <- zheight/2
	# x <- c(-xradius, xradius)
	# y <- c(-yradius, yradius)
	# z <- matrix(nrow=length(x),ncol=length(x))
	# z[,] <- 0
	
	# Plot grey square on the ground:
	if(!add)open3d()
	s <- squarewidth
	M <- matrix(c(-s,-s,0,
				  s,-s,0,
				  s,s,0,
				  -s,s,0,
				  -s,-s,0), ncol=3, byrow=TRUE)
	if(s > 0)lines3d(M, col="darkgrey", add=add)

	# Add crownhull, maybe
	if(addcrownhull){
        g <- require(geometry)
        if(g)
			ch <- crownhull(plant, alpha=hullalpha)
        else
            warning("Cannot add convex hull. Install package 'geometry'.\n")
    }
	
	
	# Plot stems, branches, and petioles.
	if(plant$inputformat == "P"){
	
	Nnodes <- nrow(plant$pdata)
	
	# Stem segments.
	if(stems){
		Ms <- list()
		for(i in 1:Nnodes){
			Ms[[i]] <- rbind(rbind(plant$stems[[i]]$xyz$from, plant$stems[[i]]$xyz$to))
		}
		Ms <- do.call("rbind", Ms)
		Ms <- shiftm(Ms,shiftxyz)
		segments3d(Ms, col=stemcol)
			
		# Branches.
		Ms <- list()
		for(i in 1:Nnodes){
			Ms[[i]] <- rbind(rbind(plant$branches[[i]]$xyz$from, plant$branches[[i]]$xyz$to))
		}
		Ms <- do.call("rbind", Ms)
		Ms <- shiftm(Ms,shiftxyz)
		segments3d(Ms, col=branchcol)
	}
	# Add cylinder sections.
	if(stems & cylinderstems)plotstemsections(plant)

	
	# Petioles.
	Ms <- list()
	for(i in 1:Nnodes){
		Ms[[i]] <- rbind(rbind(plant$petioles[[i]]$xyz$from, plant$petioles[[i]]$xyz$to))
	}
	Ms <- do.call("rbind", Ms)
	Ms <- shiftm(Ms,shiftxyz)
	segments3d(Ms, col=petiolecol)
	}
	
	if(leafoutline){
		LM <- list()
		np <- nrow(plant$leaves[[1]]$XYZ)
		for(i in 1:plant$nleaves){
			LM[[i]] <- plant$leaves[[i]]$XYZ
			LM[[i]] <- rbind(LM[[i]], LM[[i]][1,])  # duplicate first point to complete polygon.
			if(np %% 2 > 0)LM[[i]] <- rbind(LM[[i]], LM[[i]][np,])
			nr <- nrow(LM[[i]])
			LM[[i]] <- LM[[i]][rep(1:nr,each=2),]
			LM[[i]] <- LM[[i]][-c(1,nr*2),]
		}
		LM <- do.call("rbind", LM)
		LM <- shiftm(LM,shiftxyz)
		segments3d(LM, col="black")
	}
	
	# Fill in leaves.
	r <- require(gpclib)
	if(!r & leaffill){
		leaffill <- FALSE
		warning("Could not fill leaves. Install package 'gpclib'.")
	}
	
	if(leaffill & !noleaves){
		sms <- list()
		tri <- list()
		
		fillLeaves <- function(indices, leafcol){
			for(i in indices){
				
				m <- plant$leaves[[i]]$XYZ
				m <- shiftm(m,shiftxyz)
				x <- m[,1]
				y <- m[,2]
				z <- m[,3]
				
				triangles <- triangulate(as(cbind(x,y), "gpc.poly"))
				options(warn=-1)  # if leaf hanging straight down, lm will give warning.
				sms[[i]] <- lm(z ~ x + y)
				# Leaf is hanging straight down; can't predict z values from x,y values,
				# instead predict y from x an z values.
				if(is.na(coef(sms[[i]])[[3]])){
					triangles <- triangulate(as(cbind(x,z), "gpc.poly"))
					sms[[i]] <- lm(y ~ x + z)	
					yfit <- predict(sms[[i]], newdata=data.frame(x=triangles[,1], z=triangles[,2]))
					material3d(col=leafcol, shininesss=0, lit=TRUE, specular=leafcol)
					tri[[i]] <- cbind(triangles[,1], yfit, triangles[,2])
				# Normal situation (z fit from x,y).
				} else {
					zfit <- predict(sms[[i]], newdata=data.frame(x=triangles[,1], y=triangles[,2]))
					material3d(col=leafcol, shininesss=0, lit=TRUE, specular=leafcol)
					tri[[i]] <- cbind(triangles, zfit)
				}
			}
			tri<- do.call("rbind", tri)
			triangles3d(tri, col=leafcol)
		}
		
		if(is.null(markleaves))
			fillLeaves(1:plant$nleaves, leafcolor)
		else {
			if(max(markleaves) > plant$nleaves)
				stop("Max. markleaves > number of leaves on plant.")
			otherleaves <- setdiff(1:plant$nleaves, markleaves)
			fillLeaves(otherleaves, leafcolor)
			fillLeaves(markleaves, markcolor)
		}
		
	}

options(warn=0)
}

# loadplot <- function()source("C:\\remko\\SYDNEY\\MODELS\\R Packages\\YPLANTER2\\R\\plot.plant3d.R")
# loadorig <- function()source("C:\\remko\\SYDNEY\\MODELS\\R Packages\\YPLANTER\\R\\plotplant3d.R")