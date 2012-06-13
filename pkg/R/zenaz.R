
zenaz <- function(year=2012, month=4, day=1, 
	lat= -33.6, long=150.7, 
	tzlong=long, KHRS=24, timeofday=NA, LAT=FALSE){    
  
  
    DATE <- as.Date(ISOdate(year,month,day))
    DJUL <- as.vector(DATE - as.Date("1900-1-1") + 1)
  
	k <- pi/180
	  	 
    ALAT <- lat * k
	
	if(long < 0){
	    long <- 360.0 - long
        tzlong <-  360.0 - tzlong
	}
		
	ALONG <- long * k
    tzlong <- tzlong * k  
	
	if(!LAT){
		# Note: set KHRS to 24.
		TTIMD <- (24/ (2*pi))*(ALONG - tzlong)
	} else {
		TTIMD <- 0
	}

	# Maestra evaluates solar position mid-timestep (see zenaz subroutine).
	if(all(is.na(timeofday))){
		HOUR <- seq(from=24/KHRS/2, by=24/KHRS, length=KHRS)
	    HOURS <- HOUR
	} else {
		HOURS <- timeofday
		KHRS <- length(timeofday)
	}
	
	# init outputs.
	DEC <- EQNTIM <- DAYL <- SUNSET <- ECC <- -999
	EPS <- V <- OMEG <- E <- -999
	
	f <- .Fortran("SUN",
	         as.double(DJUL),
	         as.double(ALAT),
			 as.double(TTIMD),
			 as.double(DEC),
			 as.double(EQNTIM),
			 as.double(DAYL),
			 as.double(SUNSET),
			 as.integer(24),
			 PACKAGE="YplantQMC")
	
	
	DEC <- f[[4]]
	EQNTIM <- ifelse(LAT, 0, f[[5]])  # Set 'equation of time' to zero if input is in LAT.
	DAYL <- f[[6]]
	SUNSET <- f[[7]]

	# cat(DEC,f[[5]],TTIMD,ALAT,"\n")


	
	BEAR <- 0
	ZEN <- as.double(rep(-999, KHRS))
	AZ <- as.double(rep(-999, KHRS))
	
	
	# Slightly adjusted version, can set HOURS directly.
	f2 <- .Fortran("ZENAZ2",
	               as.double(HOURS),
	               as.double(ALAT),
				   as.double(TTIMD),
				   as.double(BEAR),
				   as.double(DEC),
				   as.double(EQNTIM),
				   ZEN,
				   AZ,
				   as.integer(KHRS),
				   PACKAGE="YplantQMC")
				   # browser()
	zenith <- f2[[7]]
    azimuth <-	f2[[8]]
	
	# cat(f2[[2]],f2[[3]],f2[[4]],f2[[5]],f2[[6]],"\n")
	
	dfr <- data.frame(zen=zenith, az=azimuth)
	dfr[dfr$zen > pi/2,] <- NA
	dfr <- dfr / k
	
	# Convert azimuth definition:
	dfr$az <- 180 - dfr$az
	dfr$az[dfr$az < 0 & !is.na(dfr$az)] <- 360 + dfr$az[dfr$az < 0 & !is.na(dfr$az)]
	
	# Solar altitude.
	dfr$alt <- 90 - dfr$zen
	
return(list(hour=HOURS, altitude=dfr$alt, azimuth=dfr$az, 
	daylength=DAYL, sunset = SUNSET, zenrad=(pi/180)*dfr$zen))
}

