
zenaz <- function(year=2012, month=4, day=1, 
  lat= -33.6, long=150.7, 
	tzlong=long, KHRS=24, timeofday=NA, LAT=FALSE){    

  # Private function SUN (only gets declination, time corrections.)
      SUN <- function(DOY, ALAT, TTIMD){
      
      # Compatability issue
      KHRS <- 24
      HHRS <- 12
      
      # Private functions
      ECCENT <- function(T) 0.01675104 - (4.08E-5 + 1.26E-7*T)*T
      
      
      ANOM <- function(T,D){
        
        anom <- -1.52417 + (1.5E-4 + 3.0E-6*T)*T^2
        anom <- anom + 0.98560*D
        if(anom > 360)
          anom <- anom - 360.0*floor(anom/360.0)
        
        return(anom * pi/180)
      }
      
      
      EPSIL <- function(T)(23.452294- (1.30125E-2+ (1.64E-6-5.03E-7*T)*T)*T)*pi/180
      
      
      OMEGA <- function(T,D)(281.22083+ (4.53E-4+3.0E-6*T)*T*T+4.70684E-5*D)*pi/180
      
      ALUNAR <- function(T,D) (259.1833+ (2.078E-3+2.0E-6*T)*T*T-0.05295*D)*pi/180
      # End private functions.
      
      T <- DOY/36525
      # COMPUTE SOLAR ORBIT
      ECC <- ECCENT(T)
      
      RM <- ANOM(T,DOY)
      E <- RM
      for(IM in 1:3){
        E <- E + (RM- (E-ECC*sin(E)))/ (1-ECC*cos(E))
      }
      
      V <- 2.0*atan(sqrt((1+ECC)/ (1-ECC))*tan(0.5*E))
      
      if(V < 0) V <- V + 2*pi
      R <- 1 - ECC*cos(E)
      
      EPS <- EPSIL(T)
      OMEG <- OMEGA(T,DOY)
      
      # COMPUTE NUTATION TERMS
      LUNLON <- ALUNAR(T,DOY)
      NUTOBL <- (2.5583E-3+2.5E-7*T)*cos(LUNLON)*pi/180
      EPS <- EPS + NUTOBL
      NUTLON <- - (4.7872E-3+4.7222E-6*T)*sin(LUNLON)*pi/180
      
      # COMPUTE SOLAR DECLINATION
      DEC <- asin(sin(EPS)*sin(V+OMEG))
      
      # COMPUTE EQN OF TIME
      MLON <- OMEG + RM
      if(MLON < 0)MLON <- MLON + 2*pi
      if(MLON > 2*pi)MLON <- MLON - 2*pi*floor(MLON/(2*pi))    
      Y <- (tan(EPS/2))^2
      Y <- (1-Y)/ (1+Y)
      
      SL <- OMEG + NUTLON + V
      if(SL < 0) SL <- SL + 2*pi
      if(SL > 2*pi)SL <- SL - 2*pi*floor(SL/(2*pi))
      AO <- atan(Y*tan(SL))
      
      EQNTIM <- AO - MLON
      EQNTIM <- EQNTIM - pi*floor(EQNTIM/pi)
      if(abs(EQNTIM) > 0.9*pi) EQNTIM <- EQNTIM - pi*EQNTIM/abs(EQNTIM)
      AO <- EQNTIM + MLON
      if(AO > 2*pi) AO <- AO - 2*pi*floor(AO/(2*pi))
      # DAY LENGTH
      MUM <- cos(ALAT-DEC)
      MUN <- -cos(ALAT+DEC)
      MUA <- 0.0
      REFAC <- 0.0
      UMN <- -MUM*MUN
      if(UMN > 0) REFAC = 0.05556/sqrt(UMN)
      if(MUN > MUA) MUA <- MUN    
      if(MUM > MUA){
          FRACSU <- sqrt((MUA-MUN)/ (MUM-MUA))
          FRACSU <- 1.0 - 2.0*atan(FRACSU)/pi
          SUNSET <- HHRS*FRACSU
          SUNRIS <- SUNSET
          SUNSET <- SUNSET + REFAC + EQNTIM*HHRS/pi
          SUNRIS <- SUNRIS + REFAC - EQNTIM*HHRS/pi
          SUNSET <- SUNSET + HHRS + TTIMD
          SUNRIS <- HHRS - SUNRIS + TTIMD
          EQNTIM <- EQNTIM*HHRS/pi
          DAYL <- SUNSET - SUNRIS
      }
    
        
      return(list(DEC=DEC,EQNTIM=EQNTIM,DAYL=DAYL,SUNSET=SUNSET))
      
    }
      
  
  # continue zenaz.
    DATE <- as.Date(ISOdate(year,month,day))
    DJUL <- as.vector(DATE - as.Date("1900-1-1") + 1)
  
	k <- pi/180
	
  # latitude in radians
  ALAT <- lat * k
	
	if(long < 0){
	    long <- 360.0 - long
        tzlong <-  360.0 - tzlong
	}
		
	ALONG <- long * k
  tzlong <- tzlong * k  
	
	if(!LAT){
		TTIMD <- (24/ (2*pi))*(ALONG - tzlong)
	} else {
		TTIMD <- 0
	}

	# Maestra evaluates solar position mid-timestep (see zenaz subroutine).
	if(all(is.na(timeofday))){
		HOURS <- seq(from=24/KHRS/2, by=24/KHRS, length=KHRS)
	} else {
		HOURS <- timeofday
    if(length(HOURS)==1)stop("Must provide more than one time of day.")
		KHRS <- length(timeofday)
	}
	
	SUNcall <- SUN(DJUL, ALAT, TTIMD)
  
  DEC <- SUNcall$DEC  
  EQNTIM <- SUNcall$EQNTIM
  DAYL <- SUNcall$DAYL
  SUNSET <- SUNcall$SUNSET
  
    # To match old definition, with 24 hours in a day.
	  solarnoon <- 12
	  
    ZEN <- c()
    AZ <- c()  
  
    # get solar zenith and azimuth angles.
    for(i in 1:KHRS){

        # hour angle
        SolarTime <- HOURS[i] - TTIMD - EQNTIM
        HI <- (pi/180) * (12 - SolarTime)*15
#         HI <- (HOURS[i]-TTIMD-EQNTIM-solarnoon)*pi/solarnoon

        # zenith angle 
        ZEN[i] <- acos(sin(ALAT)*sin(DEC) + 
          cos(ALAT)*cos(DEC)*cos(HI))
        
        # Cosine of the azimuth angle (Iqbal)
        H <- pi/2 - ZEN[i]
        COSAZ <- (sin(H)*sin(ALAT)- sin(DEC)) /
                     (cos(H)*cos(ALAT))
        if (COSAZ >  1.0)
          COSAZ <- 1.0
        if (COSAZ < -1.0) 
          COSAZ <- -1.0
        
        AZ[i] <- acos(COSAZ)
        
        if(SolarTime > 12)AZ[i] <- -AZ[i]
    }
      
      
      # What MAESTRA uses. Note Iqbal warns against this one (p.15)
#         # sine of the azimuth angle
#         SINAZ <- cos(DEC)*sin(HI)/sin(ZEN[i])
#         
#         if (SINAZ >  1.0)
#           SINAZ <- 1.0
#         if (SINAZ < -1.0) 
#           SINAZ <- -1.0
#         
#         AZ[i] <- asin(SINAZ)
    
# 
#     browser()  
#       
#     for(i in 1:(KHRS-1)){
# 
#         AI <- AZ[i]
#         AIP1 <- AZ[i+1]
#         if (AI >  0.0)
#           if (AIP1 < AI) AZ[i+1] <- pi- AIP1
#         else
#           if(AI > AIP1) AZ[i] <- -pi - AI   
#         
#         if (ALAT < 0)
#          AZ[i] <- AZ[i] + pi + BEAR
#         else
#          AZ[i] <- -AZ[i] + BEAR 
#       
#     }

#     browser()
      
  dfr <- data.frame(zen=ZEN, az= pi - AZ)
	dfr[dfr$zen > pi/2,] <- NA
	dfr <- dfr / k
      
	# Solar altitude.
	dfr$alt <- 90 - dfr$zen
	
  return(list(hour=HOURS, altitude=dfr$alt, azimuth=dfr$az, 
	daylength=DAYL, sunset = SUNSET, zenrad=k*dfr$zen))

}


# Old version uses Fortran stuff.
#   
# zenaz <- function(year=2012, month=4, day=1, 
#   lat= -33.6, long=150.7, 
# 	tzlong=long, KHRS=24, timeofday=NA, LAT=FALSE){    
#   
#   
#     DATE <- as.Date(ISOdate(year,month,day))
#     DJUL <- as.vector(DATE - as.Date("1900-1-1") + 1)
#   
# 	k <- pi/180
# 	  	 
#     ALAT <- lat * k
# 	
# 	if(long < 0){
# 	    long <- 360.0 - long
#         tzlong <-  360.0 - tzlong
# 	}
# 		
# 	ALONG <- long * k
#     tzlong <- tzlong * k  
# 	
# 	if(!LAT){
# 		# Note: set KHRS to 24.
# 		TTIMD <- (24/ (2*pi))*(ALONG - tzlong)
# 	} else {
# 		TTIMD <- 0
# 	}
# 
# 	# Maestra evaluates solar position mid-timestep (see zenaz subroutine).
# 	if(all(is.na(timeofday))){
# 		HOUR <- seq(from=24/KHRS/2, by=24/KHRS, length=KHRS)
# 	    HOURS <- HOUR
# 	} else {
# 		HOURS <- timeofday
# 		KHRS <- length(timeofday)
# 	}
# 	
# 	# init outputs.
# 	DEC <- EQNTIM <- DAYL <- SUNSET <- ECC <- -999
# 	EPS <- V <- OMEG <- E <- -999
# 	
# 	f <- .Fortran("SUN",
# 	         as.double(DJUL),
# 	         as.double(ALAT),
# 			 as.double(TTIMD),
# 			 as.double(DEC),
# 			 as.double(EQNTIM),
# 			 as.double(DAYL),
# 			 as.double(SUNSET),
# 			 as.integer(24),
# 			 PACKAGE="YplantQMC")
# 	
# 	
# 	DEC <- f[[4]]
# 	EQNTIM <- ifelse(LAT, 0, f[[5]])  # Set 'equation of time' to zero if input is in LAT.
# 	DAYL <- f[[6]]
# 	SUNSET <- f[[7]]
# 
# 	# cat(DEC,f[[5]],TTIMD,ALAT,"\n")
# 
# 
# 	
# 	BEAR <- 0
# 	ZEN <- as.double(rep(-999, KHRS))
# 	AZ <- as.double(rep(-999, KHRS))
# 	
# 	
# 	# Slightly adjusted version, can set HOURS directly.
# 	f2 <- .Fortran("ZENAZ2",
# 	               as.double(HOURS),
# 	               as.double(ALAT),
# 				   as.double(TTIMD),
# 				   as.double(BEAR),
# 				   as.double(DEC),
# 				   as.double(EQNTIM),
# 				   ZEN,
# 				   AZ,
# 				   as.integer(KHRS),
# 				   PACKAGE="YplantQMC")
# 				   # browser()
# 	zenith <- f2[[7]]
#     azimuth <-	f2[[8]]
# 	
# 	# cat(f2[[2]],f2[[3]],f2[[4]],f2[[5]],f2[[6]],"\n")
# 	
# 	dfr <- data.frame(zen=zenith, az=azimuth)
# 	dfr[dfr$zen > pi/2,] <- NA
# 	dfr <- dfr / k
# 	
# 	# Convert azimuth definition:
# 	dfr$az <- 180 - dfr$az
# 	dfr$az[dfr$az < 0 & !is.na(dfr$az)] <- 360 + dfr$az[dfr$az < 0 & !is.na(dfr$az)]
# 	
# 	# Solar altitude.
# 	dfr$alt <- 90 - dfr$zen
# 	
# return(list(hour=HOURS, altitude=dfr$alt, azimuth=dfr$az, 
# 	daylength=DAYL, sunset = SUNSET, zenrad=(pi/180)*dfr$zen))
# }

  