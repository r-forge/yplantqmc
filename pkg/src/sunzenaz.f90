
!**********************************************************************
      
	  !!! Modified to take 'day of year' as input, not Maestra days since 1900.
	  SUBROUTINE SUN(DOY,ALAT,TTIMD,DEC,EQNTIM,DAYL,SUNSET,KHRS)
! this is a subroutine to calculate the daylength. it is from
!  a paper by Bruce Barkstrom (1981,'what time does the sun rise
! and set?' Byte.102-114)
!**********************************************************************

      !USE maestcom
      IMPLICIT NONE
      INTEGER IM,HHRS,KHRS
      !INTEGER, EXTERNAL :: DAYJUL
      DOUBLE PRECISION LUNLON,MLON,NUTOBL,NUTLON,MUM,MUN,MUA,ALAT
      DOUBLE PRECISION TTIMD,DEC,EQNTIM,DAYL,SUNSET,T,ECC,RM,E
      DOUBLE PRECISION V,R,EPS,Y,SL,AO,REFAC,OMEG,UMN,FRACSU,SUNRIS,DOY
      DOUBLE PRECISION PID180,PI,TWOPI
      DOUBLE PRECISION, EXTERNAL :: ECCENT
      DOUBLE PRECISION, EXTERNAL :: ANOM
      DOUBLE PRECISION, EXTERNAL :: EPSIL
      DOUBLE PRECISION, EXTERNAL :: OMEGA
      DOUBLE PRECISION, EXTERNAL :: ALUNAR
   
      PI = 4.0*DATAN(DBLE(1.0))   !3.1415927
	  PID180 = PI / 180.0
	  TWOPI = PI * 2.0
	  HHRS = KHRS / 2
	  
      T = DOY/36525
! COMPUTE SOLAR ORBIT
      ECC = ECCENT(T)
      RM = ANOM(T,DOY)
      E = RM
      DO IM = 1,3
        E = E + (RM- (E-ECC*DSIN(E)))/ (1-ECC*DCOS(E))
      ENDDO
 
      V = 2.0*DATAN(DSQRT((1+ECC)/ (1-ECC))*DTAN(0.5*E))
      IF (V.LT.0.0) V = V + TWOPI
      R = 1 - ECC*DCOS(E)
      EPS = EPSIL(T)
      OMEG = OMEGA(T,DOY)
! COMPUTE NUTATION TERMS
      LUNLON = ALUNAR(T,DOY)
      NUTOBL = (2.5583E-3+2.5E-7*T)*DCOS(LUNLON)*PID180
      EPS = EPS + NUTOBL
      NUTLON = - (4.7872E-3+4.7222E-6*T)*DSIN(LUNLON)*PID180
! COMPUTE SOLAR DECLINATION
      DEC = DASIN(DSIN(EPS)*DSIN(V+OMEG))
! COMPUTE EQN OF TIME
      MLON = OMEG + RM
      IF (MLON.LT.0.0) MLON = MLON + TWOPI
      IF (MLON.GT.TWOPI) MLON = MLON - TWOPI*IFIX(REAL(MLON/TWOPI))
      Y = DTAN(0.5*EPS)
      Y = Y*Y
      Y = (1-Y)/ (1+Y)
      SL = OMEG + NUTLON + V
      IF (SL.LT.0.0) SL = SL + TWOPI
      IF (SL.GT.TWOPI) SL = SL - TWOPI*IFIX(REAL(SL/TWOPI))
      AO = DATAN(Y*DTAN(SL))
      EQNTIM = AO - MLON
      EQNTIM = EQNTIM - PI*IFIX(REAL(EQNTIM/PI))
      IF (DABS(EQNTIM).GT.0.9*PI) EQNTIM = EQNTIM - PI*EQNTIM/DABS(EQNTIM)
      AO = EQNTIM + MLON
      IF (AO.GT.TWOPI) AO = AO - TWOPI*IFIX(REAL(AO/TWOPI))
! DAY LENGTH
      MUM = DCOS(ALAT-DEC)
      MUN = -DCOS(ALAT+DEC)
      MUA = 0.0
      REFAC = 0.0
      UMN = -MUM*MUN
      IF (UMN.GT.0.0) REFAC = 0.05556/DSQRT(UMN)
      IF (MUN.GT.MUA) MUA = MUN
      IF (MUM.GT.MUA)THEN
      FRACSU = DSQRT((MUA-MUN)/ (MUM-MUA))
      FRACSU = 1.0 - 2.0*DATAN(FRACSU)/PI
      SUNSET = HHRS*FRACSU
      SUNRIS = SUNSET
      SUNSET = SUNSET + REFAC + EQNTIM*HHRS/PI
      SUNRIS = SUNRIS + REFAC - EQNTIM*HHRS/PI
      SUNSET = SUNSET + HHRS + TTIMD
      SUNRIS = HHRS - SUNRIS + TTIMD
      EQNTIM = EQNTIM*HHRS/PI
      DAYL = SUNSET - SUNRIS
      ENDIF
	  
	  RETURN
      END !Sun


!**********************************************************************
!      INTEGER FUNCTION DAYJUL(IDATE)
! calculateS JULIAN DAY - NEEDED FOR SUN
!**********************************************************************
!
!      IMPLICIT NONE
!      INTEGER JD,IYEAR,NY,NLEAP,IDATE,JDATE
!      REAL D
!
!      JD = JDATE(IDATE)
!      IYEAR = (IDATE - JD)*100/36525 + 1951
!      D = REAL(JD)
!
!      NY = IYEAR + 4712
!      NLEAP = (NY-1)/4
!      DAYJUL = 365*NY + NLEAP + D
!      IF (IYEAR.LT.1583) GO TO 10
!      DAYJUL = DAYJUL - 10
!      DAYJUL = DAYJUL - (IYEAR-1501)/100
!      DAYJUL = DAYJUL + (IYEAR-1201)/400
!   10 IF (IYEAR.EQ.1582 .AND. D.GE.319.0) DAYJUL = DAYJUL - 10
!      IF (4.0* (IYEAR/4).EQ.IYEAR .AND. JD.GE.60) DAYJUL = DAYJUL + 1
!      DAYJUL = DAYJUL - 2415020 ! Subtract DAYJUL(1/1/1900)
!
!      RETURN
!      END  !DayJul

!**********************************************************************
      DOUBLE PRECISION FUNCTION ECCENT(T)
! calculateS ECCENTRICITY - USED IN SUN
!**********************************************************************
      
      IMPLICIT NONE
      DOUBLE PRECISION T
      
      ECCENT = 0.01675104 - (4.08E-5+1.26E-7*T)*T
      RETURN
      END  !Eccent

!**********************************************************************
      DOUBLE PRECISION FUNCTION ANOM(T,D)
! calculateS MEAN ANOMALY IN RADIANS - USED IN SUN
!**********************************************************************

      !USE maestcom
      IMPLICIT NONE
      DOUBLE PRECISION PI,T,D,PID180
      PI = 4.0*DATAN(DBLE(1.0))
	  PID180 = PI / 180.0
	  
      ANOM = -1.52417 + (1.5E-4+3.0E-6*T)*T*T
      ANOM = ANOM + 0.98560*D
      IF (ANOM.LE.360.0) GO TO 10
      ANOM = ANOM - 360.0*IFIX(REAL(ANOM)/360.0)
   10 ANOM = ANOM*PID180

      RETURN
      END !Anom


!**********************************************************************
      DOUBLE PRECISION FUNCTION EPSIL(T)
! calculate OBLIQUITY OF ECLIPTIC - USED IN SUN
!**********************************************************************

      !USE maestcom
      IMPLICIT NONE
      DOUBLE PRECISION T, PID180,PI
	  PI = 4.0*DATAN(DBLE(1.0))
	  PID180 = PI / 180
      
      EPSIL = (23.452294- (1.30125E-2+ (1.64E-6-5.03E-7*T)*T)*T)*PID180
      !EPSIL = (23.452294- (1.30125E-2+ (1.64E-6-5.03E-7*T)*T)*T)*PID180

      RETURN
      END !Epsil

!**********************************************************************
      DOUBLE PRECISION FUNCTION OMEGA(T,D)
! calculateS MEAN LONGITUDE OF PERIGEE - USED IN SUN
!**********************************************************************

      !USE maestcom
      IMPLICIT NONE
      DOUBLE PRECISION PI,T,D,PID180
	  PI = 4.0*DATAN(DBLE(1.0))
	  PID180 = PI / 180

      OMEGA = (281.22083+ (4.53E-4+3.0E-6*T)*T*T+4.70684E-5*D)*PID180

      RETURN
      END  !Omega


!**********************************************************************
      DOUBLE PRECISION FUNCTION ALUNAR(T,D)
! COMPUTE LONGITUDE OF ASCENDING NODE OF LUNAR ORBIT  - USED IN SUN
!**********************************************************************

      !USE maestcom
      IMPLICIT NONE
      DOUBLE PRECISION T,D,PID180,PI
	  PI = 4.0*DATAN(DBLE(1.0))
	  PID180 = PI / 180
 
      ALUNAR = (259.1833+ (2.078E-3+2.0E-6*T)*T*T-0.05295*D)*PID180

      RETURN
      END !Alunar

!**********************************************************************
      SUBROUTINE ZENAZ(ALAT,TTIMD,BEAR,DEC,EQNTIM,ZEN,AZ,KHRS)
! Calculates hourly zenith and azimuth angles.
! Corrected 07/05 BM
! Code from ORIGMAES from Ying Ping
! Note: BEAR is the bearing (in degrees clockwise) of the x-axis from south
! If the x-axis is S, BEAR = 0; x-axis W, BEAR = 90; x-axis N, BEAR 180; x-axis E, BEAR -90
! AZ, the azimuth angle, is in radians anticlockwise from S (trig convention) 
! e.g. azimuth S, AZ = 0; azimuth W, AZ = -pi/2; azimuth N, AZ = pi, azimuth E, AZ = +pi/2
!**********************************************************************
    
      !USE maestcom
      IMPLICIT NONE
      INTEGER I,KHRS,HHRS
      DOUBLE PRECISION ALAT,TTIMD,BEAR,DEC,EQNTIM
      DOUBLE PRECISION ZEN(KHRS),AZ(KHRS),SINAZ
      DOUBLE PRECISION R,HI,AI,AIP1,PI

	  PI = 4.0*DATAN(DBLE(1.0))
	  HHRS = KHRS / 2 
	  
      DO 10 I = 1,KHRS
        R = I - 0.5
        HI = (R-TTIMD-EQNTIM-HHRS)*PI/HHRS

        ZEN(I) = DACOS(DSIN(ALAT)*DSIN(DEC) &
                   + DCOS(ALAT)*DCOS(DEC)*DCOS(HI))
        SINAZ = DCOS(DEC)*DSIN(HI)/DSIN(ZEN(I))
        IF (SINAZ.GT.1.0) THEN
          SINAZ = 1.0
        ELSE IF (SINAZ.LT.-1.0) THEN
          SINAZ = -1.0
        END IF
        AZ(I) = DASIN(SINAZ)
10    CONTINUE

      DO 20 I = 1,KHRS

      IF (DABS(ZEN(I)).LE.1.57) THEN    ! Daytime calcs only
        AI=AZ(I)
        AIP1 = AZ(I+1)
        IF (AI.GE.0.0) THEN
          IF (AIP1.LT.AI) AZ(I+1)=PI-AIP1
        ELSE
          IF (AI.GE.AIP1) AZ(I)=-PI-AI   
        END IF
          IF (ALAT.LT.0.0) THEN
        AZ(I) = AZ(I)+PI+BEAR        ! Southern hemisphere
        ELSE
        AZ(I) = -AZ(I)+BEAR            ! Northern hemisphere
        END IF
      END IF

20    CONTINUE


! PREVIOUS INCORRECT CODE
!        AZ(I) = Azimth-BEAR
!        IF (ALAT.LT.0.0) AZ(I)=-AZ(I)

      RETURN
      END


	  
!**********************************************************************
      SUBROUTINE ZENAZ2(HOURS,ALAT,TTIMD,BEAR,DEC,EQNTIM,ZEN,AZ,KHRS)
! Calculates hourly zenith and azimuth angles.
! Corrected 07/05 BM
! Code from ORIGMAES from Ying Ping
! Note: BEAR is the bearing (in degrees clockwise) of the x-axis from south
! If the x-axis is S, BEAR = 0; x-axis W, BEAR = 90; x-axis N, BEAR 180; x-axis E, BEAR -90
! AZ, the azimuth angle, is in radians anticlockwise from S (trig convention) 
! e.g. azimuth S, AZ = 0; azimuth W, AZ = -pi/2; azimuth N, AZ = pi, azimuth E, AZ = +pi/2
!**********************************************************************
    
      !USE maestcom
      IMPLICIT NONE
      INTEGER I,KHRS
      DOUBLE PRECISION ALAT,TTIMD,BEAR,DEC,EQNTIM
      DOUBLE PRECISION ZEN(KHRS),AZ(KHRS),SINAZ
      DOUBLE PRECISION R,HI,AI,AIP1,PI,HHRS
	  DOUBLE PRECISION HOURS(KHRS)

	  PI = 4.0*DATAN(DBLE(1.0))
	  
	  HHRS = 12
	  
      DO I = 1,KHRS
        !R = DBLE(I - 0.5)
        R = HOURS(I)
		
		HI = (R-TTIMD-EQNTIM-HHRS)*PI/HHRS

        ZEN(I) = DACOS(DSIN(ALAT)*DSIN(DEC) &
                   + DCOS(ALAT)*DCOS(DEC)*DCOS(HI))
        SINAZ = DCOS(DEC)*DSIN(HI)/DSIN(ZEN(I))
        IF (SINAZ.GT.1.0) THEN
          SINAZ = 1.0
        ELSE IF (SINAZ.LT.-1.0) THEN
          SINAZ = -1.0
        END IF
        AZ(I) = DASIN(SINAZ)
      ENDDO

      DO I = 1,KHRS

      IF (DABS(ZEN(I)).LE.1.57) THEN    ! Daytime calcs only
        AI=AZ(I)
        AIP1 = AZ(I+1)
        IF (AI.GE.0.0) THEN
          IF (AIP1.LT.AI) AZ(I+1)=PI-AIP1
        ELSE
          IF (AI.GE.AIP1) AZ(I)=-PI-AI   
        END IF
          IF (ALAT.LT.0.0) THEN
        AZ(I) = AZ(I)+PI+BEAR        ! Southern hemisphere
        ELSE
        AZ(I) = -AZ(I)+BEAR            ! Northern hemisphere
        END IF
      END IF

      ENDDO

      RETURN
      END

