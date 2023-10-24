**********************************************************************************************
**                                                                                          **   
**    Copyright by Teng Tong, Southeast University                                          **
**    by 2022                                                                               **
**    Mechanical - CDP model, Creep - rate type in USDFLD, Shrinkage - UEXPAN               **
**                                                                                          **    
**********************************************************************************************
*    
*      
*     ----------------------------------------------------------------------------------      *
*     ----------------------------------------------------------------------------------      *
*     ----------------------------------------------------------------------------------      *
*
*
      
      
      SUBROUTINE SIGINI(SIGMA,COORDS,NTENS,NCRDS,NOEL,NPT,LAYER,
     1    KSPT,LREBAR,REBARN)
     
      INCLUDE 'ABA_PARAM.INC'
C
      DIMENSION SIGMA(NTENS),COORDS(NCRDS)
      CHARACTER*80 REBARN
C
C     HORIZONTAL PRESTRESSED TENDONS
C
	call Coeff_Random(THETA_Cr, THETA_Sh, THETA_fc, 
     +                  THETA_Pre, THETA_H)
*
*
*      
	IF( (NOEL.GE.35434) .and.(NOEL.LE.55199) ) THEN
          SIGMA(1) = 1200.0D6	 *   THETA_Pre 
      ENDIF
      
      IF( (NOEL.GE.55200) ) THEN
          SIGMA(1) = 000.0D6	 *   THETA_Pre    
	ENDIF
C
      RETURN
      END   
C
C     INITIALIZE INTERNAL VARABLES
C

      
      
      SUBROUTINE USDFLD(FIELD,STATEV,PNEWDT,DIRECT,T,CELENT,
     1 TIME,DTIME,CMNAME,ORNAME,NFIELD,NSTATV,NOEL,NPT,LAYER,
     2 KSPT,KSTEP,KINC,NDI,NSHR,COORD,JMAC,JMATYP,MATLAYO,LACCFLA)
*
      INCLUDE 'ABA_PARAM.INC'
      
      PARAMETER (NS_YU = 15)
*
      CHARACTER*80 CMNAME,ORNAME
      CHARACTER*3  FLGRAY(15)
      DIMENSION FIELD(NFIELD),STATEV(NSTATV),DIRECT(3,3),
     1 T(3,3),TIME(2)
      DIMENSION ARRAY(15),JARRAY(15),JMAC(*),JMATYP(*),COORD(*)
      
      DIMENSION STRESS(6)
      
      DIMENSION DSTRES(6),D(3,3), YU_STRAIN(6)
	DIMENSION YU_D(6,6),YU_DELTA_STRESS(6)
	DIMENSION YU_T_MU(NS_YU), YU_DELTA_Y_MU(NS_YU)
	DIMENSION YU_L_MU(NS_YU), YU_A_MU(NS_YU), YU_LAM_MU(NS_YU)
	DIMENSION YU_IN(NS_YU,6), YU_EN(NS_YU,6)
      
      DIMENSION YU_L_MU_Q2(15), YU_A_MU_Q2(15)
      DIMENSION YU_S(6)
      DIMENSION YU_STRAIN_4(6)
*     ----------------------------------------------------------------------------------      *
*     1.0 Parameter inputs
*     ----------------------------------------------------------------------------------      *
*
      call Coeff_Random(THETA_Cr, THETA_Sh, THETA_fc, 
     +                  THETA_Pre, THETA_H)
      
      TIME_PRIME = 6.99D0*1.0D0 
*      
      TIME_LOAD = TIME(2)
*      
*
      YU_E28 = 30.0D9
      
*     POSSION'S RATIO
      V_YU  = 0.18
*     B3 CONSTANT N
      YU_N = 0.1
      
      YU_RT = 1.0D0
      
      
      
*     Original PARAMETERS INPUT
*       --Cement content      
      YU_CEMENT = 500.0D0      !kg/m3
*       --CONCRETE DENSITY      
      YU_CONCRETE = 2350.0D0      !kg/m3      
*       --w/c      
      YU_WATER_TO_CEMENT = 0.366D0
*       --a/c     
      YU_AGG_TO_CEMENT = 3.53D0
*       --h
      YU_HUMIDITY = 0.7 D0 * THETA_H
*      
      YU_HUMIDITY = min(YU_HUMIDITY, 0.99)
C       --T
      YU_TEMPERATURE = 20.0 D0
*       --V/S
      YU_VOL_SUR = 650.0D0   !   v/s
*     
      YU_FC = 0.79D0*50.0D6*1.1645D0 
*      
*      YU_E28 = 4734.0D0*(YU_FC/1.0D6)**(0.50D0) *1.0D6  !Pa  
      YU_E28 = 27.72D9  !Pa
*
*
*       --HUMIDITY DEPENDENCE
      YU_KH = 1-YU_HUMIDITY**3.0D0
      
*       --EFFECTIVE THICKNESS
      YU_THICKNESS = 2.0D0*YU_VOL_SUR   !   2v/s
      
*       --DRYING SHRINKAGE HALFTIME
      YU_DRY_SHRIN_0 = 0.016D0*(YU_AGG_TO_CEMENT/6.0D0)**(-0.330D0)
     +      *(YU_WATER_TO_CEMENT/0.38D0)**(-0.06D0)
     +      *(6.5D0*YU_CEMENT/YU_CONCRETE)**(-0.1D0)   
      
      YU_DRY_SHRIN = YU_DRY_SHRIN_0 * (YU_THICKNESS)**2
*
*       --FINAL DRYING SHRINKAGE
      YU_STRAIN0=360.0D-6*(YU_AGG_TO_CEMENT/6.0D0)**(-0.80D0)
     +      *(YU_WATER_TO_CEMENT/0.38D0)**1.1D0
     +      *(6.5D0*YU_CEMENT/YU_CONCRETE)**(0.11D0)
*     
*       --E(607)
      YU_E607 = YU_E28*(607.0D0/(4.0D0+(6.0D0/7.0D0)*607.0D0))**0.5D0
            
*       --E(T0+TSH)
      YU_E_T0SH = YU_E28*((TIME_ACT+YU_DRY_SHRIN)/(4.0D0+
     +            (6.0D0/7.0D0)*(TIME_ACT+YU_DRY_SHRIN)))**0.5D0

*       --CORRECTION OF SHRINKAGE
      YU_STRAIN_INFI = -1.0D0* YU_STRAIN0 * YU_E607 / YU_E_T0SH  
*
      YU_AUTOSTRAIN_INFI =-210.0D-6*(YU_AGG_TO_CEMENT/6.0D0)**(-0.750D0)
     +      *(YU_WATER_TO_CEMENT/0.38D0)**(-3.5D0) 
     
*       --CORRECTION OF AUTO SHRINKAGE
      YU_AUTOSTRAIN = 1.0D0*(YU_WATER_TO_CEMENT/0.38D0)**(3.0D0)       
*
*
*
      YU_Q1 = 0.40D0/(YU_E28/1.0D6)/1.0D6     !/Pa 
*     
      YU_Q2 =50.7D-3*(YU_WATER_TO_CEMENT/0.38D0)**3.0D0/1.0D3/1.0D6 !/Pa
*                                     
      YU_Q3 = 109.0D-3*YU_Q2*(YU_AGG_TO_CEMENT/6.0D0)**(-1.10D0)
     +      *(YU_WATER_TO_CEMENT/0.38D0)**0.4D0
*           
      YU_Q4 = 3.6D-3*(YU_AGG_TO_CEMENT/6.0D0)**(-0.90D0)
     +      *(YU_WATER_TO_CEMENT/0.38D0)**2.45D0/1.0D3/1.0D6
      


      
*     ----------------------------------------------------------------------------------      *      
*     2.0 Obtian the VARs(Stress, strain, etc) at the material points 
*     ----------------------------------------------------------------------------------      *
*
*     ---------------------------------------------------------      *
*     2.1 Obtain stress :
*
      JERROR = 0
*     Stress tensor:
      CALL GETVRM('S',ARRAY,JARRAY,FLGRAY,JRCD,JMAC,JMATYP,
     1 MATLAYO,LACCFLA)
*      
      JERROR = JERROR + JRCD
*      
      STRESS(1) = array(1)
      STRESS(2) = array(2)
      STRESS(3) = array(3)
      STRESS(4) = array(4)
      STRESS(5) = array(5)
      STRESS(6) = array(6)
*      
*     If error, write comment to .DAT file:
      IF(JERROR.NE.0)THEN
        WRITE(6,*) 'REQUEST ERROR IN UVARM FOR ELEMENT NUMBER ',
     1      NOEL,'INTEGRATION POINT NUMBER ',NPT
      ENDIF
*
*     ---------------------------------------------------------      *
*     2.2 Obtain princinple stress :
*
      JERROR = 0
*     Stress tensor:
      CALL GETVRM('SP',ARRAY,JARRAY,FLGRAY,JRCD,JMAC,JMATYP,
     1 MATLAYO,LACCFLA)
*
      JERROR = JERROR + JRCD
*      
      Stress_prin1  = ARRAY(1)
      Stress_prin2  = ARRAY(2)
      Stress_prin3  = ARRAY(3)
*      
*     If error, write comment to .DAT file:
      IF(JERROR.NE.0)THEN
        WRITE(6,*) 'REQUEST ERROR IN UVARM FOR ELEMENT NUMBER ',
     1      NOEL,'INTEGRATION POINT NUMBER ',NPT
      ENDIF            
*
*     ---------------------------------------------------------      *
*     2.3 Calculate coefficient for nonlinear creep
*      
      fck = 34.3d6 * 1.0d0 * THETA_fc
*      
      com_ratio = abs(STRESS(3)) / (fck)
      
      
      if (  (com_ratio .le. 0.2d0) ) then
          creep_ratio = 1.00d0
      else
          creep_ratio_term1 = com_ratio - 0.2d0
          creep_ratio_term2 = creep_ratio_term1 * 2.0d0
          creep_ratio_term3 = creep_ratio_term2 ** 1.8d0
*          
          creep_ratio = 1.0d0 + creep_ratio_term3
      end if
      
      if ( (time(2) .le. 258.0d0) ) then
          creep_ratio = 1.0d0
      endif
      
      
      creep_ratio = max(1.0d0, creep_ratio)
      
      
      
      STATEV(151) = creep_ratio
      STATEV(152) = Stress_prin
      STATEV(153) = com_ratio     
      
 

*     ----------------------------------------------------------------------------------      *      
*     3.0 Creep calculation
*     ----------------------------------------------------------------------------------      *      
*  
      IF(TIME_LOAD .lT. TIME_PRIME)THEN
*     ----------------------------------------------------------------------------------      *          
          YU_STRAIN(1) = 0.0d0
          YU_STRAIN(2) = 0.0d0
          YU_STRAIN(3) = 0.0d0
          YU_STRAIN(4) = 0.0d0
          YU_STRAIN(5) = 0.0d0
          YU_STRAIN(6) = 0.0d0
*          
          FIELD(1) = 1.0
*     ----------------------------------------------------------------------------------      *                  
      ELSE
*     ----------------------------------------------------------------------------------      *          
*         define the field variable to interpolate the elastic modulus
*
          FIELD(1) = SQRT(YU_MEDTIME/(4+0.85*YU_MEDTIME))
          
          YU_T0 = TIME_PRIME
*      
          YU_MEDTIME = YU_T0 + SQRT((TIME(2)-YU_T0)*(TIME(2)+DTIME-YU_T0))

****************************************************************************************
*         Retardation Time
*        
          YU_T_MU(1) = 1.0D-6
	    DO I = 2, NS_YU
	        YU_T_MU(I) = YU_T_MU(I-1)*10.0D0
	    ENDDO
*
*         Dtime/Retardation time
	    DO I = 1, NS_YU
	        YU_DELTA_Y_MU(I) = DTIME/YU_T_MU(I)
          ENDDO
          
****************************************************************************************          
*         GIVE VALUE TO SPECTRUM A(tu ): for Q2 & Q3
*          
	    DO I = 1, NS_YU
              TEMX = 3*YU_T_MU(I)
              X1 = YU_N*(YU_N-1.0D0)*(YU_N-2.0D0)*TEMX**(YU_N-3.0D0)
              X2 = (1.0D0 + TEMX**YU_N)
              X3 = YU_N*YU_N*(YU_N-1.0D0)*TEMX**(2.0D0*YU_N-3.0D0)
              X4 = (1.0D0 + TEMX**YU_N)**2.0D0
              X5 = YU_N*YU_N*(2.0D0*YU_N-2.0D0)*TEMX**(2.0D0*YU_N-3.0D0)
              X6 = (1.0D0 + TEMX**YU_N)**2.0D0
              X7 = 2.0D0*YU_N*YU_N*YU_N*TEMX**(3.0D0*YU_N-3.0D0)
              X8 = (1.0D0 + TEMX**YU_N)**3.0D0
              X9   = TEMX**3.0D0/2.0D0
              X10   = (X1/X2 - X3/X4 - X5/X6 + X7/X8)*X9
	        YU_A_MU(I) = X10*log(10.0d0)
	    ENDDO
	    
****************************************************************************************
*
*         LAMBDA FOR Q2&Q3
*     
	    DO I = 1, NS_YU
	        TEMP_YU      = EXP( 0.0D0-YU_DELTA_Y_MU(I) )
	        YU_LAM_MU(I) = (1.0D0 - TEMP_YU)/YU_DELTA_Y_MU(I)
          ENDDO          

****************************************************************************************
*         CALCULATE THE INEALSTIC INCREMENT OF STRAIN DUE TO CREEP
*         YU_E: EFFECTIVE MODULUS CONSIDERED THE CREEP
*         YU_V: COEFFICIENT OF VISCOSITY
*         YU_F: EFFECT OF APPLIED STRESS. HERE WE CONSIDER IT EQUAL 1.0
*         LATER, WE WILL WRITE IT AS EQUATION OF PRINCIPLE STRESS
*         YU_MEDTIME: TIME AT MID-STEP OF LOG-TIME SCALE

*         GIVE VALUE TO E" : FOR Q3 &Q3

          YU_E = 0.0D0
	    YU_V = SQRT(1.0D0/YU_MEDTIME) + YU_Q3/YU_Q2
          YU_F = 1.0D0
      
*         CALCULATE EFFECTIVE MODULUS    
          DO I = 1, NS_YU
	        YU_E = YU_E + ( 1.0D0-YU_LAM_MU(I) )*YU_Q2*YU_A_MU(I) 
          ENDDO 

          
      
          YU_S(1) = STRESS(1)
          YU_S(2) = STRESS(2)
          YU_S(3) = STRESS(3)
          YU_S(4) = STRESS(4)
          YU_S(5) = STRESS(5)
          YU_S(6) = STRESS(6)

*
*         INELASTIC STRAIN GAMMA CAUSED BY CREEP
*         READ INELASTIC CREEP STRAIN AT LAST STEP
          K1 = 1
          DO I = 1, NS_YU
	        DO J = 1, 6
	            K1 = K1 + 1
	            YU_IN(I,J) = STATEV(K1)
              ENDDO
          ENDDO
          
          
****************************************************************************************                    
*      
*         CALCULATE THE INELASTIC STRAIN INCREMENT
          DO I = 1,6
	        YU_STRAIN(I) = 0.0D0
	    ENDDO
*	
	    DO I = 1, NS_YU
	        DO J = 1, 6
	            YU_STRAIN(J) = YU_STRAIN(J) 
     +	           + YU_IN(I,J) * (1.0D0 - EXP( 0.0D0-YU_DELTA_Y_MU(I) ))
	        ENDDO
	    ENDDO         

*        CONSIDER COMBINED EFFECT OF Q2 AND Q3
         DO I = 1,6
	       YU_STRAIN(I) = YU_STRAIN(I) * YU_V
         ENDDO          
          
          
****************************************************************************************          
  
C         CALCULATE THE INELASTIC STRAIN--FOR Q4  
	
C         YU_STRAIN_4 = 0.0D0
          DO I = 1, 6
	        YU_STRAIN_4(I) = YU_Q4*DTIME/YU_MEDTIME *YU_S(I)
          ENDDO      
      
      
C    ---4.5: GET TOTAL INELASTIC STRAIN
c     INELASTIC STRAIN
	    DO I = 1, 6
	        YU_STRAIN(I) = YU_STRAIN_4(I) + YU_STRAIN(I) 
          ENDDO
      
          YU_STRAIN = YU_STRAIN * creep_ratio
          YU_STRAIN = YU_STRAIN * THETA_Cr
      
****************************************************************************************    
*     
*         YU_EN: NOW IS JUST STORE THE INELASTIC INCREMENT OF GAMMA
*         ELASTIC INCREMENT OF GAMMA NEED TO BE ADDED
*         UPDATE GAMA_I for   Q2&Q3 
      
*         Obtain the delta_stress
*         
          YU_DELTA_STRESS(1) = stress(1) - STATEV(191)
          YU_DELTA_STRESS(2) = stress(2) - STATEV(192)
          YU_DELTA_STRESS(3) = stress(3) - STATEV(193)
          YU_DELTA_STRESS(4) = stress(4) - STATEV(194)
          YU_DELTA_STRESS(5) = stress(5) - STATEV(195)
          YU_DELTA_STRESS(6) = stress(6) - STATEV(196)

*      
*         UPDATE GAMA_I
          YU_EN = 0.0D0
	    DO I = 1, NS_YU
	        DO J = 1, 6
                  YU_EN(I,J) = YU_IN(I,J)*EXP( 0.0D0-YU_DELTA_Y_MU(I) )
     +         + YU_LAM_MU(I)*YU_DELTA_STRESS(J)*YU_Q2*YU_A_MU(I)
              ENDDO
	    ENDDO
*         
*         STORE THE UPDATED GAMA FOR NEXT STEP	
          K1 = 1
          DO I = 1,NS_YU
	        DO J = 1,6
	            K1 = K1 + 1
	            STATEV(K1) = YU_EN(I,J)
              ENDDO
          ENDDO  

*     ----------------------------------------------------------------------------------      *          
      END IF
*     ----------------------------------------------------------------------------------      *          
*
*     ----------------------------------------------------------------------------------      *      
*     4.0 UPDATE OTHER STATE VARIABLES
*     ----------------------------------------------------------------------------------      *      
*
*     161~166    TOTAL_CREEP_STRAIN          
      STATEV(161)=  YU_STRAIN(1) + STATEV(161)
      STATEV(162)=  YU_STRAIN(2) + STATEV(162)
      STATEV(163)=  YU_STRAIN(3) + STATEV(163)
      STATEV(164)=  YU_STRAIN(4) + STATEV(164)
      STATEV(165)=  YU_STRAIN(5) + STATEV(165)
      STATEV(166)=  YU_STRAIN(6) + STATEV(166)
*      
*     171~177    DELTA_CREEP_STRAIN    
      STATEV(171)=  YU_STRAIN(1)
      STATEV(172)=  YU_STRAIN(2)
      STATEV(173)=  YU_STRAIN(3)
      STATEV(174)=  YU_STRAIN(4)
      STATEV(175)=  YU_STRAIN(5)
      STATEV(176)=  YU_STRAIN(6)
*      
*     181~186    DELTA_STRESS      
      STATEV(181)=  YU_DELTA_STRESS(1)
      STATEV(182)=  YU_DELTA_STRESS(2)
      STATEV(183)=  YU_DELTA_STRESS(3)
      STATEV(184)=  YU_DELTA_STRESS(4)
      STATEV(185)=  YU_DELTA_STRESS(5)
      STATEV(186)=  YU_DELTA_STRESS(6)
*      
*     191~196    STRESS       
      STATEV(191) = STRESS(1)
      STATEV(192) = STRESS(2)
      STATEV(193) = STRESS(3)
      STATEV(194) = STRESS(4)
      STATEV(195) = STRESS(5)
      STATEV(196) = STRESS(6)

*     ----------------------------------------------------------------------------------      *            
      RETURN
      END      


*     ----------------------------------------------------------------------------------      *            
*     ----------------------------------------------------------------------------------      *            
*     ----------------------------------------------------------------------------------      *            
      
      
      
      SUBROUTINE UMAT(STRESS,STATEV,DDSDDE,SSE,SPD,SCD,
     1 RPL,DDSDDT,DRPLDE,DRPLDT,
     2 STRAN,DSTRAN,TIME,DTIME,TEMP,DTEMP,PREDEF,DPRED,CMNAME,
     3 NDI,NSHR,NTENS,NSTATV,PROPS,NPROPS,COORDS,DROT,PNEWDT,
     4 CELENT,DFGRD0,DFGRD1,NOEL,NPT,LAYER,KSPT,KSTEP,KINC)
C
      INCLUDE 'ABA_PARAM.INC'
C  
       
      CHARACTER*80 CMNAME
      DIMENSION STRESS(NTENS),STATEV(NSTATV),
     1 DDSDDE(NTENS,NTENS),
     2 DDSDDT(NTENS),DRPLDE(NTENS),
     3 STRAN(NTENS),DSTRAN(NTENS),TIME(2),PREDEF(1),DPRED(1),
     4 PROPS(NPROPS),COORDS(3),DROT(3,3),DFGRD0(3,3),DFGRD1(3,3)
      
      DIMENSION STRAN_TOT(6)
     
*     ----------------------------------------------------------------------------------      *            
*     PARAMETER INPUT
*     
     
      ESTEEL  = 200.0d9
*
      RHOX = 0.0d0/100.0d0
      RHOY = 0.0d0/100.0d0
      RHOZ = 1.0d0/100.0d0
*     ----------------------------------------------------------------------------------      *            
*     TOTAL STRAIN
*      
      STRAN_TOT(1) = DSTRAN(1) + STRAN(1)
      STRAN_TOT(2) = DSTRAN(2) + STRAN(2)
      STRAN_TOT(3) = DSTRAN(3) + STRAN(3)
	  
      STRAN_TOT(4) = DSTRAN(4) + STRAN(4)
      STRAN_TOT(5) = DSTRAN(5) + STRAN(5)
      STRAN_TOT(6) = DSTRAN(6) + STRAN(6)
*
*     ----------------------------------------------------------------------------------      *            
*     REBAR STRESS
*      
      SIGSX =  ESTEEL*STRAN_TOT(1)
      SIGSY =  ESTEEL*STRAN_TOT(2)
      SIGSZ =  ESTEEL*STRAN_TOT(3)

      SIGSXY = ESTEEL*STRAN_TOT(4)
      SIGSXZ = ESTEEL*STRAN_TOT(5)
      SIGSYZ = ESTEEL*STRAN_TOT(6)
	  
     
      STRESS(1) = RHOX*SIGSX
      STRESS(2)=  RHOY*SIGSY
      STRESS(3) = RHOZ*SIGSZ 
      
      STRESS(4) = RHOX*SIGSX/10000000.0d0
      STRESS(5)=  RHOX*SIGSY/10000000.0d0
      STRESS(6) = RHOX*SIGSZ/10000000.0d0
      
*
*     ----------------------------------------------------------------------------------      *            
*     TANGENT MODULUS
*        
      E1 = RHOX*ESTEEL
      E2 = RHOY*ESTEEL
      E3 = RHOZ*ESTEEL
*     ----------------------------------------------------------------------------------      *            
*     UPDATE DDSDDE
*        
      DO K1=1,NTENS
         DO K2=1,NTENS
            DDSDDE(K2,K1) = 0.0D0
         END DO
      END DO
     
      DDSDDE(1,1) = E1
      DDSDDE(2,2) = E2
      DDSDDE(3,3) = E3
	  
	DDSDDE(4,4) = E1/10000000.0d0
      DDSDDE(5,5) = E2/10000000.0d0
      DDSDDE(6,6) = E3/10000000.0d0
      
      
      STATEV(1) = SIGSX
      STATEV(2) = SIGSY
      STATEV(3) = SIGSZ
*     ----------------------------------------------------------------------------------      *            


      RETURN
      END
*    
*      
*     ----------------------------------------------------------------------------------      *
*     ----------------------------------------------------------------------------------      *
*     ----------------------------------------------------------------------------------      *      
      

*      
*     ----------------------------------------------------------------------------------      *
*     ----------------------------------------------------------------------------------      *
*     ----------------------------------------------------------------------------------      *
*
*
      SUBROUTINE UEXPAN(EXPAN,DEXPANDT,TEMP,TIME,DTIME,PREDEF,DPRED,
     $	              STATEV,CMNAME,NSTATV,NOEL)
      
      INCLUDE 'ABA_PARAM.INC'
*     TIME OF CURING
	PARAMETER(SHRINKAGE = 7.0D0)

	CHARACTER*80 CMNAME

	DIMENSION EXPAN(*),DEXPANDT(*),TEMP(2),TIME(2),PREDEF(*),
     $          DPRED(*),STATEV(NSTATV)
      
      DIMENSION CREEP_STRAIN(6)

*     ----------------------------------------------------------------------------------      *      
*     1.0 Parameters
*     ----------------------------------------------------------------------------------      *      
*
      call Coeff_Random(THETA_Cr, THETA_Sh, THETA_fc, 
     +                  THETA_Pre, THETA_H)

      
      Iden_Ele =  35434     !( <Iden_Ele is concrete element,   >=Iden_Ele is steel element
*
*     ----------------------------------
*     1.1 Parameters for concrete creep
*     ----------------------------------
      TIME_ACT = 253.1d0      ! should be greater than the period of activation time (7 day)   
*      
*     CALCULATION OF YU_Q5
      
      
*     Original PARAMETERS INPUT
*       --Cement content      
      YU_CEMENT = 500.0D0      !kg/m3
*       --CONCRETE DENSITY      
      YU_CONCRETE = 2350.0D0      !kg/m3      
*       --w/c      
      YU_WATER_TO_CEMENT = 0.366D0
*       --a/c     
      YU_AGG_TO_CEMENT = 3.53D0
*       --h
      YU_HUMIDITY = 0.7 D0 * THETA_H
      
      YU_HUMIDITY = min(YU_HUMIDITY, 0.99d0)     
*       --T
      YU_TEMPERATURE = 20.0 D0
*       --V/S
      YU_VOL_SUR = 650.0D0   !   v/s
*    
      YU_FC = 0.79D0*50.0D6*1.1645D0 
*     
*     YU_E28 = 4734.0D0*(YU_FC/1.0D6)**(0.50D0) *1.0D6  !Pa  
      YU_E28 = 27.72D9  !Pa
*
*
*       --HUMIDITY DEPENDENCE
      YU_KH = 1-YU_HUMIDITY**3.0D0
      
*       --EFFECTIVE THICKNESS
      YU_THICKNESS = 2.0D0*YU_VOL_SUR   !   2v/s
      
*       --DRYING SHRINKAGE HALFTIME
      YU_DRY_SHRIN_0 = 0.016D0*(YU_AGG_TO_CEMENT/6.0D0)**(-0.330D0)
     +      *(YU_WATER_TO_CEMENT/0.38D0)**(-0.06D0)
     +      *(6.5D0*YU_CEMENT/YU_CONCRETE)**(-0.1D0)   
      
      YU_DRY_SHRIN = YU_DRY_SHRIN_0 * (YU_THICKNESS)**2
*
*       --FINAL DRYING SHRINKAGE
      YU_STRAIN0=360.0D-6*(YU_AGG_TO_CEMENT/6.0D0)**(-0.80D0)
     +      *(YU_WATER_TO_CEMENT/0.38D0)**1.1D0
     +      *(6.5D0*YU_CEMENT/YU_CONCRETE)**(0.11D0)
*     
*       --E(607)
      YU_E607 = YU_E28*(607.0D0/(4.0D0+(6.0D0/7.0D0)*607.0D0))**0.5D0
            
*       --E(T0+TSH)
      YU_E_T0SH = YU_E28*((TIME_ACT+YU_DRY_SHRIN)/(4.0D0+
     +            (6.0D0/7.0D0)*(TIME_ACT+YU_DRY_SHRIN)))**0.5D0

*       --CORRECTION OF SHRINKAGE
      YU_STRAIN_INFI = -1.0D0* YU_STRAIN0 * YU_E607 / YU_E_T0SH  
*
      YU_AUTOSTRAIN_INFI =-210.0D-6*(YU_AGG_TO_CEMENT/6.0D0)**(-0.750D0)
     +      *(YU_WATER_TO_CEMENT/0.38D0)**(-3.5D0) 
     
*       --CORRECTION OF AUTO SHRINKAGE
      YU_AUTOSTRAIN = 1.0D0*(YU_WATER_TO_CEMENT/0.38D0)**(3.0D0)        
      
      
      YU_Q5 = 350.0D-6*(YU_AGG_TO_CEMENT/6.0D0)**(-1.00D0)
     +      *(YU_WATER_TO_CEMENT/0.38D0)**0.78D0*
     +      (ABS(YU_KH*YU_STRAIN_INFI))**(-0.85D0) /1.0D3/1.0D6
      
      


*
*     ----------------------------------
*     1.2 Parameters for steel relaxation
*     ----------------------------------    
* 
*     TIME_REL: THE TIME TO RELEASE PRESTRESS
      TIME_RE = 253.1d0 
*          
      RE_1000 = 0.035D0               !RELAX AT 1000 HOURS
	STRAIN0 = 0.0035D0              !INITIAL STRAIN OF STEEL
	                                !THIS SHOULD BE DETERMINED BY
	                                !APPLIED PRESTRESS
      
      
      
*
*     ----------------------------------------------------------------------------------      *      
*     2.0 & 3.0 Concrete creep and shrinkage calculation
*     ----------------------------------------------------------------------------------      *      
      IF(NOEL.LT.Iden_Ele) THEN
*     ----------------------------------------------------------------------------------      *      
*         2.0 Inherit the creep strain increment
*     ----------------------------------------------------------------------------------      *      
*
          CREEP_STRAIN(1) = STATEV(171)
          CREEP_STRAIN(2) = STATEV(172)
          CREEP_STRAIN(3) = STATEV(173)
          CREEP_STRAIN(4) = STATEV(174)
          CREEP_STRAIN(5) = STATEV(175)
          CREEP_STRAIN(6) = STATEV(176) 
*     ----------------------------------------------------------------------------------      *      
*         3.0 Drying creep calculation
*     ----------------------------------------------------------------------------------      *   
          IF(TIME(2) .GE. TIME_ACT) THEN                              

              TONG_TIME1 = TIME(2) - TIME_ACT
              TONG_TIME2 = MAX(TIME(2) - TIME_ACT - DTIME, 0.0D0)
*       
              TONG_S1 = TANH(SQRT(TONG_TIME1/YU_DRY_SHRIN))
              TONG_S2 = TANH(SQRT(TONG_TIME2/YU_DRY_SHRIN))
*       
              TONG_G1 = - 8.0D0 * (1.0D0 - (1.0D0 - YU_HUMIDITY)*TONG_S1)
              TONG_G2 = - 8.0D0 * (1.0D0 - (1.0D0 - YU_HUMIDITY)*TONG_S2)
* 
              TONG_CD_TERM1 = SQRT(EXP(TONG_G1)-EXP(-8.0)) 
              TONG_CD_TERM2 = SQRT(EXP(TONG_G2)-EXP(-8.0)) 
              TONG_CD = YU_Q5 * (TONG_CD_TERM1 - TONG_CD_TERM2)    
* 
              TONG_CDEXPAN1 = 1.0D0 * TONG_CD * STATEV(191)
              TONG_CDEXPAN2 = 1.0D0 * TONG_CD * STATEV(192)
              TONG_CDEXPAN3 = 1.0D0 * TONG_CD * STATEV(193)
              TONG_CDEXPAN4 = 1.0D0 * TONG_CD * STATEV(194)
              TONG_CDEXPAN5 = 1.0D0 * TONG_CD * STATEV(195)
              TONG_CDEXPAN6 = 1.0D0 * TONG_CD * STATEV(196)
*     ----------------------------------------------------------------------------------      *   
          ENDIF    
*     ----------------------------------------------------------------------------------      *   
*         3.0 Shrinkage calculation
*     ----------------------------------------------------------------------------------      *   
*
*         SHRINKAGE CALCULATION: £¨1£© auto shrinkage + (2) drying shrinkage
*
          DELTA_SHRINKAGE = 0.0D0

*         TIME HISTORY
*         AT THE END OF CURRENT STEP
          TIME_DRY = TIME(2)
          TIME_S   = 0.0D0
*     
*         DRYING     
          IF(TIME_DRY.GT.0)THEN
              T_CURRENT  = TIME_DRY - TIME_ACT
              T_PREVIOUS = T_CURRENT - DTIME
              IF(T_PREVIOUS.LT.0.0D0)THEN
                  T_PREVIOUS =  0.0D0
              ENDIF
          ENDIF      
*
*         SHRINKAGE
*	
          IF(T_PREVIOUS.GT.0.0D0) THEN  
*          
              YU_ST_CURRENT  = TANH(SQRT( (TIME(2) - TIME_ACT) /YU_DRY_SHRIN))
              YU_ST_PREVIOUS = TANH(SQRT( (TIME(2)- TIME_ACT - 
     +                        DTIME)/YU_DRY_SHRIN))
*
*             1. Drying shrinkage
*
              YU_SHRINKAGE_1 =  YU_STRAIN_INFI*YU_KH*(YU_ST_CURRENT
     +                -YU_ST_PREVIOUS)
*
*       2. Auto shrinkage
*            
              YU_SHRINKAGE_21 =  YU_AUTOSTRAIN_INFI*
     +                (1+(YU_AUTOSTRAIN /(TIME(2)))**
     +                (YU_WATER_TO_CEMENT/0.38D0))**(-4.5D0)  
*      
              YU_SHRINKAGE_22=  YU_AUTOSTRAIN_INFI*
     +                (1+(YU_AUTOSTRAIN /(TIME(2)-DTIME))**
     +                (YU_WATER_TO_CEMENT/0.38D0))**(-4.5D0)
*    
              YU_SHRINKAGE_2  = YU_SHRINKAGE_21 - YU_SHRINKAGE_22
*
              DELTA_SHRINKAGE = YU_SHRINKAGE_2 + YU_SHRINKAGE_1
*
*             ISOTROPIC SHRINKAGE STRAIN
*         
              TONG_SHEXPAN1 = 1.0D0*DELTA_SHRINKAGE
              TONG_SHEXPAN2 = 1.0D0*DELTA_SHRINKAGE
              TONG_SHEXPAN3 = 1.0D0*DELTA_SHRINKAGE 
          ENDIF          
*     ----------------------------------------------------------------------------------      *   

          creep_ratio = STATEV(151)
          creep_ratio = MAX(1.0D0, creep_ratio)
          
          EXPAN(1) = TONG_CDEXPAN1 * creep_ratio * THETA_Cr + CREEP_STRAIN(1) + TONG_SHEXPAN1 * THETA_Sh
          EXPAN(2) = TONG_CDEXPAN2 * creep_ratio * THETA_Cr + CREEP_STRAIN(2) + TONG_SHEXPAN2 * THETA_Sh
          EXPAN(3) = TONG_CDEXPAN3 * creep_ratio * THETA_Cr + CREEP_STRAIN(3) + TONG_SHEXPAN3 * THETA_Sh
          EXPAN(4) = TONG_CDEXPAN4 * creep_ratio * THETA_Cr + CREEP_STRAIN(4)
          EXPAN(5) = TONG_CDEXPAN5 * creep_ratio * THETA_Cr + CREEP_STRAIN(5)
          EXPAN(6) = TONG_CDEXPAN6 * creep_ratio * THETA_Cr + CREEP_STRAIN(6)
          
*     ----------------------------------------------------------------------------------      *   
      end if

      

      
       

*     ----------------------------------------------------------------------------------      *
*     4.0 Calculate the relaxation of the steel strand
*     ----------------------------------------------------------------------------------      *        
      IF(NOEL.GE.Iden_Ele) THEN
*     ----------------------------------------------------------------------------------      *

*         END OF CURRENT STEP       
          TIME_RELAX = TIME(2) 
*         RELAXATION FOLLOWS CEB-FIP FORMULA
          IF(TIME(2) .LE. TIME_RE)THEN
              EXPAN(1) = 0.0D0
          ENDIF
        
        
          IF(TIME(2) .GT. TIME_RE) THEN
            ! TIME IN HOUR
	      T_CUR = (TIME_RELAX - TIME_RE)*24 
	      T_PRE = (TIME_RELAX - TIME_RE - DTIME)*24
	   
	      IF(T_PRE.LT.0.0D0) T_PRE = 0.0D0
	   
	      EXPAN(1) = STRAIN0 * RE_1000 * 
     +	    ((T_CUR / 1000)**0.12D0 - 
     +      (T_PRE / 1000)**0.12D0)  
            
            
             STATEV(200) =  STATEV(200) + EXPAN(1)

*
          ENDIF      
*     ----------------------------------------------------------------------------------      *
      END IF
*     ----------------------------------------------------------------------------------      *
         
            
            
*     ----------------------------------------------------------------------------------      *
      
	RETURN
      END
      
      
      
      
      
      
      
      
      SUBROUTINE Coeff_Random(THETA_1, THETA_2, THETA_3, 
     +                            THETA_4, THETA_5)
      
      INCLUDE 'ABA_PARAM.INC'
      
      DIMENSION Coeff_Latin(1000,5)
*     *********************
      II = 21
*     *********************   
*       Coeff_Latin(	1	,	:)	=	(/	1.0	,	1.0	,	1.0	,	1.0,	1.0/)
*
*       THETA_Cr, THETA_Sh, THETA_fc, THETA_Pre, THETA_H      
*
        Coeff_Latin(	1	,	:)	=	(/	0.805434794	,	0.806814113	,	0.889898578	,	1.095907195	,	1.127142905	/)
        Coeff_Latin(	2	,	:)	=	(/	0.84684516	,	1.4535344	,	1.322817762	,	1.11480106	,	0.933973386	/)
        Coeff_Latin(	3	,	:)	=	(/	0.548825213	,	1.277514606	,	0.710846314	,	0.976190174	,	0.943518325	/)
        Coeff_Latin(	4	,	:)	=	(/	0.673351726	,	0.323180272	,	1.240387217	,	0.998291023	,	0.861005046	/)
        Coeff_Latin(	5	,	:)	=	(/	1.008301964	,	1.408803064	,	0.887350411	,	0.93989929	,	1.269381717	/)
        Coeff_Latin(	6	,	:)	=	(/	0.884458057	,	0.722359004	,	0.565287064	,	0.897693933	,	1.316054319	/)
        Coeff_Latin(	7	,	:)	=	(/	1.173216693	,	1.719546388	,	1.242496343	,	0.997445142	,	1.138360105	/)
        Coeff_Latin(	8	,	:)	=	(/	0.627721535	,	1.648668756	,	0.962273042	,	0.852444261	,	1.058552021	/)
        Coeff_Latin(	9	,	:)	=	(/	1.060500741	,	1.584052009	,	0.76003171	,	0.960084615	,	1.084365731	/)
        Coeff_Latin(	10	,	:)	=	(/	1.147944245	,	1.648063818	,	0.635515827	,	0.942374984	,	1.16180726	/)
        Coeff_Latin(	11	,	:)	=	(/	0.919737535	,	0.737226294	,	1.102157332	,	1.101123292	,	1.25485713	/)
        Coeff_Latin(	12	,	:)	=	(/	0.908435195	,	0.935804365	,	1.092639999	,	0.992578737	,	1.193654754	/)
        Coeff_Latin(	13	,	:)	=	(/	0.939131712	,	1.521206374	,	1.005445182	,	1.018552991	,	1.122160645	/)
        Coeff_Latin(	14	,	:)	=	(/	0.833072127	,	0.988000318	,	1.006260033	,	1.108545661	,	0.539685247	/)
        Coeff_Latin(	15	,	:)	=	(/	0.94477466	,	0.930543042	,	1.0006694	,	1.018638004	,	0.93267641	/)
        Coeff_Latin(	16	,	:)	=	(/	1.098269666	,	1.222177762	,	1.122786038	,	0.905084454	,	0.934296401	/)
        Coeff_Latin(	17	,	:)	=	(/	1.09613239	,	1.353192837	,	0.930464367	,	0.897354495	,	0.992653718	/)
        Coeff_Latin(	18	,	:)	=	(/	1.209669224	,	1.01876418	,	0.928171189	,	0.948310736	,	0.550801819	/)
        Coeff_Latin(	19	,	:)	=	(/	0.857309423	,	1.028142851	,	0.88622076	,	1.008194401	,	0.915678127	/)
        Coeff_Latin(	20	,	:)	=	(/	0.953942157	,	0.710536864	,	0.833138758	,	1.095898982	,	0.963448448	/)
        Coeff_Latin(	21	,	:)	=	(/	0.868821742	,	0.928978443	,	1.266526278	,	1.003452848	,	0.866371386	/)
        Coeff_Latin(	22	,	:)	=	(/	0.890892226	,	0.908161437	,	1.162812592	,	1.133384176	,	1.07438172	/)
        Coeff_Latin(	23	,	:)	=	(/	0.560638593	,	1.090550958	,	1.025090327	,	0.960368398	,	0.97904478	/)
        Coeff_Latin(	24	,	:)	=	(/	1.324489229	,	0.321918278	,	0.820478333	,	1.014691308	,	0.646610953	/)
        Coeff_Latin(	25	,	:)	=	(/	0.887029535	,	0.46405379	,	0.747821678	,	1.001002626	,	1.174833994	/)
        Coeff_Latin(	26	,	:)	=	(/	1.013494903	,	0.829241839	,	0.934287849	,	0.967381075	,	0.93672774	/)
        Coeff_Latin(	27	,	:)	=	(/	1.069751367	,	0.834194156	,	1.010585457	,	1.030878379	,	1.032023081	/)
        Coeff_Latin(	28	,	:)	=	(/	0.989507112	,	0.486889329	,	1.011196199	,	0.931868311	,	0.987270905	/)
        Coeff_Latin(	29	,	:)	=	(/	1.330908511	,	1.589795596	,	1.036131023	,	0.973446256	,	0.992806625	/)
        Coeff_Latin(	30	,	:)	=	(/	0.420825388	,	0.301840378	,	0.96499641	,	1.031094083	,	1.253081744	/)
        Coeff_Latin(	31	,	:)	=	(/	0.92372086	,	1.30576559	,	1.237490612	,	1.03901306	,	1.328970177	/)
        Coeff_Latin(	32	,	:)	=	(/	1.133389582	,	1.196762346	,	1.068329917	,	0.948482211	,	0.963394701	/)
        Coeff_Latin(	33	,	:)	=	(/	0.712454889	,	0.360458792	,	1.097570548	,	0.927564327	,	0.988204742	/)
        Coeff_Latin(	34	,	:)	=	(/	1.029042015	,	1.160643441	,	0.948151242	,	0.937405387	,	1.093314537	/)
        Coeff_Latin(	35	,	:)	=	(/	0.797012595	,	0.88980809	,	0.718274022	,	0.974471313	,	1.35351578	/)
        Coeff_Latin(	36	,	:)	=	(/	1.33260844	,	1.208399658	,	0.998027865	,	1.089650531	,	1.599085882	/)
        Coeff_Latin(	37	,	:)	=	(/	1.037410515	,	0.856803477	,	0.836446719	,	0.986908566	,	1.252548745	/)
        Coeff_Latin(	38	,	:)	=	(/	1.079886477	,	0.997002334	,	1.147114966	,	1.073139962	,	1.230806234	/)
        Coeff_Latin(	39	,	:)	=	(/	0.844367494	,	0.81558766	,	1.319057055	,	1.016791586	,	0.932157535	/)
        Coeff_Latin(	40	,	:)	=	(/	0.859647769	,	1.468740892	,	1.346684432	,	1.0548019	,	0.85945674	/)
        Coeff_Latin(	41	,	:)	=	(/	0.902889082	,	0.541656394	,	0.914530904	,	0.941357162	,	0.747068733	/)
        Coeff_Latin(	42	,	:)	=	(/	1.396890587	,	0.796775949	,	0.971428938	,	1.005028443	,	1.210645405	/)
        Coeff_Latin(	43	,	:)	=	(/	0.967819306	,	0.996997148	,	0.921131388	,	0.963646588	,	0.886900821	/)
        Coeff_Latin(	44	,	:)	=	(/	1.271649025	,	0.930120797	,	0.798873517	,	0.949125053	,	1.087369091	/)
        Coeff_Latin(	45	,	:)	=	(/	1.519555282	,	0.815779148	,	0.811252868	,	0.976394226	,	1.167638472	/)
        Coeff_Latin(	46	,	:)	=	(/	1.154970831	,	0.808025538	,	1.078692446	,	1.049301682	,	0.805602415	/)
        Coeff_Latin(	47	,	:)	=	(/	0.810310394	,	1.448529395	,	1.023992477	,	1.065729838	,	0.795992078	/)
        Coeff_Latin(	48	,	:)	=	(/	1.032578784	,	1.26798869	,	0.865295589	,	0.963892581	,	1.022216376	/)
        Coeff_Latin(	49	,	:)	=	(/	0.9695648	,	0.891895906	,	1.045493858	,	1.019935642	,	1.503822792	/)
        Coeff_Latin(	50	,	:)	=	(/	0.903410191	,	0.747020125	,	0.846033125	,	1.000467068	,	0.928286297	/)
        Coeff_Latin(	51	,	:)	=	(/	1.085681259	,	1.193686172	,	1.127303707	,	0.978488555	,	0.779024288	/)
        Coeff_Latin(	52	,	:)	=	(/	1.197688184	,	0.949697443	,	1.152120927	,	1.077054108	,	0.975384129	/)
        Coeff_Latin(	53	,	:)	=	(/	0.901768251	,	1.282811645	,	1.188436545	,	0.999403424	,	0.854307402	/)
        Coeff_Latin(	54	,	:)	=	(/	1.255541787	,	0.799135638	,	1.032191945	,	0.960649174	,	1.170716705	/)
        Coeff_Latin(	55	,	:)	=	(/	0.771755545	,	1.237281854	,	0.890882514	,	0.994524954	,	1.061961511	/)
        Coeff_Latin(	56	,	:)	=	(/	1.244793012	,	0.995889088	,	0.971884175	,	1.069052704	,	1.061369056	/)
        Coeff_Latin(	57	,	:)	=	(/	1.126364028	,	0.990563308	,	0.874960858	,	0.985746725	,	0.653148606	/)
        Coeff_Latin(	58	,	:)	=	(/	0.998909811	,	1.615647371	,	1.013156758	,	1.06657015	,	1.044487431	/)
        Coeff_Latin(	59	,	:)	=	(/	0.796473241	,	1.107769625	,	1.071292211	,	0.988337262	,	0.654328281	/)
        Coeff_Latin(	60	,	:)	=	(/	1.112313966	,	1.109232337	,	0.889549197	,	1.041926525	,	1.080428037	/)
        Coeff_Latin(	61	,	:)	=	(/	0.948758747	,	1.023889296	,	1.154971936	,	1.056534136	,	1.497687363	/)
        Coeff_Latin(	62	,	:)	=	(/	0.955859152	,	1.192275263	,	1.209374677	,	0.96536357	,	1.262650553	/)
        Coeff_Latin(	63	,	:)	=	(/	1.027958184	,	1.205676924	,	0.955419351	,	0.960245623	,	0.854715798	/)
        Coeff_Latin(	64	,	:)	=	(/	0.915852805	,	1.125673417	,	1.09056654	,	1.018827686	,	1.175005219	/)
        Coeff_Latin(	65	,	:)	=	(/	0.97311553	,	1.854144153	,	1.009406123	,	0.962346409	,	0.836086443	/)
        Coeff_Latin(	66	,	:)	=	(/	0.973491428	,	1.172019075	,	0.986399056	,	0.966450739	,	0.689362452	/)
        Coeff_Latin(	67	,	:)	=	(/	0.840792031	,	1.086517858	,	0.702150101	,	0.983315139	,	0.94248525	/)
        Coeff_Latin(	68	,	:)	=	(/	1.14161159	,	0.888965586	,	1.119796801	,	1.000154247	,	0.989385221	/)
        Coeff_Latin(	69	,	:)	=	(/	0.47107758	,	0.631659963	,	0.912601226	,	1.026709429	,	0.628304832	/)
        Coeff_Latin(	70	,	:)	=	(/	0.999437002	,	0.899211432	,	0.987453613	,	1.013049085	,	0.747815074	/)
        Coeff_Latin(	71	,	:)	=	(/	0.996798047	,	1.178438363	,	0.888247172	,	1.015552277	,	0.98227541	/)
        Coeff_Latin(	72	,	:)	=	(/	1.015826579	,	1.713307919	,	1.267486095	,	1.027807652	,	0.975787367	/)
        Coeff_Latin(	73	,	:)	=	(/	1.201684232	,	1.126054443	,	0.877233053	,	1.019980336	,	1.229628004	/)
        Coeff_Latin(	74	,	:)	=	(/	0.796726509	,	1.030163408	,	0.978919953	,	1.00052352	,	0.962639006	/)
        Coeff_Latin(	75	,	:)	=	(/	0.37621724	,	0.544427271	,	1.173985765	,	1.030821929	,	0.769174843	/)
        Coeff_Latin(	76	,	:)	=	(/	1.382758963	,	1.065145334	,	0.98829221	,	1.094642558	,	1.02821314	/)
        Coeff_Latin(	77	,	:)	=	(/	0.894429381	,	1.247949986	,	0.952928222	,	1.054694779	,	0.872252852	/)
        Coeff_Latin(	78	,	:)	=	(/	0.940051107	,	0.916966815	,	1.160228525	,	1.013883021	,	0.918570378	/)
        Coeff_Latin(	79	,	:)	=	(/	1.580106634	,	1.509705477	,	1.186028295	,	0.913352612	,	0.929850421	/)
        Coeff_Latin(	80	,	:)	=	(/	1.131495723	,	1.118051397	,	1.366978302	,	0.927564818	,	0.805363144	/)
        Coeff_Latin(	81	,	:)	=	(/	1.171122569	,	0.849939138	,	1.127899609	,	0.966144707	,	0.918356337	/)
        Coeff_Latin(	82	,	:)	=	(/	0.722562156	,	1.540344394	,	0.945320372	,	0.913921355	,	1.208046871	/)
        Coeff_Latin(	83	,	:)	=	(/	1.000658753	,	1.168907504	,	1.220950887	,	0.988716594	,	0.809627451	/)
        Coeff_Latin(	84	,	:)	=	(/	0.859995561	,	0.485392744	,	1.025151569	,	1.008863334	,	1.127051176	/)
        Coeff_Latin(	85	,	:)	=	(/	1.167789389	,	1.123403115	,	0.989859517	,	1.016975638	,	1.194424082	/)
        Coeff_Latin(	86	,	:)	=	(/	0.673099464	,	0.664544501	,	1.103144559	,	0.94760473	,	0.842936468	/)
        Coeff_Latin(	87	,	:)	=	(/	1.289880667	,	0.78700055	,	1.141207431	,	0.939799483	,	1.334580877	/)
        Coeff_Latin(	88	,	:)	=	(/	0.619645651	,	0.842862722	,	0.838373117	,	1.023688255	,	1.175838225	/)
        Coeff_Latin(	89	,	:)	=	(/	0.99817628	,	0.828271994	,	0.850148213	,	0.927206619	,	1.472537216	/)
        Coeff_Latin(	90	,	:)	=	(/	0.605972125	,	0.687275679	,	0.890046646	,	1.046758434	,	0.534794657	/)
        Coeff_Latin(	91	,	:)	=	(/	1.285966203	,	0.575888173	,	0.94016629	,	1.064797946	,	0.921418953	/)
        Coeff_Latin(	92	,	:)	=	(/	1.065283959	,	1.078075135	,	1.158103749	,	0.930860206	,	0.713143597	/)
        Coeff_Latin(	93	,	:)	=	(/	1.000090678	,	0.989711688	,	1.041392141	,	0.987443562	,	0.809817503	/)
        Coeff_Latin(	94	,	:)	=	(/	0.926128511	,	1.576403998	,	0.897160804	,	1.01419709	,	0.972009521	/)
        Coeff_Latin(	95	,	:)	=	(/	0.80828906	,	0.306379023	,	0.785832674	,	0.925748006	,	1.096595499	/)
        Coeff_Latin(	96	,	:)	=	(/	1.115817586	,	1.489077455	,	0.894531607	,	1.002781434	,	0.910811548	/)
        Coeff_Latin(	97	,	:)	=	(/	1.320312237	,	1.346096497	,	1.100839303	,	0.961716647	,	0.858070643	/)
        Coeff_Latin(	98	,	:)	=	(/	1.051937985	,	0.92757479	,	1.030700174	,	1.078404124	,	0.949605764	/)
        Coeff_Latin(	99	,	:)	=	(/	1.178118514	,	1.105082279	,	0.753910247	,	1.067805381	,	1.042782581	/)
        Coeff_Latin(	100	,	:)	=	(/	0.847381697	,	0.756410769	,	0.974072901	,	1.077409512	,	1.098752476	/)
        Coeff_Latin(	101	,	:)	=	(/	1.165275957	,	1.079492598	,	0.669272607	,	0.949516882	,	1.038335319	/)
        Coeff_Latin(	102	,	:)	=	(/	1.10976695	,	0.897907742	,	1.141175613	,	0.953659204	,	0.8318219	/)
        Coeff_Latin(	103	,	:)	=	(/	1.186881949	,	0.682303166	,	1.205995091	,	1.000946834	,	1.122493729	/)
        Coeff_Latin(	104	,	:)	=	(/	0.746775799	,	0.982593964	,	0.949029335	,	1.07131819	,	0.978971305	/)
        Coeff_Latin(	105	,	:)	=	(/	0.331456109	,	1.246371452	,	1.027346228	,	0.981407695	,	0.756047692	/)
        Coeff_Latin(	106	,	:)	=	(/	1.61529314	,	1.344230124	,	0.852252812	,	1.071157812	,	0.918020055	/)
        Coeff_Latin(	107	,	:)	=	(/	1.069828982	,	1.463684439	,	0.975127483	,	0.910649574	,	0.964419721	/)
        Coeff_Latin(	108	,	:)	=	(/	1.307302453	,	0.746773869	,	0.763460835	,	1.03359563	,	1.038994166	/)
        Coeff_Latin(	109	,	:)	=	(/	0.79637003	,	1.193446501	,	1.029031376	,	1.041630463	,	0.861234095	/)
        Coeff_Latin(	110	,	:)	=	(/	1.219623412	,	0.916929387	,	1.101706867	,	1.079870438	,	0.967061731	/)
        Coeff_Latin(	111	,	:)	=	(/	0.925412934	,	0.584647055	,	1.042792559	,	1.012864406	,	0.954973521	/)
        Coeff_Latin(	112	,	:)	=	(/	0.868055194	,	1.304762727	,	0.985421337	,	1.078479739	,	0.749187461	/)
        Coeff_Latin(	113	,	:)	=	(/	0.799678435	,	0.732462506	,	0.671250293	,	0.997204932	,	1.007892262	/)
        Coeff_Latin(	114	,	:)	=	(/	1.344580953	,	0.177506709	,	1.061745965	,	0.992125699	,	0.977227175	/)
        Coeff_Latin(	115	,	:)	=	(/	1.263179045	,	1.109372313	,	0.955128631	,	0.985062302	,	0.946281392	/)
        Coeff_Latin(	116	,	:)	=	(/	1.030902688	,	1.056080918	,	0.974792126	,	0.987810817	,	1.033735838	/)
        Coeff_Latin(	117	,	:)	=	(/	1.23589088	,	1.236678395	,	1.035170482	,	0.992905951	,	0.972844042	/)
        Coeff_Latin(	118	,	:)	=	(/	1.175136172	,	1.024682816	,	0.832425417	,	1.000027318	,	0.788051527	/)
        Coeff_Latin(	119	,	:)	=	(/	1.367634416	,	0.673474752	,	1.1449616	,	0.944278925	,	0.978293978	/)
        Coeff_Latin(	120	,	:)	=	(/	1.070556451	,	0.817535545	,	0.836439588	,	0.922667474	,	1.086026489	/)
        Coeff_Latin(	121	,	:)	=	(/	0.939448871	,	0.988715923	,	1.08196589	,	0.976680894	,	1.215106983	/)
        Coeff_Latin(	122	,	:)	=	(/	0.758058355	,	0.577799257	,	1.239920028	,	0.992165698	,	1.396336102	/)
        Coeff_Latin(	123	,	:)	=	(/	1.202818591	,	0.905404303	,	1.190526904	,	1.03931866	,	0.964644991	/)
        Coeff_Latin(	124	,	:)	=	(/	0.883507145	,	0.856969968	,	0.995538838	,	0.950071599	,	0.879919405	/)
        Coeff_Latin(	125	,	:)	=	(/	0.903516866	,	0.69265546	,	0.974435704	,	1.049381319	,	0.937197277	/)
        Coeff_Latin(	126	,	:)	=	(/	0.930800955	,	0.710775698	,	1.105529172	,	0.919849542	,	0.882912523	/)
        Coeff_Latin(	127	,	:)	=	(/	1.429425992	,	0.857340893	,	1.061367737	,	1.0372708	,	0.773115783	/)
        Coeff_Latin(	128	,	:)	=	(/	0.816158602	,	0.97677528	,	0.995607765	,	0.933971103	,	1.076127146	/)
        Coeff_Latin(	129	,	:)	=	(/	1.398793843	,	0.930441938	,	1.151562716	,	0.958740052	,	1.132000487	/)
        Coeff_Latin(	130	,	:)	=	(/	1.054391255	,	0.993483588	,	0.83062609	,	0.933246351	,	1.00764186	/)
        Coeff_Latin(	131	,	:)	=	(/	0.90775505	,	0.726761476	,	1.02806335	,	1.003602064	,	0.985724066	/)
        Coeff_Latin(	132	,	:)	=	(/	0.845341764	,	0.715987583	,	1.156060388	,	1.016276386	,	1.095242269	/)
        Coeff_Latin(	133	,	:)	=	(/	0.786852713	,	0.768448692	,	0.919140655	,	0.960098877	,	1.10972745	/)
        Coeff_Latin(	134	,	:)	=	(/	0.924275747	,	0.963917619	,	0.860698199	,	0.974703082	,	0.901430865	/)
        Coeff_Latin(	135	,	:)	=	(/	1.060033879	,	0.781224298	,	1.25706183	,	0.991757908	,	0.86162492	/)
        Coeff_Latin(	136	,	:)	=	(/	0.684404155	,	0.542939652	,	1.140536217	,	0.909471946	,	0.930483165	/)
        Coeff_Latin(	137	,	:)	=	(/	1.286878407	,	1.618414336	,	1.060236542	,	1.014160626	,	1.311786405	/)
        Coeff_Latin(	138	,	:)	=	(/	1.211597425	,	1.340045086	,	0.753581805	,	0.955108671	,	1.103655671	/)
        Coeff_Latin(	139	,	:)	=	(/	0.891289198	,	0.792760917	,	1.008918737	,	1.03335383	,	0.986323301	/)
        Coeff_Latin(	140	,	:)	=	(/	1.438887889	,	0.962525678	,	0.965108845	,	1.009098422	,	0.979280793	/)
        Coeff_Latin(	141	,	:)	=	(/	0.528878802	,	0.65890532	,	0.997108457	,	0.977470932	,	1.159802758	/)
        Coeff_Latin(	142	,	:)	=	(/	1.08542852	,	1.436703875	,	0.899260848	,	0.978604743	,	0.918849273	/)
        Coeff_Latin(	143	,	:)	=	(/	0.8601637	,	1.169940948	,	0.943030845	,	1.124974372	,	0.779141413	/)
        Coeff_Latin(	144	,	:)	=	(/	0.749282647	,	1.320157038	,	1.243128106	,	0.9803475	,	1.00200175	/)
        Coeff_Latin(	145	,	:)	=	(/	1.145540595	,	0.751347642	,	0.85977963	,	1.089784803	,	0.887517241	/)
        Coeff_Latin(	146	,	:)	=	(/	0.882978031	,	1.12829634	,	1.351635642	,	1.022278134	,	1.241239514	/)
        Coeff_Latin(	147	,	:)	=	(/	1.2280201	,	0.825527336	,	0.989104735	,	0.926705327	,	0.977896697	/)
        Coeff_Latin(	148	,	:)	=	(/	0.603908414	,	1.09579254	,	0.99427708	,	0.942557943	,	1.209950052	/)
        Coeff_Latin(	149	,	:)	=	(/	1.127862928	,	0.870739786	,	1.023882497	,	1.019013972	,	1.084421486	/)
        Coeff_Latin(	150	,	:)	=	(/	0.868826233	,	1.16298378	,	1.033164788	,	0.994960931	,	1.493086784	/)
        Coeff_Latin(	151	,	:)	=	(/	0.939332264	,	1.550403134	,	1.181842389	,	0.984092355	,	0.93329521	/)
        Coeff_Latin(	152	,	:)	=	(/	0.833711248	,	1.142914571	,	0.741773115	,	0.964843776	,	1.274680142	/)
        Coeff_Latin(	153	,	:)	=	(/	1.275158689	,	0.91308666	,	1.052821638	,	0.996845033	,	0.897291288	/)
        Coeff_Latin(	154	,	:)	=	(/	0.971365987	,	0.884141357	,	1.009983362	,	1.041477412	,	1.061001747	/)
        Coeff_Latin(	155	,	:)	=	(/	0.965962195	,	0.443986879	,	0.912906913	,	1.051371156	,	1.203386784	/)
        Coeff_Latin(	156	,	:)	=	(/	0.848944625	,	0.998313031	,	0.99954564	,	1.020761949	,	1.151016815	/)
        Coeff_Latin(	157	,	:)	=	(/	1.0792218	,	1.106965685	,	1.194965958	,	0.986772211	,	1.428373248	/)
        Coeff_Latin(	158	,	:)	=	(/	0.975134212	,	0.608326417	,	0.879554724	,	0.984635849	,	0.955535039	/)
        Coeff_Latin(	159	,	:)	=	(/	1.010111831	,	0.485831246	,	0.893278233	,	1.107220841	,	0.973421133	/)
        Coeff_Latin(	160	,	:)	=	(/	1.493868363	,	1.234231949	,	1.045984031	,	0.928440003	,	0.707506599	/)
        Coeff_Latin(	161	,	:)	=	(/	1.096697567	,	1.418125937	,	0.938558123	,	0.977922116	,	0.81067408	/)
        Coeff_Latin(	162	,	:)	=	(/	0.819495952	,	1.475046719	,	1.0194835	,	0.950525769	,	0.896461525	/)
        Coeff_Latin(	163	,	:)	=	(/	1.098032234	,	1.045941798	,	1.09765947	,	0.986134629	,	1.262437734	/)
        Coeff_Latin(	164	,	:)	=	(/	1.13313907	,	0.938433099	,	0.994752322	,	0.958827295	,	1.380095483	/)
        Coeff_Latin(	165	,	:)	=	(/	1.141773836	,	0.726244023	,	1.421156285	,	1.038266056	,	0.943912315	/)
        Coeff_Latin(	166	,	:)	=	(/	0.772470941	,	0.669953729	,	1.253475406	,	1.006701217	,	1.319202309	/)
        Coeff_Latin(	167	,	:)	=	(/	1.034797044	,	1.205630891	,	0.837226057	,	0.97352193	,	1.053857096	/)
        Coeff_Latin(	168	,	:)	=	(/	1.082447195	,	0.961969837	,	0.94971822	,	0.87790901	,	0.703640413	/)
        Coeff_Latin(	169	,	:)	=	(/	0.961885529	,	1.472168489	,	0.975517307	,	1.066997098	,	0.992616697	/)
        Coeff_Latin(	170	,	:)	=	(/	1.024834656	,	0.735916252	,	0.908743308	,	1.024939231	,	1.195667226	/)
        Coeff_Latin(	171	,	:)	=	(/	0.745431566	,	1.684933206	,	0.78711431	,	0.975556152	,	1.040080893	/)
        Coeff_Latin(	172	,	:)	=	(/	1.334646505	,	0.91699592	,	1.035886861	,	0.998407207	,	1.293086777	/)
        Coeff_Latin(	173	,	:)	=	(/	1.057986436	,	1.165695059	,	1.116950711	,	1.044563842	,	0.534612076	/)
        Coeff_Latin(	174	,	:)	=	(/	1.045507141	,	0.375132683	,	0.903523685	,	0.99402067	,	0.708541616	/)
        Coeff_Latin(	175	,	:)	=	(/	1.103378381	,	1.207266481	,	1.236513119	,	1.017203715	,	1.18158191	/)
        Coeff_Latin(	176	,	:)	=	(/	0.833883397	,	0.386090776	,	1.101609309	,	1.14326353	,	0.999975197	/)
        Coeff_Latin(	177	,	:)	=	(/	1.070181934	,	1.681074579	,	1.053433453	,	1.014535008	,	1.069948243	/)
        Coeff_Latin(	178	,	:)	=	(/	0.634189703	,	0.910889524	,	0.896198286	,	1.015979197	,	1.094304073	/)
        Coeff_Latin(	179	,	:)	=	(/	1.020023038	,	0.609423482	,	0.773661176	,	0.996005885	,	1.042264838	/)
        Coeff_Latin(	180	,	:)	=	(/	0.956309444	,	0.860213709	,	0.949105936	,	1.028214789	,	0.990889689	/)
        Coeff_Latin(	181	,	:)	=	(/	0.828472864	,	0.609379589	,	0.882337695	,	1.013775182	,	0.933028124	/)
        Coeff_Latin(	182	,	:)	=	(/	1.036972878	,	0.836571364	,	1.209537231	,	1.063194642	,	0.666985082	/)
        Coeff_Latin(	183	,	:)	=	(/	0.735321875	,	0.526684518	,	0.873434507	,	1.024283157	,	1.055457514	/)
        Coeff_Latin(	184	,	:)	=	(/	0.740276752	,	1.373516441	,	0.836969747	,	1.065440571	,	1.1895044	/)
        Coeff_Latin(	185	,	:)	=	(/	1.083019123	,	0.894607935	,	0.820781005	,	1.052805169	,	0.684722642	/)
        Coeff_Latin(	186	,	:)	=	(/	0.793650524	,	1.194248359	,	0.993778932	,	0.983526473	,	1.036770899	/)
        Coeff_Latin(	187	,	:)	=	(/	1.140181005	,	1.300959091	,	1.211730811	,	0.911933661	,	0.753029791	/)
        Coeff_Latin(	188	,	:)	=	(/	1.083486059	,	1.30329785	,	1.114344547	,	1.036232206	,	1.326999909	/)
        Coeff_Latin(	189	,	:)	=	(/	0.938492704	,	0.764196475	,	0.813053014	,	1.000158758	,	1.012921456	/)
        Coeff_Latin(	190	,	:)	=	(/	1.075894201	,	0.963042901	,	1.113310908	,	1.011227557	,	0.969066402	/)
        Coeff_Latin(	191	,	:)	=	(/	1.144342281	,	1.076393006	,	0.951523646	,	0.988731106	,	1.361880855	/)
        Coeff_Latin(	192	,	:)	=	(/	1.053896281	,	1.419661721	,	1.005911801	,	1.019654002	,	0.998607122	/)
        Coeff_Latin(	193	,	:)	=	(/	0.99078751	,	0.602263427	,	1.02429358	,	0.977603489	,	0.888973707	/)
        Coeff_Latin(	194	,	:)	=	(/	0.477449868	,	1.408650072	,	0.90718452	,	0.964990373	,	0.96139508	/)
        Coeff_Latin(	195	,	:)	=	(/	1.228648317	,	1.128377758	,	0.892635315	,	1.124118727	,	1.097235922	/)
        Coeff_Latin(	196	,	:)	=	(/	1.309236985	,	0.494175403	,	0.925709673	,	0.96347406	,	1.161302406	/)
        Coeff_Latin(	197	,	:)	=	(/	1.379119523	,	0.725006791	,	1.181698115	,	0.959363508	,	0.745104324	/)
        Coeff_Latin(	198	,	:)	=	(/	1.069224561	,	0.517755308	,	1.072316992	,	0.996634638	,	0.907808513	/)
        Coeff_Latin(	199	,	:)	=	(/	0.553525607	,	1.107453661	,	0.893793291	,	0.999142296	,	0.903319523	/)
        Coeff_Latin(	200	,	:)	=	(/	1.342100298	,	1.024128417	,	0.850284661	,	0.974843103	,	0.621645855	/)
        Coeff_Latin(	201	,	:)	=	(/	0.844133595	,	1.796406147	,	0.993687384	,	1.003647234	,	0.590426539	/)
        Coeff_Latin(	202	,	:)	=	(/	1.188964467	,	0.979476892	,	0.914402218	,	1.020899102	,	0.73161761	/)
        Coeff_Latin(	203	,	:)	=	(/	1.000413196	,	0.885787655	,	1.136187388	,	0.921573957	,	0.96833625	/)
        Coeff_Latin(	204	,	:)	=	(/	0.97332655	,	1.420075391	,	1.122116461	,	0.947022816	,	1.191944299	/)
        Coeff_Latin(	205	,	:)	=	(/	0.821404519	,	0.188913911	,	0.852934585	,	1.020278245	,	1.034847368	/)
        Coeff_Latin(	206	,	:)	=	(/	0.750296015	,	1.800118481	,	1.159794898	,	0.995255327	,	0.74442086	/)
        Coeff_Latin(	207	,	:)	=	(/	1.136140099	,	0.767822893	,	0.960879583	,	1.025251274	,	0.792453067	/)
        Coeff_Latin(	208	,	:)	=	(/	0.787504974	,	0.87254876	,	0.812073987	,	0.930210164	,	1.27776853	/)
        Coeff_Latin(	209	,	:)	=	(/	0.559548814	,	1.085325396	,	0.971533394	,	0.945050102	,	1.140279763	/)
        Coeff_Latin(	210	,	:)	=	(/	0.903090344	,	0.533011079	,	1.093012245	,	0.926579308	,	1.14342453	/)
        Coeff_Latin(	211	,	:)	=	(/	0.858509808	,	0.954285226	,	1.023095917	,	1.039269401	,	0.682540766	/)
        Coeff_Latin(	212	,	:)	=	(/	1.033700133	,	1.041245228	,	0.894612725	,	0.98037556	,	0.684480515	/)
        Coeff_Latin(	213	,	:)	=	(/	1.024845779	,	1.012360926	,	0.954916683	,	0.992047297	,	1.251679005	/)
        Coeff_Latin(	214	,	:)	=	(/	0.431320216	,	1.26999757	,	1.241315024	,	1.041887473	,	0.993505395	/)
        Coeff_Latin(	215	,	:)	=	(/	0.831100834	,	0.85002967	,	1.029133057	,	1.017123013	,	1.09745022	/)
        Coeff_Latin(	216	,	:)	=	(/	1.432045226	,	0.75036821	,	0.635196406	,	1.0775898	,	0.984596305	/)
        Coeff_Latin(	217	,	:)	=	(/	1.048832097	,	0.931300429	,	1.358922364	,	0.947723185	,	0.812170977	/)
        Coeff_Latin(	218	,	:)	=	(/	0.808240331	,	0.786866293	,	1.005851396	,	0.984906289	,	0.817964011	/)
        Coeff_Latin(	219	,	:)	=	(/	1.107488295	,	0.706467048	,	0.851018612	,	1.023265719	,	0.797144563	/)
        Coeff_Latin(	220	,	:)	=	(/	0.945080613	,	1.148620193	,	1.055045701	,	0.984278274	,	1.128424865	/)
        Coeff_Latin(	221	,	:)	=	(/	1.088309115	,	0.920185084	,	1.060965403	,	1.058886301	,	0.815506326	/)
        Coeff_Latin(	222	,	:)	=	(/	1.270117007	,	1.482527913	,	0.947733656	,	1.058229629	,	1.016639808	/)
        Coeff_Latin(	223	,	:)	=	(/	0.936123897	,	1.363191982	,	1.155408447	,	0.99179312	,	1.183602716	/)
        Coeff_Latin(	224	,	:)	=	(/	1.121047325	,	1.018951959	,	1.041639434	,	1.007074054	,	1.247613812	/)
        Coeff_Latin(	225	,	:)	=	(/	1.051154025	,	1.021744129	,	1.020627225	,	1.092728076	,	1.190895876	/)
        Coeff_Latin(	226	,	:)	=	(/	0.917634032	,	0.846336649	,	0.78321711	,	1.023905318	,	0.73516673	/)
        Coeff_Latin(	227	,	:)	=	(/	1.020188496	,	0.723152924	,	1.101728514	,	1.099150761	,	1.437277669	/)
        Coeff_Latin(	228	,	:)	=	(/	0.980023442	,	1.594005529	,	1.140014192	,	1.046391743	,	1.0219507	/)
        Coeff_Latin(	229	,	:)	=	(/	0.880815105	,	0.549249484	,	0.725644137	,	1.070983989	,	1.346153565	/)
        Coeff_Latin(	230	,	:)	=	(/	1.053518785	,	0.840203205	,	1.121941721	,	0.983455631	,	0.796580807	/)
        Coeff_Latin(	231	,	:)	=	(/	1.112135607	,	0.790994003	,	0.646850582	,	1.013631268	,	1.237243671	/)
        Coeff_Latin(	232	,	:)	=	(/	1.160270772	,	0.937464687	,	1.024759298	,	1.06117423	,	1.218674508	/)
        Coeff_Latin(	233	,	:)	=	(/	1.159857011	,	0.983786146	,	1.04168016	,	0.995993509	,	0.949005114	/)
        Coeff_Latin(	234	,	:)	=	(/	1.191337174	,	0.860546731	,	1.089739012	,	0.982070508	,	0.97589061	/)
        Coeff_Latin(	235	,	:)	=	(/	0.740517858	,	0.566553842	,	0.891709308	,	1.050424344	,	0.983621826	/)
        Coeff_Latin(	236	,	:)	=	(/	0.691846771	,	1.222840564	,	0.937849553	,	0.946676262	,	0.962724586	/)
        Coeff_Latin(	237	,	:)	=	(/	0.995454632	,	0.499400716	,	1.085373675	,	0.982130891	,	1.05022353	/)
        Coeff_Latin(	238	,	:)	=	(/	1.365639147	,	1.53176324	,	0.868945015	,	1.012345345	,	0.8005533	/)
        Coeff_Latin(	239	,	:)	=	(/	1.033545267	,	1.476846333	,	1.318448132	,	1.058280895	,	0.847681578	/)
        Coeff_Latin(	240	,	:)	=	(/	0.904254127	,	0.685252972	,	1.054456199	,	0.950441778	,	0.927410955	/)
        Coeff_Latin(	241	,	:)	=	(/	1.064148114	,	1.12097568	,	1.129147379	,	0.956458386	,	0.847107125	/)
        Coeff_Latin(	242	,	:)	=	(/	1.200968518	,	1.006336979	,	1.216083789	,	0.946752594	,	1.157463028	/)
        Coeff_Latin(	243	,	:)	=	(/	1.287169953	,	1.263591323	,	1.102835585	,	0.98407607	,	1.039548004	/)
        Coeff_Latin(	244	,	:)	=	(/	1.433391975	,	1.168652698	,	0.64946594	,	1.018548261	,	0.963549367	/)
        Coeff_Latin(	245	,	:)	=	(/	1.021562355	,	1.06567511	,	0.887683907	,	1.051648178	,	1.224211874	/)
        Coeff_Latin(	246	,	:)	=	(/	0.859140251	,	1.124807569	,	1.210364291	,	1.044012668	,	0.85544672	/)
        Coeff_Latin(	247	,	:)	=	(/	0.535671853	,	1.542160547	,	0.894921235	,	1.033878275	,	1.198230346	/)
        Coeff_Latin(	248	,	:)	=	(/	0.731272117	,	0.941155884	,	0.870625081	,	1.014171373	,	0.656085984	/)
        Coeff_Latin(	249	,	:)	=	(/	1.374539307	,	0.949622409	,	0.800466026	,	1.021490774	,	1.230936406	/)
        Coeff_Latin(	250	,	:)	=	(/	0.862117946	,	0.586118152	,	0.797320359	,	0.970247337	,	1.179574094	/)
        Coeff_Latin(	251	,	:)	=	(/	1.094722024	,	0.90734011	,	0.79365718	,	0.954761077	,	1.065247812	/)
        Coeff_Latin(	252	,	:)	=	(/	1.113665113	,	1.000782996	,	0.789671296	,	1.044642848	,	1.253803011	/)
        Coeff_Latin(	253	,	:)	=	(/	0.690354535	,	1.050193262	,	0.86673918	,	0.97995201	,	0.76156661	/)
        Coeff_Latin(	254	,	:)	=	(/	1.047177787	,	0.73779561	,	0.940635863	,	0.957709102	,	0.470907184	/)
        Coeff_Latin(	255	,	:)	=	(/	0.69429427	,	0.615393568	,	1.18056893	,	1.011469511	,	0.947217196	/)
        Coeff_Latin(	256	,	:)	=	(/	0.95519743	,	1.183772726	,	0.72157046	,	0.941297584	,	0.901943785	/)
        Coeff_Latin(	257	,	:)	=	(/	0.904153131	,	0.638270824	,	0.999004848	,	1.022229651	,	0.735019935	/)
        Coeff_Latin(	258	,	:)	=	(/	1.12862839	,	0.951533463	,	0.69156424	,	1.089521712	,	1.046300319	/)
        Coeff_Latin(	259	,	:)	=	(/	0.880112454	,	1.165303624	,	0.823398465	,	1.086021182	,	1.005385466	/)
        Coeff_Latin(	260	,	:)	=	(/	0.440948003	,	0.729217195	,	0.848760987	,	0.951111746	,	1.18652788	/)
        Coeff_Latin(	261	,	:)	=	(/	0.823139893	,	1.162345367	,	1.04327337	,	1.127202298	,	0.822604909	/)
        Coeff_Latin(	262	,	:)	=	(/	0.707258807	,	1.276030237	,	0.770493214	,	1.02295767	,	0.929035687	/)
        Coeff_Latin(	263	,	:)	=	(/	0.970537136	,	1.085531254	,	0.915995486	,	1.021230645	,	1.075763871	/)
        Coeff_Latin(	264	,	:)	=	(/	1.060617001	,	0.552414658	,	0.896242653	,	0.985155287	,	1.046957766	/)
        Coeff_Latin(	265	,	:)	=	(/	0.889982742	,	0.60974777	,	1.022614037	,	0.93789468	,	1.120943728	/)
        Coeff_Latin(	266	,	:)	=	(/	0.951451563	,	0.711483018	,	1.278824328	,	0.999771921	,	1.221828199	/)
        Coeff_Latin(	267	,	:)	=	(/	0.912796149	,	1.133262127	,	0.805544109	,	0.951973242	,	0.923520724	/)
        Coeff_Latin(	268	,	:)	=	(/	1.115674041	,	1.255295055	,	0.958669692	,	0.979722479	,	0.788419938	/)
        Coeff_Latin(	269	,	:)	=	(/	1.050315221	,	1.385380386	,	0.999940096	,	1.021838582	,	0.823592696	/)
        Coeff_Latin(	270	,	:)	=	(/	1.041695643	,	1.635471316	,	0.833289853	,	1.086952004	,	1.429925835	/)
        Coeff_Latin(	271	,	:)	=	(/	0.810889474	,	1.311679311	,	1.005862706	,	0.975068179	,	1.169924985	/)
        Coeff_Latin(	272	,	:)	=	(/	0.893140642	,	1.105383294	,	1.154837397	,	0.967901437	,	1.351324826	/)
        Coeff_Latin(	273	,	:)	=	(/	0.905282611	,	1.376745395	,	1.01011727	,	0.93293547	,	0.824366518	/)
        Coeff_Latin(	274	,	:)	=	(/	0.796209767	,	0.976457497	,	0.850119352	,	1.045720543	,	1.51813942	/)
        Coeff_Latin(	275	,	:)	=	(/	1.09265411	,	0.908929042	,	0.977369386	,	0.99462397	,	1.13491967	/)
        Coeff_Latin(	276	,	:)	=	(/	1.065516876	,	1.581805188	,	1.070549411	,	0.939438045	,	0.885128033	/)
        Coeff_Latin(	277	,	:)	=	(/	0.903296497	,	1.129031854	,	1.033378893	,	1.059932171	,	1.091746635	/)
        Coeff_Latin(	278	,	:)	=	(/	0.662637102	,	1.121099103	,	0.779108349	,	0.930735657	,	1.10082346	/)
        Coeff_Latin(	279	,	:)	=	(/	1.154404067	,	0.629677565	,	1.087658768	,	1.029390036	,	0.875449474	/)
        Coeff_Latin(	280	,	:)	=	(/	0.883128922	,	0.976453942	,	0.807109908	,	0.965428606	,	1.082798635	/)
        Coeff_Latin(	281	,	:)	=	(/	1.21566859	,	0.540442414	,	1.11098463	,	1.016318966	,	0.97674541	/)
        Coeff_Latin(	282	,	:)	=	(/	1.031949209	,	1.223747097	,	0.985069792	,	0.951254955	,	1.238154175	/)
        Coeff_Latin(	283	,	:)	=	(/	0.645990398	,	0.824957809	,	1.12142704	,	1.002908134	,	0.926369064	/)
        Coeff_Latin(	284	,	:)	=	(/	1.329802714	,	0.764338035	,	0.703725045	,	1.025161668	,	1.254787172	/)
        Coeff_Latin(	285	,	:)	=	(/	0.639746582	,	0.452189473	,	0.849363097	,	0.981425122	,	0.879855914	/)
        Coeff_Latin(	286	,	:)	=	(/	0.974614724	,	1.196936362	,	0.762142163	,	0.923415383	,	0.716596062	/)
        Coeff_Latin(	287	,	:)	=	(/	0.792333144	,	0.914369123	,	0.864835832	,	0.988573453	,	0.948781619	/)
        Coeff_Latin(	288	,	:)	=	(/	1.153834292	,	1.472590828	,	0.931945748	,	1.041537738	,	0.888699218	/)
        Coeff_Latin(	289	,	:)	=	(/	1.076168054	,	0.940811331	,	0.904580904	,	1.008484138	,	0.942316683	/)
        Coeff_Latin(	290	,	:)	=	(/	0.850464382	,	0.397882815	,	0.824425925	,	1.067406805	,	1.001089303	/)
        Coeff_Latin(	291	,	:)	=	(/	1.033824638	,	0.849138631	,	1.057569766	,	0.936995337	,	1.097757339	/)
        Coeff_Latin(	292	,	:)	=	(/	1.078645451	,	0.77550682	,	0.969401941	,	0.932153642	,	0.73285393	/)
        Coeff_Latin(	293	,	:)	=	(/	1.1223329	,	0.668413644	,	1.01596505	,	1.048583067	,	0.840571954	/)
        Coeff_Latin(	294	,	:)	=	(/	0.863097229	,	0.836098516	,	0.758355336	,	1.044214338	,	0.636890466	/)
        Coeff_Latin(	295	,	:)	=	(/	1.396132407	,	1.303936081	,	0.896420983	,	1.005403059	,	0.834102253	/)
        Coeff_Latin(	296	,	:)	=	(/	0.950458093	,	0.661585653	,	1.189278915	,	1.011244385	,	0.781724164	/)
        Coeff_Latin(	297	,	:)	=	(/	1.148224341	,	0.446792167	,	0.879517245	,	0.985756821	,	0.618364712	/)
        Coeff_Latin(	298	,	:)	=	(/	1.152482329	,	1.273780555	,	0.933265869	,	0.986632676	,	0.945085897	/)
        Coeff_Latin(	299	,	:)	=	(/	1.000087521	,	1.003070914	,	1.148043007	,	0.972471663	,	0.879981303	/)
        Coeff_Latin(	300	,	:)	=	(/	1.196408469	,	0.994041643	,	0.797710928	,	0.991829431	,	0.97040434	/)
        Coeff_Latin(	301	,	:)	=	(/	1.081216968	,	0.542154933	,	0.785977935	,	1.009668679	,	1.317297799	/)
        Coeff_Latin(	302	,	:)	=	(/	1.294659479	,	0.90738916	,	0.944392227	,	1.052827989	,	0.808751348	/)
        Coeff_Latin(	303	,	:)	=	(/	0.695901873	,	1.001474579	,	1.03161008	,	1.082394568	,	0.994117944	/)
        Coeff_Latin(	304	,	:)	=	(/	0.822847536	,	0.4015562	,	0.954497052	,	1.029168377	,	0.766191994	/)
        Coeff_Latin(	305	,	:)	=	(/	0.778980187	,	0.660047985	,	0.975972476	,	0.995218706	,	0.908861085	/)
        Coeff_Latin(	306	,	:)	=	(/	0.969221612	,	1.151877049	,	1.216585982	,	0.999774416	,	0.780376167	/)
        Coeff_Latin(	307	,	:)	=	(/	0.7738045	,	1.602626949	,	0.998029225	,	0.979917411	,	0.888202304	/)
        Coeff_Latin(	308	,	:)	=	(/	1.100567494	,	0.226490231	,	1.093089338	,	0.958439335	,	0.933120815	/)
        Coeff_Latin(	309	,	:)	=	(/	1.297396175	,	1.820353841	,	0.985129741	,	1.022617833	,	1.255418503	/)
        Coeff_Latin(	310	,	:)	=	(/	0.748310356	,	1.014207194	,	0.911692936	,	1.032364849	,	0.95615901	/)
        Coeff_Latin(	311	,	:)	=	(/	0.715449895	,	1.206101587	,	0.933074715	,	1.059250502	,	1.0794311	/)
        Coeff_Latin(	312	,	:)	=	(/	0.628206165	,	0.619865637	,	1.090281286	,	0.968227357	,	0.852452527	/)
        Coeff_Latin(	313	,	:)	=	(/	1.29685228	,	0.665217627	,	1.044447651	,	1.061103449	,	1.171119302	/)
        Coeff_Latin(	314	,	:)	=	(/	1.020068902	,	1.105001532	,	1.074561588	,	0.979290162	,	0.855614142	/)
        Coeff_Latin(	315	,	:)	=	(/	1.490115537	,	0.99353072	,	1.060512115	,	0.913542818	,	0.754483023	/)
        Coeff_Latin(	316	,	:)	=	(/	1.166120263	,	0.969003854	,	0.988495443	,	0.981395257	,	0.847697272	/)
        Coeff_Latin(	317	,	:)	=	(/	1.276336895	,	0.52081992	,	0.780674147	,	0.932472237	,	0.830397274	/)
        Coeff_Latin(	318	,	:)	=	(/	0.958064402	,	1.4325409	,	1.189131225	,	0.98509755	,	0.996438084	/)
        Coeff_Latin(	319	,	:)	=	(/	1.392798817	,	0.634920969	,	1.053074411	,	0.972659879	,	0.644067715	/)
        Coeff_Latin(	320	,	:)	=	(/	0.901439077	,	1.153254762	,	0.98249612	,	0.950179565	,	1.563865834	/)
        Coeff_Latin(	321	,	:)	=	(/	1.183153488	,	1.215861584	,	1.001370579	,	1.041814986	,	0.841509717	/)
        Coeff_Latin(	322	,	:)	=	(/	1.156106901	,	1.260378484	,	0.919270975	,	1.102660675	,	0.901463563	/)
        Coeff_Latin(	323	,	:)	=	(/	1.313833319	,	0.60278519	,	1.128800161	,	0.919091475	,	0.730450478	/)
        Coeff_Latin(	324	,	:)	=	(/	0.89535131	,	1.179409657	,	0.807760502	,	1.039659544	,	0.917158442	/)
        Coeff_Latin(	325	,	:)	=	(/	1.232928979	,	1.113290355	,	0.985940389	,	0.878482723	,	0.862509629	/)
        Coeff_Latin(	326	,	:)	=	(/	1.066434801	,	0.715775857	,	1.045827257	,	1.048494751	,	0.690779435	/)
        Coeff_Latin(	327	,	:)	=	(/	0.950491201	,	1.073274799	,	0.991688623	,	1.027355677	,	0.760693698	/)
        Coeff_Latin(	328	,	:)	=	(/	0.45559866	,	0.864630626	,	0.757948066	,	0.939715758	,	1.163461545	/)
        Coeff_Latin(	329	,	:)	=	(/	0.928659443	,	0.53850756	,	0.949999124	,	1.034911096	,	1.108796208	/)
        Coeff_Latin(	330	,	:)	=	(/	1.404738627	,	0.924071413	,	1.011370503	,	1.066953277	,	0.799879147	/)
        Coeff_Latin(	331	,	:)	=	(/	0.953285304	,	1.180060775	,	1.130389926	,	0.934031945	,	0.688359637	/)
        Coeff_Latin(	332	,	:)	=	(/	1.288807278	,	0.978095027	,	1.169176306	,	1.03778571	,	1.085032221	/)
        Coeff_Latin(	333	,	:)	=	(/	0.716076636	,	0.982460827	,	1.208381783	,	0.963186826	,	0.850702426	/)
        Coeff_Latin(	334	,	:)	=	(/	1.212481238	,	0.842756704	,	0.90371459	,	1.00993347	,	0.904673288	/)
        Coeff_Latin(	335	,	:)	=	(/	0.907322083	,	0.618284096	,	1.111809159	,	0.939870096	,	0.76091262	/)
        Coeff_Latin(	336	,	:)	=	(/	1.022645553	,	0.690460642	,	1.138566278	,	0.970301418	,	0.969261967	/)
        Coeff_Latin(	337	,	:)	=	(/	0.871565741	,	0.985347596	,	1.23733313	,	0.891099513	,	0.835570411	/)
        Coeff_Latin(	338	,	:)	=	(/	1.151834146	,	0.751435683	,	0.973669854	,	0.92122301	,	0.783752771	/)
        Coeff_Latin(	339	,	:)	=	(/	0.904977412	,	1.208071881	,	1.090201483	,	0.984256719	,	0.964981236	/)
        Coeff_Latin(	340	,	:)	=	(/	0.789408541	,	1.120665089	,	0.917938218	,	1.047383384	,	0.781501359	/)
        Coeff_Latin(	341	,	:)	=	(/	1.307775505	,	0.319193449	,	1.213009327	,	0.994708957	,	1.304362328	/)
        Coeff_Latin(	342	,	:)	=	(/	1.3200376	,	1.586600841	,	0.743789088	,	0.927068982	,	0.876598363	/)
        Coeff_Latin(	343	,	:)	=	(/	1.480400283	,	1.306758171	,	0.94135313	,	0.993383572	,	0.985322954	/)
        Coeff_Latin(	344	,	:)	=	(/	1.075140047	,	1.598562308	,	1.138171661	,	1.044111979	,	0.676815313	/)
        Coeff_Latin(	345	,	:)	=	(/	1.015584357	,	0.753311957	,	1.084892482	,	0.937694957	,	0.630822226	/)
        Coeff_Latin(	346	,	:)	=	(/	1.060302969	,	1.197752901	,	0.941448442	,	0.993020319	,	1.31935201	/)
        Coeff_Latin(	347	,	:)	=	(/	0.94063082	,	1.05111577	,	0.946503248	,	1.016081854	,	0.977728403	/)
        Coeff_Latin(	348	,	:)	=	(/	0.677147537	,	1.036781705	,	1.118559613	,	1.019520268	,	0.970655437	/)
        Coeff_Latin(	349	,	:)	=	(/	0.708890155	,	1.246303511	,	1.222001278	,	0.915597303	,	0.967113523	/)
        Coeff_Latin(	350	,	:)	=	(/	0.86176589	,	0.878915107	,	0.825895902	,	1.061905122	,	0.997523034	/)
        Coeff_Latin(	351	,	:)	=	(/	1.03877836	,	0.851714708	,	0.882937553	,	0.902379009	,	0.612007632	/)
        Coeff_Latin(	352	,	:)	=	(/	0.496454553	,	1.216567958	,	0.969992342	,	0.989259064	,	0.895142343	/)
        Coeff_Latin(	353	,	:)	=	(/	0.875404862	,	1.13447636	,	1.035061466	,	1.040644044	,	0.893201768	/)
        Coeff_Latin(	354	,	:)	=	(/	0.723208547	,	1.420696886	,	0.866315837	,	1.00546436	,	0.991232444	/)
        Coeff_Latin(	355	,	:)	=	(/	1.001896818	,	1.638665132	,	1.230171601	,	0.95833144	,	0.923878686	/)
        Coeff_Latin(	356	,	:)	=	(/	1.401922043	,	0.769841972	,	0.933087824	,	1.077022891	,	1.135784292	/)
        Coeff_Latin(	357	,	:)	=	(/	0.987810165	,	0.817333722	,	1.019939085	,	1.03591491	,	1.05689068	/)
        Coeff_Latin(	358	,	:)	=	(/	1.299895808	,	0.345520947	,	1.123278159	,	1.07092528	,	0.975145245	/)
        Coeff_Latin(	359	,	:)	=	(/	1.186149191	,	1.236768924	,	1.16934034	,	0.967189866	,	0.916124901	/)
        Coeff_Latin(	360	,	:)	=	(/	0.755604059	,	0.986651378	,	0.906541457	,	1.098281143	,	1.239047415	/)
        Coeff_Latin(	361	,	:)	=	(/	0.773360091	,	0.405683568	,	1.06173725	,	1.008530953	,	1.069857841	/)
        Coeff_Latin(	362	,	:)	=	(/	0.720522177	,	0.827125035	,	1.191778753	,	1.030847168	,	0.821213719	/)
        Coeff_Latin(	363	,	:)	=	(/	1.194540071	,	1.491965773	,	1.029537298	,	0.983656861	,	0.997212946	/)
        Coeff_Latin(	364	,	:)	=	(/	0.99318341	,	1.665942606	,	0.922255759	,	1.025353958	,	0.857914467	/)
        Coeff_Latin(	365	,	:)	=	(/	1.357646619	,	1.260670422	,	1.340814314	,	1.005757418	,	0.88209574	/)
        Coeff_Latin(	366	,	:)	=	(/	0.828864287	,	1.501624816	,	0.770474915	,	0.96463563	,	1.154863981	/)
        Coeff_Latin(	367	,	:)	=	(/	0.914368071	,	1.439451991	,	0.979424641	,	0.998671723	,	0.686382315	/)
        Coeff_Latin(	368	,	:)	=	(/	1.201631305	,	0.767722109	,	0.611477619	,	1.041586733	,	0.677334936	/)
        Coeff_Latin(	369	,	:)	=	(/	0.511208264	,	1.136540627	,	1.282231596	,	0.858628091	,	1.032725827	/)
        Coeff_Latin(	370	,	:)	=	(/	0.998110267	,	1.207551123	,	1.038877091	,	1.004578675	,	1.088765112	/)
        Coeff_Latin(	371	,	:)	=	(/	1.056424665	,	1.159944659	,	1.034285749	,	0.981590857	,	1.303638125	/)
        Coeff_Latin(	372	,	:)	=	(/	1.12380927	,	0.630729689	,	0.82758632	,	0.998938166	,	0.906403342	/)
        Coeff_Latin(	373	,	:)	=	(/	1.079165184	,	0.83352193	,	1.079396133	,	1.020544436	,	1.196964122	/)
        Coeff_Latin(	374	,	:)	=	(/	0.859972906	,	0.911177829	,	1.243019921	,	0.979442521	,	0.706275779	/)
        Coeff_Latin(	375	,	:)	=	(/	0.869765604	,	1.301839005	,	0.679749865	,	0.920460451	,	0.765431534	/)
        Coeff_Latin(	376	,	:)	=	(/	1.029714343	,	1.176365542	,	0.835878698	,	1.053327826	,	0.786266384	/)
        Coeff_Latin(	377	,	:)	=	(/	0.884408639	,	0.912162778	,	0.791885119	,	0.973980861	,	1.080923505	/)
        Coeff_Latin(	378	,	:)	=	(/	0.899948926	,	1.529977158	,	0.974080428	,	0.993044761	,	1.013554098	/)
        Coeff_Latin(	379	,	:)	=	(/	1.335085902	,	1.465992516	,	0.764607041	,	0.982940496	,	0.556979683	/)
        Coeff_Latin(	380	,	:)	=	(/	1.406115539	,	0.806462772	,	1.140892017	,	1.033076257	,	1.088369339	/)
        Coeff_Latin(	381	,	:)	=	(/	0.355810031	,	1.110012328	,	1.02640546	,	1.019810906	,	1.098147045	/)
        Coeff_Latin(	382	,	:)	=	(/	0.316499585	,	0.885946774	,	0.925248941	,	1.073003764	,	0.679879038	/)
        Coeff_Latin(	383	,	:)	=	(/	0.819502134	,	0.413693377	,	0.944182091	,	0.965412845	,	1.026058989	/)
        Coeff_Latin(	384	,	:)	=	(/	1.056095391	,	0.660062746	,	0.585456079	,	0.890879982	,	0.701720054	/)
        Coeff_Latin(	385	,	:)	=	(/	0.977803188	,	0.748184246	,	0.962452315	,	1.012641076	,	0.820273116	/)
        Coeff_Latin(	386	,	:)	=	(/	0.541815223	,	1.066999905	,	1.006062943	,	1.064651469	,	1.245173955	/)
        Coeff_Latin(	387	,	:)	=	(/	0.753874236	,	0.773168081	,	1.017628998	,	1.023068537	,	1.016563878	/)
        Coeff_Latin(	388	,	:)	=	(/	0.572567783	,	1.105310787	,	0.980711718	,	0.951356256	,	1.065721	/)
        Coeff_Latin(	389	,	:)	=	(/	0.755762188	,	0.965322305	,	1.03217748	,	1.040255561	,	1.019340766	/)
        Coeff_Latin(	390	,	:)	=	(/	1.548145918	,	1.153710232	,	1.139757605	,	0.94671223	,	0.660259935	/)
        Coeff_Latin(	391	,	:)	=	(/	0.848760352	,	1.029045335	,	0.765182601	,	0.973034897	,	1.079446548	/)
        Coeff_Latin(	392	,	:)	=	(/	0.872839099	,	1.187415035	,	1.32759273	,	0.993189255	,	0.86413393	/)
        Coeff_Latin(	393	,	:)	=	(/	0.9489049	,	1.616753262	,	1.185445549	,	1.003105782	,	1.058509782	/)
        Coeff_Latin(	394	,	:)	=	(/	1.079229061	,	0.920065458	,	1.006350324	,	0.956574399	,	1.268825182	/)
        Coeff_Latin(	395	,	:)	=	(/	1.165271368	,	0.435019635	,	1.237883525	,	1.128720003	,	0.947892318	/)
        Coeff_Latin(	396	,	:)	=	(/	1.050098624	,	1.387851579	,	0.867339523	,	0.907916051	,	1.209204277	/)
        Coeff_Latin(	397	,	:)	=	(/	1.046630147	,	0.87138026	,	0.867861562	,	0.934760149	,	0.996149342	/)
        Coeff_Latin(	398	,	:)	=	(/	0.924599573	,	1.25544915	,	1.252206612	,	0.919862436	,	1.049821501	/)
        Coeff_Latin(	399	,	:)	=	(/	1.336460841	,	0.966506316	,	0.95909695	,	0.965732725	,	1.086804715	/)
        Coeff_Latin(	400	,	:)	=	(/	1.151677717	,	0.184877026	,	0.810824204	,	1.08339149	,	0.977107571	/)
        Coeff_Latin(	401	,	:)	=	(/	0.989325351	,	0.721774941	,	1.137398783	,	0.973007873	,	1.126606454	/)
        Coeff_Latin(	402	,	:)	=	(/	1.198676858	,	1.172169693	,	1.019092083	,	0.901042403	,	0.6668605	/)
        Coeff_Latin(	403	,	:)	=	(/	1.019689615	,	0.691096092	,	0.966721333	,	0.972207096	,	1.264958118	/)
        Coeff_Latin(	404	,	:)	=	(/	0.75272281	,	1.149316782	,	1.10085886	,	0.979629423	,	1.088278876	/)
        Coeff_Latin(	405	,	:)	=	(/	0.993937617	,	0.945799444	,	0.871234548	,	0.938110503	,	0.75287705	/)
        Coeff_Latin(	406	,	:)	=	(/	0.789142621	,	1.085566785	,	0.997258503	,	1.05814285	,	1.060024639	/)
        Coeff_Latin(	407	,	:)	=	(/	1.03351166	,	1.111054216	,	1.036321741	,	0.990928373	,	1.167221739	/)
        Coeff_Latin(	408	,	:)	=	(/	0.89789995	,	0.335502484	,	0.879284421	,	0.92005928	,	1.14532653	/)
        Coeff_Latin(	409	,	:)	=	(/	0.82605961	,	0.867867273	,	1.036764241	,	1.0241003	,	1.245411173	/)
        Coeff_Latin(	410	,	:)	=	(/	1.179742564	,	1.035141896	,	0.808824147	,	1.005130408	,	0.875198966	/)
        Coeff_Latin(	411	,	:)	=	(/	0.828028541	,	0.922890151	,	0.842836166	,	1.019476363	,	1.177678703	/)
        Coeff_Latin(	412	,	:)	=	(/	0.841505024	,	1.41930374	,	0.967681114	,	0.962025828	,	0.934710184	/)
        Coeff_Latin(	413	,	:)	=	(/	1.00057231	,	0.378636581	,	1.184350883	,	0.951433016	,	0.969975436	/)
        Coeff_Latin(	414	,	:)	=	(/	1.027942526	,	0.751531569	,	1.337331164	,	1.076907874	,	1.208787491	/)
        Coeff_Latin(	415	,	:)	=	(/	0.958682529	,	0.511051002	,	1.145571045	,	0.99267513	,	0.6883191	/)
        Coeff_Latin(	416	,	:)	=	(/	1.055248994	,	0.883872501	,	1.195090506	,	0.945567159	,	0.766867343	/)
        Coeff_Latin(	417	,	:)	=	(/	1.353496871	,	1.324382715	,	0.864010409	,	0.935602311	,	0.56290182	/)
        Coeff_Latin(	418	,	:)	=	(/	0.859371871	,	0.221864276	,	1.09443276	,	1.062741357	,	0.952762982	/)
        Coeff_Latin(	419	,	:)	=	(/	1.216431344	,	1.099460869	,	1.034133752	,	0.963986168	,	1.306668159	/)
        Coeff_Latin(	420	,	:)	=	(/	0.961618432	,	0.639055558	,	1.094083447	,	1.010478644	,	1.079212879	/)
        Coeff_Latin(	421	,	:)	=	(/	0.82211044	,	1.339595878	,	0.983079684	,	1.061630404	,	0.981421537	/)
        Coeff_Latin(	422	,	:)	=	(/	1.444102518	,	0.585213923	,	0.998020206	,	1.006434683	,	0.96873278	/)
        Coeff_Latin(	423	,	:)	=	(/	0.715244635	,	1.036329705	,	1.147761604	,	1.01899188	,	1.074680908	/)
        Coeff_Latin(	424	,	:)	=	(/	1.315295205	,	1.045325315	,	1.154752826	,	0.930398932	,	1.017721788	/)
        Coeff_Latin(	425	,	:)	=	(/	1.065896951	,	0.792163238	,	1.056811721	,	0.955312543	,	1.066363982	/)
        Coeff_Latin(	426	,	:)	=	(/	0.898812547	,	0.777577364	,	1.166298241	,	1.081229322	,	1.023057932	/)
        Coeff_Latin(	427	,	:)	=	(/	1.251267798	,	0.469524185	,	1.233531889	,	1.000798244	,	1.001832072	/)
        Coeff_Latin(	428	,	:)	=	(/	0.899595049	,	0.572900834	,	1.050272352	,	0.965700426	,	0.638202435	/)
        Coeff_Latin(	429	,	:)	=	(/	1.164724031	,	1.609036749	,	1.304186496	,	0.907536085	,	0.773784993	/)
        Coeff_Latin(	430	,	:)	=	(/	0.971360185	,	0.505519995	,	0.90671948	,	1.005040809	,	1.138417567	/)
        Coeff_Latin(	431	,	:)	=	(/	0.690540903	,	0.998972285	,	1.207210967	,	0.979063571	,	1.232810894	/)
        Coeff_Latin(	432	,	:)	=	(/	1.116120673	,	0.512906406	,	1.325965394	,	1.028581028	,	0.571142996	/)
        Coeff_Latin(	433	,	:)	=	(/	1.060715943	,	1.360689271	,	0.740578299	,	0.972356157	,	1.386313545	/)
        Coeff_Latin(	434	,	:)	=	(/	1.02392425	,	1.311009163	,	1.164371186	,	1.046724213	,	1.294378291	/)
        Coeff_Latin(	435	,	:)	=	(/	1.141885694	,	0.55158976	,	1.112137414	,	0.973917375	,	1.176996097	/)
        Coeff_Latin(	436	,	:)	=	(/	0.506623532	,	0.976147489	,	1.275177358	,	1.06155367	,	1.0552241	/)
        Coeff_Latin(	437	,	:)	=	(/	0.872322793	,	1.165752454	,	1.115882266	,	1.027438959	,	1.124708494	/)
        Coeff_Latin(	438	,	:)	=	(/	0.856945799	,	1.994133779	,	0.982836739	,	1.011465217	,	0.849600754	/)
        Coeff_Latin(	439	,	:)	=	(/	1.077335089	,	0.947981798	,	1.072088998	,	1.072705684	,	0.471576575	/)
        Coeff_Latin(	440	,	:)	=	(/	1.049519302	,	1.151563722	,	0.9933813	,	0.940139856	,	1.406345209	/)
        Coeff_Latin(	441	,	:)	=	(/	0.927437347	,	1.137644601	,	0.80801251	,	0.992479963	,	1.462133036	/)
        Coeff_Latin(	442	,	:)	=	(/	0.676983787	,	1.41466665	,	0.96704291	,	0.985630661	,	0.879065098	/)
        Coeff_Latin(	443	,	:)	=	(/	0.494229073	,	1.12857469	,	0.923215774	,	1.014734425	,	1.143192161	/)
        Coeff_Latin(	444	,	:)	=	(/	1.153565685	,	1.136710891	,	1.072069473	,	1.033282533	,	1.065551651	/)
        Coeff_Latin(	445	,	:)	=	(/	0.988156854	,	0.804712244	,	0.9812659	,	1.080288287	,	1.12553369	/)
        Coeff_Latin(	446	,	:)	=	(/	0.647259265	,	1.120729381	,	0.893272317	,	1.037107947	,	0.862884341	/)
        Coeff_Latin(	447	,	:)	=	(/	0.953860415	,	0.651049735	,	1.091839883	,	1.008525494	,	1.108750188	/)
        Coeff_Latin(	448	,	:)	=	(/	0.919728845	,	1.114096631	,	1.209422878	,	0.973015013	,	0.945558487	/)
        Coeff_Latin(	449	,	:)	=	(/	1.077183229	,	0.137050297	,	1.145153358	,	1.024707878	,	1.247240648	/)
        Coeff_Latin(	450	,	:)	=	(/	0.795838023	,	1.621687586	,	0.946276595	,	0.952983082	,	1.086229586	/)
        Coeff_Latin(	451	,	:)	=	(/	0.934938529	,	0.842784384	,	0.890225184	,	1.026304792	,	0.881211071	/)
        Coeff_Latin(	452	,	:)	=	(/	0.955064339	,	0.664612853	,	0.891437989	,	1.008721116	,	0.949921419	/)
        Coeff_Latin(	453	,	:)	=	(/	0.702926	,	0.993079331	,	1.217572222	,	0.972050526	,	0.86479576	/)
        Coeff_Latin(	454	,	:)	=	(/	1.056046363	,	1.060197067	,	0.892628325	,	0.976202485	,	0.96165231	/)
        Coeff_Latin(	455	,	:)	=	(/	1.06028559	,	1.268003375	,	0.917709799	,	0.969426396	,	0.893592877	/)
        Coeff_Latin(	456	,	:)	=	(/	0.633065888	,	0.711247403	,	0.930311159	,	1.003024506	,	0.909361312	/)
        Coeff_Latin(	457	,	:)	=	(/	1.020045372	,	1.081000085	,	1.236253191	,	0.942357026	,	1.05984509	/)
        Coeff_Latin(	458	,	:)	=	(/	0.864027316	,	1.196992298	,	1.144499984	,	1.113362397	,	0.904906797	/)
        Coeff_Latin(	459	,	:)	=	(/	1.379237562	,	0.365601898	,	0.84965527	,	0.978546967	,	0.853927854	/)
        Coeff_Latin(	460	,	:)	=	(/	0.780289011	,	0.439519592	,	0.68012039	,	1.019420293	,	1.132083126	/)
        Coeff_Latin(	461	,	:)	=	(/	1.209918159	,	1.074425155	,	0.822775975	,	1.018063645	,	1.316737626	/)
        Coeff_Latin(	462	,	:)	=	(/	1.0044233	,	0.754285964	,	0.865292075	,	0.997066926	,	1.028877642	/)
        Coeff_Latin(	463	,	:)	=	(/	0.83448517	,	0.971741958	,	0.978899208	,	0.935072478	,	1.224976762	/)
        Coeff_Latin(	464	,	:)	=	(/	1.265514493	,	1.213953854	,	0.739780121	,	1.01705317	,	1.120544773	/)
        Coeff_Latin(	465	,	:)	=	(/	1.289088405	,	0.757804748	,	1.082234734	,	1.068621356	,	1.06256109	/)
        Coeff_Latin(	466	,	:)	=	(/	0.570494007	,	0.528315241	,	1.363772605	,	0.931102006	,	0.641672154	/)
        Coeff_Latin(	467	,	:)	=	(/	0.925925678	,	0.501088694	,	1.010025804	,	0.936693955	,	0.969370282	/)
        Coeff_Latin(	468	,	:)	=	(/	0.603986392	,	1.360482174	,	0.702678258	,	1.027591018	,	0.977251178	/)
        Coeff_Latin(	469	,	:)	=	(/	0.850042847	,	0.792463144	,	0.950224876	,	0.999507731	,	0.825737913	/)
        Coeff_Latin(	470	,	:)	=	(/	0.733192995	,	0.594485881	,	0.908617322	,	0.987564787	,	1.016019053	/)
        Coeff_Latin(	471	,	:)	=	(/	0.571256277	,	1.229587642	,	0.857456799	,	0.953063615	,	0.844620906	/)
        Coeff_Latin(	472	,	:)	=	(/	0.864892301	,	0.960747336	,	1.050191411	,	0.952732814	,	1.219636368	/)
        Coeff_Latin(	473	,	:)	=	(/	0.957572035	,	1.201519597	,	0.960688244	,	1.039874163	,	0.705036496	/)
        Coeff_Latin(	474	,	:)	=	(/	0.844585963	,	0.82115406	,	1.105109225	,	1.045721762	,	1.102407669	/)
        Coeff_Latin(	475	,	:)	=	(/	1.246325331	,	0.833649525	,	0.844589805	,	0.975096369	,	1.154051298	/)
        Coeff_Latin(	476	,	:)	=	(/	0.947470107	,	1.207875484	,	0.871628761	,	0.936969007	,	1.057845934	/)
        Coeff_Latin(	477	,	:)	=	(/	1.35333456	,	0.879856289	,	1.010728219	,	0.95488701	,	1.06907412	/)
        Coeff_Latin(	478	,	:)	=	(/	0.828098932	,	0.959751682	,	0.962023437	,	1.057754758	,	0.849714524	/)
        Coeff_Latin(	479	,	:)	=	(/	0.932901279	,	0.623546036	,	0.982562687	,	1.010997081	,	1.099792784	/)
        Coeff_Latin(	480	,	:)	=	(/	1.001480243	,	0.548335797	,	1.08390212	,	1.01459825	,	0.900038599	/)
        Coeff_Latin(	481	,	:)	=	(/	0.80777037	,	0.830813105	,	1.136386193	,	0.984622069	,	0.753930342	/)
        Coeff_Latin(	482	,	:)	=	(/	0.855917964	,	0.424690015	,	0.858273412	,	1.01921047	,	0.998740264	/)
        Coeff_Latin(	483	,	:)	=	(/	0.945770727	,	0.746720027	,	1.031734266	,	1.009408646	,	0.952062402	/)
        Coeff_Latin(	484	,	:)	=	(/	0.987579886	,	1.046925937	,	0.914589336	,	0.97252686	,	1.006257293	/)
        Coeff_Latin(	485	,	:)	=	(/	0.948229432	,	0.707572841	,	0.833510875	,	0.970081709	,	0.890534934	/)
        Coeff_Latin(	486	,	:)	=	(/	0.766162107	,	0.764110123	,	0.76994704	,	1.007301168	,	0.971423379	/)
        Coeff_Latin(	487	,	:)	=	(/	1.436104507	,	0.745431477	,	1.037835184	,	0.955467056	,	1.089265303	/)
        Coeff_Latin(	488	,	:)	=	(/	1.018601436	,	0.636034584	,	1.009577432	,	1.029283865	,	1.127404476	/)
        Coeff_Latin(	489	,	:)	=	(/	1.23781629	,	1.529304953	,	0.838739232	,	0.981223147	,	0.997145229	/)
        Coeff_Latin(	490	,	:)	=	(/	1.082925948	,	0.663900316	,	0.959897077	,	1.027088301	,	0.885091552	/)
        Coeff_Latin(	491	,	:)	=	(/	1.198471673	,	0.94300232	,	0.846790923	,	1.037683645	,	0.452366678	/)
        Coeff_Latin(	492	,	:)	=	(/	1.031445547	,	1.457273036	,	0.669376919	,	1.046031816	,	1.24724076	/)
        Coeff_Latin(	493	,	:)	=	(/	0.867085612	,	0.909945315	,	0.962701209	,	0.969444122	,	1.314262872	/)
        Coeff_Latin(	494	,	:)	=	(/	0.631319462	,	0.518986905	,	1.271534052	,	1.013567302	,	0.798312941	/)
        Coeff_Latin(	495	,	:)	=	(/	0.998510679	,	1.279287538	,	1.359873867	,	1.056159892	,	1.171340074	/)
        Coeff_Latin(	496	,	:)	=	(/	1.248048601	,	0.780221222	,	0.924137028	,	1.138303822	,	0.787024184	/)
        Coeff_Latin(	497	,	:)	=	(/	1.544528844	,	0.480952499	,	1.061497701	,	0.881520667	,	1.045039004	/)
        Coeff_Latin(	498	,	:)	=	(/	1.00879783	,	0.875281544	,	0.824035405	,	0.981453849	,	1.091160177	/)
        Coeff_Latin(	499	,	:)	=	(/	0.959793905	,	0.804987476	,	1.241979896	,	1.038269212	,	1.014274281	/)
        Coeff_Latin(	500	,	:)	=	(/	0.726888573	,	0.778652391	,	0.766923979	,	1.016368533	,	0.70835657	/)
        Coeff_Latin(	501	,	:)	=	(/	1.314533223	,	0.99057218	,	1.033986148	,	1.030080241	,	1.056489425	/)
        Coeff_Latin(	502	,	:)	=	(/	1.067220634	,	0.359765282	,	1.254274786	,	0.995865802	,	0.988054938	/)
        Coeff_Latin(	503	,	:)	=	(/	0.652355321	,	0.592774855	,	0.830922306	,	1.006594334	,	0.773966031	/)
        Coeff_Latin(	504	,	:)	=	(/	0.903036994	,	1.089060579	,	1.157889934	,	1.031154155	,	0.932215462	/)
        Coeff_Latin(	505	,	:)	=	(/	0.866906547	,	1.205608505	,	1.285440033	,	0.894954835	,	1.274874943	/)
        Coeff_Latin(	506	,	:)	=	(/	1.293203627	,	1.076221009	,	0.980946423	,	0.994622489	,	0.877906135	/)
        Coeff_Latin(	507	,	:)	=	(/	1.215409526	,	0.658010832	,	1.319583358	,	0.955776759	,	0.668463701	/)
        Coeff_Latin(	508	,	:)	=	(/	0.876621542	,	0.316341971	,	0.882315595	,	0.978281212	,	1.162011196	/)
        Coeff_Latin(	509	,	:)	=	(/	0.86880281	,	1.558382945	,	1.014622878	,	0.955950275	,	0.998638061	/)
        Coeff_Latin(	510	,	:)	=	(/	0.962577631	,	0.610942585	,	1.01150768	,	0.969770701	,	1.25909555	/)
        Coeff_Latin(	511	,	:)	=	(/	1.200709808	,	1.393337881	,	0.864487374	,	1.024337555	,	0.916489408	/)
        Coeff_Latin(	512	,	:)	=	(/	0.971620625	,	1.056788566	,	0.807211639	,	0.878139497	,	0.571091675	/)
        Coeff_Latin(	513	,	:)	=	(/	1.220793922	,	1.342722621	,	0.959414709	,	1.001489261	,	0.741004164	/)
        Coeff_Latin(	514	,	:)	=	(/	0.957589791	,	1.094701803	,	1.264883974	,	1.029624557	,	1.120339391	/)
        Coeff_Latin(	515	,	:)	=	(/	1.130852376	,	1.156744972	,	0.872862243	,	1.037407596	,	1.196425285	/)
        Coeff_Latin(	516	,	:)	=	(/	1.376613518	,	1.372793316	,	1.185372446	,	0.982273185	,	0.8130985	/)
        Coeff_Latin(	517	,	:)	=	(/	0.51470604	,	0.959466437	,	0.868539199	,	0.947373857	,	0.997933083	/)
        Coeff_Latin(	518	,	:)	=	(/	0.737364358	,	1.132607804	,	1.225383321	,	1.006836231	,	1.356416039	/)
        Coeff_Latin(	519	,	:)	=	(/	1.111269398	,	0.954208765	,	0.6965634	,	1.014675844	,	0.880146576	/)
        Coeff_Latin(	520	,	:)	=	(/	0.982418903	,	0.972461294	,	0.955984376	,	1.018561715	,	1.214786406	/)
        Coeff_Latin(	521	,	:)	=	(/	1.471121787	,	0.370127523	,	0.983603466	,	0.968104244	,	1.261531731	/)
        Coeff_Latin(	522	,	:)	=	(/	0.549863488	,	0.468498406	,	1.141743972	,	1.083239617	,	1.422541005	/)
        Coeff_Latin(	523	,	:)	=	(/	1.164233137	,	0.503691341	,	0.899130011	,	1.020075882	,	1.154941239	/)
        Coeff_Latin(	524	,	:)	=	(/	1.046026034	,	1.131078678	,	1.137524921	,	1.018420455	,	0.854619005	/)
        Coeff_Latin(	525	,	:)	=	(/	1.00769798	,	1.412271261	,	1.142306492	,	0.988475942	,	1.097574366	/)
        Coeff_Latin(	526	,	:)	=	(/	0.580977487	,	1.011783005	,	0.927364526	,	1.061483784	,	1.065916307	/)
        Coeff_Latin(	527	,	:)	=	(/	0.952699393	,	1.187426294	,	0.887140832	,	0.966431439	,	1.013319062	/)
        Coeff_Latin(	528	,	:)	=	(/	0.728525622	,	1.437723611	,	0.995742528	,	1.003694039	,	0.99533963	/)
        Coeff_Latin(	529	,	:)	=	(/	1.034828483	,	1.045423289	,	0.891923398	,	0.978763276	,	1.182671255	/)
        Coeff_Latin(	530	,	:)	=	(/	0.525840145	,	0.855885449	,	0.717195074	,	1.027525136	,	1.102590673	/)
        Coeff_Latin(	531	,	:)	=	(/	1.158167424	,	1.277000915	,	1.039523894	,	1.066349036	,	1.103224613	/)
        Coeff_Latin(	532	,	:)	=	(/	1.21419397	,	0.802597942	,	1.12249783	,	1.102704907	,	0.847967352	/)
        Coeff_Latin(	533	,	:)	=	(/	1.158269773	,	1.056076349	,	0.649528821	,	0.994666773	,	1.02162676	/)
        Coeff_Latin(	534	,	:)	=	(/	0.484872596	,	0.500990778	,	0.922134204	,	0.958063977	,	1.093362745	/)
        Coeff_Latin(	535	,	:)	=	(/	1.273178787	,	0.525216543	,	1.109427109	,	1.084395675	,	1.114080719	/)
        Coeff_Latin(	536	,	:)	=	(/	1.021539419	,	0.825686502	,	1.033240079	,	0.962847814	,	1.02516137	/)
        Coeff_Latin(	537	,	:)	=	(/	1.225930886	,	0.950927345	,	1.180226854	,	0.85538704	,	0.979999148	/)
        Coeff_Latin(	538	,	:)	=	(/	0.920548299	,	0.607112733	,	1.09106278	,	0.980313862	,	1.051204252	/)
        Coeff_Latin(	539	,	:)	=	(/	1.466701828	,	0.72145044	,	0.944873975	,	1.028218167	,	1.117651897	/)
        Coeff_Latin(	540	,	:)	=	(/	0.678351763	,	0.744611207	,	1.114599534	,	1.09011843	,	1.030635789	/)
        Coeff_Latin(	541	,	:)	=	(/	0.863714719	,	1.541846366	,	0.888268412	,	1.034650679	,	0.476803296	/)
        Coeff_Latin(	542	,	:)	=	(/	0.957458655	,	1.824559382	,	0.983001393	,	0.946747862	,	0.907090368	/)
        Coeff_Latin(	543	,	:)	=	(/	0.879065832	,	1.15395427	,	1.071932803	,	0.999943755	,	1.04999613	/)
        Coeff_Latin(	544	,	:)	=	(/	1.257360699	,	1.515214849	,	0.893271081	,	1.062716739	,	0.982617726	/)
        Coeff_Latin(	545	,	:)	=	(/	1.017733298	,	0.928451762	,	1.045315474	,	1.102948359	,	0.878709297	/)
        Coeff_Latin(	546	,	:)	=	(/	0.996265315	,	0.923194554	,	1.173353074	,	0.983110345	,	0.772292355	/)
        Coeff_Latin(	547	,	:)	=	(/	0.860262386	,	1.392123541	,	1.201636364	,	0.95704785	,	0.91804855	/)
        Coeff_Latin(	548	,	:)	=	(/	0.888267145	,	1.244114889	,	0.821891608	,	1.034167538	,	1.033377278	/)
        Coeff_Latin(	549	,	:)	=	(/	1.133510832	,	1.286434276	,	0.984744119	,	0.987666482	,	0.890508006	/)
        Coeff_Latin(	550	,	:)	=	(/	0.31253236	,	0.883175429	,	0.99219207	,	0.972972245	,	0.857808357	/)
        Coeff_Latin(	551	,	:)	=	(/	1.08159461	,	0.863009531	,	1.245412689	,	1.006182871	,	0.976786985	/)
        Coeff_Latin(	552	,	:)	=	(/	1.253468823	,	1.271732905	,	1.005784773	,	0.95389915	,	0.993624525	/)
        Coeff_Latin(	553	,	:)	=	(/	1.106484498	,	1.344787357	,	1.370525128	,	0.980592435	,	1.299729779	/)
        Coeff_Latin(	554	,	:)	=	(/	1.306330166	,	0.834755415	,	1.007754811	,	1.030659597	,	0.871860451	/)
        Coeff_Latin(	555	,	:)	=	(/	0.903056529	,	0.52868918	,	0.951073557	,	1.02332303	,	0.859713992	/)
        Coeff_Latin(	556	,	:)	=	(/	0.568163217	,	0.806811237	,	0.944569843	,	0.97547693	,	0.958872059	/)
        Coeff_Latin(	557	,	:)	=	(/	1.27912907	,	1.143466037	,	0.823343499	,	0.976473132	,	1.108368359	/)
        Coeff_Latin(	558	,	:)	=	(/	0.906408305	,	0.980732945	,	1.136141044	,	1.022090064	,	1.139824651	/)
        Coeff_Latin(	559	,	:)	=	(/	1.068895386	,	1.031138441	,	0.895509233	,	0.904698114	,	1.05120858	/)
        Coeff_Latin(	560	,	:)	=	(/	1.12406837	,	1.203878678	,	0.771908232	,	1.106130771	,	0.897828324	/)
        Coeff_Latin(	561	,	:)	=	(/	1.065180192	,	1.146838929	,	0.741857709	,	0.970626628	,	1.066294155	/)
        Coeff_Latin(	562	,	:)	=	(/	0.868746977	,	1.119935461	,	0.811991672	,	1.019951115	,	0.915027609	/)
        Coeff_Latin(	563	,	:)	=	(/	1.013634553	,	1.162699051	,	1.081405019	,	0.974258077	,	1.041901058	/)
        Coeff_Latin(	564	,	:)	=	(/	0.912904779	,	1.458234872	,	0.892516009	,	1.062933548	,	1.073814245	/)
        Coeff_Latin(	565	,	:)	=	(/	0.967794927	,	0.983381863	,	0.934820119	,	1.026671054	,	0.797843782	/)
        Coeff_Latin(	566	,	:)	=	(/	0.927517056	,	0.588339024	,	0.794028835	,	1.023949413	,	0.74914991	/)
        Coeff_Latin(	567	,	:)	=	(/	0.906355689	,	0.873238157	,	0.786615104	,	0.991121076	,	1.098808935	/)
        Coeff_Latin(	568	,	:)	=	(/	1.174396633	,	0.890942428	,	1.215067726	,	1.038792557	,	1.015395919	/)
        Coeff_Latin(	569	,	:)	=	(/	0.583765181	,	0.716002259	,	0.995099577	,	1.006194157	,	1.26744415	/)
        Coeff_Latin(	570	,	:)	=	(/	0.988375537	,	1.072995625	,	1.083759748	,	0.937126513	,	0.899756374	/)
        Coeff_Latin(	571	,	:)	=	(/	0.661714092	,	1.086538843	,	0.901853978	,	1.030482768	,	0.9150259	/)
        Coeff_Latin(	572	,	:)	=	(/	0.87368348	,	1.950371298	,	1.307095151	,	1.056161708	,	0.977019132	/)
        Coeff_Latin(	573	,	:)	=	(/	1.235683069	,	0.953251091	,	1.400837814	,	1.04950002	,	0.927730392	/)
        Coeff_Latin(	574	,	:)	=	(/	0.851528865	,	1.492796126	,	1.141075791	,	1.015272252	,	0.851131754	/)
        Coeff_Latin(	575	,	:)	=	(/	1.07314281	,	0.947348763	,	1.07411522	,	1.057019598	,	1.099409403	/)
        Coeff_Latin(	576	,	:)	=	(/	1.239515225	,	1.168880896	,	1.068609837	,	0.917309225	,	0.89045604	/)
        Coeff_Latin(	577	,	:)	=	(/	0.800327395	,	0.589580736	,	1.090675631	,	0.894065485	,	1.243256088	/)
        Coeff_Latin(	578	,	:)	=	(/	0.851286919	,	1.212977346	,	1.083076522	,	1.055289	,	1.104993277	/)
        Coeff_Latin(	579	,	:)	=	(/	0.986901902	,	1.147542055	,	0.970281054	,	1.019699257	,	0.863468335	/)
        Coeff_Latin(	580	,	:)	=	(/	1.222515237	,	0.973190688	,	1.10495491	,	1.036842467	,	1.326868197	/)
        Coeff_Latin(	581	,	:)	=	(/	0.69631732	,	1.279543325	,	0.935311465	,	0.980646442	,	1.243522491	/)
        Coeff_Latin(	582	,	:)	=	(/	0.837160644	,	0.833338093	,	0.799578609	,	1.02054798	,	1.140115892	/)
        Coeff_Latin(	583	,	:)	=	(/	0.901344058	,	1.127837576	,	0.869220112	,	0.900926072	,	0.986720954	/)
        Coeff_Latin(	584	,	:)	=	(/	0.583948768	,	1.289545455	,	0.792654499	,	1.019603576	,	1.417708637	/)
        Coeff_Latin(	585	,	:)	=	(/	1.084558141	,	1.664643416	,	1.295249376	,	1.084287145	,	0.828754791	/)
        Coeff_Latin(	586	,	:)	=	(/	1.109696839	,	1.189106025	,	0.746583556	,	1.015074267	,	1.154251278	/)
        Coeff_Latin(	587	,	:)	=	(/	1.20975429	,	0.786694565	,	0.970603319	,	1.02149212	,	0.832806345	/)
        Coeff_Latin(	588	,	:)	=	(/	1.070191715	,	1.086601633	,	1.036314399	,	0.970055209	,	0.982413291	/)
        Coeff_Latin(	589	,	:)	=	(/	1.260099337	,	0.555735615	,	1.176051997	,	1.069908014	,	0.970805774	/)
        Coeff_Latin(	590	,	:)	=	(/	1.22123529	,	0.485444426	,	0.872882058	,	0.995416466	,	1.08025341	/)
        Coeff_Latin(	591	,	:)	=	(/	1.264427868	,	1.168054368	,	1.244812898	,	1.018621262	,	0.898101527	/)
        Coeff_Latin(	592	,	:)	=	(/	0.980592948	,	1.060819352	,	1.020635177	,	0.98374028	,	0.89379774	/)
        Coeff_Latin(	593	,	:)	=	(/	0.781534436	,	1.325186015	,	1.119025174	,	1.026014203	,	0.855929123	/)
        Coeff_Latin(	594	,	:)	=	(/	1.136388511	,	0.429897382	,	1.078828379	,	1.045854753	,	1.295494074	/)
        Coeff_Latin(	595	,	:)	=	(/	0.899783212	,	0.633044013	,	0.83151811	,	0.942205856	,	0.932403468	/)
        Coeff_Latin(	596	,	:)	=	(/	0.872750419	,	1.481356139	,	0.787251492	,	1.033548758	,	1.211084893	/)
        Coeff_Latin(	597	,	:)	=	(/	0.758795373	,	1.147932123	,	0.85925558	,	1.10975783	,	1.023603514	/)
        Coeff_Latin(	598	,	:)	=	(/	0.931438334	,	1.047695442	,	0.713453209	,	0.98462313	,	1.254085403	/)
        Coeff_Latin(	599	,	:)	=	(/	1.310225001	,	1.078074435	,	1.205320903	,	0.9909346	,	1.126904687	/)
        Coeff_Latin(	600	,	:)	=	(/	0.849994335	,	0.481024117	,	0.963937962	,	1.005469676	,	0.680424634	/)
        Coeff_Latin(	601	,	:)	=	(/	0.631642346	,	0.915734548	,	0.855231112	,	1.082754069	,	1.190341044	/)
        Coeff_Latin(	602	,	:)	=	(/	1.249714676	,	1.189053224	,	0.735598533	,	0.987868663	,	0.651106924	/)
        Coeff_Latin(	603	,	:)	=	(/	0.894900662	,	0.867456492	,	1.045262992	,	1.025639815	,	1.091167138	/)
        Coeff_Latin(	604	,	:)	=	(/	0.983704453	,	1.011697842	,	1.065696268	,	1.092125441	,	0.926189626	/)
        Coeff_Latin(	605	,	:)	=	(/	0.879947922	,	0.972670386	,	0.950168838	,	1.014254349	,	0.99113636	/)
        Coeff_Latin(	606	,	:)	=	(/	1.18543264	,	1.455899051	,	1.04454637	,	0.996465829	,	0.906960572	/)
        Coeff_Latin(	607	,	:)	=	(/	0.900274374	,	0.34514005	,	1.038767916	,	0.970360869	,	1.137315922	/)
        Coeff_Latin(	608	,	:)	=	(/	1.337538759	,	1.210340244	,	0.867417734	,	1.054891957	,	1.279586946	/)
        Coeff_Latin(	609	,	:)	=	(/	0.929575006	,	1.103299651	,	1.096066212	,	1.029214712	,	0.63224165	/)
        Coeff_Latin(	610	,	:)	=	(/	0.539613702	,	0.781727585	,	0.696759931	,	0.952283442	,	0.906930794	/)
        Coeff_Latin(	611	,	:)	=	(/	1.026133472	,	0.860806949	,	0.934428917	,	0.947303414	,	0.956836014	/)
        Coeff_Latin(	612	,	:)	=	(/	1.095394854	,	1.163087462	,	1.271757652	,	0.975017545	,	0.73147634	/)
        Coeff_Latin(	613	,	:)	=	(/	1.096411071	,	1.386292746	,	1.168309879	,	1.023494772	,	1.019592264	/)
        Coeff_Latin(	614	,	:)	=	(/	1.335344594	,	0.704511399	,	1.117010397	,	1.019677196	,	1.421621338	/)
        Coeff_Latin(	615	,	:)	=	(/	0.747057051	,	1.077969242	,	0.947694331	,	0.929443405	,	0.779589005	/)
        Coeff_Latin(	616	,	:)	=	(/	1.126396466	,	0.970776483	,	0.845675035	,	1.001784553	,	1.149610919	/)
        Coeff_Latin(	617	,	:)	=	(/	0.788743378	,	0.765629679	,	1.183778713	,	1.070050168	,	0.850653003	/)
        Coeff_Latin(	618	,	:)	=	(/	1.103585229	,	1.150098131	,	1.07897823	,	0.982105301	,	0.954286181	/)
        Coeff_Latin(	619	,	:)	=	(/	1.236767018	,	1.248547434	,	1.058771849	,	0.977348022	,	1.263037714	/)
        Coeff_Latin(	620	,	:)	=	(/	1.188600095	,	0.709390683	,	0.858369317	,	1.065365722	,	0.79911378	/)
        Coeff_Latin(	621	,	:)	=	(/	0.920739749	,	1.00984858	,	1.107575864	,	0.907501633	,	1.322960843	/)
        Coeff_Latin(	622	,	:)	=	(/	1.027723402	,	0.922457702	,	0.706234218	,	1.014097782	,	0.762692258	/)
        Coeff_Latin(	623	,	:)	=	(/	0.660735093	,	1.139297747	,	1.271175534	,	1.023628637	,	1.193304978	/)
        Coeff_Latin(	624	,	:)	=	(/	1.00928244	,	1.016149034	,	0.991860611	,	1.015860132	,	0.833889938	/)
        Coeff_Latin(	625	,	:)	=	(/	1.346496163	,	1.056917468	,	0.76375547	,	0.899615706	,	0.724696367	/)
        Coeff_Latin(	626	,	:)	=	(/	0.711886783	,	0.636792596	,	0.911519729	,	1.007582487	,	1.027039486	/)
        Coeff_Latin(	627	,	:)	=	(/	1.053865563	,	0.702683905	,	0.919084862	,	1.063532541	,	1.247745413	/)
        Coeff_Latin(	628	,	:)	=	(/	1.433708289	,	0.689776269	,	1.093949135	,	1.062105254	,	0.954562359	/)
        Coeff_Latin(	629	,	:)	=	(/	1.234467048	,	0.953527943	,	0.986472935	,	1.069799714	,	0.611758357	/)
        Coeff_Latin(	630	,	:)	=	(/	1.129661861	,	1.01517022	,	0.998025507	,	0.978780408	,	1.130786612	/)
        Coeff_Latin(	631	,	:)	=	(/	1.255302222	,	1.234690516	,	0.899021042	,	1.008033764	,	0.927145343	/)
        Coeff_Latin(	632	,	:)	=	(/	0.72779476	,	1.15517166	,	0.831764256	,	1.006757691	,	1.174017835	/)
        Coeff_Latin(	633	,	:)	=	(/	0.815292542	,	1.538162981	,	0.979709466	,	1.054825633	,	0.994667035	/)
        Coeff_Latin(	634	,	:)	=	(/	0.900160238	,	0.578930434	,	1.200718002	,	1.031857892	,	0.793369191	/)
        Coeff_Latin(	635	,	:)	=	(/	1.605492237	,	0.775400662	,	0.674035791	,	1.029788355	,	0.785274494	/)
        Coeff_Latin(	636	,	:)	=	(/	0.990782015	,	0.602916277	,	1.118773599	,	0.982142141	,	0.938689585	/)
        Coeff_Latin(	637	,	:)	=	(/	0.979674153	,	0.685737813	,	0.944584298	,	0.894836779	,	0.604186727	/)
        Coeff_Latin(	638	,	:)	=	(/	1.082975378	,	1.328442081	,	0.89800681	,	1.026779107	,	1.000336979	/)
        Coeff_Latin(	639	,	:)	=	(/	1.120157691	,	0.875439084	,	1.151712241	,	0.982105673	,	0.827000382	/)
        Coeff_Latin(	640	,	:)	=	(/	1.099316251	,	1.020420714	,	0.957615715	,	0.936962853	,	0.981633315	/)
        Coeff_Latin(	641	,	:)	=	(/	1.136284996	,	1.426127825	,	0.988364209	,	1.010618777	,	0.923848127	/)
        Coeff_Latin(	642	,	:)	=	(/	1.175426403	,	0.6445578	,	1.011132862	,	1.022576127	,	1.008863186	/)
        Coeff_Latin(	643	,	:)	=	(/	1.402905356	,	1.296984303	,	0.987684005	,	1.006213239	,	0.903309145	/)
        Coeff_Latin(	644	,	:)	=	(/	0.955345583	,	0.792828862	,	1.017706603	,	0.936564191	,	1.378837362	/)
        Coeff_Latin(	645	,	:)	=	(/	0.609321295	,	1.604127059	,	0.983717321	,	0.980989106	,	1.10925476	/)
        Coeff_Latin(	646	,	:)	=	(/	0.754435058	,	1.03300841	,	1.427037792	,	0.941841397	,	1.06839346	/)
        Coeff_Latin(	647	,	:)	=	(/	1.053242684	,	1.015137882	,	0.895735553	,	1.012642921	,	0.920715309	/)
        Coeff_Latin(	648	,	:)	=	(/	1.315048011	,	1.107930132	,	0.91343792	,	1.00560162	,	1.423625949	/)
        Coeff_Latin(	649	,	:)	=	(/	0.60300773	,	0.67583254	,	1.10590151	,	0.97301757	,	1.233709471	/)
        Coeff_Latin(	650	,	:)	=	(/	0.692436453	,	1.179683565	,	0.795391723	,	1.074808824	,	0.936217702	/)
        Coeff_Latin(	651	,	:)	=	(/	1.423827902	,	1.479090661	,	1.029104554	,	0.905896603	,	1.423566834	/)
        Coeff_Latin(	652	,	:)	=	(/	0.560329648	,	1.233479058	,	1.067753474	,	0.940162716	,	0.885259789	/)
        Coeff_Latin(	653	,	:)	=	(/	0.497291475	,	0.547796433	,	0.896560778	,	0.949289017	,	1.133710532	/)
        Coeff_Latin(	654	,	:)	=	(/	0.618204203	,	1.411520934	,	1.12629903	,	1.006765098	,	0.596983017	/)
        Coeff_Latin(	655	,	:)	=	(/	0.569114501	,	0.835004563	,	0.890436401	,	0.995745553	,	0.941451291	/)
        Coeff_Latin(	656	,	:)	=	(/	1.154233805	,	1.028789767	,	0.967620473	,	1.064594427	,	1.163308109	/)
        Coeff_Latin(	657	,	:)	=	(/	0.3183782	,	1.218697448	,	1.232767903	,	0.9847341	,	0.620084614	/)
        Coeff_Latin(	658	,	:)	=	(/	1.083149849	,	0.48135398	,	1.074303209	,	0.984542584	,	0.757602446	/)
        Coeff_Latin(	659	,	:)	=	(/	0.841362523	,	0.709117604	,	1.117207606	,	0.947542033	,	0.755676832	/)
        Coeff_Latin(	660	,	:)	=	(/	0.647112969	,	1.008967755	,	1.146732541	,	0.886479159	,	1.024993028	/)
        Coeff_Latin(	661	,	:)	=	(/	1.359291969	,	1.253174	,	0.738278576	,	0.922631427	,	1.174085345	/)
        Coeff_Latin(	662	,	:)	=	(/	0.710474727	,	1.029032276	,	1.034980642	,	0.97889213	,	0.893162965	/)
        Coeff_Latin(	663	,	:)	=	(/	1.027211307	,	0.56678334	,	1.097955357	,	0.971573967	,	1.146797095	/)
        Coeff_Latin(	664	,	:)	=	(/	0.881064999	,	0.679607827	,	1.038448161	,	0.971489146	,	1.084960882	/)
        Coeff_Latin(	665	,	:)	=	(/	0.997543412	,	0.610112118	,	0.92806953	,	0.909902749	,	1.126660842	/)
        Coeff_Latin(	666	,	:)	=	(/	1.004642044	,	0.619716261	,	0.823242669	,	0.993202913	,	1.144049757	/)
        Coeff_Latin(	667	,	:)	=	(/	1.178032226	,	0.847292626	,	1.17199101	,	1.020807938	,	0.898838334	/)
        Coeff_Latin(	668	,	:)	=	(/	0.716532644	,	0.784748671	,	0.892929602	,	0.929849526	,	0.806436845	/)
        Coeff_Latin(	669	,	:)	=	(/	0.735020074	,	1.135154141	,	1.184174115	,	1.004405504	,	1.059488016	/)
        Coeff_Latin(	670	,	:)	=	(/	0.748681113	,	0.80750262	,	0.927623267	,	0.962129007	,	0.960534638	/)
        Coeff_Latin(	671	,	:)	=	(/	0.670307007	,	1.022547377	,	1.1316372	,	1.067520165	,	0.821148064	/)
        Coeff_Latin(	672	,	:)	=	(/	0.754932262	,	0.660243869	,	1.11709593	,	1.082258296	,	0.828338115	/)
        Coeff_Latin(	673	,	:)	=	(/	0.793988626	,	1.022093424	,	1.125525418	,	0.986513202	,	0.656488898	/)
        Coeff_Latin(	674	,	:)	=	(/	1.50623449	,	1.637419791	,	0.735314283	,	1.130754528	,	1.079115953	/)
        Coeff_Latin(	675	,	:)	=	(/	0.728196007	,	1.03438126	,	1.056223035	,	0.989148597	,	0.829003022	/)
        Coeff_Latin(	676	,	:)	=	(/	1.026755555	,	1.30001024	,	1.032528748	,	0.92533596	,	1.015864191	/)
        Coeff_Latin(	677	,	:)	=	(/	1.366783133	,	1.265874238	,	1.058877302	,	1.014227827	,	0.893219544	/)
        Coeff_Latin(	678	,	:)	=	(/	0.646945093	,	0.708566199	,	1.198975673	,	0.930018626	,	0.831175492	/)
        Coeff_Latin(	679	,	:)	=	(/	1.108917473	,	1.109334141	,	1.072673028	,	1.015519808	,	1.042761281	/)
        Coeff_Latin(	680	,	:)	=	(/	0.764807838	,	1.760537963	,	0.990376515	,	1.08310363	,	0.793070122	/)
        Coeff_Latin(	681	,	:)	=	(/	0.662153285	,	0.865020907	,	1.102862533	,	1.138531104	,	1.039907982	/)
        Coeff_Latin(	682	,	:)	=	(/	0.696570597	,	1.932665824	,	1.335733401	,	1.035638336	,	0.847053647	/)
        Coeff_Latin(	683	,	:)	=	(/	1.262493791	,	1.501208456	,	0.891353228	,	0.921162213	,	0.790009362	/)
        Coeff_Latin(	684	,	:)	=	(/	1.102037795	,	1.609662686	,	0.937866638	,	0.919727676	,	0.745083231	/)
        Coeff_Latin(	685	,	:)	=	(/	0.575398523	,	0.646021741	,	0.89364071	,	0.983986285	,	0.637227233	/)
        Coeff_Latin(	686	,	:)	=	(/	1.426075399	,	1.149429436	,	1.210251086	,	1.017645083	,	1.122594579	/)
        Coeff_Latin(	687	,	:)	=	(/	1.38648899	,	1.202115364	,	0.964125086	,	1.012164756	,	1.420861994	/)
        Coeff_Latin(	688	,	:)	=	(/	0.772249332	,	1.181804652	,	0.915719972	,	0.987360179	,	0.764231076	/)
        Coeff_Latin(	689	,	:)	=	(/	0.45387973	,	1.280994673	,	1.216852742	,	1.055877042	,	0.888769535	/)
        Coeff_Latin(	690	,	:)	=	(/	0.907003005	,	0.402805353	,	0.852897083	,	1.060233193	,	0.774828576	/)
        Coeff_Latin(	691	,	:)	=	(/	0.939340924	,	1.577220978	,	1.001808104	,	1.057010475	,	1.211921341	/)
        Coeff_Latin(	692	,	:)	=	(/	1.127894782	,	1.280145861	,	1.035924942	,	0.995436953	,	0.669669501	/)
        Coeff_Latin(	693	,	:)	=	(/	0.761069111	,	0.946009175	,	1.018665258	,	1.096352774	,	1.43800062	/)
        Coeff_Latin(	694	,	:)	=	(/	0.971154882	,	1.058325325	,	1.163248005	,	0.949153659	,	0.877742097	/)
        Coeff_Latin(	695	,	:)	=	(/	0.931042311	,	0.993114946	,	1.219689031	,	0.98142064	,	1.115483695	/)
        Coeff_Latin(	696	,	:)	=	(/	0.954496123	,	0.808614605	,	0.872748436	,	1.037286071	,	0.914953216	/)
        Coeff_Latin(	697	,	:)	=	(/	1.19973901	,	0.226980427	,	1.103311786	,	0.984240966	,	1.087982455	/)
        Coeff_Latin(	698	,	:)	=	(/	1.291582219	,	0.96111906	,	0.579477733	,	1.005366677	,	1.292720509	/)
        Coeff_Latin(	699	,	:)	=	(/	0.977097866	,	1.234202886	,	1.009527101	,	0.996399476	,	0.9902946	/)
        Coeff_Latin(	700	,	:)	=	(/	1.020206762	,	0.721775698	,	1.303718961	,	1.095942129	,	1.219297056	/)
        Coeff_Latin(	701	,	:)	=	(/	1.069625503	,	1.054324401	,	1.037529963	,	1.043590245	,	0.551215663	/)
        Coeff_Latin(	702	,	:)	=	(/	1.033561915	,	0.976010468	,	1.140333176	,	0.929952563	,	1.022598722	/)
        Coeff_Latin(	703	,	:)	=	(/	0.991026582	,	0.62090306	,	1.03326084	,	0.950645179	,	1.350188333	/)
        Coeff_Latin(	704	,	:)	=	(/	0.902449174	,	0.984401461	,	1.115785655	,	0.985258918	,	0.937858614	/)
        Coeff_Latin(	705	,	:)	=	(/	0.567641042	,	0.396712371	,	1.210043587	,	0.941146359	,	1.497556413	/)
        Coeff_Latin(	706	,	:)	=	(/	0.670447	,	1.130134176	,	0.965812342	,	1.031400474	,	1.117928118	/)
        Coeff_Latin(	707	,	:)	=	(/	1.233740555	,	0.883121676	,	1.139325991	,	0.959740601	,	0.968832846	/)
        Coeff_Latin(	708	,	:)	=	(/	1.149952443	,	0.78559746	,	1.167179168	,	1.052442488	,	1.070009401	/)
        Coeff_Latin(	709	,	:)	=	(/	0.891407981	,	1.256587458	,	1.123192143	,	1.098797519	,	1.258654986	/)
        Coeff_Latin(	710	,	:)	=	(/	1.506032806	,	0.440429366	,	1.086945566	,	0.968529517	,	1.136698966	/)
        Coeff_Latin(	711	,	:)	=	(/	1.029988269	,	0.886498865	,	1.101683411	,	0.979975503	,	0.554240892	/)
        Coeff_Latin(	712	,	:)	=	(/	0.961016908	,	0.595753836	,	1.205951272	,	1.044340357	,	1.117423226	/)
        Coeff_Latin(	713	,	:)	=	(/	0.926697534	,	0.817606368	,	1.069254825	,	0.976369405	,	0.919314521	/)
        Coeff_Latin(	714	,	:)	=	(/	0.803834185	,	1.575562838	,	1.076404256	,	0.969415838	,	0.796083387	/)
        Coeff_Latin(	715	,	:)	=	(/	0.915940107	,	1.304468973	,	1.111721388	,	1.032828556	,	1.294983093	/)
        Coeff_Latin(	716	,	:)	=	(/	0.999146695	,	1.284424355	,	1.041718111	,	1.032468277	,	1.166130961	/)
        Coeff_Latin(	717	,	:)	=	(/	0.960423365	,	0.998914651	,	1.18336967	,	0.93899754	,	0.960738288	/)
        Coeff_Latin(	718	,	:)	=	(/	0.780685278	,	0.691416453	,	1.14347027	,	0.939103797	,	0.907579892	/)
        Coeff_Latin(	719	,	:)	=	(/	0.853897647	,	1.340274437	,	1.10827173	,	1.053445111	,	0.763416325	/)
        Coeff_Latin(	720	,	:)	=	(/	0.958965505	,	1.013978762	,	0.994009105	,	0.994557771	,	0.859946943	/)
        Coeff_Latin(	721	,	:)	=	(/	0.87023313	,	1.057747481	,	1.067620999	,	0.962973453	,	0.836579067	/)
        Coeff_Latin(	722	,	:)	=	(/	1.163224883	,	1.024750614	,	0.923332451	,	0.960183949	,	0.883791045	/)
        Coeff_Latin(	723	,	:)	=	(/	1.137494236	,	1.240078914	,	1.12347008	,	0.92490622	,	0.74920768	/)
        Coeff_Latin(	724	,	:)	=	(/	0.725543356	,	0.8757976	,	1.048058984	,	0.999199376	,	0.955023093	/)
        Coeff_Latin(	725	,	:)	=	(/	1.332268396	,	1.233484987	,	0.956907901	,	1.020266926	,	1.112788513	/)
        Coeff_Latin(	726	,	:)	=	(/	1.086560114	,	0.907952309	,	1.066059714	,	1.004662182	,	0.824530599	/)
        Coeff_Latin(	727	,	:)	=	(/	0.845706932	,	1.051030305	,	0.768775966	,	1.000098867	,	1.348616642	/)
        Coeff_Latin(	728	,	:)	=	(/	1.29385757	,	1.067132524	,	1.031759413	,	0.937026674	,	1.154373185	/)
        Coeff_Latin(	729	,	:)	=	(/	1.083557456	,	0.599971906	,	1.020017049	,	1.056722408	,	1.069992441	/)
        Coeff_Latin(	730	,	:)	=	(/	1.013302396	,	0.95315017	,	0.951156471	,	0.95862244	,	0.93332231	/)
        Coeff_Latin(	731	,	:)	=	(/	0.883767064	,	1.102168264	,	0.746157592	,	1.003188431	,	0.87045256	/)
        Coeff_Latin(	732	,	:)	=	(/	0.832123918	,	0.565050196	,	0.706310578	,	0.93828482	,	0.966513795	/)
        Coeff_Latin(	733	,	:)	=	(/	0.802245174	,	0.898479094	,	0.951104303	,	0.939341078	,	1.324056261	/)
        Coeff_Latin(	734	,	:)	=	(/	0.636125402	,	1.207722676	,	1.119287763	,	1.015867319	,	1.061685448	/)
        Coeff_Latin(	735	,	:)	=	(/	0.999045415	,	0.84847269	,	1.28824585	,	0.879399119	,	1.128066904	/)
        Coeff_Latin(	736	,	:)	=	(/	0.555592228	,	1.313710061	,	1.174313201	,	1.051296966	,	0.726660275	/)
        Coeff_Latin(	737	,	:)	=	(/	1.266264539	,	1.517752651	,	0.847045195	,	0.932928695	,	0.925141122	/)
        Coeff_Latin(	738	,	:)	=	(/	1.063332575	,	0.773997448	,	0.792886873	,	1.026221589	,	0.741555825	/)
        Coeff_Latin(	739	,	:)	=	(/	0.924830089	,	1.423403294	,	0.896090607	,	0.990131938	,	1.009855892	/)
        Coeff_Latin(	740	,	:)	=	(/	1.091022562	,	0.939371676	,	0.984108833	,	1.045840872	,	0.990235061	/)
        Coeff_Latin(	741	,	:)	=	(/	0.886884633	,	1.121643915	,	1.005084863	,	1.043144661	,	1.141242461	/)
        Coeff_Latin(	742	,	:)	=	(/	1.305722774	,	0.868604101	,	1.022842224	,	1.067400355	,	1.072294714	/)
        Coeff_Latin(	743	,	:)	=	(/	0.743336599	,	0.748854686	,	1.320318596	,	0.982878005	,	1.092840781	/)
        Coeff_Latin(	744	,	:)	=	(/	0.944425873	,	1.327548881	,	1.070052345	,	0.98493931	,	1.003959865	/)
        Coeff_Latin(	745	,	:)	=	(/	1.091726349	,	0.959174353	,	0.733571742	,	0.992693743	,	1.2177968	/)
        Coeff_Latin(	746	,	:)	=	(/	1.354971276	,	1.023331995	,	1.095961821	,	0.863096067	,	1.097401899	/)
        Coeff_Latin(	747	,	:)	=	(/	1.164147773	,	0.929386671	,	0.844991816	,	1.041655396	,	0.590563809	/)
        Coeff_Latin(	748	,	:)	=	(/	0.698419177	,	1.277274721	,	1.017710314	,	0.986847769	,	0.890243836	/)
        Coeff_Latin(	749	,	:)	=	(/	1.301229589	,	1.066931781	,	1.154340057	,	1.020433929	,	1.102163551	/)
        Coeff_Latin(	750	,	:)	=	(/	1.174770835	,	0.7009435	,	0.800602663	,	0.959364724	,	0.931374345	/)
        Coeff_Latin(	751	,	:)	=	(/	1.144475529	,	0.945521253	,	0.959253276	,	0.973193745	,	0.798484005	/)
        Coeff_Latin(	752	,	:)	=	(/	1.156687158	,	0.67943379	,	1.033150901	,	0.965849817	,	1.16226033	/)
        Coeff_Latin(	753	,	:)	=	(/	1.128546667	,	1.183638209	,	1.155301111	,	0.966919207	,	0.970967237	/)
        Coeff_Latin(	754	,	:)	=	(/	0.69643397	,	1.228644931	,	1.130535589	,	1.000294724	,	1.049976166	/)
        Coeff_Latin(	755	,	:)	=	(/	1.468491786	,	0.761259618	,	0.902217214	,	0.983294941	,	1.004608774	/)
        Coeff_Latin(	756	,	:)	=	(/	0.763255888	,	1.031083566	,	1.065806646	,	1.06750203	,	1.223096358	/)
        Coeff_Latin(	757	,	:)	=	(/	1.130460867	,	1.050218298	,	1.065770142	,	1.107227769	,	1.11878051	/)
        Coeff_Latin(	758	,	:)	=	(/	0.859061554	,	1.371898966	,	0.897465294	,	0.945605964	,	0.846753069	/)
        Coeff_Latin(	759	,	:)	=	(/	1.083417497	,	1.094163806	,	1.086182541	,	1.026889468	,	0.844229776	/)
        Coeff_Latin(	760	,	:)	=	(/	1.112281436	,	1.117368147	,	0.984005917	,	1.034204417	,	0.855155075	/)
        Coeff_Latin(	761	,	:)	=	(/	1.301238797	,	1.653654563	,	0.973159905	,	0.958134754	,	1.432183707	/)
        Coeff_Latin(	762	,	:)	=	(/	1.234285296	,	0.947125357	,	1.042520917	,	0.994939208	,	1.154376255	/)
        Coeff_Latin(	763	,	:)	=	(/	1.213815803	,	1.26016599	,	0.954009065	,	0.988275972	,	1.409527217	/)
        Coeff_Latin(	764	,	:)	=	(/	1.145821716	,	0.846178416	,	0.843809155	,	1.021367568	,	1.302465871	/)
        Coeff_Latin(	765	,	:)	=	(/	1.008073466	,	1.386336424	,	0.887385314	,	0.965669353	,	0.737922956	/)
        Coeff_Latin(	766	,	:)	=	(/	0.673847265	,	0.660384296	,	0.889713721	,	0.967448069	,	0.616034008	/)
        Coeff_Latin(	767	,	:)	=	(/	0.876041529	,	1.437399054	,	1.098552209	,	1.030973287	,	1.346014628	/)
        Coeff_Latin(	768	,	:)	=	(/	0.756744844	,	1.151707364	,	1.004678459	,	0.982265352	,	0.736657628	/)
        Coeff_Latin(	769	,	:)	=	(/	0.777457664	,	0.917192853	,	0.925471585	,	1.100685396	,	0.694865624	/)
        Coeff_Latin(	770	,	:)	=	(/	1.06437264	,	1.590315467	,	0.93060059	,	1.021202483	,	0.982829693	/)
        Coeff_Latin(	771	,	:)	=	(/	0.743587039	,	0.801875597	,	1.053826239	,	1.025494979	,	0.821173955	/)
        Coeff_Latin(	772	,	:)	=	(/	1.227571315	,	0.518753459	,	1.252113601	,	1.072137099	,	0.951565056	/)
        Coeff_Latin(	773	,	:)	=	(/	1.090143547	,	0.806574505	,	0.827732376	,	0.994805501	,	1.029843379	/)
        Coeff_Latin(	774	,	:)	=	(/	0.984017665	,	0.8904256	,	0.999793571	,	1.027565404	,	1.024863778	/)
        Coeff_Latin(	775	,	:)	=	(/	1.174518912	,	0.943865178	,	1.213864889	,	1.034203424	,	1.090118174	/)
        Coeff_Latin(	776	,	:)	=	(/	1.428650973	,	0.984335242	,	0.785996714	,	1.024440644	,	1.279313063	/)
        Coeff_Latin(	777	,	:)	=	(/	1.178406623	,	1.713218285	,	1.055163451	,	0.994077403	,	1.031799962	/)
        Coeff_Latin(	778	,	:)	=	(/	1.412643075	,	0.599219348	,	1.256665959	,	0.928752005	,	0.718976389	/)
        Coeff_Latin(	779	,	:)	=	(/	1.062252923	,	1.205626628	,	0.932020544	,	0.941492584	,	0.732093917	/)
        Coeff_Latin(	780	,	:)	=	(/	1.328831475	,	0.824401218	,	0.810609865	,	0.923960653	,	1.136763183	/)
        Coeff_Latin(	781	,	:)	=	(/	0.988888274	,	0.711914921	,	0.904310858	,	0.933957156	,	0.841699615	/)
        Coeff_Latin(	782	,	:)	=	(/	1.368595224	,	0.917488655	,	0.928216314	,	0.956149013	,	1.077944137	/)
        Coeff_Latin(	783	,	:)	=	(/	0.937328657	,	1.137816517	,	0.831995917	,	0.975490142	,	1.106171644	/)
        Coeff_Latin(	784	,	:)	=	(/	0.934600043	,	1.033750424	,	0.691714092	,	1.040684618	,	0.964532377	/)
        Coeff_Latin(	785	,	:)	=	(/	0.600090238	,	0.924854538	,	1.201242664	,	1.07459456	,	1.160998553	/)
        Coeff_Latin(	786	,	:)	=	(/	1.029955756	,	1.366754519	,	1.121282495	,	0.972278523	,	1.025338253	/)
        Coeff_Latin(	787	,	:)	=	(/	0.971735432	,	1.322158728	,	1.021550596	,	0.94899937	,	0.893735234	/)
        Coeff_Latin(	788	,	:)	=	(/	0.891210564	,	0.827515534	,	0.918774833	,	1.048144097	,	0.876407304	/)
        Coeff_Latin(	789	,	:)	=	(/	0.673918244	,	1.234355015	,	1.075975089	,	0.918465819	,	1.234744146	/)
        Coeff_Latin(	790	,	:)	=	(/	1.353834187	,	1.248017207	,	0.852541801	,	0.999521181	,	0.740331011	/)
        Coeff_Latin(	791	,	:)	=	(/	1.139478228	,	0.735967262	,	1.159325083	,	0.984316973	,	0.653326438	/)
        Coeff_Latin(	792	,	:)	=	(/	0.809720357	,	1.15828218	,	0.886015855	,	1.079621716	,	1.043658765	/)
        Coeff_Latin(	793	,	:)	=	(/	1.114117093	,	1.091555945	,	1.119710101	,	1.072545493	,	1.102557595	/)
        Coeff_Latin(	794	,	:)	=	(/	1.131986152	,	1.721138394	,	0.987805029	,	1.004848203	,	1.062049454	/)
        Coeff_Latin(	795	,	:)	=	(/	0.627583892	,	1.014416582	,	0.878568057	,	0.984818026	,	1.557507526	/)
        Coeff_Latin(	796	,	:)	=	(/	1.248519858	,	0.655363029	,	1.159695112	,	1.089622227	,	1.047689649	/)
        Coeff_Latin(	797	,	:)	=	(/	0.334086316	,	1.157091329	,	1.068415194	,	0.971384975	,	0.913668425	/)
        Coeff_Latin(	798	,	:)	=	(/	0.88323191	,	1.030971642	,	0.824458177	,	1.000502018	,	1.269144193	/)
        Coeff_Latin(	799	,	:)	=	(/	0.864090402	,	0.467146628	,	1.033330865	,	0.964830884	,	1.236138427	/)
        Coeff_Latin(	800	,	:)	=	(/	0.56571275	,	0.68566916	,	0.993934586	,	0.950814339	,	1.013045376	/)
        Coeff_Latin(	801	,	:)	=	(/	1.02601487	,	1.02515912	,	1.040313132	,	0.937443755	,	1.222878105	/)
        Coeff_Latin(	802	,	:)	=	(/	0.969442302	,	1.403880327	,	0.911196191	,	1.089946629	,	0.886690732	/)
        Coeff_Latin(	803	,	:)	=	(/	0.979585465	,	1.139159628	,	0.971848163	,	1.03598759	,	0.8469715	/)
        Coeff_Latin(	804	,	:)	=	(/	1.496008043	,	0.854748673	,	0.730207585	,	0.989274532	,	0.979210521	/)
        Coeff_Latin(	805	,	:)	=	(/	0.805232829	,	1.69908519	,	1.155100473	,	0.962705423	,	1.021180215	/)
        Coeff_Latin(	806	,	:)	=	(/	0.975696469	,	0.769203275	,	1.11165913	,	1.057555995	,	0.976652921	/)
        Coeff_Latin(	807	,	:)	=	(/	1.34848951	,	0.999051785	,	1.164859539	,	0.956588695	,	1.374628222	/)
        Coeff_Latin(	808	,	:)	=	(/	1.022366157	,	0.794564969	,	0.990215562	,	1.050810517	,	1.063502315	/)
        Coeff_Latin(	809	,	:)	=	(/	0.79257494	,	0.743641322	,	0.906024839	,	0.936572691	,	0.852583214	/)
        Coeff_Latin(	810	,	:)	=	(/	1.267778107	,	0.410068139	,	0.961552448	,	0.913086409	,	0.939339773	/)
        Coeff_Latin(	811	,	:)	=	(/	0.450848262	,	0.570087028	,	0.844964708	,	1.025924495	,	1.330259196	/)
        Coeff_Latin(	812	,	:)	=	(/	0.690540526	,	0.41584715	,	1.0959376	,	1.008424707	,	0.920608091	/)
        Coeff_Latin(	813	,	:)	=	(/	1.336882435	,	1.22189226	,	1.294268409	,	1.056136551	,	1.337529637	/)
        Coeff_Latin(	814	,	:)	=	(/	1.173401035	,	0.962551801	,	0.948718771	,	1.016756693	,	1.0723185	/)
        Coeff_Latin(	815	,	:)	=	(/	0.93383099	,	0.502316605	,	1.000150148	,	1.091231746	,	0.956718931	/)
        Coeff_Latin(	816	,	:)	=	(/	1.140243915	,	1.264046893	,	1.034532539	,	1.020786087	,	0.99394879	/)
        Coeff_Latin(	817	,	:)	=	(/	0.617610948	,	0.850212382	,	1.091720747	,	0.908145654	,	0.841384929	/)
        Coeff_Latin(	818	,	:)	=	(/	0.958068651	,	0.699970005	,	0.722882742	,	1.048207672	,	1.169013371	/)
        Coeff_Latin(	819	,	:)	=	(/	1.060645416	,	0.660079377	,	0.852221703	,	1.00251166	,	0.901403246	/)
        Coeff_Latin(	820	,	:)	=	(/	1.186523472	,	1.308723898	,	0.978480774	,	0.885793791	,	1.070781418	/)
        Coeff_Latin(	821	,	:)	=	(/	0.525134671	,	0.485337155	,	0.968580633	,	1.006564847	,	0.958490221	/)
        Coeff_Latin(	822	,	:)	=	(/	1.521819008	,	1.084506088	,	1.082381495	,	1.062773269	,	1.08369984	/)
        Coeff_Latin(	823	,	:)	=	(/	1.040875336	,	0.912800159	,	0.958709124	,	1.056820495	,	1.25721467	/)
        Coeff_Latin(	824	,	:)	=	(/	0.973412653	,	1.26022756	,	0.97357797	,	1.071942111	,	0.753249556	/)
        Coeff_Latin(	825	,	:)	=	(/	0.891170878	,	0.997206173	,	1.285363166	,	1.003446189	,	1.083908897	/)
        Coeff_Latin(	826	,	:)	=	(/	0.842436838	,	0.407624474	,	0.982669595	,	1.01385848	,	1.037071179	/)
        Coeff_Latin(	827	,	:)	=	(/	0.704510249	,	1.02664622	,	1.09699987	,	0.94614657	,	1.294514877	/)
        Coeff_Latin(	828	,	:)	=	(/	1.624412974	,	0.222360963	,	1.253086729	,	0.996009808	,	1.096817324	/)
        Coeff_Latin(	829	,	:)	=	(/	0.856744303	,	1.3635265	,	1.049913313	,	1.008866918	,	0.943994135	/)
        Coeff_Latin(	830	,	:)	=	(/	1.176026026	,	1.203398316	,	1.223737537	,	1.030693556	,	1.005314841	/)
        Coeff_Latin(	831	,	:)	=	(/	1.145114349	,	1.107448737	,	0.834191855	,	0.990670445	,	0.901151517	/)
        Coeff_Latin(	832	,	:)	=	(/	1.125113172	,	1.248797855	,	0.822094572	,	1.002017419	,	0.757197065	/)
        Coeff_Latin(	833	,	:)	=	(/	1.509748998	,	1.047703821	,	1.091907384	,	0.977383801	,	0.913509693	/)
        Coeff_Latin(	834	,	:)	=	(/	0.613223397	,	1.141704554	,	0.924067074	,	1.07026312	,	0.946656225	/)
        Coeff_Latin(	835	,	:)	=	(/	0.926919567	,	1.321265991	,	1.103305346	,	1.000764866	,	0.883225621	/)
        Coeff_Latin(	836	,	:)	=	(/	0.320741723	,	1.634287553	,	1.151848791	,	1.062657139	,	1.434351928	/)
        Coeff_Latin(	837	,	:)	=	(/	1.140548475	,	1.338986056	,	1.126895777	,	1.091832873	,	1.168396525	/)
        Coeff_Latin(	838	,	:)	=	(/	1.148531405	,	0.995538265	,	1.152478301	,	1.002199319	,	0.908427416	/)
        Coeff_Latin(	839	,	:)	=	(/	0.807662118	,	1.100898778	,	0.891946588	,	0.944388101	,	1.202994728	/)
        Coeff_Latin(	840	,	:)	=	(/	1.129521206	,	0.811274684	,	0.803092414	,	0.935839151	,	0.990390816	/)
        Coeff_Latin(	841	,	:)	=	(/	0.921206577	,	0.922872168	,	0.854655385	,	0.961651373	,	1.085589376	/)
        Coeff_Latin(	842	,	:)	=	(/	1.056609667	,	0.852209204	,	0.94097716	,	1.00715374	,	1.12984351	/)
        Coeff_Latin(	843	,	:)	=	(/	0.713600775	,	0.850134632	,	1.180328314	,	1.001387248	,	1.092624274	/)
        Coeff_Latin(	844	,	:)	=	(/	0.940515999	,	1.202741682	,	0.777926117	,	1.019285028	,	0.672171595	/)
        Coeff_Latin(	845	,	:)	=	(/	1.017910716	,	1.099149202	,	1.004397434	,	1.048220996	,	0.940968246	/)
        Coeff_Latin(	846	,	:)	=	(/	0.601770774	,	0.829654438	,	0.88864958	,	1.017601434	,	1.092862646	/)
        Coeff_Latin(	847	,	:)	=	(/	1.081977257	,	0.949748864	,	0.983490764	,	0.99841308	,	0.479682594	/)
        Coeff_Latin(	848	,	:)	=	(/	0.825149348	,	0.9178041	,	0.893580044	,	0.873451521	,	1.279019074	/)
        Coeff_Latin(	849	,	:)	=	(/	0.932551167	,	1.171841127	,	0.897160886	,	1.005775516	,	0.789375265	/)
        Coeff_Latin(	850	,	:)	=	(/	0.868128053	,	0.653350426	,	1.077492962	,	0.982681019	,	0.976173224	/)
        Coeff_Latin(	851	,	:)	=	(/	1.104029185	,	0.881741186	,	1.088534362	,	1.039984364	,	1.191111122	/)
        Coeff_Latin(	852	,	:)	=	(/	1.216922751	,	0.80314769	,	1.006662795	,	1.037548538	,	0.742348799	/)
        Coeff_Latin(	853	,	:)	=	(/	0.958012376	,	0.446968303	,	1.086204155	,	1.088868111	,	1.334738641	/)
        Coeff_Latin(	854	,	:)	=	(/	0.960015514	,	1.011327755	,	0.959666348	,	0.946798577	,	0.931993261	/)
        Coeff_Latin(	855	,	:)	=	(/	0.901460974	,	0.84225836	,	1.141158989	,	0.961620023	,	0.788377541	/)
        Coeff_Latin(	856	,	:)	=	(/	1.016605399	,	0.653001337	,	1.150093099	,	1.024478781	,	1.152521264	/)
        Coeff_Latin(	857	,	:)	=	(/	0.653709501	,	1.339687603	,	1.172885423	,	1.00559859	,	1.085354315	/)
        Coeff_Latin(	858	,	:)	=	(/	0.883827703	,	1.278599997	,	0.788111993	,	1.01609405	,	1.03320842	/)
        Coeff_Latin(	859	,	:)	=	(/	1.387161359	,	0.871987384	,	0.800662674	,	0.939909202	,	0.686963932	/)
        Coeff_Latin(	860	,	:)	=	(/	0.881224995	,	1.054780663	,	0.995184753	,	0.994647348	,	1.088518901	/)
        Coeff_Latin(	861	,	:)	=	(/	1.254182062	,	1.099514876	,	1.104181224	,	1.097375383	,	1.045861842	/)
        Coeff_Latin(	862	,	:)	=	(/	1.105536115	,	0.315369051	,	1.047662559	,	0.929102394	,	0.866246857	/)
        Coeff_Latin(	863	,	:)	=	(/	0.703223084	,	1.033257492	,	0.890786945	,	0.968997778	,	0.463362754	/)
        Coeff_Latin(	864	,	:)	=	(/	1.136573783	,	0.879127571	,	0.915763929	,	1.013084584	,	1.016399454	/)
        Coeff_Latin(	865	,	:)	=	(/	1.027004777	,	0.634251432	,	0.934105369	,	0.966300879	,	1.281066484	/)
        Coeff_Latin(	866	,	:)	=	(/	1.024836675	,	0.594183195	,	1.137050805	,	1.052230622	,	0.90387228	/)
        Coeff_Latin(	867	,	:)	=	(/	0.923956481	,	1.022780554	,	1.038215469	,	1.042915448	,	1.071059078	/)
        Coeff_Latin(	868	,	:)	=	(/	0.811550384	,	0.507256316	,	0.979744976	,	0.967961353	,	0.986889528	/)
        Coeff_Latin(	869	,	:)	=	(/	0.765432691	,	1.142584307	,	1.142475714	,	0.998031669	,	0.972541795	/)
        Coeff_Latin(	870	,	:)	=	(/	1.06111279	,	1.467073599	,	1.000262865	,	0.970285994	,	0.920878063	/)
        Coeff_Latin(	871	,	:)	=	(/	0.816333965	,	1.384921246	,	0.875653799	,	0.967108924	,	1.286061976	/)
        Coeff_Latin(	872	,	:)	=	(/	1.172090585	,	1.063412842	,	0.708356119	,	1.043137306	,	0.955260461	/)
        Coeff_Latin(	873	,	:)	=	(/	0.915046341	,	1.00811881	,	0.661437862	,	1.024389416	,	1.152414374	/)
        Coeff_Latin(	874	,	:)	=	(/	1.528585406	,	1.226138533	,	1.10892314	,	1.036983866	,	1.228725005	/)
        Coeff_Latin(	875	,	:)	=	(/	1.05630441	,	0.982043942	,	1.042880511	,	0.922932886	,	0.792505536	/)
        Coeff_Latin(	876	,	:)	=	(/	0.672057349	,	1.06813494	,	1.35579674	,	0.997998406	,	0.910346824	/)
        Coeff_Latin(	877	,	:)	=	(/	1.051264587	,	0.561639418	,	0.836801157	,	1.097411775	,	0.902030403	/)
        Coeff_Latin(	878	,	:)	=	(/	1.198459598	,	1.607160261	,	1.090873914	,	0.992035873	,	0.682256939	/)
        Coeff_Latin(	879	,	:)	=	(/	0.824648663	,	1.127323016	,	1.106725676	,	0.964098886	,	0.939277533	/)
        Coeff_Latin(	880	,	:)	=	(/	1.590587481	,	1.187282568	,	1.071688887	,	1.034152489	,	0.702160366	/)
        Coeff_Latin(	881	,	:)	=	(/	1.040240497	,	0.407735447	,	1.109184057	,	0.986147707	,	0.947695691	/)
        Coeff_Latin(	882	,	:)	=	(/	0.821465875	,	1.087683084	,	1.023921586	,	0.891202932	,	1.239188985	/)
        Coeff_Latin(	883	,	:)	=	(/	0.990631923	,	0.879567553	,	1.193570649	,	0.996849711	,	1.391298787	/)
        Coeff_Latin(	884	,	:)	=	(/	1.196511712	,	0.903599464	,	0.907446332	,	0.99536756	,	1.072558837	/)
        Coeff_Latin(	885	,	:)	=	(/	0.78710706	,	1.640766044	,	1.068884953	,	0.998765362	,	1.062489899	/)
        Coeff_Latin(	886	,	:)	=	(/	0.949420285	,	1.443611092	,	1.049026139	,	1.002436093	,	0.720733932	/)
        Coeff_Latin(	887	,	:)	=	(/	0.961225603	,	1.097388377	,	0.851904199	,	1.025256387	,	0.780024997	/)
        Coeff_Latin(	888	,	:)	=	(/	1.298492041	,	1.087530283	,	1.264382253	,	0.974279521	,	0.988911578	/)
        Coeff_Latin(	889	,	:)	=	(/	1.083436191	,	1.191284173	,	1.104975707	,	0.944566132	,	1.084530832	/)
        Coeff_Latin(	890	,	:)	=	(/	1.510184404	,	0.623187289	,	0.853297399	,	0.987635033	,	0.925702124	/)
        Coeff_Latin(	891	,	:)	=	(/	1.008901135	,	1.035972558	,	1.119841992	,	1.007993937	,	1.024994947	/)
        Coeff_Latin(	892	,	:)	=	(/	1.066453384	,	0.167750296	,	1.109826552	,	0.926158639	,	1.147105831	/)
        Coeff_Latin(	893	,	:)	=	(/	1.115775997	,	1.439288766	,	0.813828225	,	1.082986352	,	0.447106188	/)
        Coeff_Latin(	894	,	:)	=	(/	0.801647057	,	1.232964601	,	0.946653849	,	0.977911501	,	0.841667616	/)
        Coeff_Latin(	895	,	:)	=	(/	1.455520364	,	0.825154005	,	0.978852326	,	1.004452012	,	0.917398221	/)
        Coeff_Latin(	896	,	:)	=	(/	1.22355763	,	1.492706957	,	0.826341225	,	1.008023303	,	0.78678419	/)
        Coeff_Latin(	897	,	:)	=	(/	1.180781713	,	1.153708053	,	0.8646037	,	0.981528474	,	0.988232764	/)
        Coeff_Latin(	898	,	:)	=	(/	1.065284641	,	1.360716759	,	0.888258714	,	0.944138541	,	0.57496526	/)
        Coeff_Latin(	899	,	:)	=	(/	0.74490303	,	1.043584607	,	1.08433987	,	1.007992136	,	1.016100749	/)
        Coeff_Latin(	900	,	:)	=	(/	1.436585373	,	0.520232175	,	0.941653594	,	1.031127224	,	0.664979316	/)
        Coeff_Latin(	901	,	:)	=	(/	1.165579574	,	0.640401577	,	0.979093325	,	1.127898718	,	0.937801829	/)
        Coeff_Latin(	902	,	:)	=	(/	1.039089547	,	1.52631252	,	0.956159361	,	1.102340126	,	0.922887735	/)
        Coeff_Latin(	903	,	:)	=	(/	1.038342965	,	0.183842066	,	1.15331926	,	1.040542789	,	1.043460199	/)
        Coeff_Latin(	904	,	:)	=	(/	0.940232807	,	0.94891574	,	1.189668354	,	0.945796358	,	1.20371864	/)
        Coeff_Latin(	905	,	:)	=	(/	0.958860086	,	0.724371054	,	0.914010272	,	1.00216764	,	0.875873134	/)
        Coeff_Latin(	906	,	:)	=	(/	0.726384678	,	1.256825852	,	1.052740395	,	0.995585895	,	1.015670013	/)
        Coeff_Latin(	907	,	:)	=	(/	0.523085359	,	1.309667759	,	1.198961723	,	1.018634448	,	1.262050006	/)
        Coeff_Latin(	908	,	:)	=	(/	1.148178384	,	0.896649647	,	1.01430158	,	1.037697345	,	0.906723646	/)
        Coeff_Latin(	909	,	:)	=	(/	1.178511503	,	1.134437775	,	0.87614876	,	1.066253981	,	1.026113893	/)
        Coeff_Latin(	910	,	:)	=	(/	0.62499458	,	0.743099928	,	0.810147383	,	1.034191755	,	0.799894102	/)
        Coeff_Latin(	911	,	:)	=	(/	0.669382499	,	0.879555933	,	1.101422855	,	0.896301956	,	1.004628623	/)
        Coeff_Latin(	912	,	:)	=	(/	0.98813197	,	1.419507003	,	1.032221581	,	0.992316658	,	0.999866542	/)
        Coeff_Latin(	913	,	:)	=	(/	0.921721744	,	1.138418597	,	1.204935003	,	0.945995794	,	0.918399393	/)
        Coeff_Latin(	914	,	:)	=	(/	1.061298656	,	1.039365473	,	0.979868945	,	1.055232261	,	1.285811929	/)
        Coeff_Latin(	915	,	:)	=	(/	1.161289628	,	1.225318217	,	1.0442202	,	1.023793795	,	0.972705371	/)
        Coeff_Latin(	916	,	:)	=	(/	1.261018139	,	1.030667316	,	0.98816792	,	1.085785136	,	1.01835234	/)
        Coeff_Latin(	917	,	:)	=	(/	0.906712931	,	1.23108849	,	0.909750292	,	0.942334407	,	1.161004914	/)
        Coeff_Latin(	918	,	:)	=	(/	0.910194138	,	0.525626195	,	0.766987289	,	0.995485915	,	0.541468228	/)
        Coeff_Latin(	919	,	:)	=	(/	0.657074432	,	1.012316584	,	0.770583051	,	1.09127748	,	0.860208197	/)
        Coeff_Latin(	920	,	:)	=	(/	0.686454002	,	0.49300594	,	0.766528217	,	1.041959496	,	1.247810861	/)
        Coeff_Latin(	921	,	:)	=	(/	1.067157278	,	1.185393457	,	0.999920778	,	1.0822532	,	1.266302147	/)
        Coeff_Latin(	922	,	:)	=	(/	1.052990693	,	0.935030415	,	0.898601519	,	1.066013915	,	1.129456441	/)
        Coeff_Latin(	923	,	:)	=	(/	1.022359961	,	0.741139007	,	0.821524756	,	0.936376616	,	0.543020931	/)
        Coeff_Latin(	924	,	:)	=	(/	0.964793954	,	0.556434456	,	0.889571618	,	1.011419832	,	0.999925578	/)
        Coeff_Latin(	925	,	:)	=	(/	0.994509518	,	1.348244911	,	0.720551299	,	0.948285593	,	1.266003477	/)
        Coeff_Latin(	926	,	:)	=	(/	0.911461429	,	0.823203112	,	0.831294121	,	1.073589541	,	1.039881094	/)
        Coeff_Latin(	927	,	:)	=	(/	0.756983382	,	0.671897635	,	1.106121038	,	0.975842322	,	0.651328078	/)
        Coeff_Latin(	928	,	:)	=	(/	1.106344766	,	1.101661143	,	1.212072923	,	0.926244111	,	1.127620097	/)
        Coeff_Latin(	929	,	:)	=	(/	1.147939194	,	1.581413336	,	1.029920268	,	1.121090763	,	1.13046183	/)
        Coeff_Latin(	930	,	:)	=	(/	0.916407831	,	1.475153314	,	1.181616621	,	1.100517854	,	0.982313447	/)
        Coeff_Latin(	931	,	:)	=	(/	0.999598198	,	0.862634595	,	1.056698476	,	1.031627298	,	1.021202747	/)
        Coeff_Latin(	932	,	:)	=	(/	0.787222093	,	1.225466626	,	0.980939797	,	1.028076387	,	0.96846178	/)
        Coeff_Latin(	933	,	:)	=	(/	1.22041063	,	0.909928763	,	0.989170996	,	0.989490087	,	1.036892455	/)
        Coeff_Latin(	934	,	:)	=	(/	0.562866912	,	1.386448356	,	1.352598405	,	1.046668729	,	1.385844347	/)
        Coeff_Latin(	935	,	:)	=	(/	0.648203013	,	1.789089774	,	0.950289495	,	0.906421001	,	0.602808513	/)
        Coeff_Latin(	936	,	:)	=	(/	1.078101733	,	1.384097244	,	0.897113967	,	1.003964424	,	1.416314189	/)
        Coeff_Latin(	937	,	:)	=	(/	1.129672277	,	1.025533869	,	0.702616146	,	1.046824561	,	0.982331143	/)
        Coeff_Latin(	938	,	:)	=	(/	0.841915827	,	0.831690882	,	1.097368723	,	0.970596483	,	0.882784318	/)
        Coeff_Latin(	939	,	:)	=	(/	1.513991557	,	1.146061073	,	1.205099711	,	1.025122539	,	0.840211626	/)
        Coeff_Latin(	940	,	:)	=	(/	0.874599612	,	1.266377245	,	0.909398444	,	0.998234917	,	0.926434952	/)
        Coeff_Latin(	941	,	:)	=	(/	1.295102595	,	0.55223742	,	0.936078862	,	1.116107116	,	0.551150626	/)
        Coeff_Latin(	942	,	:)	=	(/	1.09978889	,	1.588282443	,	0.862741799	,	1.059851192	,	1.051578836	/)
        Coeff_Latin(	943	,	:)	=	(/	1.1048616	,	0.939893156	,	0.822871821	,	1.020747598	,	1.361950182	/)
        Coeff_Latin(	944	,	:)	=	(/	1.158979375	,	0.91872003	,	1.189760304	,	0.988975829	,	0.543602447	/)
        Coeff_Latin(	945	,	:)	=	(/	1.120433831	,	1.104658923	,	0.847710804	,	1.032908939	,	1.027393531	/)
        Coeff_Latin(	946	,	:)	=	(/	0.985044017	,	0.552207524	,	1.163677615	,	0.985949264	,	1.442656915	/)
        Coeff_Latin(	947	,	:)	=	(/	1.265191886	,	1.143133006	,	0.942888533	,	1.009658755	,	1.218434369	/)
        Coeff_Latin(	948	,	:)	=	(/	0.747868362	,	0.965037121	,	1.117046611	,	0.99411485	,	1.123772624	/)
        Coeff_Latin(	949	,	:)	=	(/	0.939358755	,	1.224954432	,	0.839815754	,	1.061620242	,	1.008647949	/)
        Coeff_Latin(	950	,	:)	=	(/	1.320980664	,	0.812134588	,	0.993750957	,	1.07982414	,	0.879939128	/)
        Coeff_Latin(	951	,	:)	=	(/	0.870588629	,	0.932048584	,	1.066506507	,	1.009109962	,	1.11784981	/)
        Coeff_Latin(	952	,	:)	=	(/	0.910033173	,	0.959232646	,	1.095909794	,	0.910126778	,	1.436455589	/)
        Coeff_Latin(	953	,	:)	=	(/	1.117791939	,	1.075739841	,	1.169535602	,	1.054058408	,	1.209289365	/)
        Coeff_Latin(	954	,	:)	=	(/	1.100814214	,	1.077175625	,	0.772133215	,	0.98459662	,	0.587084348	/)
        Coeff_Latin(	955	,	:)	=	(/	1.134240933	,	0.951435895	,	1.003169711	,	1.054716056	,	1.499060378	/)
        Coeff_Latin(	956	,	:)	=	(/	1.204711684	,	1.214043156	,	0.930889777	,	0.979642658	,	0.998041933	/)
        Coeff_Latin(	957	,	:)	=	(/	1.094689595	,	1.040321044	,	1.047946392	,	0.978165705	,	1.200052716	/)
        Coeff_Latin(	958	,	:)	=	(/	0.673776677	,	0.939651418	,	0.954374467	,	1.018300937	,	1.019235233	/)
        Coeff_Latin(	959	,	:)	=	(/	1.175597437	,	1.066591476	,	1.035950194	,	1.052800759	,	1.034100888	/)
        Coeff_Latin(	960	,	:)	=	(/	1.191728044	,	1.594154157	,	0.908306525	,	0.978866003	,	1.040086336	/)
        Coeff_Latin(	961	,	:)	=	(/	0.960890778	,	0.771261245	,	1.056958243	,	1.066427768	,	1.090743035	/)
        Coeff_Latin(	962	,	:)	=	(/	1.386338183	,	0.759145517	,	1.027220847	,	1.062140128	,	0.919120278	/)
        Coeff_Latin(	963	,	:)	=	(/	0.839303841	,	1.011054217	,	1.118716874	,	0.983318965	,	0.712214854	/)
        Coeff_Latin(	964	,	:)	=	(/	1.08937059	,	0.845914081	,	1.062778519	,	1.022042558	,	0.638333229	/)
        Coeff_Latin(	965	,	:)	=	(/	0.822557529	,	0.726531812	,	0.746533894	,	1.054440135	,	1.029386984	/)
        Coeff_Latin(	966	,	:)	=	(/	1.158431076	,	1.415697029	,	0.793549312	,	0.998097258	,	0.867887961	/)
        Coeff_Latin(	967	,	:)	=	(/	0.745790282	,	0.996718174	,	0.944110161	,	1.101599032	,	0.867048085	/)
        Coeff_Latin(	968	,	:)	=	(/	0.830178881	,	1.082532292	,	1.013158368	,	0.992104261	,	1.082953481	/)
        Coeff_Latin(	969	,	:)	=	(/	1.016832528	,	1.033651313	,	1.11286598	,	1.057749217	,	0.966793457	/)
        Coeff_Latin(	970	,	:)	=	(/	1.380518224	,	1.088034298	,	1.018955956	,	0.937667476	,	1.354287051	/)
        Coeff_Latin(	971	,	:)	=	(/	1.312678653	,	1.070352356	,	1.085723967	,	0.981567597	,	1.159463011	/)
        Coeff_Latin(	972	,	:)	=	(/	0.887542315	,	0.964206615	,	1.419488401	,	0.96382072	,	1.195278319	/)
        Coeff_Latin(	973	,	:)	=	(/	0.93273418	,	1.008863597	,	0.958456127	,	0.967596612	,	1.25259717	/)
        Coeff_Latin(	974	,	:)	=	(/	0.887084994	,	1.071749357	,	1.109330696	,	1.012987978	,	0.827370443	/)
        Coeff_Latin(	975	,	:)	=	(/	0.719143752	,	1.292860125	,	1.108499911	,	0.952337364	,	0.822031858	/)
        Coeff_Latin(	976	,	:)	=	(/	0.874356651	,	0.851671056	,	1.273717278	,	1.012888936	,	1.406153888	/)
        Coeff_Latin(	977	,	:)	=	(/	1.350458507	,	0.713208771	,	1.125391431	,	0.991812336	,	1.281339261	/)
        Coeff_Latin(	978	,	:)	=	(/	1.216257848	,	1.127604241	,	1.043499261	,	1.063169535	,	0.791808367	/)
        Coeff_Latin(	979	,	:)	=	(/	0.655866413	,	1.101373376	,	0.800491551	,	0.943091559	,	1.135998679	/)
        Coeff_Latin(	980	,	:)	=	(/	1.305779044	,	0.459577422	,	1.002350506	,	1.049802619	,	0.553105359	/)
        Coeff_Latin(	981	,	:)	=	(/	0.999725375	,	0.703331535	,	0.906808142	,	0.994689368	,	0.850587378	/)
        Coeff_Latin(	982	,	:)	=	(/	0.994887283	,	0.98412272	,	0.840029777	,	1.015325968	,	1.096422165	/)
        Coeff_Latin(	983	,	:)	=	(/	1.20194596	,	0.815461348	,	1.180575589	,	0.999366477	,	0.792801714	/)
        Coeff_Latin(	984	,	:)	=	(/	1.112469082	,	1.385233904	,	1.001477341	,	1.013755218	,	1.071374114	/)
        Coeff_Latin(	985	,	:)	=	(/	0.851588692	,	0.220703317	,	1.196144361	,	1.06786967	,	0.641621725	/)
        Coeff_Latin(	986	,	:)	=	(/	0.603295907	,	0.970676893	,	1.025462003	,	0.940129054	,	1.031940431	/)
        Coeff_Latin(	987	,	:)	=	(/	0.973333115	,	0.496690773	,	0.761700514	,	1.023255737	,	0.427053128	/)
        Coeff_Latin(	988	,	:)	=	(/	1.25831496	,	1.200244927	,	0.958685236	,	1.125765108	,	0.686447091	/)
        Coeff_Latin(	989	,	:)	=	(/	1.044073398	,	1.145901418	,	1.379462002	,	0.970837718	,	0.887516663	/)
        Coeff_Latin(	990	,	:)	=	(/	1.4523037	,	0.8284456	,	0.797552865	,	0.927751689	,	1.056997036	/)
        Coeff_Latin(	991	,	:)	=	(/	0.731596072	,	0.903358859	,	1.001068956	,	0.903454295	,	1.075818581	/)
        Coeff_Latin(	992	,	:)	=	(/	0.734443158	,	1.33334386	,	0.990274272	,	1.016872386	,	0.720578047	/)
        Coeff_Latin(	993	,	:)	=	(/	0.605447188	,	0.626061347	,	0.909510308	,	1.024943142	,	0.912435424	/)
        Coeff_Latin(	994	,	:)	=	(/	0.785522834	,	0.695407361	,	1.054727746	,	1.042666261	,	0.968012559	/)
        Coeff_Latin(	995	,	:)	=	(/	0.83659554	,	0.480397607	,	0.808016178	,	0.955605315	,	1.238969602	/)
        Coeff_Latin(	996	,	:)	=	(/	0.92384126	,	1.244681864	,	0.953766851	,	1.043779337	,	0.789343436	/)
        Coeff_Latin(	997	,	:)	=	(/	0.518965695	,	0.754611536	,	1.030934608	,	0.963195184	,	1.069112449	/)
        Coeff_Latin(	998	,	:)	=	(/	1.164820861	,	1.346847666	,	1.096635652	,	1.044766666	,	1.040219301	/)
        Coeff_Latin(	999	,	:)	=	(/	0.928566918	,	0.784042713	,	1.276316873	,	1.149571284	,	1.346368544	/)
        Coeff_Latin(	1000	,	:)	=	(/	1.089636439	,	1.313010557	,	0.983731348	,	1.065849843	,	1.298440488	/)



*     *********************      

      
      
      
      THETA_1 = Coeff_Latin(II,1)
      THETA_2 = Coeff_Latin(II,2)
      THETA_3 = Coeff_Latin(II,3)
      THETA_4 = Coeff_Latin(II,4)
      THETA_5 = Coeff_Latin(II,5)
      
     
      RETURN
      END
      
      
