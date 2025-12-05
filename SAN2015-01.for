c 150107 jrv no real changes.  write SAN version number to screen
c 120924 jrv updated graphics
c
       PROGRAM SAN12 
C	 (4,120),MAIN CALLING PROGRAM FOR SALLY ANN 
C                    <851219.1455>
CCCCC 
C***********************************************************************
C 
C                           PROGRAM:  SALLY                                     
C 
C
C      20 October 2011 Changed the code to print an output with all the parameters 
C                       in a format easily incorporated in spread sheets.  The original
c                        output file is still output.  Background set points were added to
c                        the plot too. sfh
c
C      18 SEPT 90 KW
C      Incident energy was removed as an extra parameter and replaced
C       with q2, the second q-value when mul>1. Input file format was
C       thus changed a bit - inc. energy now follows ang., as detailed
C       in the newsan12.help file in the [williams.san12] directory.
C       Also, the output file format was modified to make it easier to
C       read - the changes are self-explanatory.
C
C      10 JULY 90 KEITH WILLIAMS
C      Kinematics portion of program (subrtn. calc) was rewritten -
C       the method now used is that found in kinmat...
C       Output file was also modified slightly to print out
C       exit energies & flt. times for the excited states.
C                                                                  
C 
C      17 MAY 85  SEH 
C       INCREASED THE SIZE OF SOME ARRAYS SO THAT MORE THAN 10
C       PEAKS COULD BE FIT. 
C 
C 
C       Updated  6 June 84  SEH 
C       Input NFILE,NSPEC in the data file.
C	NFILE is the name of the histogram file from REPLAY,
C	and NSPEC is the histogram num. 
C 
C       Updated 24 Jan 85   SEH 
C       Added ISKPY, to allow skipping of channels in 
C                    X-square calculation.
C 
C       THIS PROGRAM READS NEUTRON TIME OF FLIGHT DATA, FITS GAUSSIANS
C     TO THE PEAKS IN THE TIME OF FLIGHT SPECTRA, AND CALCULATES A 'CHI-
C     SQUARE' FOR THE FIT.  IT VARIES SEVERAL PARAMETERS IN ORDER TO
C     MINIMIZE THE VALUE OF 'CHI-SQUARE'.  THIS VERSION OF SALLY IN-
C     CLUDES AN OPTION (MUL) WHICH ALLOWS FOR SEARCHIN ON THE POSITIONS 
C     OF A GROUP OF PEAKS WHILE PRESERVING THE ENERGY SEPARATION BETWEEN
C     THEM. THE VARIABLES FOR THE MULTI-PEAK SEARCHING OPTION ARE:
C     MUL,IMUL,MULN,CHR,CHO,FTR,CALB,DEL(I),AND RT(I).  THIS VER- 
C     SION OF SALLY ALSO INCLUDES AN OPTION (NUOP) TO USE A NON-
C     LINEAR WIDTH FUNCTION.  THE VARIABLES ASSOCIATED WITH THIS
C     OPTION ARE: NUOP,DELTX,DELTI,DELTR,FP,MASS,NREF,SIGR,CALB,
C     AND CHO.
C 
C 
C       FOR MORE INFORMATION ON THE INPUT FOR FIT, SEE THE
C       ' MCM  MASTER FILE '. 
C 
C       For instructions that are more up to date see 
C       the HP file ?sally.   22 Aug 85  seh
C 
C 
C        THIS VERSION OF ' SALLY ' IS NOT SEGMENTED; IT IS FOR USE ON THE
C        MICROVAX  AT THE UNIVERSITY OF KENTUCKY  VAN DE GRAAFF LAB. 
C 
C 
C        THE MAIN PROGRAM IS JUST USED TO CALL THE OTHER ROUTINES.
C        SUBROUTINE SANN4 WRITES THE INPUT FILES FOR GENPLT 
C        SUBROUTINE SANN6 gets the spectrum from a REPLAY output file on disc. 
C 
C 
C 
C       AFTER THIS IS FINISHED, THE PROGRAM EXECUTES BY TYPING- 
C 
C             @SSAN12 <INPUT> FOR A BATCH OPERATION
C 
C               WHERE: <INPUT> IS INPUT DATA FILE (NOT INCLUDING SPECTRUM)
C 
C 
C 
C 
C***********************************************************************
CCCCC 
      REAL KDATA,ITDATA,ISDATA,ISMFIT,ISMR 
      REAL MASS
	CHARACTER*30 NFIL
        CHARACTER*14 CHECK
        CHARACTER*72 TITLE
      INTEGER LL,FF,EE,BLANK,DASH,D 
      INTEGER dtval(8)
      COMMON/LOCAL/C,DDEL,DF,DNAUT,EN,I,IAUTOA,IGRID,IL,
     1 IMTRX,IP,IS,ISUM,IT,IWRITE,IYY,J,JALT,K,LASTR,LOOPMX,
     2 MFP,MPEAK,NBRNCH,NDPARM,NIMX,NKLUZ,NLHS,NLOOP,NN,NPEAK,NRHS, 
     3 NTIMES,NTOTAL,NUMPAR,NUMX,SDATE,X1,XSQP,XSQPK,XXSQ 
      COMMON/SEG02/NCHAN
      CHARACTER*20 TTERM
      COMMON/CRT_QQQ/ TTERM
      COMMON/SEG03/ILC1,ILC2,IHC1,IHC2
      COMMON/SEG04/TITLE,IGRF,IGPT,IDEV,IR
      COMMON/SEG05/LL,FF,EE,BLANK,DASH,D
      COMMON/SEG06/IRUN,IYEAR,IMONTH,IDAY,NFIL,NSPC,INTRA
     >,IFLAG,CHECK
      COMMON/BIGGY/KDATA(16384),ITDATA(16384),ISDATA(16384),
     * ISMFIT(16384),ISMR(16384),APRM(50),IA5(10),IB5(10),IPART,IPAR(50)
     *,AVARY(25,2),ACONST(25,2) 
      COMMON /COMN1/ AM1,AM2,AM3,AM4,QGS,ANG,E1,AA,BB,CC,DD,SG,E3,
     &       SAR,E4,T,SW,SI,SA,ETHR,QEX,EXCIT,AM2G(5),AM4G(5),QGSG(5) 
      COMMON /COMN3/ DIS,TOG,GPEAK,CAL,TON,DELT,DELC,EPEAK,IZPAR(50)
C 
      COMMON/PARM/XMIN,XMAX,YMIN,YMAX,XLINE,YLINE,XTIC,YTIC,YIMM, 
     &            YIDM,IBEG,ISTOP,NPTS
      COMMON IDUMY(173) 
      COMMON/BOUND/XXMIN,XXMAX,YYMIN,YYMAX,JXMIN,JXMAX,JYMIN,JYMAX
      COMMON/OPT/MOPTT,XDIM,YDIM,MM 
      COMMON/SEG07/NPRT,MOPT,XL,YL
      COMMON/SEG09/IX1,IY1,IX2,IY2,YY1,YY2,XP,YP,NUM,IX3,IX5,IY5,IX6,IY6
C 
      COMMON/XTRA/XPARAM(10),IXPAR(10)
      COMMON/KDIF/XSQOLD,XSQNEW,DIFF,ERSUM,NCHI 
      COMMON/NLINS/MUL,IMUL,CHR,FTR,DEL(20),RT(20),NSKIP, 
     &   MULG(5),IMULG(5) 
      COMMON/NLINW/DELEX,DELTI,FP,MASS,NREF,NUOP,SIGR,SIGSAV(20),ISMF,
     &  ISMSTRT,ISMEND
      COMMON/BRAV/CALB,CHO,IREF,IREFI 
      COMMON/MAINX/IWHERE,IGOTO
      COMMON/SECGA/XPOGG
      CHARACTER*40 FNAME1
C
C
C      write(*,*) 'version 2015-01'
C      WRITE(*,'('' Enter the input filename '')')
C      READ(*,'(A40)') FNAME1
c name of input file for san12
      CHARACTER(LEN=100) :: fname
      INTEGER :: nargs
      nargs = COMMAND_ARGUMENT_COUNT()
      IF (nargs .GE. 1) THEN
         CALL GET_COMMAND_ARGUMENT(1, fname)
      ELSE
         fname = "san12.inp"
      ENDIF
      OPEN(UNIT=1, FILE=TRIM(fname), STATUS='OLD')

      OPEN(UNIT=2,FILE='san12.out',STATUS='unknown')
      READ(1,'(A20)') TTERM
      TTERM = TTERM(1:LEN_TRIM(TTERM)-1)
CCCCC f
C****************************************************************** 
C 
C      MAIN SEGMENT-CALLING ROUTINE 
C 
C****************************************************************** 
CCCCC 
C 
   1  IF(IWHERE.LT.1.OR.IWHERE.GT.9) GO TO 1000 
      GO TO (100,200,300,400,1000,600,1000,1000,1000), IWHERE 
 100  CONTINUE
C*****READ IN PARAMETERS AND DO FIT 
      CALL SALY1
      GOTO 1
 200  CONTINUE
C*****SMOOTH THE DATA FROM IDATA TO KDATA 
      CALL SALY2
      GOTO 1
 300  CONTINUE
C*****DO BACKGROUND SUBTRACTION ON KDATA
      CALL SALY3
      GOTO 1
 400  CONTINUE
C*****TERMINAL GRAPHICS 
      CALL SALY4
      GOTO 1
 600  CONTINUE
C*****READ IN DATA INTO IBUF (EQUIVALENCE TO IDATA)
      CALL SALY6
      GOTO 1
 1000 STOP
      END 
C 
C***************************************************************************
      BLOCK DATA
C***************************************************************************
C 
      INTEGER BEG 
      REAL MASS 
      INTEGER LL,FF,EE,BLANK,DASH,D
	CHARACTER*30 NFIL
        CHARACTER*72 TITLE
        CHARACTER*14 CHECK
	COMMON/LOCAL/C,DDEL,DF,DNAUT,EN,I,IAUTOA,IGRID,IL,
     1 IMTRX,IP,IS,ISUM,IT,IWRITE,IYY,J,JALT,K,LASTR,LOOPMX,
     2 MFP,MPEAK,NBRNCH,NDPARM,NIMX,NKLUZ,NLHS,NLOOP,NN,NPEAK,NRHS, 
     3 NTIMES,NTOTAL,NUMPAR,NUMX,SDATE,X1,XSQP,XSQPK,XXSQ 
      COMMON/SEG02/NCHAN
      COMMON/SEG03/ILC1,ILC2,IHC1,IHC2
      COMMON/SEG04/TITLE,IGRF,IGPT,IDEV,IR
      COMMON/SEG05/LL,FF,EE,BLANK,DASH,D
      COMMON/SEG06/IRUN,IYEAR,IMONTH,IDAY,NFIL,NSPC,INTRA
     >,IFLAG,CHECK
      COMMON/CDLTA/DELTA
      COMMON/OTHER/RM,RB,BEG,JEND,ISTART,IEND,MULN,DIST,RATIO,IFIT,PJON 
     &             ,ISKPY(10,2) 
      COMMON/XTRA/XPARAM(10),IXPAR(10)
      COMMON/KDIF/XSQOLD,XSQNEW,DIFF,ERSUM,NCHI 
      COMMON/NLINS/MUL,IMUL,CHR,FTR,DEL(20),RT(20),NSKIP, 
     &   MULG(5),IMULG(5) 
      COMMON/NLINW/DELEX,DELTI,FP,MASS,NREF,NUOP,SIGR,SIGSAV(20),ISMF,
     & ISMSTRT, ISMEND 
      COMMON/BRAV/CALB,CHO,IREF,IREFI 
      COMMON/MAINX/IWHERE,IGOTO
      COMMON/SECGA/XPOGG
      COMMON /COMN1/ AM1,AM2,AM3,AM4,QGS,ANG,E1,AA,BB,CC,DD,SG,E3,
     &       SAR,E4,T,SW,SI,SA,ETHR,QEX,EXCIT,AM2G(5),AM4G(5),QGSG(5) 
      COMMON /COMN3/ DIS,TOG,GPEAK,CAL,TON,DELT,DELC,EPEAK,IZPAR(50)
C 
      COMMON/PARM/XMIN,XMAX,YMIN,YMAX,XLINE,YLINE,XTIC,YTIC,YIMM, 
     &            YIDM,IBEG,ISTOP,NPTS
      COMMON/BOUND/XXMIN,XXMAX,YYMIN,YYMAX,JXMIN,JXMAX,JYMIN,JYMAX
      COMMON/OPT/MOPTT,XDIM,YDIM,MM 
      COMMON/SEG07/NPRT,MOPT,XL,YL
      COMMON/SEG09/IX1,IY1,IX2,IY2,YY1,YY2,XP,YP,NUM,IX3,IX5,IY5,IX6,IY6
C 
      DATA IWHERE/1/, IGOTO/0/
      DATA DASH,D/1H!,1HD/
C     DATA LL,FF,EE,BLANK/1HL,1HF,1HE,1H /
      DATA LL,FF,EE,BLANK/1H*,1H+,1H=,1H /
      DATA LOOPMX/200/
      DATA LASTR/0/ 
      DATA NUMPAR/2/
      END 
C************************************************************************ 
      SUBROUTINE SALY1
C	 (5,120), PEAK-FITTING ROUTINE 
C	              <851219.1455> 
C*************************************************************************
      DIMENSION IDATA(16384)
      REAL KDATA,ITDATA,ISDATA,ISMFIT,ISMR 
      REAL MASS 
      INTEGER BEG 
      INTEGER LL,FF,EE,BLANK,DASH,D
	CHARACTER*30 NFIL,NFILE
	CHARACTER*72 HEADER
        CHARACTER*72 TITLE
        CHARACTER*14 CHECK
	character*8 idate,iclock
	character*10 cdate
	character*11 ctime
	character*15 titleshort
      COMMON/LOCAL/C,DDEL,DF,DNAUT,EN,I,IAUTOA,IGRID,IL,
     1 IMTRX,IP,IS,ISUM,IT,IWRITE,IYY,J,JALT,K,LASTR,LOOPMX,
     2 MFP,MPEAK,NBRNCH,NDPARM,NIMX,NKLUZ,NLHS,NLOOP,NN,NPEAK,NRHS, 
     3 NTIMES,NTOTAL,NUMPAR,NUMX,SDATE,X1,XSQP,XSQPK,XXSQ 
      COMMON/SEG02/NCHAN
      COMMON/SEG03/ILC1,ILC2,IHC1,IHC2
      COMMON/SEG04/TITLE,IGRF,IGPT,IDEV,IR
      COMMON/SEG05/LL,FF,EE,BLANK,DASH,D
      COMMON/SEG06/IRUN,IYEAR,IMONTH,IDAY,NFIL,NSPC,INTRA
     >,IFLAG,CHECK
      COMMON/SEG07/NPRT,MOPT,XL,YL
      COMMON/BIGGY/KDATA(16384),ITDATA(16384),ISDATA(16384),
     * ISMFIT(16384),ISMR(16384),APRM(50),IA5(10),IB5(10),IPART,IPAR(50)
     *,AVARY(25,2),ACONST(25,2) 
      COMMON/CDLTA/DELTA
      COMMON/OTHER/RM,RB,BEG,JEND,ISTART,IEND,MULN,DIST,RATIO,IFIT,PJON 
     &             ,ISKPY(10,2) 
      COMMON/SECGA/XPOGG
      COMMON /COMN1/ AM1,AM2,AM3,AM4,QGS,ANG,E1,AA,BB,CC,DD,SG,E3,
     &       SAR,E4,T,SW,SI,SA,ETHR,QEX,EXCIT,AM2G(5),AM4G(5),QGSG(5) 
      COMMON /COMN3/ DIS,TOG,GPEAK,CAL,TON,DELT,DELC,EPEAK,IZPAR(50)
      COMMON/XTRA/XPARAM(10),IXPAR(10)
      COMMON/KDIF/XSQOLD,XSQNEW,DIFF,ERSUM,NCHI 
      COMMON/NLINS/MUL,IMUL,CHR,FTR,DEL(20),RT(20),NSKIP, 
     &   MULG(5),IMULG(5) 
      COMMON/NLINW/DELEX,DELTI,FP,MASS,NREF,NUOP,SIGR,SIGSAV(20),ISMF,
     & ISMSTRT, ISMEND 
      COMMON/BRAV/CALB,CHO,IREF,IREFI 
      COMMON/MAINX/IWHERE,IGOTO
	COMMON/QINP/NFILE,NSPEC,HEADER,IDATA
	common/easy/iqcharge,itime,idate,iclock
      DIMENSION XPVRY(20) 
      DIMENSION IC5(10),ID5(10)
      DIMENSION A(25,2) 
      REAL htmax, htmin, hex, hpeakid, hpeakarea, hpeakerr
	DIMENSION htmax(30),htmin(30),hex(30),hpeakid(30),hpeakarea(30),
     &   hpeakerr(30)
      EQUIVALENCE (A(1,1),APRM(1))
      DATA KKFLG /0/
      character*64 linename
      CHARACTER(LEN=40) fmt
      CHARACTER(LEN=40) fmt1
C--- erase any previous fit_summary.txt at start of new run
      OPEN(UNIT=90, FILE='fit_summary.txt', STATUS='UNKNOWN')
      CLOSE(90, STATUS='DELETE')

C 
CCCCC 
C**************************************************************** 
C 
C     MAIN PROGRAM
C 
C**************************************************************** 
CCCCC 
C  
      open(unit=84,file='para.out',status='unknown',position='append')
      open(unit=85,file='integ.out',status='unknown',position='append')
      open(unit=86,file='date.out',status='unknown',position='append')
      open(unit=108,file='param.out',status='unknown',action='write')
      
      call date_and_time(cdate,ctime)       ! jrv 120925
C      call date_and_time(ctime)

      NSKIP=0 
      GOTO (666,702,703,704,705,706), IGOTO 
  666 CONTINUE
      IGRID=-3
      BEG=1 
      JEND=16384
      IMTRX=-1
C  I suspect this is the line controlling increment size for varying
      DELTA=0.1
      MASS=939577.0 
C 
C***********************************************************************
C     IGRID = GRID SEARCH IF NEGATIVE 
C     NUMX  = INITIALLY, NO. OF EXTRA PARAMETERS  
C     ISTOP = NUMBER OF PASSES THROUGH GRAD SEARCH
C     C     = STEP SIZE PARAMETER (GRAD)
C     DELTA = STEP SIZE PARAMETER (GRID) AND GRAD DETER. STEP 
C***********************************************************************
C
      linename = 'CHECK'
      READ(1,9102,ERR=9999) CHECK
9102  FORMAT(A14)
      linename = 'TITLE'
      READ(1,1101,ERR=9999) TITLE
      TITLE = TITLE(1:LEN_TRIM(TITLE)-1)
 1101 FORMAT(A72)
      WRITE(2,1102) TITLE 
 1102 FORMAT(32X,' TITLE ',/,32X,'=======',/,14X,A72,//) 
      WRITE(2,111)
  111 FORMAT(25X,'INPUT DATA',/,25X,'==========',/) 
      DNAUT=DELTA 
    1 CONTINUE
      C=1.
C 
C***********************************************************************
C     IRUN   = RUN NUMBER 
C     IWRITE = OPTION FOR WRITING KDATA 
C     MUL    = PEAK NO. OF 1ST PEAK IN GROUP TO BE FITTED TOGETHER
C     NUOP   = OPTION TO USE NON-LINEAR WIDTH FUNCTION, TO USE NUOP=1 
C***********************************************************************
C 
      IF (IRUN .EQ. 0) IRUN =1
      IR=IRUN
      linename = 'NFIL'
      READ(1,13,ERR=9999) NFIL
      NFIL = NFIL(1:LEN_TRIM(NFIL)-1)
13    FORMAT(A30)
      linename = 'NSPC,INTRA,NCHI'
      READ(1,*,ERR=9999)NSPC,INTRA,NCHI
      IFLAG=0 
      WRITE(2,4016) NFIL,NSPC,INTRA,NCHI
      linename = 'MUL,NREF,NUOP,ICHO'
      READ(1,*,ERR=9999) MUL,NREF,NUOP,ICHO
      IWRITE = 0
      IAUTO = 0
      MULG(1)=1 
      IF(MUL.LE.0) GO TO 1150
      write(linename,'(A,I3)') 'MULG(I),I=1,',MUL
      write(*,'(a)') linename
      READ(1,*,ERR=9999) (MULG(I),IMULG(I),I=1,MUL)
      WRITE(2,4014) (MULG(I),IMULG(I),I=1,MUL)
1150  CONTINUE
      IMUL=0
      MFP=1 
      IF(IRUN.LT.0) GO TO 999
      linename = 'ILC1,ILC2,IHC1,IHC2,NCHAN,IFIT,JSKPY'
      READ(1,*,ERR=9999)ILC1,ILC2,IHC1,IHC2,NCHAN,IFIT,JSKPY 
c	write(*,*)ilc1,ilc2,ihc1,ihc2,nchan,ifit,jskpy
      ISKPY(1,1)=0
      JJ=1
      linename = 'ISKPY(JJ,1),ISKPY(JJ,2),JSKPY'
      DO WHILE (JSKPY .EQ. 1 .AND. JJ .LE. 10)
        READ(1,*,ERR=9999) ISKPY(JJ,1),ISKPY(JJ,2),JSKPY 
      JJ=JJ+1 
      END DO
C 
      WRITE(2,4009) MUL,NREF,NUOP,IWRITE,IAUTOA,ICHO
      linename = 'ISTART,IEND,NPEAK,ISMF,ISMSTRT,ISMEND'
      READ(1,*,ERR=9999)ISTART,IEND,NPEAK,ISMF,ISMSTRT,ISMEND
      write(*,'(A)') linename
      IPART = 1
      NUMX = 10
      IF(ISMF.LE.0) GO TO 1160
      write(linename,'(A,I3)') 'ISMR(J),J=1,',ISMF
      READ(1,*,ERR=9999)(ISMR(J),J=1,ISMF) 
      SSUM=0. 
      DO 1155 J=1,ISMF
1155  SSUM=SSUM+ISMR(J) 
      DO 1157 J=1,ISMF
1157  ISMR(J)=ISMR(J)/SSUM
1160  IPT=IABS(IPART) 
C
      write(linename,'(A,I3)') 'IA5(I),IB5(I),IC5(I),ID5(I),I=2,IPT'
      IF(IPT.GT.1) READ(1,*,ERR=9999)
     &  (IA5(I),IB5(I),IC5(I),ID5(I),I=2,IPT)
      IF(INTRA .LT. 2) GOTO 118 
 4017 FORMAT(1X,A17,5X,' Normal Weighting')
 4018 FORMAT(1X,A17,5X,' Sqrt. Weighting') 
 4019 FORMAT(1X,A17,5X,'Yield plus Nchi Weighting')
      IF(IPT .LT. 2) GOTO 118 
  6   FORMAT(4I8) 
  118 CONTINUE
CCCCC 
C 
C     XPOGG DETERMINES POSITION OF SECOND GAUSSIAN WITH 
C     RESPECT TO FIRST. 
C 
CCCCC 
    5 FORMAT(6I5) 
      WRITE(2,1010) 
 1010 FORMAT(15X,'BACKGROUND SUBTRACTION REGION',/,15X,'================
     /=============',/)
      WRITE(2,112)
  112 FORMAT(18X,'ILC1',2X,'ILC2',2X,'IHC1',2X,'IHC2',1X,' NCHAN', 
     * 2X,' IFIT','  JSKPY')
      WRITE(2,11) ILC1,ILC2,IHC1,IHC2,NCHAN,IFIT,JSKPY
   11 FORMAT(16X,7I6//) 
      WRITE(2,1011) 
 1011 FORMAT(17X,'FITTING REGION INFORMATION',/,17X,'===================
     /=======',/) 
      WRITE(2,120)
  120 FORMAT(9X,' START',2X,'END',2X,'  NPEAK',2X,'NUMX',2X,
     *    ' IPART',3X,'ISMF',3X,'ISMSTRT',3X,'ISMEND')
      WRITE(2,121)ISTART,IEND,NPEAK,NUMX,IPART,ISMF,ISMSTRT,ISMEND
  121 FORMAT(8X,2I6,6I7//) 
      if (ISKPY(1,1) .LE. 0) GOTO 1126
      write(2,1122) 
1122  format(10X,'Channels left out of chi-square calculation',/,10X, 
     &           '===========================================',/) 
      i=1 
1121  CONTINUE
      WRITE(2,1123) ISKPY(i,1),ISKPY(i,2) 
1123  FORMAT(16X,I4,'  to  'I4) 
      i=i+1 
      IF (ISKPY(i,1) .GT. 0) goto 1121
1126  CONTINUE
      NTOTAL=NPEAK*NUMPAR+NUMX
      MPEAK=NPEAK 
      NIMX=NUMX 
      IF(NUMX.EQ.0) GO TO 15
      NUMX=1+(NUMX-1)/2 
      NPEAK=NPEAK+NUMX
   15 CONTINUE
      write(linename,'(A,I3)') 'A(I,2),I=1,',MPEAK
      READ(1,*,ERR=9999) (A(I,2),I=1,MPEAK)
      write(linename,'(A,I3)') 'IPAR(NPEAK+I),I=1,',MPEAK
      READ(1,*,ERR=9999) (IPAR(NPEAK+I),I=1,MPEAK) 
      write(linename,'(A,I3)') 'A(I,1),I=1,',MPEAK
      READ(1,*,ERR=9999) (A(I,1),I=1,MPEAK)
      write(linename,'(A,I3)') 'IPAR(I),I=1,',MPEAK
      READ(1,*,ERR=9999) (IPAR(I),I=1,MPEAK) 
       DO 8602 i=1,MPEAK
         write(linename,'(A,I3)') 'AVARY(J,1),J=1,',MPEAK
       IF (IPAR(i) .EQ. 2) THEN 
         READ(1,*,ERR=9999) (AVARY(j,1),j=1,MPEAK)
          DO 8601 k=1,MPEAK 
8601      ACONST(k,1)=A(k,1)
          GOTO 8604 
          END IF
8602  CONTINUE
8604  CONTINUE
      write(2,1112) 
1112  format(//)
      WRITE(2,113)
  113 FORMAT(15X,'PEAK POSITION BY CHANNEL AND HEIGHT',/,15X,'================
     1=========================',/)
      WRITE(2,4005)(A(I,2),I=1,MPEAK) 
      WRITE(2,4002) (IPAR(NPEAK+I),I=1,MPEAK) 
      WRITE(2,4005)(A(I,1),I=1,MPEAK) 
      WRITE(2,4002) (IPAR(I),I=1,MPEAK) 
      DO 9765 i=1,MPEAK 
      IF (IPAR(i) .EQ. 2) THEN
        WRITE(2,4002) (AVARY(j,1),j=1,MPEAK) 
        WRITE(2,9763)
9763     FORMAT(/,24X,'** Peak height bounds shown above. **')
         GOTO 9766
         END IF 
9765  CONTINUE
9766  CONTINUE
C    The following reads the extra parm., the vary/novary/windowvary line, and 
C    the window line...
      write(linename,'(A,I3)') 'XPARAM(I),I=1,',NIMX
      READ(1,*,ERR=9999)(XPARAM(I),I=1,NIMX)
      write(*,*) (XPARAM(I), I=1,NIMX)
      write(linename,'(A,I3)') 'IXPAR(I),I=1,',NIMX
      READ(1,*,ERR=9999) (IXPAR(I),I=1,NIMX) 
      DO 8606 i=1,NIMX
      IF (IXPAR(i) .EQ. 2) THEN 
         write(linename,'(A,I3)') 'XPVRY(I),I=1,',NIMX
        READ(1,*,ERR=9999) (XPVRY(j),j=1,NIMX)
         GOTO 8608
         END IF 
8606  CONTINUE
8608  CONTINUE
C     Here I SWITCH SOME XPARAM ASSIGNMENTS  KAW2
      QGSG(2)=XPARAM(7)
C      
      QGS=XPARAM(8) 
C     flag
      QEX=QGS 
      QGSG(1)=QGS 
      DO 22 I=1,NIMX
      K=I-I/2 
      J=I/K 
      L=MPEAK+K 
      A(L,J)=XPARAM(I)
      ACONST(L,J)=XPARAM(I) 
      AVARY(L,J)=XPVRY(I) 
      IPAR(L+(J-1)*NPEAK)=IXPAR(I)
   22 CONTINUE
      WRITE(2,115)
  115 FORMAT(//,24X,'EXTRA PARAMETERS',/,24X,'================',/)
C      WRITE(2,116)(XPARAM(I),I=1,NIMX)
  116 FORMAT(10X,5F10.4) 
C      WRITE(2,4003) (IXPAR(I),I=1,NIMX)


      WRITE(2,116)(XPARAM(I),I=1,INT(NIMX/2))
      WRITE(2,4003) (IXPAR(I),I=1,INT(NIMX/2))
	DO 6410 I=1,NIMX
	IF (IXPAR(I) .EQ. 2) THEN 
          WRITE(2,116) (XPVRY(K),K=1,INT(NIMX/2))
	ENDIF
5432	FORMAT(/)
6410    CONTINUE
        WRITE(2,5432)
        WRITE(2,116)(XPARAM(I),I=INT(NIMX/2)+1,NIMX)
        WRITE(2,4003) (IXPAR(I),I=INT(NIMX/2)+1,NIMX)
	DO 6411 I=INT(NIMX/2)+1,NIMX
	IF (IXPAR(I) .EQ. 2) THEN
          WRITE(2,116) (XPVRY(K),K=INT(NIMX/2)+1,NIMX)
          WRITE(2,6408)
	GOTO 6412
	ENDIF
6411	CONTINUE
6412	CONTINUE
 
C 
C      DO 6411 I=1,NIMX
C      IF (IXPAR(I) .EQ. 2) THEN 
C         WRITE(2,116) (XPVRY(k),k=1,NIMX) 
C         WRITE(2,6408)
6408     FORMAT(//,24X,'** Extra parameter bounds shown above. **', 
     & /,10X,'-----------------------------------------------------') 
C         GOTO  6412 
C         END IF 
C6411     CONTINUE 
C6412     CONTINUE 
  
C 
C***********************************************************************
C     NIMX   = INITIAL VALUE OF NUMX, NO. OF EXTRA PARAMETERS TO BE 
C              SEARCHED ON
C     A(J,K) = ARRAY OF ALL PARAMETERS
C     A(J,1) = PEAK AMPLITUDE, J .LE. NO. OF PEAKS
C     A(J,2) = PEAK POSITION (BY CHANNEL NO.), J .LE. NO. OF PEAKS, 
C              FROM RIGHT TO LEFT STARTING AT THE ELASTIC PEAK
C     A(J,K) = EXTRA PARAMETERS, J .GT. NO. OF PEAKS
C***********************************************************************
C 
  171 FORMAT(//,10X,'***  USING CONSTANT ENERGY SEPARATION FOR PEAKS  ' 
     > ,'***'/(21X,I2,'  THRU  ',I2)) 
1704  FORMAT(/) 
      IF(IFIT.EQ.0) WRITE(2,1700) 
      IF(IFIT.EQ.1) WRITE(2,1701) 
      IF(IFIT.EQ.2) WRITE(2,1702) 
      IF(IFIT.EQ.3) WRITE(2,1703) 
      IF(MUL.NE.0) WRITE(2,171) (MULG(I),IMULG(I),I=1,MUL)
      IF(MUL.NE.0) WRITE(2,1704)
      IF(NUOP.NE.0) WRITE(2,172)
      IF(NCHI .EQ. 1) WRITE(2,1708) 
      IF(NCHI .EQ. 0) WRITE(2,1707) 
      IF(NCHI .GE. 2) WRITE(2,1709) 
 1700 FORMAT(/,10X,'***  DOUBLE GAUSSIAN PEAK FITTING FUNCTION ***',/)
 1701 FORMAT(/,10X,'***  SINGLE GAUSSIAN + EXPONENTIAL TAIL  ***',/)
 1702 FORMAT(/,10X,'***  ASYMMETRIC GAUSSIAN + EXPONENTIAL TAIL ***',/) 
 1703 FORMAT(/,10X,'***  DOUBLE GAUSSIAN + EXPONENTIAL TAIL  ***',/)
 1707 FORMAT(/,10X,'***  1/Y  WEIGHTING FOR CHI-SQUARE  ***',/) 
 1708 FORMAT(/,10X,'***  1/SQRT(Y)  WEIGHTING FOR CHI-SQUARE  ***',/) 
 1709 FORMAT(/,10X,'***  1/(Y+NCHI)  WEIGHTING FOR CHI-SQUARE  ***',/)
  172 FORMAT(/,10X,'***  USING NON-LINEAR WIDTH FUNCTION  ***',/) 
      IF(MUL.EQ.0) GO TO 545
      linename = 'AM1,AM2G(1),AM3,AM4G(1),ANG,E1'
      READ(1,*,ERR=9999) AM1,AM2G(1),AM3,AM4G(1),ANG,E1 
      WRITE(2,4008) 
      DO 1715 I=1,MUL 
      IF(I.EQ.1) GO TO 1715 
C changed line 1715 to write e1
      write(linename,'(A,I3)') 'AM2G(I),AM4G(I),I=',I
      READ(1,*,ERR=9999) AM2G(I),AM4G(I)
1715  WRITE(2,4015) AM1,AM2G(I),AM3,AM4G(I),ANG,E1 
CC**  DEL(I)= ENERGY LEVEL CORRESPONDING TO ITH PEAK
      write(linename,'(A,I3)') 'DEL(I),I=1,',MPEAK
      READ(1,*,ERR=9999)(DEL(I),I=1,MPEAK) 
      WRITE(2,122)(DEL(I),I=1,MPEAK)
122   FORMAT(//8X,'ENERGY LEVEL CORRESPONDING TO ITH PEAK', 
     *  /(3X,5F11.4),/(3X,5F11.4)) 
    2 FORMAT(10I5,F10.5)
  167 FORMAT(16X,7I6//)
      WRITE(2,168)    
  168 FORMAT(/,17X,'KINEMATICS DATA FOR REFERENCE PEAK',/,17X,'=================
     /=========================',/)
      WRITE(2,169)
  169 FORMAT(10X,'T4 = KINETIC ENERGY OF HEAVY PRODUCT',/)
      WRITE(2,173)
  173 FORMAT(1X,'  PEAK POSITION',1X,'  FLIGHT PATH',1X,            
     >'CALIBRATION',1X,' EXIT ENERGY',  
     * 1X,'FLIGHT TIME',1X,'    CHANNEL 0')
      linename = 'FP,CALB' 
      READ(1,*,ERR=9999) FP,CALB 
      IREF=ICHO 
      IF(IREF.LE.0) IREF=MULG(1)
      CHR=A(IREF,2) 
      IREFI=1 
      DO 1734 I=1,MUL 
      IF(IREF.GE.MULG(I).AND.IREF.LE.IMULG(I)) IREFI=I  
1734  CONTINUE
C     THE NEXT FEW CALL STATEMENTS INVOKE THE KINEMATICS...
      QGS=QGSG(IREFI) 
      AM2=AM2G(IREFI) 
      AM4=AM4G(IREFI) 
      CALL FLGHT(IREF,TR)
      EN=E3 
      FTR=TR*FP 
      CHO=FTR/CALB+CHR
C     IF ICHO (TIME=0 CHANNEL) IS NOT HELD CONSTANT.  
      IF(ICHO.NE.0) A(MPEAK+5,1)=CHO
      CHO=A(MPEAK+5,1)
C 
C*****CALCULATE CENTROIDS OF GROUP FROM REFERENCE PEAK AND DEL-ENERGY 
C 
      DO 540 I=1,MUL
      QGS=QGSG(I) 
      AM2=AM2G(I) 
      AM4=AM4G(I) 
      MULL=MULG(I)    
      MUL1=MULL+1 
      CALL FLGHT(MULL,TTR)
      A(MULL,2)=CHO-(TTR*FP)/CALB 
      IPAR(NPEAK+MULL)=1
      IMUL=IMULG(I) 
      DO 535 J=MUL1,IMUL
      CALL FLGHT(J,TJ)
      IPAR(J+NPEAK)=1 
535   A(J,2)=A(MULL,2)+FP*(TTR-TJ)/CALB 
540   CONTINUE
      CHR=A(IREF,2) 
      TOFG=3.3357*FP
      CHG=CHO-TOFG/CALB      
      WRITE (2,174)CHR,FP,CALB,EN,FTR,CHO
      T4=E1-EN+QGS 
      WRITE(2,'(''         T4='',F13.4/)') T4
  123 FORMAT(16X,7I6//)
      WRITE(2,124)
  124 FORMAT(17X,'KINEMATICS DATA FOR OTHER PEAKS',/,17X,'===================
     /=================',/)
C     FLIGHT TIME AND ENERGY LOOP FOR ALL STATES (FROM UPDATE 10 JUL)
      DO 125 J=2,MPEAK
C     LOOP OVER EXCITATION ENERGIES      
      CALL FLGHT(J,T)
      T4=E1-E3+(QGS-DEL(J))
  125 WRITE(2,'(''     PEAK    EXIT ENERGY  FLT.TM.         T4''/1X,I6,   
     *   3F13.4/)') J,E3,T,T4
  174 FORMAT(1X,5F13.4,F15.4//)
      WRITE(2,4011) TOFG,CHG
C     END OF FLIGHT TIME UPDATE, 10 JUL 1990
      ZZ1=DIST*SIGR 
      ZZ2=DIST*SIGR+PJON
      ZZ3=SIGR*2.354
      WRITE(2,2211) ZZ1,ZZ2,ZZ3 
2211  FORMAT(/10X,'DIST*SIGR     DIST*SIGR+PJON', 
     &         5X,'Half width.',
     ,       /9X,F8.4,10X,F8.4,9X,F8.4)
C 
C***********************************************************************
C     CALB  = CALIBRATION CONSTANT, IN NSEC/CHANNEL 
C     CHR   = CHANNEL OF REFERENCE PEAK 
C     DEL(I)= ENERGY DIFFERENCE BETWEEN ITH PEAK AND REFERENCE PEAK 
C     EN    = EXIT ENERGY OF THE REFERENCE PEAK 
C     MULG  = PEAK NO. OF 1ST PEAK IN GROUP TO BE FITTED TOGETHER 
C     IMULG = PEAK NO. OF LAST PEAK IN GROUP TO BE FITTED TOGETHER
C     FTR   = FLIGHT TIME OF REFERENCE PEAK, IN NSEC
C     CHO   = CHANNEL CORRESPONDING TO ZERO TIME
C***********************************************************************
C 
      WRITE(2,4010) 
      WRITE(2,4005)(A(I,2),I=1,MPEAK) 
545   CONTINUE
C 
C***********************************************************************
C     DELTI= INSTRUMENTAL TIME UNCERTAINTY, IN NSEC 
C     DELEX= EXPERIMENTAL ENERGY UNCERTAINTY, IN KEV
C     FP   = FLIGHT PATH, IN METERS 
C     MASS = MASS OF NEUTRON, IN KEV/C**2 
C     NREF = PEAK NO. OF REFERENCE PEAK 
C     NUOP = OPTION TO USE NON-LINEAR WIDTH FUNCTION
C     SIGR = SIGMA FOR REFERENCE PEAK, IN CHANNELS
C***********************************************************************
C 
C     IL=2384 
      IF(IRUN.EQ.LASTR) GO TO 39
      IWHERE=6
      RETURN
 706  CONTINUE
      CALL EQUV
      IYY=0 
    4 CONTINUE
   29 FORMAT(1H1) 
      IF(IMTRX.GE.0)GO TO 50
      IF(IYY.GT.2) GO TO 999
      IS=0
      IF(IWRITE.EQ.0) GO TO 32
      DO 30 I=BEG,JEND,16 
      K=I+15
      WRITE(2,31)(KDATA(J),J=I,K),K 
   31 FORMAT(16(1X,I6),10X,I5)
   30 CONTINUE
   32 CONTINUE
      IF(IYY-1)39,40,50 
   39 CONTINUE
      IYY=IYY+1 
C     CALL SMOTH(NCHAN) 
      IWHERE=2
      IGOTO=2 
      RETURN
 702  CONTINUE
      IF(IRUN.EQ.LASTR) GO TO 40
      GO TO 4 
   40 CONTINUE
      IYY=IYY+1 
C      CALL BACKG(ILC1,ILC2,IHC1,IHC2) 
      IWHERE=3
      IGOTO=3 
      RETURN
703   CONTINUE
      IF(IRUN.EQ.LASTR) IYY=2 
      IF(IMTRX.GE.0)GO TO 50
      GO TO 4 
   50 CONTINUE
      DO 716 I=1,MPEAK
      IF(IPAR(I).NE.0) GO TO 716
      IF(A(I,1).GT.0.0) GO TO 716 
      J=A(I,2)
      A(I,1)=KDATA(J) 
716   CONTINUE
      DELTA=DNAUT 
      LASTR=IRUN
      NBRNCH=0
      NLOOP=0 
      NN=0
      JALT=0
  100 CONTINUE
C 
C***********************************************************************
C     J   =  PEAK NO. 
C     JALT=  OPTION FOR PEAK FITTING: IF JALT=0, ALL PEAKS IN REGION ARE
C            FITTED;  IF JALT.GT.0, ONLY THE PEAK FOR WHICH J=JALT IS 
C            FITTED 
C***********************************************************************
C 
      IF (INTRA .EQ. 0) WRITE(*,1002)(A(I,1),I=1,NPEAK) 
      IF (INTRA .EQ. 3) WRITE(*,1002)(A(I,1),I=1,NPEAK) 
      IF (INTRA .EQ. 0) WRITE(*,1002)(A(I,2),I=1,NPEAK) 
      IF (INTRA .EQ. 3) WRITE(*,1002)(A(I,2),I=1,NPEAK) 
      CALL SMPEK(MPEAK,JALT,NIMX)
      CALL CLXSQ
C     GRID SEARCH 
      NLOOP=0 
      NDPARM=MPEAK+1
      XSQOLD=ERSUM
      IF (INTRA .EQ. 0 .OR. INTRA .EQ. 3) 
     & WRITE(*,4004) NDPARM,IPAR(NDPARM),NTIMES,IDELT,NLOOP,XSQOLD,ERSUM
  410 CONTINUE
      NLOOP=NLOOP+1 
      NTIMES=0
  420 CONTINUE
      IF(IPAR(NDPARM).EQ.1) GO TO 431 
      IF (IFAST .EQ. 1 .AND. IZPAR(NDPARM) .EQ. 0) GOTO 431 
      J=NDPARM-((NDPARM-1)/NPEAK)*NPEAK 
      K=1+(NDPARM-1)/NPEAK
      KPARM=IPAR(NDPARM)
      CALL DLSIG(MPEAK,J,K,1,SKPIT,KPARM) 
      IF (SKPIT .LT. 0) GOTO 4429 
      SKPIT=1.
      CALL SMPEK(MPEAK,JALT,NIMX)
      CALL CLXSQ
      DIFF=(XSQOLD-ERSUM)/XSQOLD
      IF(NLOOP.GT.LOOPMX) GO TO 440 
      IDELT=(DELTA/ABS(DELTA))*1.5
      IF (INTRA .EQ. 0 .OR. INTRA .EQ. 3) 
     & WRITE(*,4004) NDPARM,IPAR(NDPARM),NTIMES,IDELT,NLOOP,XSQOLD,ERSUM
     >   ,A(J,K)
      IF(DIFF.GT.0.0) XSQOLD=ERSUM
      IF((DIFF.GT.0.0).AND.(ABS(DIFF).GT.ABS(.01*DELTA))) GO TO 410 
      IF(DIFF.GE.0.0) GO TO 429 
      CALL DLSIG(MPEAK,J,K,2,SKPIT,KPARM) 
4429  DELTA=-DELTA
      IF(DELTA.LT.0.0.AND.NLOOP.LE.1) GO TO 420 
429   DELTA=ABS(DELTA)
      NLOOP=1 
431   NDPARM=NDPARM+1 
      NDPARM=NDPARM-((NDPARM-1)/NTOTAL)*NTOTAL
      NTIMES=NTIMES+1 
C      IF (IFBRK(IDUMY))  2430,1431
C2430  CALL RESPN(KKFLG,IFAST) 
C5543  CONTINUE
C      IF (KKFLG .NE. 0) GOTO 2431 
1431  IF(NTIMES.LE.NTOTAL) GO TO 420
C 
C.......................... 
C 
2431  CONTINUE
      IF (IFAST .EQ. 1) THEN
          IFAST=0 
          NLOOP=0 
          GOTO 410
          END IF
          WRITE(2,1002)(A(I,1),I=1,NPEAK) 
      IF (INTRA .EQ. 3) WRITE(*,1002)(A(I,1),I=1,NPEAK) 
      IF (INTRA .EQ. 0) WRITE(*,1002)(A(I,1),I=1,NPEAK) 
      WRITE(2,1002)(A(I,2),I=1,NPEAK) 
      IF (INTRA .EQ. 3) WRITE(*,1002)(A(I,2),I=1,NPEAK) 
      IF (INTRA .EQ. 0) WRITE(*,1002)(A(I,2),I=1,NPEAK) 
      XSQR=XSQOLD/(IEND-ISTART+1-NTOTAL)
      WRITE(2,432)XSQOLD,XSQR,DELTA 
      IF (INTRA .EQ. 3) WRITE(*,432)XSQOLD,XSQR,DELTA 
      IF (INTRA .EQ. 0) WRITE(*,432)XSQOLD,XSQR,DELTA 
  432 FORMAT(3X,3F20.5,//)
      DDEL=ABS(DELTA) 
      DELTA=DELTA/3.
      IF(DDEL.LT.0.0005) GO TO 1007 
      IF(KKFLG.EQ.1) GO TO 1007 
      NLOOP=0 
      GO TO 410 
  440 CONTINUE
      WRITE(2,441) NLOOP
  441 FORMAT(3X,19HBAD INPUT, NLOOP= ,I6) 
 1007 CONTINUE
      WRITE(2,1008) 
 1008 FORMAT(4(1H ))
      WRITE(2,131) IRUN,IDAY,IMONTH,IYEAR 
  131 FORMAT(10X,21HRUN,DAY,MONTH,YEAR   ,4I5)  
      CALL BLCSM(MPEAK) 
      NKLUZ=0 
      WRITE(2,1001) 
 1001 FORMAT(/,14X,' PEAK AMPLITUDES AND POSITIONS',/)  
      WRITE(2,1002)(A(I,1),I=1,NPEAK) 
      WRITE(2,1002)(A(I,2),I=1,NPEAK)  
      WRITE(82,1002)(A(I,1),I=1,NPEAK) 
      WRITE(82,1002)(A(I,2),I=1,NPEAK)  
      WRITE(108,'(1X,10F6.0)')(A(I,2),I=1,MPEAK)
      WRITE(108,'(1X,10I6)')(0,I=1,MPEAK)
      WRITE(108,'(1X,10F6.0)')(A(I,1),I=1,MPEAK)
      WRITE(108,'(1X,10I6)')(0,I=1,MPEAK)
C  SEPT UPDATE CHANGED FOLLOWING LINE FROM E1= TO QGSG(2)=...
      QGSG(2)=A(MPEAK+4,1) 
      QGSG(1)=A(MPEAK+4,2)
      QGS=QGSG(IREFI) 
      AM2=AM2G(IREFI) 
      AM4=AM4G(IREFI) 
C   **********************
      CALL FLGHT(IREF,TR) 
      FTR=FP*TR 
      CHO=A(MPEAK+5,1)
      IF(ICHO.NE.0) CHO=A(MUL,2)+FTR/CALB 
      TOFG=3.3357*FP
      CHG=CHO-TOFG/CALB 
C     FOLLOWING PRINTS OUT KIN. DATA AFTER VARIATION
   82 FORMAT(16X,7I6//)
      WRITE(2,83)    
   83 FORMAT(/,17X,'KINEMATICS DATA FOR REF. PEAK AFTER FITTING',/,17X,'===
     /===========================================',/)      
      WRITE(2,173)
      WRITE(2,174) A(IREF,2),FP,CALB,E3,FTR,CHO 
	write(82,*)A(IREF,2)
      WRITE(2,4011) TOFG,CHG
C 
      ZZ1=DIST*SIGR 
      ZZ2=DIST*SIGR+PJON
      ZZ3=SIGR*2.354
      WRITE(2,2211) ZZ1,ZZ2,ZZ3 
C 
      IF(IWRITE.EQ.0) GO TO 1050
 1002 FORMAT(3X,10F12.4) 
 1003 CONTINUE
      DO 1032 I=ISTART,IEND,16
      K=I+15
      WRITE(2,1031)(ITDATA(J),J=I,K)
 1031 FORMAT(16(1X,I6)) 
 1032 CONTINUE
      IF(NKLUZ.EQ.1) GO TO 1050 
      DO 1040 I=ISTART,IEND 
      ITDATA(I)=KDATA(I)-ITDATA(I)
 1040 CONTINUE
      NKLUZ=1 
      GO TO 1003
 1050 CONTINUE
      IF(NLOOP.GT.LOOPMX) GO TO 998 
C----------------------------------------------------------------------
C  Compute area and statistical uncertainty (Poisson)
C----------------------------------------------------------------------
      DO 1060 I = 1, MPEAK
        JALT = I
        CALL SMPEK(MPEAK, JALT, NIMX)

        PKSUM  = 0.0
        VARSUM = 0.0

        NLHS = A(I,2) - 4.0*SIGSAV(I)
        NRHS = A(I,2) + 3.35*SIGSAV(I)


        DO 1080 J = ISTART, IEND
          PKSUM  = PKSUM  + ITDATA(J)
          VARSUM = VARSUM + MAX(ITDATA(J), 1.0)


1080    CONTINUE

        SIGAREA = SQRT(VARSUM)
        HPEAKAREA(I) = PKSUM
        HPEAKERR(I) = SIGAREA

C----- print to main log (san12.out)
        WRITE(2,1070) DEL(I), I, PKSUM, SIGAREA, NLHS, NRHS
C----- write to integration summary (integ.out)
        WRITE(85,1171) I, PKSUM, SIGAREA
C----- write to quick summary file
      OPEN(UNIT=90, FILE='fit_summary.txt', STATUS='UNKNOWN',
     &     POSITION='APPEND')
      WRITE(90,'(A,I2,A,F12.2,A,F10.2)') 'Peak ', I, ': Area = ',
     &     PKSUM, ' +/- ', SIGAREA
      CLOSE(90)


1070    FORMAT(/,3X,'Ex=',F8.4,' MeV',
     &          5X,'AREA(',I2,') =',F10.1,
     &          ' +/-',F8.1,
     &          3X,'LHS=',I4,2X,'RHS=',I4)
1171    FORMAT('Peak',I3,2X,'Area=',F10.2,2X,'+/-',F8.2)

1060  CONTINUE

      DF=IEND-ISTART-NTOTAL 
      XXSQ=XSQOLD/DF
      WRITE(2,1004) XSQOLD,DF,XXSQ
	write(82,*)xxsq
 1004 FORMAT(/,3X,' XSQ = ',F15.3,'    NDF = 'F4.0,'   REDXSQ =' 
     > ,F9.3,/) 
      JALT=0
      CALL SMPEK(MPEAK,JALT,NIMX)
      write(*,*) "************************************"
	  write(*,*) "      XSQ OF FITS         "      
	  DO 1061 I=1,MPEAK 
      XSQPK=0.0 
      NLHS=A(I,2)-4.00*SIGSAV(I)
      NRHS=A(I,2)+3.35*SIGSAV(I)
      DO 155 IT=NLHS,NRHS 
      X1=(KDATA(IT)-ITDATA(IT))**2
      SDATE=ABS(ISDATA(IT)) 
      IF(SDATE .LT. 1.0) SDATE=1.0
      XSQP=X1/SDATE 
      IF (NCHI .GT. 2) XSQP=X1/(SDATE+NCHI) 
      XSQPK=XSQPK+XSQP
 155  CONTINUE
      XSQPK=XSQPK/(NRHS-NLHS) 
      WRITE(2,1071)I,XSQPK,NLHS,NRHS
 1071 FORMAT(/,3X,6HXSQPK(,I2,') =',F10.3,2X,'LHS=',I4,2X,'RHS=',I4) 
	  write(*,"(3X,6HXSQPK(,I1,') =',F10.3,2X)") I, XSQPK

c  added by sfh & then jrv
      iloc=INDEX(title,'.')         ! jrv 120925
      titleshort=title(1:iloc-1)    !
      write(85,1057)titleshort,cdate,ctime,ang,i,nlhs,nrhs
 1057 format(1a15,1x,a9,1x,a8,1f4.0,1i3,1i8,1i8)
c 1057 format(1a30,1f4.0,1i3,1i8,1i8)
 1061 CONTINUe
	write(*,*) "************************************"   	
	WRITE(2,7776)
7776	FORMAT(//,1X,80(1H*))
      ICH=0 
      IF (INTRA .EQ. 1) GOTO 704
      IWHERE=4
      IGOTO=4 
c write parameters for easy evaluation
      write(82,*)mpeak
	rewind(82)
	read(82,*)(htmax(i),i=1,mpeak),hdt,hsigr,hpjon,hqs2,hchz
	read(82,*)(htmin(i),i=1,mpeak),hratio,hdist,hslop1,hqs,hde
	read(82,*)refcenter
c	do i=1,mpeak
c	read(82,*)hex(i),hpeakid(i),hpeakarea(i)
c     enddo
c	read(82,*)hredchisq
	iloc=INDEX(title,'.')         ! jrv 120925
      titleshort=title(1:iloc-1)    !    and included date,time below
!        WRITE(FMT,'("1a15,1x,1a9,1x,1a8,1x,f5.1,",I1,"f10.0,2f10.3, &
!     &		 10f10.4")') mpeak

	write(84,1063)titleshort,cdate,ctime,ang, 
     *           refcenter,
     &           hredchisq,hdt,
     &           hratio,hsigr, hdist,hpjon,hslop1,hqs2,hqs,hchz,hde,
     &           (hpeakarea(i),i=1,mpeak)
	write(108,'(1X,5F6.2,1X,1F10.2,1X,2F6.2,1X,F10.2,1X,F6.2)') hdt,
     &           hratio,hsigr, hdist,hpjon,hslop1,hqs2,hqs,hchz,hde
      write(108,*)
	write(108,"(*(F10.0))") (hpeakarea(i),i=1,mpeak)
!	WRITE(FMT1,'("1a15,1x,a9,1x,a8,1f5.1,",I0,"f10.0,2f10.3, &
!     &		10f10.4, 1x,1i10,1x,1i10,1x,1a8,1x,1a8")') mpeak
	write(86,1062)titleshort,cdate,ctime,ang,
     *           refcenter,
     &           hredchisq,hdt,
     &           hratio,hsigr, hdist,hpjon,hslop1,hqs2,hqs,hchz,hde,
     &           iqcharge,itime,idate,iclock,(hpeakarea(i),i=1,MPEAK)
      write(*,*) "************************************"
      write(*,*) "        AREA OF PEAKS (± 1σ)      "
      do i = 1, MPEAK
        WRITE(*,'(2X,I2,'' => '',F12.1,'' +/- '',F12.3)') 
     &        i, hpeakarea(i), hpeakerr(i)
      enddo
      write(*,*) "************************************"

C	  write(*,"(3X,6HXSQPK(,I1,') =',F10.3,2X)") I, XSQPK
 1063  format(1a15,1x,a9,1x,a8,1x,1f5.1,2f10.3,10f10.4,*(F10.0))
 1062  format(1a15,1x,a9,1x,a8,1f5.1,2f10.3,
     &       10f10.4, 1x,1i10,1x,1i10,1x,1a8,1x,1a8,2F10.0)
 2063  format(1a15,1x,a9,1x,a8,1x,1f5.1,1F10.0,2f10.3,10f10.4)
 2062  format(1a15,1x,a9,1x,a8,1f5.1,1F10.0,2f10.3,
     &       10f10.4, 1x,1i10,1x,1i10,1x,1a8,1x,1a8)
	close(82)
	close(84)
	close(85)
      RETURN
 704  CONTINUE
      ICH=0 
      IF (INTRA .EQ. 1) GOTO 8705 
      IF (INTRA .EQ. 2) GOTO 705
      WRITE(*,4013) 
      READ(*,4007) ICH
      IF(ICH.NE.0) GO TO 705
8705  IWHERE=4
      IGOTO=5 
      RETURN
 705  CONTINUE
      WRITE(2,1008) 
      WRITE(2,1008) 
  998 CONTINUE
      GO TO 1 
   16 CONTINUE
      WRITE(2,12) 
   12 FORMAT(5X,'ERROR DURING READ')
      GO TO 1 
  999 CONTINUE
      WRITE(2,1006) 
 1006 FORMAT(5(10X,3HEND))
      IWHERE=10 
      RETURN
C 
4001  FORMAT(20I1)
4002  FORMAT(11X,/,10(I10)) 
4003  FORMAT(10X,5I10)
4004  FORMAT(1X,5I5,1X,F12.5,1X,3F12.5) 
4005  FORMAT(1X,10F11.3) 
C4006  FORMAT(' IS THIS A GRAPHICS TERMINAL? (Y/N): _') 
4007  FORMAT(A1)
4008  FORMAT(///10X,'       AM1       AM2       AM3       AM4', 
     *   '         ANGLE  INC.ENERGY')
4009  FORMAT(//20X,'MUL   NREF     NUOP  IWRITE IAUTOA  ICHO'/16X,6I7//)
4010  FORMAT(/4X,'NEW PEAK POSITIONS') 
4011  FORMAT(/4X,' TOF GAMMA     GAMMA CHANNEL'/4X,F9.3,F16.3) 
4012  FORMAT(1H1//20X,'STARDATE --  ',A17///)
4013  FORMAT(' DO YOU WANT A PRINTER PLOT? (Y/N): _') 
4014  FORMAT(19X,'MULG  IMULG'/(15X,2I7)) 
4015  FORMAT(10X,4F11.5,F10.1,F10.5)
4016  FORMAT(10X,'FILE NAME : ',A30,//
     & ,10X,'         NSPC    INTRA   NCHI',
     &  /,16X,3I7//)
 9999 WRITE(*,'(A,A)') 'Error reading ',linename
      stop 'Abort'
	END
C*****************************************************************
      SUBROUTINE FLGHT(J,TJ)
C*****************************************************************
      COMMON /COMN1/ AM1,AM2,AM3,AM4,QGS,ANG,E1,AA,BB,CC,DD,SG,E3,
     &       SAR,E4,T,SW,SI,SA,ETHR,QEX,EXCIT,AM2G(5),AM4G(5),QGSG(5) 
      COMMON/NLINS/MUL,IMUL,CHR,FTR,DEL(20),RT(20),NSKIP, 
     &   MULG(5),IMULG(5) 
      QEX=QGS-DEL(J)
      SG=+1.
c      CALL CALC(QEX)  ! jrv 120924 
      CALL CALC
      TJ=T
      RETURN
      END 
C******************************************************************** 
C     THE FOLLOWING SUBROUTINE WAS ALTERED TO USE THE KINEMATICS
C     METHOD FOUND IN KINMAT.  THE FORMULAS ARE ALL RELATIVISTIC.
C     (UPDATE 10 JULY 1990 KAW)
C    THIS SUBROUTINE DOES THE KINEMATIC CALCULATIONS
C     OF ENERGIES, INTENSITY AND FLIGHT TIME
C*********************************************************************
      SUBROUTINE CALC
C********************************************************************** 
      COMMON /COMN1/ AM1,AM2,AM3,AM4,QGS,ANG,E1,AA,BB,CC,DD,SG,E3,
     &       SAR,E4,T,SW,SI,SA,ETHR,QEX,EXCIT,AM2G(5),AM4G(5),QGSG(5) 
      COMMON /COMN3/ DIS,TOG,GPEAK,CAL,TON,DELT,DELC,EPEAK,IZPAR(50)
      COMMON/COMN4/  A,B,MM1,MM2,MM3,MM4,P1
C    
      SW=0     
      ANGR=ANG/57.29577951
C     QEX=QGS-DEL(I)  WHERE DEL(I)  ARE  EXCITATION ENERGIES
      DEN=(AM1+AM2)*(AM3+AM4)
      MCONV=931.478 
      MM1=AM1*MCONV  
      MM2=AM2*MCONV
      MM3=AM3*MCONV
      MM4=AM4*MCONV   
      P1=SQRT(E1**2+2.*MM1*E1)
      ET=E1+MM1+MM2
      A=2.*MM2*E1+2.*MM1*MM3+2.*MM2*MM3+2.*QEX*(MM1+MM2-MM3)-QEX**2
      P1=SQRT(E1**2+2.*MM1*E1)
      B=ET**2-(P1**2)*(COS(ANGR))**2       
      XQ2=A**2-4.*B*MM3**2
C   The old kinematics is used only to find the maximum angle. 
      AA=AM1*AM4*(E1/ET)/DEN
      BB=AM1*AM3*(E1/ET)/DEN
      CC=(AM2*AM3/DEN)*(1.0+AM1*QEX/(AM2*ET)) 
      DD=(AM2*AM4/DEN)*(1.0+AM1*QEX/(AM2*ET)) 
      IF(BB .LE. DD) GOTO 120 
C The maximum angle allowed is calculated here. 
C This angle meets the condition on p. 141 Marion & Fowler. 
      SI=ATAN(SQRT(DD/(BB-DD))) 
      SI=SI*180/3.14159 
      WRITE(*,25)SI 
 25   FORMAT(/
     *'                                              ', 
     *'  The maximum allowed angle is--',F7.3,'      ', 
     *'                                              ') 
      IF(ANG .LE. SI) GOTO 120
      SW=1
      RETURN
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  E3 Is the lab kinetic energy of the light product
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
120   IF (XQ2 .LT. 0.0) THEN
        WRITE(*,*) 'Error level cannot be populated at this energy'
        STOP 'Abort'
      ENDIF
      E3=(ET*A+P1*COS(ANGR)*SQRT(XQ2))/(2.*B)-MM3 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     E4 Is the lab kinetic energy of the heavy product.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      E4=E1-E3+QGS
      SAR=(SQRT(AA*CC)*SQRT(DD/BB-(SIN(ANGR))**2))*(ET/E3)
      SA=1.0/SAR
C The following calculates the flight time in nanoseconds/meter.
C E3 is the KINETIC energy of the light product.
C A relativistic correction has been made.
      T=1./(.299792456*SQRT(1-1/(1+E3/939.55)**2)) 
C     T=72.356/SQRT(E3) IS NONRELATIVISTIC!
      RETURN
      END 
C 
C***********************************************************************
      SUBROUTINE SMPEK(MPEAK,JALT,NIMX)
C	,COMPUTES FIT 
C***********************************************************************
C 
      REAL KDATA,ITDATA,ISMFIT,ISMR,IXDATA
      REAL MASS 
      INTEGER BEG 
      COMMON/TXQ/ IXDATA(10,16384),MXPEAK
      COMMON/BIGGY/KDATA(16384),ITDATA(16384),DUMMY1(16384),
     * ISMFIT(16384),ISMR(16384),APRM(50),IA5(10),IB5(10),IPART,IPAR(50)
     *,AVARY(25,2),ACONST(25,2) 
      COMMON/OTHER/RM,RB,BEG,JEND,ISTART,IEND,MULN,DIST,RATIO,IFIT,PJON 
     &             ,ISKPY(10,2) 
      COMMON/NLINW/DELEX,DELTI,FP,MASS,NREF,NUOP,SIGR,SIGSAV(20),ISMF,
     & ISMSTRT,ISMEND 
      COMMON/BRAV/CALB,CHO,IREF,IREFI 
      DIMENSION A(25,2) 
      EQUIVALENCE (A(1,1),APRM(1))
C 
      IQ=MPEAK+1
      IIQ=IQ+1
      IIQQ=IQ+2 
      DELT=A(IQ,1)
      RB=DELT 
      RATIO=A(IQ,2) 
      SIGR=A(IIQ,1) 
      RM=SIGR 
      DIST=A(IIQ,2) 
      PJON=A(IIQQ,1)
      SLOP1=A(IIQQ,2) 
      CHO=A(IQ+4,1) 
      DELEX=A(IQ+4,2) 
      IF(RATIO.EQ.0.0) RATIO=0.384
      IF(DIST.EQ.0.0) DIST=1.9
      IF(PJON.EQ.0.0) PJON=1.0
      DO 104 I=ISTART,IEND
104   ITDATA(I)=0.0 
C*****START OF PEAK LOOP************************************************
      CHREF=A(NREF,2) 
      MXPEAK = MPEAK
      DO 3000 J=1,MPEAK 
      PKJ=A(J,1)
      CHJ=A(J,2)
      IF((JALT.NE.0).AND.(J.NE.JALT)) GO TO 3000
      IF(NUOP.EQ.0) GO TO 301 
C 
C  0.08987= (c/10)**2 
C 
      DELTR=.8987E-01*(((CALB*(CHO-CHREF    ))**3)*DELEX)/(MASS*FP**2)
      DELTX=.8987E-01*(((CALB*(CHO-CHJ   ))**3)*DELEX)/(MASS*FP**2) 
      REF=(DELT   **2)+(DELTR**2) 
      SIG=SQRT(((DELT   **2)+(DELTX**2))/REF)*SIGR
      GO TO 206 
301   SIG=RM*CHJ + RB 
      SIG=ABS(SIG)
  206 CONTINUE
      SIGSAV(J)=SIG 
C*****START OF CHANNEL LOOP***************************************
      DO 2900 I=ISTART,IEND 
      IF(IFIT.EQ.0) GOTO 601
      IF(IFIT.EQ.1.OR.IFIT.EQ.5) GOTO 503 
      IF(IFIT.EQ.3) GOTO 801
      IF(I.LT.(CHJ   -RATIO)) GOTO 701
      IF(I.LE.CHJ   ) GOTO 702
      EXPS=(-0.5*((I-CHJ   )/SIG)**2) 
      GOTO 703
 702  EXPS=(-0.5*((I-CHJ   )/DIST/SIG)**2)
 703  IF(EXPS.LT.-10.00) GOTO 10
      DFIT=EXP(EXPS)
      GOTO 40 
 701  IF(IFIT.EQ.6.AND.I.LT.(CHJ   -RATIO-PJON)) GOTO 50
      EXPS=(-0.5*RATIO*(2.*(CHJ   -I)-RATIO)/(DIST*SIG)**2) 
      DFIT=EXP(EXPS)
      GOTO 40 
 503  IF(I.LT.(CHJ   -RATIO)) GOTO 501
      EXPS=(-0.5*((I-CHJ   )/SIG)**2) 
      IF(EXPS.LT.-10.00) GOTO 10
      DFIT=EXP(EXPS)
      GOTO 40 
 501  IF(IFIT.EQ.5.AND.I.LT.(CHJ   -RATIO-PJON)) GOTO 70
      EXPS=(-0.5*RATIO*(2.*(CHJ   -I)-RATIO)/SIG**2)
      DFIT=EXP(EXPS)
      GOTO 40 
 601  EXPS=(-0.5*((I-CHJ   )/SIG)**2) 
      EXPD=(-0.5*((I-CHJ   +DIST*SIG)/SIG)**2)
      IF(EXPS.LT.-10.00.AND.EXPD.LT.-10.00) GOTO 10 
      IF(EXPS.LT.-10.00) GOTO 20
      IF(EXPD.LT.-10.00) GOTO 30
      DFIT=EXP(EXPS)+RATIO*EXP(EXPD)
      GOTO 40 
 801  IF(NIMX.EQ.4) GOTO 803
      IF(I.LE.(CHJ   -DIST*SIG-PJON)) GOTO 802
      GOTO 804
 803  IF(I.LE.(CHJ   -2.5*SIG-DIST)) GOTO 802 
      EXPS=(-0.5*((I-CHJ   )/SIG)**2) 
      EXPD=(-0.5*((I-CHJ   +2.5*SIG)/SIG)**2) 
      GOTO 808
 804  EXPS=(-0.5*((I-CHJ   )/SIG)**2) 
      EXPD=(-0.5*((I-CHJ   +DIST*SIG)/SIG)**2)
 808  IF(EXPS.LT.-10.00.AND.EXPD.LT.-10.00) GOTO 10 
      IF(EXPS.LT.-10.00) GOTO 20
      IF(EXPD.LT.-10.00) GOTO 30
      DFIT=EXP(EXPS)+RATIO*EXP(EXPD)
      GOTO 40 
 802  IF(NIMX.EQ.4) GOTO 805
      EXPS=(-0.5*(DIST*SIG+PJON)*(2.*(CHJ   -I)-(DIST*SIG)-PJON)/SIG**2)
      EXPD=(-0.5*PJON*(2.*(CHJ   -DIST*SIG-I)-PJON)/SIG**2) 
      GOTO 806
 805  EXPS=(-0.5*(2.5*SIG+DIST)*(2.*(CHJ   -I)-(2.5*SIG)-DIST)/SIG**2)
      EXPD=(-0.5*DIST*(2.*(CHJ   -2.5*SIG-I)-DIST)/SIG**2)
 806  DFIT=EXP(EXPS)+RATIO*EXP(EXPD)
      GOTO 40 
C     EXPD=(-0.5*((I-CHJ   +XPOGG*SIG)/SIG)**2) 
C     EXPD=(-0.5*((I-CHJ   +DIST    *SIG)/SIG)**2)
C     EXPD=(-0.5*((I-CHJ   +2.5*SIG)/SIG)**2) 
C     IF(EXPS.LT.-10.00.AND.EXPD.LT.-10.00) GO TO 10
C     IF(EXPS.LT.-10.00) GO TO 20 
C     IF(EXPD.LT.-10.00) GO TO 30 
C     DFIT=EXP(EXPS)+RATIO*EXP(EXPD)
   10 CONTINUE
      DFIT=0.0
      GO TO 40
   20 CONTINUE
      DFIT=EXP(EXPD)*RATIO
      GO TO 40
   30 CONTINUE
      DFIT=EXP(EXPS)
   40 CONTINUE
      DFIT=PKJ   *DFIT*(0.9485) 
      GOTO 260 
   50 CONTINUE
      EXPS=(-0.5*RATIO*(RATIO+2*PJON)/(SIG*DIST)**2)
      DFIT=EXP(EXPS)
      DFIT=PKJ   *(0.9485)*DFIT 
      DFIT=DFIT+SLOP1*(CHJ   -RATIO-PJON-I) 
      GOTO 260 
   70 CONTINUE
      EXPS=(-0.5*RATIO*(RATIO+2*PJON)/(SIG**2)) 
      DFIT=EXP(EXPS)
      DFIT=PKJ   *(0.9485)*DFIT 
      DFIT=DFIT+DIST*(CHJ   -RATIO-PJON-I)
260   ITDATA(I)=ITDATA(I)+DFIT
      IXDATA(J,I) = DFIT
60    CONTINUE
      ZMGAUS=ITDATA(I)
C     WRITE(2,300)EXPS,EXPD,ZMGAUS,I
  300 FORMAT(3X,3F10.3,3X,I4) 
2900  CONTINUE
C*****END OF CHANNEL LOOP***********************************************
3000  CONTINUE
C*****END OF PEAK LOOP**************************************************
      IF(ISMF.LE.1) RETURN
C*****SMEAR FIT WITH A UNIT AREA CONVOLUTION FUNCTION 
      DO 3300 I=ISMSTRT,ISMEND 
      ISMFIT(I)=0.
      DO 3300 J=1,ISMF
      JI=I-J+1
      IF(JI.LT.ISMSTRT) GO TO 3300 
      ISMFIT(JI)=ISMFIT(JI)+ITDATA(I)*ISMR(J) 
3300  CONTINUE
      DO 3400 I=ISMSTRT,ISMEND 
3400  ITDATA(I)=ISMFIT(I) 
      RETURN
      END 
C 
C***********************************************************************
      SUBROUTINE EQUV
C	,  STORES DATA IN ANOTHER ARRAY
C***********************************************************************
C 
      DIMENSION IDATA(16384)
      REAL KDATA,ITDATA,ISMFIT,ISMR 
	CHARACTER NFILE*30,HEADER*72
      COMMON/BIGGY/KDATA(16384),ITDATA(16384),DUMMY1(16384),
     * ISMFIT(16384),ISMR(16384),APRM(50),IA5(10),IB5(10),IPART,IPAR(50)
     *,AVARY(25,2),ACONST(25,2)
	COMMON/QINP/NFILE,NSPEC,HEADER,IDATA
      DO 100 I=1,16384 
      IF(IDATA(I).EQ.0.0) IDATA(I)=1. 
      KDATA(I)=IDATA(I) 
  100 CONTINUE
      RETURN
      END 
C 
C***********************************************************************
      SUBROUTINE CLXSQ
C	, CALCULATES CHI-SQUARE 
C***********************************************************************
C 
C      DIMENSION IDATA(16384)
      INTEGER BEG 
      REAL KDATA,ITDATA,ISDATA,ISMFIT,ISMR 
      COMMON/KDIF/XSQOLD,XSQNEW,DIFF,ERSUM,NCHI 
      COMMON/OTHER/RM,RB,BEG,JEND,ISTART,IEND,MULN,DIST,RATIO,IFIT,PJON 
     &             ,ISKPY(10,2) 
      COMMON/BIGGY/KDATA(16384),ITDATA(16384),ISDATA(16384),
     * ISMFIT(16384),ISMR(16384),APRM(50),IA5(10),IB5(10),IPART,IPAR(50)
     *,AVARY(25,2),ACONST(25,2) 
C 
      ERSUM=0.0 
      DO 100 I=ISTART,IEND
C 
C    SKIP POINT IN X-SQ CALCULATION IF DESIRED. 
C
	DO 90 JJ=1,10 
         IF (ISKPY(JJ,1) .LE. 0) GOTO 91
         IF (I .GE. ISKPY(JJ,1) .AND. I .LE. ISKPY(JJ,2)) GOTO 100
  90     CONTINUE 
  91  CONTINUE
      X=(KDATA(I)-ITDATA(I))**2
C********************************** 
      IF(NCHI .GE. 0) GOTO 101
  101 CONTINUE
      SDATA=ABS(ISDATA(I))
      IF(SDATA.LT.1.0) SDATA=1.0
      IF(NCHI.EQ.1) XSQ=X/SQRT(SDATA) 
      IF(NCHI.EQ.0) XSQ=X/SDATA 
      IF(NCHI.GE.2) XSQ=X/(SDATA+NCHI)
      ERSUM=ERSUM+XSQ 
  100 CONTINUE
      RETURN
      END 
C 
C***********************************************************************
      SUBROUTINE DLSIG(MPEAK,J,K,IRSET,SKPIT,KPARM) 
C            Varies and resets parameters.
C***********************************************************************
C 
      REAL KDATA,ITDATA,ISDATA,ISMFIT,ISMR 
      COMMON/BIGGY/KDATA(16384),ITDATA(16384),ISDATA(16384),
     * ISMFIT(16384),ISMR(16384),APRM(50),IA5(10),IB5(10),IPART,IPAR(50)
     *,AVARY(25,2),ACONST(25,2) 
      COMMON/CDLTA/DELTA
      COMMON/NLINS/MUL,IMUL,CHR,FTR,DEL(20),RT(20),NSKIP, 
     &   MULG(5),IMULG(5) 
      COMMON/NLINW/DELEX,DELTI,FP,MASS,NREF,NUOP,SIGR,SIGSAV(20),ISMF,
     & ISMSTRT,ISMEND
      COMMON/BRAV/CALB,CHO,IREF,IREFI 
      COMMON /COMN1/ AM1,AM2,AM3,AM4,QGS,ANG,E1,AA,BB,CC,DD,SG,E3,
     &       SAR,E4,T,SW,SI,SA,ETHR,QEX,EXCIT,AM2G(5),AM4G(5),QGSG(5) 
      DIMENSION ASAV(25,2)
      DIMENSION A(25,2) 
      EQUIVALENCE (A(1,1),APRM(1))
C 
C     IRSET=2 TO RESET VARIED PARAMETERS
C 
      SKPIT=1 
      IF(IRSET.EQ.2) GO TO 1000 
C 
      QGSG(1)=A(MPEAK+4,2)
      ASAV(J,K)=A(J,K)
      IF(J.EQ.NSKIP) GOTO 1100
C     IF PEAK POSITION
      IF(J.LE.MPEAK.AND.K.EQ.2) GO TO 220 
C     IF ZERO TIME PEAK 
      IF(J.EQ.MPEAK+5.AND.K.EQ.1) GO TO 20
C     IF Q-VALUE OR ENERGY
      IF(J.EQ.MPEAK+4) GO TO 500
C  ELSE, -- OTHER PARAMETERS. 
      TEMPA=A(J,K)+DELTA*A(J,K) 
      IF (KPARM .NE. 2) GOTO 2001 
      IF (DELTA .GE. 0) THEN
         AMAX=ACONST(J,K) + AVARY(J,K)
         IF (TEMPA .GT. AMAX) SKPIT=-1. 
      ELSE
         AMIN=ACONST(J,K) - AVARY(J,K)
         IF (TEMPA .LT. AMIN) SKPIT=-1. 
         END IF 
2001  IF (SKPIT .GT. 0) A(J,K)=TEMPA
      GO TO 1100
C     VARIATIONS IN PEAK POSITION DUE TO ZERO TIME PEAK 
20    IF(MUL.EQ.0) GO TO 1100 
C     SAVE ZERO TIME PEAK 
      ASAV(J,1)=A(J,1)
      DO 215 M=1,MUL
      PDELJ=DELTA/60. 
      IMUL=IMULG(M) 
      MULL=MULG(M)
      PDELJ=PDELJ*A(J,1)/MUL
C     MOVE ZERO TIME PEAK 
      A(MPEAK+5,1)=A(MPEAK+5,1)+PDELJ 
      DO 50 I=MULL,IMUL 
      ASAV(I,2)=A(I,2)
      A(I,2)=A(I,2) + PDELJ 
   50 CONTINUE
215   CONTINUE
      GO TO 1100
220   IF(MUL.EQ.0) GO TO 240
      DO 230 I=1,MUL
      IF(J.GE.MULG(I).AND.J.LE.IMULG(I)) GO TO 1100 
230   CONTINUE
240   PDELTA=DELTA/30.
      A(J,K)=A(J,K)+A(J,K)*PDELTA 
      GO TO 1100
C     J=MPEAK+4 FOR VARIATION OF E1 OR QGS
500   IF(MUL.EQ.0) GO TO 1100 
      A(J,K)=A(J,K)*(1.+DELTA/100.) 
C     KAW  HERE FIRST LINE CHANGED E1 TO QGSG(2)
      QGSG(2)=A(J,1) 
      QGSG(1)=A(J,2)
      ASAV(MPEAK+5,1)=A(MPEAK+5,1)
      IF(IPAR(MPEAK+5).EQ.1) GO TO 510
      QGS=QGSG(IREFI) 
      AM2=AM2G(IREFI) 
      AM4=AM4G(IREFI) 
      CALL FLGHT(IREF,TR) 
      A(MPEAK+5,1)=A(IREF,2)+FP*TR/CALB 
510   CONTINUE
      DO 540 M=1,MUL
      QGS=QGSG(M) 
      AM2=AM2G(M) 
      AM4=AM4G(M) 
      MULL=MULG(M)
      IMUL=IMULG(M) 
      DO 530 I=MULL,IMUL
      ASAV(I,2)=A(I,2)
      CALL FLGHT(I,TI)
      A(I,2)=A(MPEAK+5,1) - FP*TI/CALB
530   CONTINUE
540   CONTINUE
      GO TO 1100  
C 
C     COME HERE TO RESET PARAMETERS 
C 
1000  A(J,K)=ASAV(J,K)
      IF((J.NE.MPEAK+5.OR.K.NE.1).AND.J.NE.MPEAK+4) GO TO 1100
      IF(MUL.EQ.0) GO TO 1100 
      A(MPEAK+5,1)=ASAV(MPEAK+5,1)
      DO 1055 M=1,MUL 
      MULL=MULG(M)
      IMUL=IMULG(M) 
      DO 1050 I=MULL,IMUL 
1050  A(I,2)=ASAV(I,2)
1055  CONTINUE
1100  RETURN
      END 
C 
C***********************************************************************
      SUBROUTINE BLCSM(MPEAK)
C	, CALCULATES AREA OF FITTING REGION
C***********************************************************************
C 
      REAL KDATA,ITDATA,ISMFIT,ISMR 
      COMMON/BIGGY/KDATA(16384),ITDATA(16384),DUMMY1(16384),
     * ISMFIT(16384),ISMR(16384),APRM(50),IA5(10),IB5(10),IPART,IPAR(50)
     *,AVARY(25,2),ACONST(25,2) 
      DIMENSION A(25,2) 
      EQUIVALENCE (A(1,1),APRM(1))
      J=1 
  100 CONTINUE
      I=A(J,2)
    1 CONTINUE
      IF(ITDATA(I).LT.1.0) GO TO 2
      I=I+1 
      IF(I.GT.16384) GO TO 901 
      GO TO 1 
    2 CONTINUE
      IRHS=I
      K=A(J,2)
    3 CONTINUE
      IF(ITDATA(K).LT.1.0) GO TO 4
      K=K-1 
      IF(K.LT.1) GO TO 903
      GO TO 3 
    4 CONTINUE
      ILHS=K
      PKSUM=0.0 
      DO 200 I=ILHS,IRHS
      PKSUM=PKSUM+KDATA(I)
  200 CONTINUE
      WRITE(2,20) ILHS,IRHS,PKSUM 
   20 FORMAT(/4X,'FOR BLOCK BEG AT ',I4,' ENDING AT ',I4,' SUM OF ', 
     1 'DATA IS',F7.0)
  300 CONTINUE
      IA=A(J,2) 
C     IF(IA.LT.ILHS) GO TO 100
      IF(IA.GT.IRHS) GOTO 100 
      J=J+1 
      IF(J.GT.MPEAK) GO TO 999
      GO TO 300 
  901 CONTINUE
      WRITE(2,902) J,I
  902 FORMAT('   FOR ',I2,'TH PEAK,ERROR. I = ',I4) 
      GO TO 905 
  903 CONTINUE
      WRITE(2,904) J,K
  904 FORMAT('   FOR ',I2,'TH PEAK,ERROR. K = ',I4) 
  905 CONTINUE
      J=J+1 
      IF(J.GT.MPEAK) GO TO 999
      GO TO 100 
  999 CONTINUE
      RETURN
      END 
C************************************************************************ 
      SUBROUTINE SALY2
C	 (5,120), DATA-SMOOTHING ROUTINE 
C                 <851219.1455> 
C***********************************************************************
C 
C***********************************************************************
C     SUBROUTINE SMOTH(NCHAN), SMOOTHS DATA 
C***********************************************************************
C 
      DIMENSION IDATA(16384)
      REAL KDATA,ITDATA,ISMFIT,ISMR 
      INTEGER BEG 
	CHARACTER NFILE*30,HEADER*72
      COMMON/BIGGY/KDATA(16384),ITDATA(16384),DUMMY1(16384),
     * ISMFIT(16384),ISMR(16384),APRM(50),IA5(10),IB5(10),IPART,IPAR(50)
     *,AVARY(25,2),ACONST(25,2) 
      COMMON/SEG02/NCHAN
      COMMON/MAINX/IWHERE,IGOTO
      COMMON/OTHER/RM,RB,BEG,JEND,ISTART,IEND,MULN,DIST,RATIO,IFIT,PJON 
     &             ,ISKPY(10,2)
	COMMON/QINP/NFILE,NSPEC,HEADER,IDATA 
C 
      IF(NCHAN.EQ.0) GO TO 60 
      NCHAN=(NCHAN/2)*2+1 
      J=NCHAN/2 
      DO 20 I=1,16384
   20 KDATA(I)=0
      JBEG=BEG+J
      JJEND=JEND-J
      DO 31 I=JBEG,JJEND
      IS=0
      DO 30 K=1,NCHAN 
      L=K+I-J-1 
      IS=IS+IDATA(L)
   30 CONTINUE
      KDATA(I)=IS/NCHAN 
   31 CONTINUE
      DO 40 I=1,J 
      KDATA(I)=IDATA(I) 
   40 CONTINUE
      KEND=JJEND+1
      DO 50 I=KEND,JEND 
      KDATA(I)=IDATA(I) 
   50 CONTINUE
   60 CONTINUE
      IWHERE=1
      IGOTO=2 
      RETURN
      END 
C********************************************************************** 
      SUBROUTINE SALY3
C	 (5,120), COMPUTES AND SUBTRACTS BACKGROUND
C                 <851219.1455> 
C************************************************************************ 
C***********************************************************************
C     SUBROUTINE BACKG(ILC1,ILC2,IHC1,IHC2), SUBTRACTS LINEAR BKG.
C***********************************************************************
C 
      REAL KDATA,ITDATA,ISDATA,ISMFIT,ISMR 
      INTEGER BEG 
      COMMON/BIGGY/KDATA(16384),ITDATA(16384),ISDATA(16384),
     * ISMFIT(16384),ISMR(16384),APRM(50),IA5(10),IB5(10),IPART,IPAR(50)
     *,AVARY(25,2),ACONST(25,2) 
      COMMON/SEG03/ILC1,ILC2,IHC1,IHC2
      COMMON/MAINX/IWHERE,IGOTO
      COMMON/OTHER/RM,RB,BEG,JEND,ISTART,IEND,MULN,DIST,RATIO,IFIT,PJON 
     &             ,ISKPY(10,2) 
      COMMON/BGL/REALM,B
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
C      IF(ILC1.EQ.1) GOTO 100
      I1=(ILC2-ILC1)
      I2=(IHC2-IHC1)
      ILC=ILC1+I1/2 
      IHC=IHC1+I2/2 
      D=IHC-ILC 
      SUM1=0.0
      DO 10 I=ILC1,ILC2 
   10 SUM1=SUM1+KDATA(I)
      AVE1=SUM1/(I1+1)
      SUM2=0.0
      DO 20 I=IHC1,IHC2 
   20 SUM2=SUM2+KDATA(I)
      AVE2=SUM2/(I2+1)
      CL=ILC
      CH=IHC
      REALM=(AVE2-AVE1)/D 
      B=(AVE1*CH-AVE2*CL)/D+0.5 
      WRITE(2,35)ILC,AVE1,IHC,AVE2,REALM,B
   35 FORMAT(//,12X,'LOW CHAN','=',I4,12X,'AVE1=',F5.1,
     & /,12X,'HIGH CHAN=',I4,11X,'AVE2=',F5.1,
     & /,8X,'SLOPE=',F10.5,8X,'INTERCEPT=',F10.5,//)      
      IB=B
C     DO 30 I=BEG,JEND
      DO 30 I=1,16384
      CHAN=I
      ISDATA(I)=KDATA(I)
      KDATA(I)=KDATA(I)-REALM*CHAN-IB 
   30 CONTINUE
C      GOTO 130
C  100 CONTINUE
C      CHNL=ILC1 
C      CNTL=ILC2 
C      CHNH=IHC1 
C      CNTH=IHC2 
C      SLOPE=(CNTL-CNTH)/(CHNL-CHNH) 
C      YCEPT=0.5*(CNTL+CNTH)-0.5*SLOPE*(CHNL+CHNH) 
C      DO 110 I=1,16384 
C      CHAN=I
C      ISDATA(I)=KDATA(I)
C      BKGND=SLOPE*CHAN+YCEPT
C      KDATA(I)=KDATA(I)-BKGND 
C  110 CONTINUE
C      WRITE(2,119)
C  119 FORMAT(/,10X,'*****  USING DIRECT-BACKGROUND OPTION  *****',/)
C      WRITE(2,120) ILC1,ILC2,IHC1,IHC2,SLOPE,YCEPT
C  120 FORMAT(/,5X,'LOW CHAN=',I2,5X,'COUNTS=',I5,5X,'HIGH CHAN=',I5,
C     1 5X,'COUNTS=',I5,5X,'SLOPE=',F8.5,5X,'INTERCEPT=',F10.5,//) 
C  130 CONTINUE
      IWHERE=1
      IGOTO=3 
      RETURN
      END 
C********************************************************************** 
      SUBROUTINE SALY4
C	 (5,99 ), GRAPHICS PLOTTING SEGMENT
C             <851219.1455>
C***********************************************************************
C 
C        GRAPHICS  PLOTTING  ROUTINE
C 
C***********************************************************************
CCCC
      REAL KDATA,ITDATA
        CHARACTER*72 TITLE,HEADER
      COMMON/OTHER/RM,RB,BEG,JEND,ISTART,IEND,MULN,DIST,RATIO,IFIT,PJON 
     &             ,ISKPY(10,2) 
      COMMON/MAINX/ IWHERE,IGOTO
      COMMON/BIGGY/KDATA(16384),ITDATA(16384),DUMMY1(16384),
     * ISMFIT(16384),ISMR(16384),APRM(50),IA5(10),IB5(10),IPART,IPAR(50)
     *,AVARY(25,2),ACONST(25,2) 
      CHARACTER*20 TTERM
      COMMON/CRT_QQQ/ TTERM
      COMMON/AA/X(16384),Y(16384)
      CHARACTER*30 NFILE
      DIMENSION IDATA(16384)
      COMMON/QINP/NFILE,NSPEC,HEADER,IDATA
      COMMON /FIT2/NDATA,NL
      COMMON/SEG04/TITLE,IGRF,IGPT,IDEV,IR
      CHARACTER*1 ANK
      CALL CRT_PLOT
c      WRITE(*,1051)
c 1051  FORMAT(1X,'Do you want a PostScript plot of the fit ? (y/n)')
c      READ(*,1052) ANK
c 1052  FORMAT(A1)
c      IF(ANK.EQ.'Y'.OR.ANK.EQ.'y') THEN
c        TTERM='/CPS'
c        CALL CRT_PLOT
C        CALL LASER_PRT
c        WRITE(*,'(A)') 'PostScript plot file is pgplot.ps'
c      ENDIF
      IWHERE=10
      RETURN
      END
      
      SUBROUTINE CRT_PLOT
C=======================================================================
C  Replacement for CRT_PLOT: writes plot data files for ROOT plotting
C=======================================================================
      IMPLICIT NONE

C-----------------------------------------------------------------------
C  Declarations
C-----------------------------------------------------------------------
      INTEGER I, J, I1, NDATA
      CHARACTER*80 FILENAME

C  Arrays and data
      REAL X(16384), Y(16384)
      REAL IXDATA(10,16384)
      REAL KDATA(16384), ITDATA(16384), DUMMY1(16384)
      REAL ISMFIT(16384), ISMR(16384)
      REAL APRM(50), AVARY(25,2), ACONST(25,2)
      INTEGER IA5(10), IB5(10), IPAR(50), IPART

C  Shared fit/baseline parameters
      REAL SLOPE, YCEPT
      REAL RM, RB, DIST, RATIO, PJON
      INTEGER BEG, JEND, ISTART, IEND, MULN, IFIT, ISKPY(10,2)

C-----------------------------------------------------------------------
C  COMMON blocks matching SAN12 main program
C-----------------------------------------------------------------------
      COMMON /AA/ X, Y
      COMMON /TXQ/ IXDATA, MXPEAK
      INTEGER MXPEAK

      COMMON /BGL/ SLOPE, YCEPT
      COMMON /OTHER/ RM, RB, BEG, JEND, ISTART, IEND,
     &               MULN, DIST, RATIO, IFIT, PJON, ISKPY
      COMMON /BIGGY/ KDATA, ITDATA, DUMMY1, ISMFIT, ISMR,
     &               APRM, IA5, IB5, IPART, IPAR, AVARY, ACONST

C-----------------------------------------------------------------------
C  Clean up old plot files from previous runs
C-----------------------------------------------------------------------
      INTEGER :: ios, jdel
      LOGICAL :: exists
      CHARACTER*80 :: delname

C--- Remove old histogram.dat if present
      INQUIRE(FILE='histogram.dat', EXIST=exists)
      IF (exists) THEN
         OPEN(UNIT=91, FILE='histogram.dat', STATUS='OLD', IOSTAT=ios)
         IF (ios .EQ. 0) CLOSE(91, STATUS='DELETE')
      ENDIF

C--- Remove old total_fit.dat
      INQUIRE(FILE='total_fit.dat', EXIST=exists)
      IF (exists) THEN
         OPEN(UNIT=92, FILE='total_fit.dat', STATUS='OLD', IOSTAT=ios)
         IF (ios .EQ. 0) CLOSE(92, STATUS='DELETE')
      ENDIF

C--- Remove old background.dat
      INQUIRE(FILE='background.dat', EXIST=exists)
      IF (exists) THEN
         OPEN(UNIT=93, FILE='background.dat', STATUS='OLD', IOSTAT=ios)
         IF (ios .EQ. 0) CLOSE(93, STATUS='DELETE')
      ENDIF

C--- Remove all old peak_N.dat files (up to 100)
      DO jdel = 1, 100
         WRITE(delname, '(A,I0,A)') 'peak_', jdel, '.dat'
         INQUIRE(FILE=delname, EXIST=exists)
         IF (exists) THEN
            OPEN(UNIT=94, FILE=delname, STATUS='OLD', IOSTAT=ios)
            IF (ios .EQ. 0) CLOSE(94, STATUS='DELETE')
         ENDIF
      END DO

C-----------------------------------------------------------------------
C  Generate arrays and write output data for ROOT plotting
C-----------------------------------------------------------------------
      NDATA = IEND - ISTART + 1

C  --- Compute X and Y (data + background)
      DO I = ISTART, IEND
        I1 = I - ISTART + 1
        X(I1) = FLOAT(I)
        Y(I1) = KDATA(I) + SLOPE * X(I1) + YCEPT
      END DO

C-----------------------------------------------------------------------
C  Write histogram (experimental data)
C-----------------------------------------------------------------------
      OPEN(UNIT=11, FILE='histogram.dat', STATUS='REPLACE')
      DO I = 1, NDATA
        WRITE(11,'(F12.4,1X,F12.4)') X(I), Y(I)
      END DO
      CLOSE(11)

C-----------------------------------------------------------------------
C  Write total fit (sum of fitted peaks)
C-----------------------------------------------------------------------
      OPEN(UNIT=12, FILE='total_fit.dat', STATUS='REPLACE')
      DO I = ISTART, IEND
        I1 = I - ISTART + 1
        WRITE(12,'(F12.4,1X,F12.4)') X(I1),
     &       ITDATA(I) + SLOPE * X(I1) + YCEPT
      END DO
      CLOSE(12)

C-----------------------------------------------------------------------
C  Write individual Gaussian (or component) fits
C-----------------------------------------------------------------------
      DO J = 1, MXPEAK
        WRITE(FILENAME, '(A,I0,A)') 'peak_', J, '.dat'
        OPEN(UNIT=20+J, FILE=FILENAME, STATUS='REPLACE')
        DO I = ISTART, IEND
          I1 = I - ISTART + 1
          WRITE(20+J,'(F12.4,1X,F12.4)') X(I1),
     &         IXDATA(J,I) + SLOPE * X(I1) + YCEPT
        END DO
        CLOSE(20+J)
      END DO

C-----------------------------------------------------------------------
C  Write background line (two points)
C-----------------------------------------------------------------------
      OPEN(UNIT=99, FILE='background.dat', STATUS='REPLACE')
      WRITE(99,'(F12.4,1X,F12.4)') X(1), SLOPE*X(1) + YCEPT
      WRITE(99,'(F12.4,1X,F12.4)') X(NDATA), SLOPE*X(NDATA) + YCEPT
      CLOSE(99)

      RETURN
      END

C*********************************************************************
      SUBROUTINE SALY6
C	 (5,120), DATA-RETRIEVING SEGMENT FOR FIT
C                 <851219.1455>
C*********************************************************************
C                                                                    *
C           THIS SEGMENT READS IN THE SPECTRUM.                      *
C                                                                    *
C                                                                    *
C                                                                    *
C           IF SPECTRUM IS READ FROM NPDATA FILE,                    *
C           FILE NO. (NFILE) AND SPECTRUM NO. (NSPEC)                *
C           ARE READ IN INTERACTIVELY FROM TERMINAL.                 *
C 
C       Default reading from NP data file.  6 June 84 SEH 
C       To read from tape, input a -1 for N-file. 
C 
C*********************************************************************
CCCCC 
C 
	IMPLICIT INTEGER (A-Z)
      DIMENSION IDATA(16384)
      REAL KDATA,ITDATA,ISDATA,ISMFIT,ISMR 
	CHARACTER*30 NFIL,NFILE
	CHARACTER*72 HEADER
        CHARACTER*72 TITLE
        CHARACTER*14 CHECK
      COMMON/BIGGY/KDATA(16384),ITDATA(16384),ISDATA(16384),
     * ISMFIT(16384),ISMR(16384),APRM(50),IA5(10),IB5(10),IPART,IPAR(50)
     *,AVARY(25,2),ACONST(25,2)
      COMMON/SEG06/IRUN,IYEAR,IMONTH,IDAY,NFIL,NSPC,INTRA
     >,IFLAG,CHECK
      COMMON/MAINX/IWHERE,IGOTO
      COMMON/SEG04/TITLE,IGRF,IGPT,IDEV,IR
      COMMON/KDIF/XSQOLD,XSQNEW,DIFF,ERSUM,NCHI
	COMMON/QINP/NFILE,NSPEC,HEADER,IDATA
      	 
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
      IGRF=0
C      IF (NFIL .GT. 0) GOTO 6111
C 
C 
C  NFILE & NSPEC were read in data file!
C 
6111  CONTINUE
      IF(NCHI .GE. 0) GOTO 6120 
6120  CONTINUE
      NFILE=NFIL
      NSPEC=NSPC
6115  CONTINUE
      CALL LSI_READ
      IF(CHECK.EQ.'READ_FROM_FILE') TITLE = HEADER 
	IF (INTRA.EQ. 0) THEN
       WRITE(*,203) NFILE,NSPEC,IRUN,HEADER
	ENDIF
 203  FORMAT(//,' FILE',A30,/,'  SPEC',I4,'  RUN',I5,3X,A72)
500   CONTINUE
      WRITE(2,504)
      WRITE(2,505)
      WRITE(2,503) IRUN,HEADER 
      WRITE(2,505)
      IR=IRUN 
503   FORMAT(15X,'Run #: ',I4,'   Spectrum ID: ',A72)
504   FORMAT(//)
505   FORMAT(1X,'***********************************************', 
     &          '****************************')
 506  FORMAT(I8,/,2X,'Run # ',I4,'   I.D.: ',A72)
      IWHERE=1
      IGOTO=6 
      RETURN
 600  CONTINUE
      IWHERE=10 
      RETURN
      END 
C
C     READ LSI FORMATTED DATA FILE
C
      SUBROUTINE LSI_READ
      CHARACTER*72 HEADER
      character*60 char60
      CHARACTER*30 NFILE
      character*11 char11
	character*8 idate,iclock
	character*6 icharge,kcharge,idumit,mcharge,iamadummy
      COMMON/QINP/NFILE,NSPEC,HEADER,IDATA(16384)
	common/easy/iqcharge,itime,idate,iclock
      COMMON/AA/X(16384),Y(16384)
	data icharge/'time ='/
	data kcharge/'Time ='/
	data idumit/'****Da'/

	write(*,*)'enter lsi_read'
2109  DO 7100 I = 1,16384
        Y(I) = 0.0
7100  CONTINUE
      NSIZE = 0
      write(*,*) 'opening ',NFILE
      OPEN(UNIT=4,FILE=NFILE,STATUS='OLD')
      DO I = 1,3
        READ(4,'(A1)') IDUM
      END DO
10    READ(4,'(18X,I2,A80)') NHIS1,TITLE
      READ(4,'(7X,I5)') NSIZE
	WRITE(*,*) ' ',NSIZE, NHIS1,TITLE
      NSKIP = NSIZE/10 + 4
      IF(NHIS1.NE.NSPEC) THEN
        DO I = 1,NSKIP
          READ(4,'(A1)') IDUM
        END DO
        GO TO 10
      ENDIF
      READ(4,'(10F10.0)') (Y(I),I=1,NSIZE)
      write(*,*) 'read ',NSIZE,' channels'
      DO I = 1,NSIZE
        IDATA(I) = Y(I)
      END DO
c	write(*,*)'ready to close(4)'
c      CLOSE(UNIT=4)
c      DO I = 1,16384
c        write(17,*) IDATA(I)
c      END DO
c  write out charge, time, date information - sfh
c	do I=1,30000
c	  write(*,*) 'in loop'
c	  read(4,30,end=50)mCHARGE
c	  write(*,30)mCHARGE
c	  if (mCHARGE .eq. ICHARGE .or. mCHARGE .eq. kcharge)then
c	     write(*,*)'got to here'
c	     backspace(4)
c           read(4,42)itime
c	     write(*,42)itime
c	     read(4,40)iqcharge
c	     write(*,40)iqcharge
c
c	     do jk=1,10
c	      read(4,30)iamadummy
c	      write(*,30)iamadummy
c	      if(iamadummy .eq. idumit)then
c	          read(4,43)idate,iclock
c	          write(*,43)idate,iclock
c	         goto 27
c	      endif
c	      enddo
c 27         continue
c	  endif
c	enddo
      do i=1,30000
        read(4,'(a60)',end=27) char60
        char11=char60(1:11)
        if(char11.eq.'**** Scaler') then
          do j=1,20
            read(4,'(a60)',end=27) char60
            char11=char60(1:11)
            if(char11.eq.'**** Histog') goto 27
            write(*,'(a60)') char60
          enddo
        endif
      enddo
   27 continue
c
	close(unit=4)
 30   format(1a6)
 40   format(8x,1I10)
 42   format(6x,1I10)
 43   format(1a8,1x,1a8)
 50   continue	  
      RETURN
      END

C
C     SUBROUTINE TO SET SCALE FACTOR DECIMAL DATA
C     INPUT :
C              NX IS THE NUMBER OF (X,Y) PAIRS
C              N*10**M
C              N1*10**M1
C     IF N = 9 AUTO SCALE
C     OUTPUT:
C              SCLMAX
C              SCLMIN
C              NCYCLE FOR SEMI-LOG PLOT
C
      SUBROUTINE REL_SCLDTA(Y,NX,M,N,M1,N1,SCLMAX,SCLMIN,NCYCLE)
      DIMENSION Y(16384)
      IEX = N
      IEX1 = N1
      IF(IEX.EQ.9)THEN
        CALL REL_AUTOSC(Y,NX,M,N,M1,N1,SCLMAX,SCLMIN,NCYCLE)
        RETURN
      ENDIF
      ISC = M
      IF(ISC.EQ.0)THEN
        ISC=1
      ELSE
        SCLMAX = FLOAT(ISC*10**IEX)
      ENDIF
      ISC1 = M1
      IF(ISC1.EQ.0)THEN
        ISC1=1
      ELSE
        SCLMIN = FLOAT(ISC1*10**IEX1)
      ENDIF
      NCYCLE = M-M1+1
      RETURN
      END
C
C     SUBROUTINE TO AUTOSCALE DATA
C
      SUBROUTINE REL_AUTOSC(Y,NX,M,N,M1,N1,SCLMAX,SCLMIN,NCYCLE)
      DIMENSION Y(16384)
      CALL AMAX(Y,NX,YMAX)
      IF(YMAX.EQ.0) THEN
        M = 0
      ELSE
        M = ALOG10(ABS(YMAX)) 
      ENDIF
      IF (ABS(YMAX).LE.1) THEN
        M = M - 1
        N = IFIX(YMAX*10.0**(-M)+0.99999)
        SCLMAX = FLOAT(N)/10.0**(-M)
      ELSE
        N = IFIX(YMAX/10.0**(M)+0.99999)
        SCLMAX = FLOAT(N)*10.0**(M)
      ENDIF
      CALL AMIN(Y,NX,YMIN)
      IF(YMIN.EQ.0.0) THEN 
        M1 = 0
      ELSE
        M1 = ALOG10(ABS(YMIN))
      ENDIF
      IF (ABS(YMIN).LE.1) THEN
        M1 = M1 - 1
        N1 = IFIX(YMIN*10.0**(-M1)-0.99999)
        SCLMIN = FLOAT(N1)/10.0**(-M1)
      ELSE
        N1 = IFIX(YMIN/10.0**M1-0.99999)
        SCLMIN = FLOAT(N1)*10.0**(M1)
      ENDIF
      NCYCLE = M-M1+1
      RETURN
      END
C
C     FUNACTION TO FIND THE MINIMUM IN ARRAY X
C        
      SUBROUTINE AMIN(X,NX,XMIN)
      DIMENSION X(16384)
      XMIN = 1.0E+15
      DO 10 I = 1,NX
      IF(X(I).LT.XMIN)XMIN=X(I)
10    CONTINUE
      RETURN
      END
C
C     FUNACTION TO FIND THE MAXIMUM IN ARRAY X
C        
      SUBROUTINE AMAX(X,NX,XMAX)
      DIMENSION X(16384)
      XMAX = -1.0E+15
      DO 10 I = 1,NX
      IF(X(I).GT.XMAX)XMAX=X(I)
10    CONTINUE
      RETURN
      END
