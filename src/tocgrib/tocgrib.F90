!> @file
!> @brief Create new GRIB2 file from exiting GRIB2 file.
!> @author Gilbert @date 2003-03-28

!> Create new GRIB2 file from exiting GRIB2 file.
!>
!> This program reads selected GRIB2 fields from a file, adds a TOC
!> Flag Field separator block and WMO Header in front of each GRIB2
!> field, and writes them out to a new file. The output file is in the
!> format required for TOC's FTP Input Service, which can be used to
!> disseminate the GRIB bulletins. This service is described at
!> http://weather.gov/tg/ftpingest.html.
!>
!> ### Input Files
!>  - 5 list of grib fields and associated wmo headers.
!>  - 11 input grib file.
!>  - 31 corresponding input grib index file.
!>  - parm pass in 4 characters 'kwbx' with parm field
!>
!> ### Output Files (Including Scratch Files)
!>  - 6 standard fortran print file
!>  - 51 output grib bulletin file in toc format
!>
!> @return
!> -  0 Successful run
!> - 10 Error opening input GRIB data file
!> - 20 Error opening output GRIB transmission file
!> - 19 Error reading control card file - all bulletins missing
!> - 30 Some BULLETINS ARE MISSING
!>
!> @author Gilbert @date 2003-03-28
PROGRAM tocgrib
  PARAMETER (MXSIZ3=1000000)

  INTEGER DUM
  INTEGER JGDS(200)
  INTEGER MPDS(200)
  INTEGER KGDS(200)
  INTEGER KPDS(200)
  INTEGER MAPNUM
  INTEGER NBITS
  INTEGER NBUL
  INTEGER NPARM
  INTEGER PUNUM
  INTEGER,dimension(28) :: HEXPDS

  CHARACTER * 6 BULHED
  CHARACTER * 100 CPARM
  CHARACTER * 20 DESC
  CHARACTER * 3 EOML
  CHARACTER * 1 GRIB(MXSIZ3)
  CHARACTER * 200 fileb,filei,fileo
  CHARACTER * 6 envvar
  CHARACTER * 4 KWBX
  CHARACTER * 1 PDS(28)
  CHARACTER * 1 PDSL(28)
  CHARACTER * 1 CSEP(80)
  CHARACTER * 132 TITLE
  integer,parameter :: lenhead=21
  CHARACTER * 1 WMOHDR(lenhead)

  LOGICAL IW3PDS

  HEXPDS = 0
  LUGB = 11
  LUGI = 31
  LUGO = 51

  ! Get parm field with up to 100 characters. Parm field should
  ! contain the originating center part of the WMO Header.
  CPARM = '    '
  KWBX  = 'KWBC'
  CALL W3AS00(NPARM, CPARM, IER)
  IF (IER .EQ. 0) THEN
     IF (NPARM .EQ. 0 .OR. CPARM(1:4) .EQ. '    ') THEN
        PRINT *,'THERE IS A PARM FIELD BUT IT IS EMPTY'
        PRINT *,'OR BLANK, I WILL USE THE DEFAULT KWBC'
     ELSE
        KWBX(1:4) = CPARM(1:4)
     END IF
  ELSE IF (IER .EQ. 2 .OR. IER .EQ. 3) THEN
     PRINT *,'W3AS00 ERROR = ', IER
     PRINT *,'THERE IS NO PARM FIELD, I USED DEFAULT KWBC'
  ELSE
     PRINT *,'W3AS00 ERROR = ', IER
  END IF
  PRINT *,'NPARM = ',NPARM
  PRINT *,'CPARM = ',CPARM(1:4)
  PRINT *,'KWBX  = ',KWBX(1:4)

  ! Read GRIB data and index file names from the FORTnn
  ! environment variables, and open the files.
  envvar='FORT  '
  write(envvar(5:6),fmt='(I2)') lugb
  call getenv(envvar,fileb)
  write(envvar(5:6),fmt='(I2)') lugi
  call getenv(envvar,filei)

  call baopenr(lugb,fileb,iret1)
  if ( iret1  .ne. 0 ) then
     write(6,fmt='(" Error opening GRIB file: ", A200)') fileb
     write(6,fmt='(" baopenr error = ",I5)') iret1
     stop 10
  endif

  ! Open GRIB index file. If doesn't open, use just the data
  ! file.
  call baopenr(lugi,filei,iret2)
  if ( iret2  .ne. 0 ) then
     lugi=0
  endif

  ! Read output GRIB bulletin file name from FORTnn
  ! environment variable, and open file.
  write(envvar(5:6),fmt='(I2)') lugo
  call getenv(envvar,fileo)
  call baopenw(lugo,fileo,iret1)
  if ( iret1  .ne. 0 ) then
     write(6,fmt='(" Error opening GRIB file: ",A200)') fileo
     write(6,fmt='(" baopenw error = ",I5)') iret1
     stop 20
  endif

  IRET = 0
  iopt=2
  insize=19
  NBUL = 0

  ! loop through input control records.
  nrec = 0
  foreachelement: do

     READ (*,66,iostat=ios) (HEXPDS(J),J=1,12), &
          (HEXPDS(J),J=17,20), PUNUM, DESC
66   FORMAT(3(2X,4Z2.2),3X,4Z2.2,6X,I3,1X,A20)
     if ( ios .ne. 0 ) then
        write(6,fmt='(" Error reading PDS from input file. iostat = ", i5)') ios
        exit
     endif
     PDS=char(HEXPDS)

     ! exit loop, if no more bulletins in input cards
     IF ( mova2i(pds(1)) .EQ. 255) exit

     nrec = nrec + 1
     WRITE(6,FMT='(/,''***********************************'', ''********************************************'')')
     print *,'Start new record no. = ',nrec
     WRITE (6,FMT='('' INPUT PDS, PUNUM'', '' & DESC...DESIRED GRIB MAPS LISTED ON FOLLOWING '', ' // &
          ' ''LINES...'',/,4X,3(2X,4z2.2),3X,4z2.2,6X,I3,1X, A20)') (HEXPDS(J),J=1,12), &
          (HEXPDS(J),J=17,20), PUNUM, DESC

     ! Read WNO Header associated with this element
     READ (*,iostat=ios,FMT='(4X,I3,2X,I2,2X,A6,1X,I3,24X,A3)') &
          MAPNUM,NBITS, BULHED, DUM, EOML
     WRITE (6,FMT='(4X,I3,2X,I2,2X,A6,1X,I3,24X,A3)') &
          MAPNUM,NBITS, BULHED, DUM, EOML
     if ( ios .ne. 0 ) then
        write(6,fmt='(" Error reading PDS from input file. iostat =", i6)') ios
     endif

     ! Set up 25 word PDS array of GRIB field to read
     JREW = 0
     JGDS = -1
     MPDS = -1
     MPDS(3) = mova2i(PDS(7))
     MPDS(5) = mova2i(PDS(9))
     MPDS(6) = mova2i(PDS(10))
     MPDS(7) = mova2i(PDS(11)) * 256 + mova2i(PDS(12))
     MPDS(14) = mova2i(PDS(19))
     MPDS(15) = mova2i(PDS(20))

     ! Read and return packed GRIB field
     CALL GETGBP(LUGB,LUGI,MXSIZ3,JREW,MPDS,JGDS, &
          itot,KREW,KPDS,KGDS,GRIB,IRET)
     IF (IRET.NE.0) THEN
        IF (IRET.LT.96) PRINT *,'GETGB-W3FI63: ERROR = ',IRET
        IF (IRET.EQ.96) PRINT *,'GETGB: ERROR READING INDEX FILE'
        IF (IRET.EQ.97) PRINT *,'GETGB: ERROR READING GRIB FILE'
        IF (IRET.EQ.98) THEN
           PRINT *,'GETGB ERROR: NUM. OF DATA POINTS GREATER THAN JF'
        END IF
        IF (IRET.EQ.99) PRINT *,'GETGB ERROR: REQUEST NOT FOUND'
        IF (IRET.GT.99) PRINT *,'GETGB ERROR = ',IRET
        cycle
     END IF
     PRINT *,'RECORD NO. OF GRIB RECORD IN INPUT FILE = ',KREW

     ! COMPARE RECORD (GRIB) TO CONTROL CARD (PDS), THEY SHOULD MATCH
     PDSL(1:28)=GRIB(9:36)
     KEY = 2
     IF (.NOT.IW3PDS(PDSL,PDS,KEY)) THEN
        PRINT 2900, nrec,(mova2i(PDSL(j)),j=1,28), &
             (mova2i(PDS(j)),j=1,28)
2900    FORMAT ( 1X,I4,' (PDS) IN RECORD DOES NOT MATCH (PDS) IN ', 'CONTROL CARD ',/,7(1X,4Z2.2), /,7(1X,4Z2.2))
        cycle
     END IF

     ! Print PDS
     PRINT 2, (mova2i(PDSL(J)),J=1,28)
2    FORMAT (' PDS = ',7(4Z2.2,1X))

     ! Construct and print Description of GRIB field
     CALL W3FP11 (GRIB,PDSL,TITLE,IER)
     IF (IER.NE.0) PRINT *,'W3FP11 ERROR = ',IER
     PRINT *,TITLE(1:86)

     PRINT *,' Size of GRIB Field = ',ITOT

     ! MAKE Flag Field Separator block
     call mkfldsep(csep,iopt,insize,itot+lenhead,lenout)

     ! MAKE WMO HEADER
     ! Get system date and time
     CALL MAKWMO (BULHED,KPDS(10),KPDS(11),KWBX,WMOHDR)

     ! write out Separator block, Abbreviated WMO Heading,
     ! and GRIB field to output file.
     call wryte(lugo,lenout,csep)
     call wryte(lugo,lenhead,WMOHDR)
     call wryte(lugo,itot,grib)
     nbul=nbul+1
  enddo foreachelement

  ! CLOSING SECTION
  IF (NBUL .EQ. 0 ) THEN
     WRITE (6,FMT='('' SOMETHING WRONG WITH DATA CARDS...'', ''NOTHING WAS PROCESSED'')')
     stop 19
  ELSE
     CALL BACLOSE (LUGB,iret)
     CALL BACLOSE (LUGI,iret)
     CALL BACLOSE (LUGO,iret)
     WRITE (6,FMT='(//,'' ******** RECAP OF THIS EXECUTION '', ''********'',/,5X,''READ  '',I6,'' INDIVIDUAL IDS'', ' // &
          ' /,5X,''WROTE '',I6,'' BULLETINS OUT FOR TRANSMISSION'', //)') nrec, NBUL
  ENDIF

  ! TEST TO SEE IF ANY BULLETINS MISSING
  MBUL = nrec - NBUL
  IF (MBUL.NE.0) THEN
     PRINT *,'BULLETINS MISSING = ',MBUL
     stop 30
  END IF

  STOP
END PROGRAM tocgrib
