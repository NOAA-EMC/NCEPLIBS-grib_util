!> @file
!> @brief Create new GRIB2 file with fields from an existing GRIB2
!> file, with a TOC Flag Field separator block and WMO header.
!> @author Stephen Gilbert @date 2004-05-17

!> Program reads selected GRIB2 fields from a file, adds a TOC Flag
!> Field separator block and WMO Header in front of each GRIB2 field,
!> and writes them out to a new file. The output file is in the format
!> required for TOC's FTP Input Service, which can be used to
!> disseminate the GRIB2 bulletins. This service is described at
!> http://weather.gov/tg/ftpingest.html.
!>
!> @note The "EXTRACT" variable in the namelist allows users to choose
!> whether they want the entire GRIB2 message containing the requested
!> field (extract=.false.), OR a GRIB2 message containing only the
!> requested field (extract=.true.). Both options return the same
!> message if the requested field is the only field in the GRIB2
!> message.
!>
!> ## Input Files
!> - 5 namelist of grib fields and associated wmo headers.
!> - 11 input grib2 file.
!> - 31 corresponding input grib2 index file.
!>
!> ## Output Files (Including Scratch Files)
!> - 6 standard fortran print file
!> - 51 output grib bulletin file in toc format
!>
!> @return
!> -  0 - Successful run
!> - 10 - Error opening input GRIB2 data file
!> - 20 - Error opening output GRIB transmission file
!> - 19 - Error reading control card file - all bulletins missing
!> - 30 - Some bulletins are missing
!>
!> @author Stephen Gilbert @date 2004-05-17
!> @author Alex Richert, Edward Hartnett
PROGRAM tocgrib2
  use grib_mod
  use pdstemplates
  use gridtemplates
  integer, dimension(200) :: IDS, GDT, PDT
  integer   ::    DSCPL, GDTN, PDTN
  integer   ::    nbul, nrec, mbul, dayofmonth, hourofday
  integer, parameter :: lenhead=21, jrew=0

  CHARACTER * 6   BULHED
  CHARACTER * 80  DESC, WMOHEAD
  CHARACTER * 200  fileb, filei, fileo
  CHARACTER * 6   envvar
  CHARACTER * 4   KWBX
  CHARACTER * 1   CSEP(80)
  CHARACTER * 1   WMOHDR(lenhead)
  character(len=1), pointer, dimension(:) :: gribm

  logical :: extract=.false.
  integer idxver
  integer (kind = 8) :: itot8

  interface
     subroutine getgb2p2(lugb, lugi, j, jdisc, jids, jpdtn, jpdt, jgdtn, jgdt, &
          extract, idxver, k, gribm, leng8, iret)
       integer, intent(in) :: lugb, lugi, j, jdisc
       integer, dimension(:) :: jids(*)
       integer, intent(in) :: jpdtn
       integer, dimension(:) :: jpdt(*)
       integer, intent(in) :: jgdtn
       integer, dimension(:) :: jgdt(*)
       logical, intent(in) :: extract
       integer, intent(inout) :: idxver
       integer, intent(out) :: k
       character(len = 1), pointer, dimension(:) :: gribm
       integer (kind = 8), intent(out) :: leng8
       integer, intent(out) :: iret
     end subroutine getgb2p2
  end interface
  NAMELIST /GRIBIDS/DSCPL,IDS,GDTN,GDT,PDTN,PDT,DESC,WMOHEAD,EXTRACT

  CALL W3TAGB('tocgrib2', 2012, 0916, 0083, 'NP11')

  lugb=11      ! Input GRIB2 File
  lugi=31      ! Input GRIB2 INdex File
  lugo=51      ! Output transmission file.

  !        Read GRIB2 data and index file names from the FORT_nn
  !        environment variables, and open the files.
  envvar='FORT  '
  write(envvar(5:6), fmt='(I2)') lugb
  call getenv(envvar, fileb)
  write(envvar(5:6), fmt='(I2)') lugi
  call getenv(envvar, filei)

  call baopenr(lugb, fileb, iret1)
  if (iret1  .ne. 0) then
     write(6, fmt='(" Error opening GRIB file: ", A200)') fileb
     write(6, fmt='(" baopenr error = ", I5)') iret1
     stop 10
  endif

  !         Open GRIB2 index file.  If doesn't open, use just the data
  !         file.
  call baopenr(lugi, filei, iret2)
  if (iret2  .ne. 0) then
     lugi=0
  endif

  !        Read output GRIB bulletin file name from  FORTnn
  !        environment variable, and open file.
  write(envvar(5:6), fmt='(I2)') lugo
  call getenv(envvar, fileo)
  call baopenw(lugo, fileo, iret1)
  if (iret1  .ne. 0) then
     write(6, fmt='(" Error opening output transmission file: ", &
          A200)') fileo
     write(6, fmt='(" baopenw error = ", I5)') iret1
     stop 20
  endif

  !        loop through input control records.
  iret=0
  nbul=0
  nrec = 0
  foreachinputrecord: do

     !  Set Namelist defaults
     DSCPL=-1     ! Grib2 Discipline number
     IDS=-9999    ! GRIB2 Identification Section
     GDTN=-1      ! Grid Definition Template Number
     GDT=-9999    ! Grid Definition Template
     PDTN=-1      ! Product Definition Template Number
     PDT=-9999    ! Product Definition Template
     WMOHEAD='TTAAnn CCCC'
     EXTRACT=.false.

     READ (*, GRIBIDS, iostat=ios, end=999)
     nrec = nrec + 1
     if (ios .ne. 0) then
        write(6, fmt='(" Error reading PDS from input file. iostat = " &
             , i5)') ios
        cycle
     endif

     !  Echo input record
     WRITE(6, FMT='(/, ''***********************************'', &
          ''********************************************'')')
     write(6, '(A, I0)') ' Start new record no. =  ', nrec
     write(6, '(73A)') ' DESC=', DESC(1:73)
     write(6, '(11A)') ' WMOHEAD=', WMOHEAD(1:11)
     write(6, '(A, I0)') ' GRIB2 DISCIPLINE= ', DSCPL
     write(6, '(A, 20(1x, I0))')' Section 1=', &
          (IDS(j2), j2=1, 13)
     if (GDTN .ne. -1) then
        write(6, '(A, I0, A, 100(1x, I0))') ' GDT 3. ', GDTN, ' =', &
             (GDT(j2), j2=1, getgdtlen(GDTN))
     endif
     if (PDTN .ne. -1) then
        write(6, '(A, I0, A, 100(1x, I0))') ' PDT 4. ', PDTN, ' =', &
             (PDT(j2), j2=1, getpdtlen(PDTN))
     endif

     !        Read and return packed GRIB field
     idxver = 2
     CALL GETGB2P2(lugb, lugi, jrew, DSCPL, IDS, PDTN, PDT, &
          GDTN, GDT, extract, idxver, KREW, gribm, itot8, iret)
     itot = int(itot8, kind(4))
     IF (IRET.NE.0) THEN
        IF (IRET.EQ.96)WRITE(6, '(A)')' GETGB2P: ERROR READING INDEX' &
             //' FILE'
        IF (IRET.EQ.97)WRITE(6, '(A)')' GETGB2P: ERROR READING GRIB' &
             //' FILE'
        IF (IRET.EQ.99)WRITE(6, '(A)')' GETGB2P: ERROR REQUEST NOT' &
             //' FOUND'
        cycle
     END IF
     WRITE (6, '(A, 1x, I0)')' RECORD NO. OF GRIB RECORD IN INPUT ' &
          //'FILE = ', KREW
     !
     WRITE (6, '(A, I0)')' Size of GRIB Field = ', itot

     !        MAKE Flag Field Separator block
     iopt=2
     insize=19
     call mkfldsep(csep, iopt, insize, itot+lenhead, lenout)
     !         WRITE(6, '(A, 80A)')'  csep = ', csep

     !        MAKE WMO HEADER
     dayofmonth=mova2i(gribm(16+16))
     hourofday=mova2i(gribm(16+17))
     CALL MAKWMO (WMOHEAD(1:6), dayofmonth, hourofday, &
          WMOHEAD(8:11), WMOHDR)
     !         WRITE(6, '(21A)') '  WMOHEADER= ', WMOHDR

     !        write out Separator block, Abbreviated WMO Heading, 
     !        and GRIB2 field to output file.
     call wryte(lugo, lenout, csep)
     call wryte(lugo, lenhead, WMOHDR)
     call wryte(lugo, itot, gribm)
     nbul=nbul+1
     if (associated(gribm)) then
        deallocate(gribm)
        nullify(gribm)
     endif
     !
  enddo foreachinputrecord

  ! CLOSING SECTION
999 if (nbul .EQ. 0) then
     WRITE (6, FMT='('' SOMETHING WRONG WITH DATA CARDS...'', &
          ''NOTHING WAS PROCESSED'')')
     !        CALL W3TAGE('tocgrib2')
     stop 19
  else
     call baclose (LUGB, iret)
     call baclose (LUGI, iret)
     call baclose (LUGO, iret)
     WRITE (6, FMT='(//, '' ******** RECAP OF THIS EXECUTION '', &
          ''********'', /, 5X, ''READ  '', I6, '' INDIVIDUAL IDS'', &
          /, 5X, ''WROTE '', I6, '' BULLETINS OUT FOR TRANSMISSION'', &
          //)') nrec, NBUL
  endif
  ! TEST TO SEE IF ANY BULLETINS MISSING
  mbul = nrec - nbul
  IF (mbul .ne. 0) THEN
     WRITE(6, '(A, 1X, I0)')' BULLETINS MISSING = ', mbul
     !        CALL W3TAGE('tocgrib2')
     stop 30
  END IF

  !      CALL W3TAGE('tocgrib2')
  STOP
END PROGRAM tocgrib2
