!> @file
!> @brief Read fields from a GRIB2 file and write them to a new file
!> with WMO super header.
!> @author Gilbert @date 2004-05-17

!> This program reads selected GRIB2 fields from a file, adds a flag Field
!> separator block and the size in bytes of the grib file, and the WMO
!> super header and the time stamp, adds a TOC Flag Field separator
!> block and WMO Header in front of each GRIB2 field, and writes them
!> out to a new file. The output file is in the format required for
!> TOC's FTP Input Service, which can be used to disseminate the GRIB2
!> bulletins. This service is described at
!> http://weather.gov/tg/ftpingest.html.
!>
!> @note The "EXTRACT" variable in the namelist allows users to choose
!> whether they want the entire GRIB2 message containing the requested
!> field (extract=.false.), OR a GRIB2 message containing only the
!> requested field (extract=.true.). Both options return the same
!> message if the requested field is the only field in the GRIB2
!> message.
!>
!> ### Input Files
!> - 5 Namelist of grib fields and associated wmo headers.
!> - 11 Input grib2 file.
!> - 12 Get the file size of the grib file
!> - 31 Corresponding input grib2 index file.
!>
!> ### Output Files  (Including Scratch Files)
!> - 6 standard fortran print file
!> - 51 output grib bulletin file in toc format
!>
!> @return
!> -  0 - Successful run
!> - 10 - Error opening input GRIB2 data file
!> - 20 - Error opening output GRIB transmission file
!> - 19 - error reading control card file - all bulletins missing
!> - 30 - some bulletins are missing
!>
!> @author Gilbert @date 2004-05-17
PROGRAM tocgrib2super
  use grib_mod
  use pdstemplates
  use gridtemplates
  !
  integer,dimension(200) :: IDS,GDT,PDT
  integer   ::    DSCPL,GDTN,PDTN
  integer   ::    nbul,nrec,mbul,dayofmonth,hourofday,minofhour
  integer,parameter :: lenhead=21,jrew=0
  integer    itime(8)
  integer    fsize
  !
  CHARACTER * 16  SUPERWMO
  CHARACTER * 80  DESC,WMOHEAD
  CHARACTER * 200 fileb,filei,fileo,filea
  CHARACTER * 6   envvar
  CHARACTER * 4   KWBX
  CHARACTER * 1   CSEP(80)
  CHARACTER * 1   WMOHDR(lenhead)
  character(len=1),pointer,dimension(:) :: gribm

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
  !
  CALL W3TAGB('tocgrib2super',2010,0900,0246,'NP11')
  lugb=11      ! Input GRIB2 File
  luga=12      ! Input file size
  lugi=31      ! Input GRIB2 INdex File
  lugo=51      ! Output transmission file.
  SUPERWMO = 'SUPER WMO HEADER'
  insize = 19  ! The size of the flag field separator
  !
  !        Read GRIB2 data and index file names from the FORTnn
  !        environment variables, and open the files.
  !
  envvar='FORT  '
  write(envvar(5:6),fmt='(I2)') lugb
  call getenv(envvar,fileb)
  write(envvar(5:6),fmt='(I2)') luga
  call getenv(envvar,filea)
  write(envvar(5:6),fmt='(I2)') lugi
  call getenv(envvar,filei)
  !
  !     Get the file size of the grib file
  !
  !     call baopenr(luga,filea,iret1)
  open(unit=luga,file='filesize',form='formatted',status='old', &
       iostat=iret1)
  if (iret1  .ne. 0) then
     write(6,fmt='(" Error opening GRIB file: ",A200)') filea
     write(6,fmt='(" baopenr error = ",I5)') iret1
     stop 10
  endif
  !
  READ(luga,'(I15)') FSIZE
  print *,' filesize ', fsize
  !
  call baopenr(lugb,fileb,iret1)
  if (iret1  .ne. 0) then
     write(6,fmt='(" Error opening GRIB file: ",A200)')fileb
     write(6,fmt='(" baopenr error = ",I5)') iret1
     stop 10
  endif
  !
  !         Open GRIB2 index file. If doesn't open, use just the data
  !         file.
  !
  call baopenr(lugi,filei,iret2)
  if (iret2  .ne. 0) then
     lugi=0
  endif
  !
  !        Read output GRIB bulletin file name from FORT
  !        environment variable, and open file.
  !
  envvar='FORT  '
  write(envvar(5:6),fmt='(I2)') lugo
  call getenv(envvar,fileo)
  call baopenw(lugo,fileo,iret1)
  if (iret1  .ne. 0) then
     write(6,fmt='(" Error opening output transmission file: ", &
          A200)') fileo
     write(6,fmt='(" baopenw error = ",I5)') iret1
     stop 20
  endif
  !
  !     NDFD grib file begins with a flag flied separator and size bytes of the file
  !     ****nnnnnnnnn****\lf  where "nnnnnnnn" is the size of the file(minus the separator)
  !     The size of the flag field separator is 19 bytes.
  !
  if (fsize >= insize) then
     fsize = fsize - insize
  else
     fsize=insize
  ENDIF
  !
  !        loop through input control records.
  !
  iret=0
  nbul=0
  nrec = 0
  foreachinputrecord: do

     !
     !  Set Namelist defaults
     !
     DSCPL=-1     ! Grib2 Discipline number
     IDS=-9999    ! GRIB2 Identification Section
     GDTN=-1      ! Grid Definition Template Number
     GDT=-9999    ! Grid Definition Template
     PDTN=-1      ! Product Definition Template Number
     PDT=-9999    ! Product Definition Template
     WMOHEAD='TTAAnn CCCC'
     EXTRACT=.false.
     iopt=2

     READ (*,GRIBIDS,iostat=ios,end=999)
     !
     if (DESC(1:16) .EQ. SUPERWMO(1:16)) then
        !
        !        MAKE Flag Field Separator block
        !
        call mkfldsep(csep,iopt,insize,fsize,lenout)
        !
        !        MAKE SUPER WMO HEADER
        ! &
        !        GET COMPUTER DATE-TIME  SAVE FOR DATA DATE VERIFICATION
        CALL W3UTCDAT(ITIME)
        dayofmonth=ITIME(3)
        hourofday=ITIME(5)
        minofhour=ITIME(6)
        CALL MAKWMO (WMOHEAD(1:6),dayofmonth,hourofday,minofhour, &
             WMOHEAD(8:11),WMOHDR)
        !
        !        write out Separator block, Abbreviated WMO Heading,
        !        and GRIB2 field to output file.
        !
        call wryte(lugo,lenout,csep)
        call wryte(lugo,lenhead,WMOHDR)
        !
        write(6,'(16A)') ' DESC=',DESC(1:16)
        write(6,'(11A)') ' WMOHEAD=',WMOHEAD(1:11)
        !
        cycle
     endif

     nrec = nrec + 1
     if (ios .ne. 0) then
        write(6,fmt='(" Error reading PDS from input file. iostat = " &
             ,i5)') ios
        cycle
     endif
     !
     !  Echo input record
     !
     WRITE(6,FMT='(/,''***********************************'', &
          ''********************************************'')')
     write(6,'(A,I0)') ' Start new record no. =  ',nrec
     write(6,'(73A)') ' DESC=',DESC(1:73)
     write(6,'(11A)') ' WMOHEAD=',WMOHEAD(1:11)
     write(6,'(A,I0)') ' GRIB2 DISCIPLINE= ',DSCPL
     write(6,'(A,20(1x,I0))')' Section 1=', &
          (IDS(j2),j2=1,13)
     if (GDTN .ne. -1) then
        write(6,'(A,I0,A,100(1x,I0))') ' GDT 3. ',GDTN,' =', &
             (GDT(j2),j2=1,getgdtlen(GDTN))
     endif
     if (PDTN .ne. -1) then
        write(6,'(A,I0,A,100(1x,I0))') ' PDT 4. ',PDTN,' =', &
             (PDT(j2),j2=1,getpdtlen(PDTN))
     endif
     !
     !        Read and return packed GRIB field
     !
     idxver = 2
     CALL GETGB2P2(lugb,lugi,jrew,DSCPL,IDS,PDTN,PDT, &
          GDTN,GDT,extract,idxver,KREW,gribm,itot8,iret)
     itot = int(itot8, kind(4))
     IF (IRET.NE.0) THEN
        IF (IRET.EQ.96)WRITE(6,'(A)')' GETGB2P: ERROR READING INDEX' &
             //' FILE'
        IF (IRET.EQ.97)WRITE(6,'(A)')' GETGB2P: ERROR READING GRIB' &
             //' FILE'
        IF (IRET.EQ.99)WRITE(6,'(A)')' GETGB2P: ERROR REQUEST NOT' &
             //' FOUND'
        cycle
     END IF
     WRITE (6,'(A,1x,I0)')' RECORD NO. OF GRIB RECORD IN INPUT ' &
          //'FILE = ', KREW
     !
     WRITE (6,'(A,I0)')' Size of GRIB Field = ',itot
     !
     !        MAKE Flag Field Separator block
     !
     call mkfldsep(csep,iopt,insize,itot+lenhead,lenout)
     !         WRITE(6,'(A,80A)')'  csep = ',csep
     !
     !        MAKE WMO HEADER
     !
     CALL MAKWMO (WMOHEAD(1:6),dayofmonth,hourofday,minofhour, &
          WMOHEAD(8:11),WMOHDR)
     !         WRITE(6,'(21A)') '  WMOHEADER= ',WMOHDR
     !
     !        write out Separator block, Abbreviated WMO Heading,
     !        and GRIB2 field to output file.
     !
     call wryte(lugo,lenout,csep)
     call wryte(lugo,lenhead,WMOHDR)
     call wryte(lugo,itot,gribm)
     nbul=nbul+1
     if (associated(gribm)) then
        deallocate(gribm)
        nullify(gribm)
     endif
     !
  enddo foreachinputrecord
  !
  !     CLOSING SECTION
  !
999 if (nbul .EQ. 0) then
     WRITE (6,FMT='('' SOMETHING WRONG WITH DATA CARDS...'', &
          ''NOTHING WAS PROCESSED'')')
     !        CALL W3TAGE('tocgrib2super')
     stop 19
  else
     call baclose (LUGB,iret)
     call baclose (LUGI,iret)
     call baclose (LUGO,iret)
     WRITE (6,FMT='(//,'' ******** RECAP OF THIS EXECUTION '', &
          ''********'',/,5X,''READ  '',I6,'' INDIVIDUAL IDS'', &
          /,5X,''WROTE '',I6,'' BULLETINS OUT FOR TRANSMISSION'', &
          //)') nrec, NBUL
  endif
  !
  !         TEST TO SEE IF ANY BULLETINS MISSING
  !
  mbul = nrec - nbul
  IF (mbul .ne. 0) THEN
     WRITE(6,'(A,1X,I0)')' BULLETINS MISSING = ',mbul
     !        CALL W3TAGE('tocgrib2super')
     stop 30
  END IF
  !
  !      CALL W3TAGE('tocgrib2super')
  STOP
END PROGRAM tocgrib2super
