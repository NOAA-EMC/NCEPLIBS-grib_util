!> @file
!> @brief Convert files between GRIB1 and GRIB2.
!> @author Stephen Gilbert @date 2003-06-06

!> This program converts every GRIB field in a file between GRIB1 and
!> GRIB2.
!>
!> The following conversions are supported:
!> 1. GRIB1 to GRIB2
!> 2. GRIB2 to GRIB1
!> 3. GRIB2 to GRIB2.
!>
!> ### Program History Log
!> Date | Programmer | Comments
!> -----|------------|---------
!> 2003-06-06 | Gilbert | Initial
!> 2008-05-14 | Vuong | Added the option -m0 (No explicit missing values included within the datavalues, modified the options and help messages
!> 2010-12-02 | Vuong | Changed Master Table Version Number from 2 to 6. Add option -mastertable_ver_x where x is mater table version 2 to 10
!> 2012-03-29 | Vuong | Changed Master Table Version Number from 2 to 8.
!> 2013-07-24 | Vuong | Changed Master Table Version Number from 2 to 11
!> 2016-09-30 | Vuong | Modified to correct forecast hour beyon F252 when convert from grib2 to grib1. Fixed memory leak and complex packing
!> 2018-07-26 | Vuong | Checked Time Range for continuous accumulated APCP after F252 when convert from grib2 to grib1
!>
!> @return
!> - 0 Success.
!> - 2 Problem processing command line arguments.
!> - 3 Problem opening input GRIB file.
!> - 4 Problem opening output GRIB file.
!> - 5 Unknown conversion option.
!>
!> @author Stephen Gilbert @date 2003-06-06
program cnvgrib

  integer :: inver = 0, outver = 0, ipack = -1
  character(len = 500) :: gfilein, gfileout, copt
  character(len = 2) :: master_table_ver, curmastertab_ver
  INTEGER(4) NARG, IARGC, table_ver, mastertab
  logical :: usemiss = .false., uvvect = .true.
  !
  !     Set current Master table version 2
  !
  curmastertab_ver = '2'
  table_ver = 2
  mastertab = 21    ! WMO GRIB2 version 21 (released in May 2, 2018)

  !  GET ARGUMENTS
  NARG = IARGC()
  IF (NARG .lt. 3) THEN       ! may be a problem with args
     IF (NARG .eq. 0) THEN
        !CALL ERRMSG('cnvgrib:  Incorrect usage')
        call usage(0)
        CALL ERREXIT(2)
     ELSE                !  look for -h "help" option
        do j = 1, NARG
           call getarg(j, copt)
           if (copt .eq. '-h' .or. copt .eq. '-help') then
              call usage(1)
              CALL ERREXIT(0)
           endif
        ENDDO
        call usage(0)
        CALL ERREXIT(2)
     ENDIF
  ELSE
     j = 1
     do while (j.le.NARG-2)        ! parse first narg-2 args
        call getarg(j, copt)
        j = j+1
        selectcase(copt)
        case('-g12')
           inver = 1
           outver = 2
        case('-g21')
           inver = 2
           outver = 1
        case('-g22')
           inver = 2
           outver = 2
        case('-p0')
           ipack = 0
        case('-p2')
           ipack = 2
        case('-p31')
           ipack = 31
        case('-p32')
           ipack = 32
        case('-p40')
           ipack = 40
        case('-p41')
           ipack = 41
        case('-p40000')       ! Obsolete
           ipack = 40000
        case('-p40010')       ! Obsolete
           ipack = 40010
        case('-m')
           usemiss = .true.
           imiss = 1
        case('-m0')
           usemiss = .true.
           imiss = 0
        case('-nv')
           uvvect = .false.
        case('-mastertable_ver_1')
           table_ver = 1
           master_table_ver = '1'
        case('-mastertable_ver_2')
           table_ver = 2
           master_table_ver = '2'
        case('-mastertable_ver_3')
           table_ver = 3
           master_table_ver = '3'
        case('-mastertable_ver_4')
           table_ver = 4
           master_table_ver = '4'
        case('-mastertable_ver_5')
           table_ver = 5
           master_table_ver = '5'
        case('-mastertable_ver_6')
           table_ver = 6
           master_table_ver = '6'
        case('-mastertable_ver_7')
           table_ver = 7
           master_table_ver = '7'
        case('-mastertable_ver_8')
           table_ver = 8
           master_table_ver = '8'
        case('-mastertable_ver_9')
           table_ver = 9
           master_table_ver = '9'
        case('-mastertable_ver_10')
           table_ver = 10
           master_table_ver = '10'
        case('-mastertable_ver_11')
           table_ver = 11
           master_table_ver = '11'
        case('-mastertable_ver_12')
           table_ver = 12
           master_table_ver = '12'
        case('-mastertable_ver_13')
           table_ver = 13
           master_table_ver = '13'
        case('-mastertable_ver_14')
           table_ver = 14
           master_table_ver = '14'
        case('-mastertable_ver_15')
           table_ver = 15
           master_table_ver = '15'
        case('-mastertable_ver_16')
           table_ver = 16
           master_table_ver = '16'
        case('-mastertable_ver_17')
           table_ver = 17
           master_table_ver = '17'
        case('-mastertable_ver_18')
           table_ver = 18
           master_table_ver = '18'
        case('-mastertable_ver_19')
           table_ver = 19
           master_table_ver = '19'
        case('-mastertable_ver_20')
           table_ver = 20
           master_table_ver = '20'
        case('-mastertable_ver_21')
           table_ver = 21
           master_table_ver = '21'
        case('-mastertable_ver_22')
           table_ver = 22
           master_table_ver = '22'
        case default
           call usage(0)
           CALL ERREXIT(2)
        end select
     ENDDO

     if (table_ver .le. 1 .OR. &
          table_ver .gt. mastertab) then
        call usage(0)
        call errmsg ('  ')
        call errmsg('cnvgrib: cannot change to master table '// &
             'version ' // master_table_ver)
        call errmsg ('  ')
        call errmsg('Current GRIB master table version is '// &
             curmastertab_ver)
        call errmsg ('  ')
        CALL ERREXIT(2)
     end if
     !
     !   get filenames from last two arguments
     !
     CALL GETARG(NARG-1, gfilein)
     CALL GETARG(NARG, gfileout)
     !
     !   If -p option specified, must be writing out grib2
     !
     if ((ipack .ne. -1).and.(outver .eq. 1)) then
        CALL ERRMSG('cnvgrib: -pxx option ignored when using -g21')
     endif
     !
     !   Must have -g option
     !
     if ((inver .eq. 0).or.(outver .eq. 0)) then
        CALL ERRMSG('cnvgrib: must use one -gxx option')
        call usage(0)
        CALL ERREXIT(2)
     endif
     !
     !   If -m or -m0 option specified, must be writing out grib2
     !   and using DRT 5.2 or 5.3
     !
     if ((usemiss).and.(ipack .ne. 2 .AND. ipack .ne. 31 .AND. &
          ipack .ne. 32)) then
        CALL ERRMSG('cnvgrib: -m or -m0 option ignored when not '// &
             'using -p2, -p31 or -p32.')
        usemiss = .false.
     endif
  ENDIF
  !
  !  Open input and output grib files
  !
  IFL1 = 10
  IFL2 = 50
  NCGB = LEN_TRIM(gfilein)
  CALL BAOPENR(ifl1, gfilein(1:NCGB), IOS)
  if (IOS .NE. 0) then
     call errmsg('cnvgrib: cannot open input GRIB file '// &
          gfilein(1:NCGB))
     call errexit(3)
  endif
  NCGB = LEN_TRIM(gfileout)
  CALL BAOPENW(ifl2, gfileout(1:NCGB), IOS)
  if (IOS .NE. 0) then
     call errmsg('cnvgrib: cannot open output GRIB file '// &
          gfileout(1:NCGB))
     call errexit(4)
  endif
  !
  !  convert grib file
  !
  if ((inver .eq. 1).AND.(outver .eq. 2)) then
     call cnv12(ifl1, ifl2, ipack, usemiss, imiss, uvvect, table_ver)
  elseif ((inver .eq. 2).AND.(outver .eq. 1)) then
     call cnv21(ifl1, ifl2)
  elseif ((inver .eq. 2).AND.(outver .eq. 2)) then
     call cnv22(ifl1, ifl2, ipack, usemiss, imiss, table_ver)
  else
     print *, ' Unknown conversion option.'
     call errexit(5)
  endif
  !
  !  close grib files
  !
  CALL BACLOSE(ifl1, IOS)
  CALL BACLOSE(ifl2, IOS)

  stop
end program cnvgrib

!> This routine prints a brief description of the command line options.
!>
!> @param[in] iopt ouput option: 1 = print description of arguments,
!> otherwise, print command usage summary.
!>
!> @author Stephen Gilbert @date 2003-06-06
subroutine usage(iopt)
  character(len = 15) :: cnvgrib_ver = "cnvgrib-v3.1.1"
  integer, intent(in) :: iopt

  if (iopt .eq. 0) then
     call errmsg ('  ')
     call errmsg('Usage: cnvgrib [-h] {-g12|-g21|-g22} [-m|-m0]'// &
          ' [-nv] [-mastertable_ver_x]')
     call errmsg('               [{-p0|-p2|-p31|-p32|-p40'// &
          '|-p41}]  ingribfile   outgribfile')
     call errmsg ('  ')
     call errmsg('Usage: cnvgrib  -h  For helps and shows all'// &
          ' options')
     call errmsg ('  ')
  endif

  if (iopt .eq. 1) then
     call errmsg ('  ')
     call errmsg('cnvgrib:  version '//cnvgrib_ver)
     call errmsg ('  ')
     call errmsg('Must use one of the following options:')
     call errmsg('   -g12     converts GRIB1 to GRIB2')
     call errmsg('   -g21     converts GRIB2 to GRIB1')
     call errmsg('   -g22     converts GRIB2 to GRIB2 '// &
          ' (used to change packing option)')
     call errmsg ('  ')
     call errmsg('Optional packing options: (for use with '// &
          ' -g12 and -g22 only)')
     call errmsg('   -p0      simple packing')
     call errmsg('   -p2      complex packing')
     call errmsg('   -p31     complex pack with 1st order diffs')
     call errmsg('   -p32     complex pack with 2nd order diffs')
     call errmsg('   -p40     JPEG2000 encoding')
     call errmsg('   -p41     PNG encoding')
     call errmsg ('  ')
     call errmsg('Other Optional options: ')
     call errmsg('   -nv      Do not combine U, V wind components')
     call errmsg ('  ')
     call errmsg('   Use missing value management'// &
          ' instead of bitmap')
     call errmsg('   (ONLY valid with Complex Packing options:'// &
          ' -p2, -p31 or -p32)')
     call errmsg ('  ')
     call errmsg('   -m      Primary missing values'// &
          ' included within the data values')
     call errmsg('   -m0     No explicit missing values'// &
          ' included within the data values')
     call errmsg('   -mastertable_ver_x     Master Table version'// &
          ' where x is number from 2 to 21')
     call errmsg ('  ')
  endif
  return
end subroutine usage
