!> @file
!> @brief Copy all or part of one GRIB2 file to another GRIB2 file.
!> @author Iredell @date 1998-10-22

!> The command copygb2 copies all or part of one GRIB2 file to another
!> GRIB2 file, interpolating if necessary.
!>
!> Unless otherwise directed (-x option), the GRIB2 index file is also
!> used to speed the reading.
!>
!> The fields are interpolated to an output grid if specified (-g
!> option). The interpolation type defaults to bilinear but may be
!> specified directly (-i option).
!>
!> The copying may be limited to specific fields (-k option). It may
!> also be limited to a specified subgrid of the output grid or to a
!> subrange of the input fields (-B and -b, -A, and -K
!> options). Fields can be identified as scalars or vectors (-v
!> option), which are interpolated differently. The invalid data in
!> the output field can be filled with mask values or merged with a
!> merge field (-M and -m options).
!>
!> The output GRIB2 message can also be appended to a file (-a
!> option).
!>
!> Some defaults can be overridden in a namelist file (-N option).
!>
!> ## COMMAND LINE OPTIONS:
!>   -a
!>      Appends rather than overwrites the output GRIB file.
!>
!>   -A "<> mapthreshold"
!>      Inequality and threshold used in determining
!>      where on the map the data will be copied.
!>      The data are copied only where the given
!>      map field is on the correct side of the threshold.
!>      The mapthreshold defaults to '>-1.e30'; in this case,
!>      only the map field's bitmap will limit the domain.
!>
!>   -b mapindex
!>      Optional index file used to get the map field.
!>
!>   -B mapgrib
!>      GRIB file used to get the map field. The map field
!>      is read from the GRIB file and compared to the
!>      map threshold to determine for which region on the map
!>      the data will be copied. The mapgrib can be the name
!>      of an actual GRIB file (in which case the index
!>      file may be specified with the -b option) or it can
!>      be '-1'. If mapgrib is '-1', then the input GRIB file
!>      (first positional argument) is used.
!>      The -K option specifies which field to read from
!>      the mapgrib GRIB file. If mapgrib is an actual file,
!>      then the first field is taken if -K is not specified.
!>      On the other hand, if mapgrib is '-1', then if the
!>      if -K is not specified, the current field is taken
!>      as the map field. A special exception is if -K '-1'
!>      is specified, in which case the current field is
!>      taken as the map field and it is applied before any
!>      interpolation; otherwise the map field is always
!>      applied after interpolation.
!>
!>   -g "kgdtn [kgds]"
!>      Output grid identification. If kgdtn=-1 (the default),
!>      then the output grid is the same as the input grid.
!>      If kgdtn=-4, then the grid is that of the map field.
!>      If kgdtn=-5, then the grid is that of the merge field.
!>      If 0<=kgdtn<65535, then grid designates a specific
!>      GRIB2 Grid Definition Template (GDT) Number. In this
!>      case, kgdt is the list of the full set of Grid
!>      Definition Template values for the GDT 3.kgdtn,
!>      defining the output grid.
!>
!>   -i "ip [ipopts]"
!>      Interpolation options. The default is bilinear
!>      interpolation (ip=0). Other interpolation options
!>      are bicubic (ip=1), neighbor (ip=2), budget (ip=3),
!>      and spectral (ip=4). Spectral interpolation is forced
!>      even if the input and output grids are the same.
!>      See the documentation for iplib for further details.
!>
!>   -k "kpdtn kpdt"
!>      Full set of Production Definition Template parameters
!>      determining the field(s) to be copied. kpdtn is
!>      Product Definition Template (PDT) number 4.kpdtn.
!>      A wildcard, indicating search all template numbers,
!>      is specified by -1 (the default). The kpdt array
!>      contains the values of each entry of PDT 4.kpdtn,
!>      and a wildcard for any entry can be specified as
!>      -9999. If the -k is not specified, then copygb will
!>      attempt to copy every field in the input GRIB file.
!>
!>   -K "mapkpds"
!>      Full set of kpds parameters determing a GRIB PDS
!>      (product definition section) in the W3FI63 format
!>      determining the map field to be used to determine
!>      where on the map the data will be copied.
!>      A wildcard is specified by -1 (the defaults).
!>
!>   -m mergeindex
!>      Optional index file used to get the merge field.
!>
!>   -M "mask"/mergegrib
!>      Mask used to fill out bitmapped areas of the map.
!>      If specified, there will be no bitmap in the output.
!>      The mask must be in the format '\#value' where value
!>      is the real number used to fill out the field.
!>      Otherwise, the argument is interpreted as a merge
!>      GRIB file. Then for each GRIB message copied,
!>      a merge field is found in the merge GRIB file
!>      with the same parameter and level indicators
!>      as the copied field. This merge field is interpolated
!>      to the output grid and used to fill out the bitmapped
!>      areas of the map, at least where the merge field
!>      is not bitmapped. No merging is done if no merge
!>      field is found.
!>
!>   -N namelist
!>      Namelist file to override default output options.
!>      The namelist must start with " &NLCOPYGB" and end with "/".
!>      Namelist variables are
!>        IDS(255)      Output decimal scaling by parameter
!>        IBS(255)      Output binary scaling by parameter
!>        NBS(255)      Output number of bits by parameter
!>
!>   -v "uparms"
!>      Parameter indicator(s) for the u-component of vectors.
!>      A specific parameter is indicated by three numbers:
!>      disc|cat|num. Any parameter value in uparms is defined
!>      as (65536*disc)+(256*cat)+num.
!>      The parameter indicator for the v-component is assumed
!>      to be one more than that of the u-component.
!>      If the -v option is not specified, then the wind
!>      components (parameters 0|2|2 = 514 and 0|2|3 = 515)
!>      are the only fields assumed to be vector components
!>      in the GRIB file.
!>
!>   -x
!>      Turns off the use of an index file. The index records
!>      are then extracted from the GRIB file, which
!>      will increase the time taken by copygb2.
!>
!>   -X
!>      Turns on verbose printout. This option is
!>      incompatible with GRIB output to standard output.
!>
!> ### Files
!> #### Input Files
!>   - unit   11    input grib file
!>   - unit   14    map grib file
!>   - unit   15    merge grib file
!>   - unit   31    input grib index file
!>   - unit   34    map grib index file
!>   - unit   35    merge grib index file
!>
!> #### Output Files
!>   - unit   51    output grib file
!>
!> @return 0 for success.
!> @author Iredell @date 1998-10-22
PROGRAM COPYGB2
  CHARACTER*256 CARG,CG1,CX1,CGB,CXB,CGM,CXM,CG2,CNL
  INTEGER KARG(100)
  INTEGER KGDTI(200),IPOPT(20),JPDT(200),JPDSB(200),IUV(100)
  REAL RARG(100)
  CHARACTER*400 GDS
  DATA IGDTN/-1/,KGDTI/200*0/
  DATA IP/0/,IPOPT/20*-1/
  DATA JPDTN/-1/,JPDT/200*-9999/,JPDSB/200*-1/
  DATA IUV/514,99*0/,NUV/1/
  DATA LWG/0/,LAPP/0/,LXX/0/,LX/1/,KZ1/-1/,KZ2/-2/
  DATA JB/0/,JBK/0/,LAB/1/,AB/-1.E30/,LAM/0/,AM/0./
  DATA CGB/' '/,CXB/' '/,CGM/' '/,CXM/' '/,CNL/' '/
  INTEGER IDS(255),IBS(255),NBS(255)
  NAMELIST/NLCOPYGB/ IDS,IBS,NBS
  DATA IDS/255*-9999/,IBS/255*-9999/,NBS/255*-9999/

  !  PARSE COMMAND LINE OPTIONS
  NARG=IARGC()
  IARG=1
  LSTOPT=0
  DO WHILE(IARG.LE.NARG.AND.LSTOPT.EQ.0)
     CALL GETARG(IARG,CARG)
     LARG=LEN_TRIM(CARG)
     IARG=IARG+1
     IF(CARG(1:1).NE.'-') THEN
        LSTOPT=1
        IARG=IARG-1
     ELSEIF(LARG.EQ.1) THEN
        CALL ERRMSG('copygb2: invalid option -')
        CALL EUSAGE
        CALL ERREXIT(1)
     ELSE
        L=2
        DO WHILE(L.LE.LARG)
           IF(CARG(L:L).EQ.'-') THEN
              LSTOPT=1
           ELSEIF(CARG(L:L).EQ.'a') THEN
              LAPP=1
           ELSEIF(CARG(L:L).EQ.'A') THEN
              IF(L.EQ.LARG) THEN
                 L=0
                 CALL GETARG(IARG,CARG)
                 LARG=LEN_TRIM(CARG)
                 IARG=IARG+1
              ENDIF
              IF(CARG(L+1:L+1).EQ.'>') THEN
                 LAB=1
                 L=L+1
              ELSEIF(CARG(L+1:L+1).EQ.'<') THEN
                 LAB=-1
                 L=L+1
              ELSE
                 CALL ERRMSG('copygb2: invalid threshold '// &
                      CARG(L+1:LARG))
                 CALL EUSAGE
                 CALL ERREXIT(1)
              ENDIF
              CALL FPARSER(CARG(L+1:LARG),1,AB)
              L=LARG
              call errmsg('Option -A Ignored...Not yet implemented.')
              LAB=1      ! default value, since -A option not yet implemented.
              AB=-1.E30  ! default value, since -A option not yet implemented.
           ELSEIF(CARG(L:L).EQ.'B') THEN
              IF(L.EQ.LARG) THEN
                 L=0
                 CALL GETARG(IARG,CARG)
                 LARG=LEN_TRIM(CARG)
                 IARG=IARG+1
              ENDIF
              LCGB=LARG-L
              CGB=CARG(L+1:LARG)
              L=LARG
              call errmsg('Option -B Ignored...Not yet implemented.')
              LCGB=1     ! default value, since -B option not yet implemented.
              CGB=' '    ! default value, since -B option not yet implemented.
           ELSEIF(CARG(L:L).EQ.'b') THEN
              IF(L.EQ.LARG) THEN
                 L=0
                 CALL GETARG(IARG,CARG)
                 LARG=LEN_TRIM(CARG)
                 IARG=IARG+1
              ENDIF
              LCXB=LARG-L
              CXB=CARG(L+1:LARG)
              L=LARG
              call errmsg('Option -b Ignored...Not yet implemented.')
              LCXB=1     ! default value, since -B option not yet implemented.
              CXB=' '    ! default value, since -B option not yet implemented.
           ELSEIF(CARG(L:L).EQ.'g') THEN
              IF(L.EQ.LARG) THEN
                 L=0
                 CALL GETARG(IARG,CARG)
                 LARG=LEN_TRIM(CARG)
                 IARG=IARG+1
              ENDIF
              KARG(1)=IGDTN
              KARG(2:100)=0
              CALL FPARSEI(CARG(L+1:LARG),100,KARG)
              IGDTN=KARG(1)
              IF(IGDTN.GE.0.AND.IGDTN.LE.65534) THEN
                 KGDTI(1:99)=KARG(2:100)
              ENDIF
              IF(IGDTN.LT.-5.OR.IGDTN.EQ.-2.OR. &
                   IGDTN.EQ.-3.OR.IGDTN.GT.65534) THEN
                 CALL ERRMSG('copygb2: invalid output grid '// &
                      CARG(L+1:LARG))
                 CALL EUSAGE
                 CALL ERREXIT(1)
              ENDIF
              IF ( IGDTN.GE.0 ) THEN
                 MI=NUMPTS(IGDTN,KGDTI)
                 IF(MI.LE.0) THEN
                    CALL ERRMSG('copygb2: unsupported output grid '// &
                         CARG(L+1:LARG))
                    CALL EUSAGE
                    CALL ERREXIT(1)
                 ENDIF
              ENDIF
              L=LARG
           ELSEIF(CARG(L:L).EQ.'i') THEN
              IF(L.EQ.LARG) THEN
                 L=0
                 CALL GETARG(IARG,CARG)
                 LARG=LEN_TRIM(CARG)
                 IARG=IARG+1
              ENDIF
              KARG(1)=IP
              KARG(2:21)=IPOPT
              CALL FPARSEI(CARG(L+1:LARG),21,KARG)
              IP=KARG(1)
              IPOPT=KARG(2:21)
              L=LARG
           ELSEIF(CARG(L:L).EQ.'K') THEN
              IF(L.EQ.LARG) THEN
                 L=0
                 CALL GETARG(IARG,CARG)
                 LARG=LEN_TRIM(CARG)
                 IARG=IARG+1
              ENDIF
              JBK=1
              CALL FPARSEI(CARG(L+1:LARG),100,JPDSB)
              IF(JPDSB(5).EQ.0) THEN
                 CALL ERRMSG('copygb2: invalid PDS parms '// &
                      CARG(L+1:LARG))
                 CALL EUSAGE
                 CALL ERREXIT(1)
              ENDIF
              L=LARG
              call errmsg('Option -K Ignored...Not yet implemented.')
              JBK=0     ! default value, since -K option not yet implemented.
              JPDSB=-1  ! default value, since -K option not yet implemented.
           ELSEIF(CARG(L:L).EQ.'k') THEN
              IF(L.EQ.LARG) THEN
                 L=0
                 CALL GETARG(IARG,CARG)
                 LARG=LEN_TRIM(CARG)
                 IARG=IARG+1
              ENDIF
              KARG(1)=JPDTN
              KARG(2:100)=JPDT(1:99)
              CALL FPARSEI(CARG(L+1:LARG),100,KARG)
              JPDTN=KARG(1)
              JPDT(1:99)=KARG(2:100)
              IF(JPDTN.LT.-1 .OR. JPDTN.GE.65535) THEN
                 CALL ERRMSG('copygb2: invalid PDT parms '// &
                      CARG(L+1:LARG))
                 CALL EUSAGE
                 CALL ERREXIT(1)
              ENDIF
              L=LARG
           ELSEIF(CARG(L:L).EQ.'M') THEN
              IF(L.EQ.LARG) THEN
                 L=0
                 CALL GETARG(IARG,CARG)
                 LARG=LEN_TRIM(CARG)
                 IARG=IARG+1
              ENDIF
              IF(CARG(L+1:L+1).EQ.'#') THEN
                 L=L+1
                 CALL FPARSER(CARG(L+1:LARG),1,AM)
                 LAM=1
              ELSE
                 LCGM=LARG-L
                 CGM=CARG(L+1:LARG)
                 LAM=5
              ENDIF
              L=LARG
           ELSEIF(CARG(L:L).EQ.'m') THEN
              IF(L.EQ.LARG) THEN
                 L=0
                 CALL GETARG(IARG,CARG)
                 LARG=LEN_TRIM(CARG)
                 IARG=IARG+1
              ENDIF
              LCXM=LARG-L
              CXM=CARG(L+1:LARG)
              L=LARG
           ELSEIF(CARG(L:L).EQ.'N') THEN
              IF(L.EQ.LARG) THEN
                 L=0
                 CALL GETARG(IARG,CARG)
                 LARG=LEN_TRIM(CARG)
                 IARG=IARG+1
              ENDIF
              LCNL=LARG-L
              CNL=CARG(L+1:LARG)
              L=LARG
              call errmsg('Option -N Ignored...Not yet implemented.')
              LCNL=1     ! default value, since -N option not yet implemented.
              CNL=' '    ! default value, since -N option not yet implemented.
           ELSEIF(CARG(L:L).EQ.'v') THEN
              IF(L.EQ.LARG) THEN
                 L=0
                 CALL GETARG(IARG,CARG)
                 LARG=LEN_TRIM(CARG)
                 IARG=IARG+1
              ENDIF
              CALL FPARSEI(CARG(L+1:LARG),100,IUV)
              NUV=1
              DO JUV=2,100
                 IF(IUV(JUV).NE.0) NUV=JUV
              ENDDO
              L=LARG
           ELSEIF(CARG(L:L).EQ.'x') THEN
              LX=0
           ELSEIF(CARG(L:L).EQ.'X') THEN
              LXX=1
           ELSE
              CALL ERRMSG('copygb2: invalid option '//CARG(L:L))
              CALL EUSAGE
              CALL ERREXIT(1)
           ENDIF
           L=L+1
        ENDDO
     ENDIF
  ENDDO

  !  PARSE COMMAND LINE POSITIONAL ARGUMENTS
  NXARG=LX+2
  IF(NARG-IARG+1.NE.NXARG) THEN
     CALL ERRMSG('copygb2: incorrect number of arguments')
     CALL EUSAGE
     CALL ERREXIT(NXARG)
  ENDIF
  CALL GETARG(IARG,CG1)
  LCG1=LEN_TRIM(CG1)
  IARG=IARG+1
  LG1=11
  CALL BAOPENR(LG1,CG1(1:LCG1),IRETBA)
  IF(IRETBA.NE.0) THEN
     CALL ERRMSG('copygb2:  error accessing file '//CG1(1:LCG1))
     CALL ERREXIT(8)
  ENDIF
  IF(LX.GT.0) THEN
     CALL GETARG(IARG,CX1)
     LCX1=LEN_TRIM(CX1)
     IARG=IARG+1
     LX1=31
     CALL BAOPENR(LX1,CX1(1:LCX1),IRETBA)
     IF(IRETBA.NE.0) THEN
        CALL ERRMSG('copygb2:  error accessing file '//CX1(1:LCX1))
        CALL ERREXIT(8)
     ENDIF
  ELSE
     LX1=0
  ENDIF
  CALL GETARG(IARG,CG2)
  LCG2=LEN_TRIM(CG2)
  IARG=IARG+1
  IF(CG2(1:LCG2).EQ.'-') THEN
     IF(LXX.GT.0) THEN
        CALL ERRMSG('copygb2:  piping incompatible with the X option')
        CALL ERREXIT(1)
     ENDIF
     LG2=6
  ELSE
     LG2=51
     IF(LAPP.EQ.0) THEN
        CALL BAOPENWT(LG2,CG2(1:LCG2),IRETBA)
     ELSE
        CALL BAOPENWA(LG2,CG2(1:LCG2),IRETBA)
     ENDIF
     IF(IRETBA.NE.0) THEN
        CALL ERRMSG('copygb2:  error accessing file '//CG2(1:LCG2))
        CALL ERREXIT(8)
     ENDIF
  ENDIF

  !  OPEN MAP FILE
  IF(CGB.NE.' ') THEN
     IF(CGB(1:2).EQ.'-1') THEN
        IF(JPDSB(5).EQ.-1) THEN
           JB=1
        ELSE
           JB=4
           LGB=LG1
           LXB=LX1
        ENDIF
     ELSE
        JB=4
        LGB=14
        CALL BAOPENR(LGB,CGB(1:LCGB),IRETBA)
        IF(IRETBA.NE.0) THEN
           CALL ERRMSG('copygb2:  error accessing file '//CGB(1:LCGB))
           CALL ERREXIT(8)
        ENDIF
        IF(CXB(1:1).NE.' ') THEN
           LXB=34
           CALL BAOPENR(LXB,CXB(1:LCXB),IRETBA)
           IF(IRETBA.NE.0) THEN
              CALL ERRMSG('copygb2: error accessing file '//CXB(1:LCXB))
              CALL ERREXIT(8)
           ENDIF
        ELSE
           LXB=0
        ENDIF
     ENDIF
  ENDIF

  !  OPEN MERGE FILE
  IF(CGM.NE.' ') THEN
     LAM=5
     LGM=15
     CALL BAOPENR(LGM,CGM(1:LCGM),IRETBA)
     IF(IRETBA.NE.0) THEN
        CALL ERRMSG('copygb2:  error accessing file '//CGM(1:LCGM))
        CALL ERREXIT(8)
     ENDIF
     IF(CXM(1:1).NE.' ') THEN
        LXM=35
        CALL BAOPENR(LXM,CXM(1:LCXM),IRETBA)
        IF(IRETBA.NE.0) THEN
           CALL ERRMSG('copygb2:  error accessing file '//CXM(1:LCXM))
           CALL ERREXIT(8)
        ENDIF
     ELSE
        LXM=0
     ENDIF
  ENDIF

  !  OPEN AND READ NAMELIST FILE
  IF(CNL.NE.' ') THEN
     LNL=2
     OPEN(LNL,FILE=CNL(1:LCNL),STATUS='OLD',IOSTAT=IRET)
     IF(IRET.NE.0) THEN
        CALL ERRMSG('copygb2:  error accessing file '//CNL(1:LCNL))
        CALL ERREXIT(8)
     ENDIF
     READ(LNL,NLCOPYGB,IOSTAT=IRET)
     IF(IRET.NE.0) THEN
        CALL ERRMSG('copygb2:  error reading namelist from file '// &
             CNL(1:LCNL))
        CALL ERREXIT(8)
     ENDIF
  ENDIF

  !  GO
  IF(LXX.GT.0) THEN
     CALL W3TAGB('COPYGB2 ',1998,0295,0047,'NP23   ')
  ENDIF
  CALL CPGB(LG1,LX1,LGB,LXB,LGM,LXM,LG2, &
       IGDTN,KGDTI,IP,IPOPT,JPDTN,JPDT,NUV,IUV, &
       JPDSB,JB,JBK,LAB,AB,LAM,AM,LXX,LWG, &
       IDS,IBS,NBS)
  IF(LXX.GT.0) THEN
     CALL W3TAGE('COPYGB2 ')
  ENDIF

CONTAINS

  !> Print proper usage to stderr.
  !>
  !> @author Iredell @date 1996-07-19
  SUBROUTINE EUSAGE
    CALL ERRMSG('Usage: copygb2'// &
         ' [-g "kgdtn [kgdt]"] [-i "ip [ipopts]"]')
    CALL ERRMSG('              '// &
         ' [-k "kpdtn kpdt"] [-v "uparms"]')
    CALL ERRMSG('              '// &
         ' [-B mapgrib [-b mapindex] [-A "<> mapthreshold"]'// &
         ' [-K "mapkpds"]]')
    CALL ERRMSG('              '// &
         ' [-M "mask"/mergegrib [-m mergeindex]] [-X] [-a]'// &
         ' [-N namelist]')
    CALL ERRMSG('       then either:')
    CALL ERRMSG('              '// &
         ' grib2in index1 grib2out')
    CALL ERRMSG('            or:')
    CALL ERRMSG('              '// &
         ' -x grib2in grib2out')

  END SUBROUTINE EUSAGE

  !> Copy grib files.
  !>
  !> @param[in] lg1 integer unit number for grib file 1
  !> @param[in] lx1 integer unit number for grib index file 1
  !> @param[in] lgb integer unit number for grib file map
  !> @param[in] lxb integer unit number for grib index file map
  !> @param[in] lgm integer unit number for grib file merge
  !> @param[in] lxm integer unit number for grib index file merge
  !> @param[in] lg2 integer unit number for grib file 2
  !> @param[in] igdtn integer output grid identification
  !> @param[in] kgdti integer (200) output grid definition template values
  !> @param[in] ip integer interpolation type
  !> @param[in] ipopt integer (20) interpolation options
  !> @param[in] jpdtn integer product definition template number search options
  !> @param[in] jpdt integer (100) product definition template values search options
  !> @param[in] nuv integer number of vector parameter ids
  !> @param[in] iuv integer (100) vector parameter ids
  !> @param[in] jpdsb integer (100) kpds search options (map)
  !> @param[in] jb integer flag for map option
  !> @param[in] jbk integer flag for map option
  !> @param[in] lab integer flag for map threshold inequality
  !> @param[in] ab real map threshold
  !> @param[in] lam integer flag for mask value
  !> @param[in] am real mask value
  !> @param[in] lxx integer flag for verbose output
  !> @param[in] lwg integer flag for stdin selection
  !> @param[in] ids integer (255) decimal scaling (-9999 for no change)
  !> @param[in] ibs integer (255) binary scaling (-9999 for no change)
  !> @param[in] nbs integer (255) number of bits (-9999 for no change)
  !>
  !> @author Iredell @date 96-07-19
  SUBROUTINE CPGB(LG1,LX1,LGB,LXB,LGM,LXM,LG2, &
       IGDTN,KGDTI,IP,IPOPT,JPDTN,JPDT,NUV,IUV, &
       JPDSB,JB,JBK,LAB,AB,LAM,AM,LXX,LWG, &
       IDS,IBS,NBS)
    USE GRIB_MOD

    PARAMETER(MBUF=256*1024)
    CHARACTER CBUFB(MBUF)
    INTEGER JIDS(200),JPDT(200),JGDT(200)
    INTEGER JJPDT(200)
    INTEGER JPDSB(100),IUV(100)
    INTEGER KGDTI(200)
    INTEGER IPOPT(20)
    INTEGER IDS(255),IBS(255),NBS(255)
    INTEGER JPDS(200),JGDS(200),JENS(5)
    INTEGER KPDSB(200),KGDSB(200),KENSB(5)
    INTEGER KPDSM(200),KGDSM(200),KENSM(5)
    CHARACTER*80 CIN
    LOGICAL UNPACK
    TYPE( GRIBFIELD ) :: GFLD1,GFLDM

    MM = 0
    !  READ GRIB HEADERS
    IF(LXX.GT.0) CALL INSTRUMENT(6,KALL0,TTOT0,TMIN0,TMAX0)
    IF(JB.EQ.4) THEN
       JGDS=-1
       JENS=-1
       KRB=-1
       KPDSB=0
       KGDSB=0
       CALL GETGBEMH(LGB,LXB,KRB,JPDSB,JGDS,JENS, &
            MBUF,CBUFB,NLENB,NNUMB,MNUMB, &
            KB,MB,KRBX,KPDSB,KGDSB,KENSB,IRET)
       IF(IRET.EQ.0.AND.MB.LE.0) IRET=255
       IF(LXX.GT.0) THEN
          IF(IRET.EQ.99) THEN
             PRINT *,'copygb2 map field not found'
          ELSEIF(IRET.NE.0) THEN
             PRINT *,'copygb2 map field retrieval error code ',IRET
          ENDIF
       ENDIF
    ELSE
       MB=1
       IRET=0
    ENDIF
    IF(IRET.EQ.0) THEN
       KR1=0
       !IF(LWG.EQ.1) THEN
       !  READ (*,*,IOSTAT=IRET) CIN
       !  IF(IRET.EQ.0) THEN
       !    NDEL=SCAN(CIN,":")
       !    IF(NDEL.GT.0) CIN=CIN(:NDEL-1)
       !    READ(CIN,*) KR1
       !    KR1=-KR1
       !  ENDIF
       !ENDIF
       IF(IRET.EQ.0) THEN
          JDISC=-1
          JIDS=-9999
          JGDTN=-1
          JGDT=-9999
          UNPACK=.FALSE.
          CALL GETGB2(LG1,LX1,KR1,JDISC,JIDS,JPDTN,JPDT,JGDTN, &
               JGDT,UNPACK,KR1X,GFLD1,IRET)
          !          CALL GETGBEMH(LG1,LX1,KR1,JPDS1,JGDS,JENS, &
          !                       MBUF,CBUF1,NLEN1,NNUM1,MNUM1, &
          !                       K1,M1,KR1X,KPDS1,KGDS1,KENS1,IRET)
          IF(IRET.EQ.0.AND.GFLD1%NDPTS.LE.0) IRET=255
          M1=GFLD1%NGRDPTS
          KR1=KR1X
          IF(LXX.GT.0) THEN
             IF(IRET.EQ.99) THEN
                PRINT *,'copygb2 field not found'
             ELSEIF(IRET.NE.0) THEN
                PRINT *,'copygb2 header retrieval error code ',IRET
             ENDIF
          ENDIF
       ENDIF
    ENDIF

    !  LOOP UNTIL DONE
    NO=0
    DO WHILE(IRET.EQ.0)
       IF(LAM.EQ.5) THEN
          JDISC=GFLD1%DISCIPLINE
          JIDS=-9999
          JJPDTN=GFLD1%IPDTNUM
          JJPDT=-9999
          JJPDT(1:2)=GFLD1%IPDTMPL(1:2)
          JJPDT(10:15)=GFLD1%IPDTMPL(10:15)
          JGDTN=-1
          JGDT=-9999
          UNPACK=.FALSE.
          KRM=0
          CALL GETGB2(LGM,LXM,KRM,JDISC,JIDS,JJPDTN,JJPDT, &
               JGDTN,JGDT,UNPACK,KRMX,GFLDM,IRET)
          !          CALL GETGBEMH(LGM,LXM,KRM,JPDS,JGDS,JENS, &
          !                       MBUF,CBUFM,NLENM,NNUMM,MNUMM, &
          !                       KM,MM,KRMX,KPDSM,KGDSM,KENSM,IRET)
          MM=GFLDM%NGRDPTS
          IF(IRET.EQ.0.AND.MM.LE.0) IRET=255
          IF(IRET.NE.0) THEN
             MM=0
             GFLDM%IGDTNUM=-1
             IRET=0
          ENDIF
       ENDIF
       IF(IGDTN.EQ.-1) THEN
          IGDTN=GFLD1%IGDTNUM
          KGDTI(1:GFLD1%IGDTLEN) = GFLD1%IGDTMPL(1:GFLD1%IGDTLEN)
          MI=M1
       ELSEIF(IGDTN.EQ.-4.AND.JB.EQ.4) THEN
          IGI=KPDSB(3)
          !IGDTN=
          !KGDSI=KGDSB
          !KGDTI=
          MI=MB
       ELSEIF(IGDTN.EQ.-5.AND.LAM.EQ.5) THEN
          IGDTN=GFLDM%IGDTNUM
          KGDTI(1:GFLDM%IGDTLEN) = GFLDM%IGDTMPL(1:GFLDM%IGDTLEN)
          MI=MM
       ELSE
          MI=NUMPTS(IGDTN,KGDTI)
       ENDIF
       IF(LXX.GT.0) CALL INSTRUMENT(1,KALL1,TTOT1,TMIN1,TMAX1)
       IF(IGDTN.GE.0.AND.IGDTN.LE.65534) THEN
          MF=MAX(M1,MB,MM)
          CALL CPGB1(LG1,LX1,M1, &
               MBUF,MF,MI, &
               IGDTN,KGDTI,IP,IPOPT,JPDTN,JPDT,NUV,IUV, &
               JPDSB,JB,JBK,LAB,AB,LAM,AM, &
               IDS,IBS,NBS, &
               LGB,LXB,MB,CBUFB,NLENB,NNUMB,MNUMB, &
               LGM,LXM,MM, &
               LG2,LXX,KR1-1,NO,IRET1)
       ENDIF
       IF(LAM.EQ.5) THEN       ! clean-up
          CALL GF_FREE(GFLDM)
       ENDIF
       !IF(LWG.EQ.1) THEN
       !  READ (*,*,IOSTAT=IRET) CIN
       !  IF(IRET.EQ.0) THEN
       !    NDEL=SCAN(CIN,":")
       !    IF(NDEL.GT.0) CIN=CIN(:NDEL-1)
       !    READ(CIN,*) KR1
       !    KR1=KR1-1
       !  ENDIF
       !ENDIF
       IF(IRET.EQ.0) THEN
          CALL GF_FREE(GFLD1)
          JDISC=-1
          JIDS=-9999
          JGDTN=-1
          JGDT=-9999
          UNPACK=.FALSE.
          CALL GETGB2(LG1,LX1,KR1,JDISC,JIDS,JPDTN,JPDT,JGDTN, &
               JGDT,UNPACK,KR1X,GFLD1,IRET)
          !          CALL GETGBEMH(LG1,LX1,KR1,JPDS1,JGDS,JENS, &
          !                       MBUF,CBUF1,NLEN1,NNUM1,MNUM1, &
          !                       K1,M1,KR1X,KPDS1,KGDS1,KENS1,IRET)
          IF(IRET.EQ.0.AND.GFLD1%NDPTS.LE.0) IRET=255
          M1=GFLD1%NGRDPTS
          KR1=KR1X
          IF(LXX.GT.0) THEN
             IF(IRET.NE.0.AND.IRET.NE.99) THEN
                PRINT *,'copygb2 header retrieval error code ',IRET
             ENDIF
          ENDIF
       ENDIF
    ENDDO

    IF(LXX.GT.0) THEN
       PRINT *,'copygb2 wrote ',NO,' total records'
       CALL INSTRUMENT(1,KALL1,TTOT1,TMIN1,TMAX1)
       PRINT *,'Instrumentation Report'
       PRINT '(F10.3," seconds spent searching headers")',TTOT1
       CALL INSTRUMENT(-2,KALL2,TTOT2,TMIN2,TMAX2)
       PRINT '(F10.3," seconds spent reading and unpacking")',TTOT2
       CALL INSTRUMENT(-3,KALL3,TTOT3,TMIN3,TMAX3)
       PRINT '(F10.3," seconds spent manipulating masks")',TTOT3
       CALL INSTRUMENT(-4,KALL4,TTOT4,TMIN4,TMAX4)
       PRINT '(F10.3," seconds spent interpolating or copying")',TTOT4
       CALL INSTRUMENT(-5,KALL5,TTOT5,TMIN5,TMAX5)
       PRINT '(F10.3," seconds spent merging")',TTOT5
       CALL INSTRUMENT(-6,KALL6,TTOT6,TMIN6,TMAX6)
       PRINT '(F10.3," seconds spent packing and writing")',TTOT6
       TTOTT=TTOT1+TTOT2+TTOT3+TTOT4+TTOT5+TTOT6
       PRINT '(F10.3," total seconds spent in copygb2")',TTOTT
    ENDIF

  END SUBROUTINE CPGB

  !> Copy one grib field.
  !>
  !> @param[in] lg1 integer unit number for grib file 1
  !> @param[in] lx1 integer unit number for grib index file 1
  !> @param[in] m1 integer dimension of grib field 1
  !> @param[in] mbuf integer dimension of index buffers
  !> @param[in] mf integer dimension of field
  !> @param[in] mi integer dimension of output grid
  !> @param[in] igdtn integer output grid definition template number
  !> @param[in] kgdti integer (200) output grid definition template values
  !> @param[in] ip integer interpolation type
  !> @param[in] ipopt integer (20) interpolation options
  !> @param[in] jpdtn integer product definition template number search options
  !> @param[in] jpdt integer (100) product definition template values search options
  !> @param[in] nuv integer number of vector parameter ids
  !> @param[in] iuv integer (100) vector parameter ids
  !> @param[in] jpdsb integer (100) kpds search options (map)
  !> @param[in] jb integer flag for map option
  !> @param[in] jbk integer flag for map option
  !> @param[in] lab integer flag for map threshold inequality
  !> @param[in] ab real map threshold
  !> @param[in] lam integer flag for mask value
  !> @param[in] am real mask value
  !> @param[in] ids integer (255) decimal scaling (-9999 for no change)
  !> @param[in] ibs integer (255) binary scaling (-9999 for no change)
  !> @param[in] nbs integer (255) number of bits (-9999 for no change)
  !> @param[in] lgb integer unit number for grib file map
  !> @param[in] lxb integer unit number for grib index file map
  !> @param[in] mb integer dimension of grib field map
  !> @param[in] cbufb character (mbuf) index buffer map
  !> @param[in] nlenb integer length of each index record map
  !> @param[in] nnumb integer number of index records map
  !> @param[in] mnumb integer number of index records map skipped
  !> @param[in] lgm integer unit number for grib file merge
  !> @param[in] lxm integer unit number for grib index file merge
  !> @param[in] mm integer dimension of grib field merge
  !> @param[in] lg2 integer unit number for grib file 2
  !> @param[in] lxx integer flag for verbose output
  !> @param[in] ks1 integer input record counter
  !> @param[out] no integer output record counter
  !> @param[out] iret integer return code
  !>
  !> @author Iredell @date 96-07-19
  SUBROUTINE CPGB1(LG1,LX1,M1, &
       MBUF,MF,MI, &
       IGDTN,KGDTI,IP,IPOPT,JPDTN,JPDT,NUV,IUV, &
       JPDSB,JB,JBK,LAB,AB,LAM,AM, &
       IDS,IBS,NBS, &
       LGB,LXB,MB,CBUFB,NLENB,NNUMB,MNUMB, &
       LGM,LXM,MM, &
       LG2,LXX,KS1,NO,IRET)
    USE GRIB_MOD
    USE GRIDTEMPLATES

    CHARACTER CBUFB(MBUF)
    INTEGER JPDSB(100),IUV(100)
    INTEGER JIDS(200),JPDT(100),JGDT(200)
    INTEGER JJPDT(200)
    INTEGER,TARGET :: KGDTI(200)
    INTEGER IPOPT(20)
    INTEGER IDS(255),IBS(255),NBS(255)
    INTEGER JPDS(200),JGDS(200),JENS(5)
    INTEGER KPDS1(200),KGDS1(200),KENS1(5)
    INTEGER KPDSB(200),KGDSB(200),KENSB(5)
    INTEGER KPDSM(200),KGDSM(200),KENSM(5)
    INTEGER,POINTER :: TMPPTR(:)
    LOGICAL*1 LR(MF)
    LOGICAL*1,POINTER :: L1I(:),LBI(:)
    LOGICAL UNPACK
    REAL FR(MF)
    REAL GR(MF)
    REAL,POINTER :: F1I(:)
    REAL,ALLOCATABLE,TARGET :: G1I(:)
    REAL,POINTER :: FBI(:),GBI(:)
    TYPE( GRIBFIELD ) :: GFLD1,GFLDV,GFLDM,GFLDMV
    INTEGER ISDUMMY,IADUMMY(200)

    !  GET FIELD FROM FILE 1
    JDISC=-1
    JIDS=-9999
    JGDTN=-1
    JGDT=-9999
    UNPACK=.TRUE.
    CALL GETGB2(LG1,LX1,KS1,JDISC,JIDS,JPDTN,JPDT,JGDTN, &
         JGDT,UNPACK,KR1,GFLD1,IRET)
    K1=GFLD1%NGRDPTS
    !      CALL GETGBEM(LG1,LX1,M1,KS1,JPDS1,JGDS,JENS, &
    !                  MBUF,CBUF1,NLEN1,NNUM1,MNUM1, &
    !                  K1,KR1,KPDS1,KGDS1,KENS1,LR,FR,IRET)
    IV=0
    KRV=0
    IF(IRET.EQ.0) THEN
       JUV=1
       NPARM=(65536*GFLD1%DISCIPLINE) + (256*GFLD1%IPDTMPL(1)) + &
            GFLD1%IPDTMPL(2)
       DO WHILE(JUV.LE.NUV.AND.NPARM.NE.IUV(JUV).AND. &
            NPARM.NE.IUV(JUV)+1)
          JUV=JUV+1
       ENDDO
       IF(JUV.LE.NUV.AND.NPARM.EQ.IUV(JUV)) THEN
          IV=1
          GFLD1%IPDTMPL(2)=GFLD1%IPDTMPL(2)+1
          CALL GETGB2(LG1,LX1,KRV,GFLD1%DISCIPLINE,GFLD1%IDSECT, &
               GFLD1%IPDTNUM,GFLD1%IPDTMPL,GFLD1%IGDTNUM, &
               GFLD1%IGDTMPL,UNPACK,KRVX,GFLDV,IRET)
          !          CALL GETGBEM(LG1,LX1,M1,KRV,JPDS,JGDS,JENS, &
          !                     MBUF,CBUF1,NLEN1,NNUM1,MNUM1, &
          !                     K1,KRVX,KPDS1,KGDS1,KENS1,LR,GR,IRET)
          KRV=KRVX
          GFLD1%IPDTMPL(2)=GFLD1%IPDTMPL(2)-1
       ELSEIF(JUV.LE.NUV.AND.NPARM.EQ.IUV(JUV)+1) THEN
          IRET=-1
       ENDIF
    ENDIF
    IF(LXX.GT.0) THEN
       IF(IRET.EQ.-1) THEN
          PRINT *,'copygb2 skipping 2nd vector component field'
       ELSEIF(IRET.NE.0) THEN
          PRINT *,'copygb2 data retrieval error code ',IRET
       ELSEIF(KRV.EQ.0) THEN
          PRINT *,'copygb2 read scalar field from record ',KR1
          PRINT *,'       ...PDT 4.',GFLD1%IPDTNUM,'=', &
               (GFLD1%IPDTMPL(I),I=1,GFLD1%IPDTLEN)
       ELSE
          PRINT *,'copygb2 read vector field from records ',KR1,KRV
          PRINT *,'       ...PDT 4.',GFLD1%IPDTNUM,'=', &
               (GFLD1%IPDTMPL(I),I=1,GFLD1%IPDTLEN)
          PRINT *,'       ...PDT 4.',GFLDV%IPDTNUM,'=', &
               (GFLDV%IPDTMPL(I),I=1,GFLDV%IPDTLEN)
       ENDIF
       CALL INSTRUMENT(2,KALL2,TTOT2,TMIN2,TMAX2)
    ENDIF

    !  INVOKE MAP MASK BEFORE INTERPOLATION
    IF(IRET.EQ.0.AND.JBK.EQ.1.AND.JB.EQ.1) THEN
       DO I=1,K1
          IF(LR(I)) THEN
             IF((LAB.EQ.1.AND.FR(I).LE.AB).OR. &
                  (LAB.EQ.-1.AND.FR(I).GE.AB)) THEN
                IB1=1
                LR(I)=.FALSE.
             ENDIF
          ENDIF
       ENDDO
       IF(LXX.GT.0) THEN
          PRINT *,'       applied pre-interpolation map mask'
          CALL INSTRUMENT(3,KALL3,TTOT3,TMIN3,TMAX3)
       ENDIF
    ENDIF

    !  INTERPOLATE FIELD
    IF(IRET.EQ.0) THEN
       ALLOCATE(L1I(MI))
       ALLOCATE(F1I(MI))
       ALLOCATE(G1I(MI))
       IF ( GFLD1%IBMAP.EQ.0 .OR. GFLD1%IBMAP.EQ.254 ) THEN
          IB1=1
       ELSE
          IB1=0
          ALLOCATE( GFLD1%BMAP(K1) )   ! dummy array
       ENDIF
       IF ( .NOT. ASSOCIATED(GFLD1%LIST_OPT))  &
            ALLOCATE(GFLD1%LIST_OPT(1))
       IF ( .NOT. ASSOCIATED(GFLDV%FLD) ) ALLOCATE(GFLDV%FLD(K1))
       CALL INTGRIB2(IV,IP,IPOPT,GFLD1%IGDTNUM,GFLD1%IGDTMPL, &
            GFLD1%NUM_OPT,GFLD1%LIST_OPT, &
            K1,IB1,GFLD1%BMAP, &
            GFLD1%FLD,GFLDV%FLD,IGDTN,KGDTI,MI, &
            IB1I,L1I,F1I,G1I,IRET)
       IF(LXX.GT.0) THEN
          IF(IRET.EQ.0) THEN
             PRINT *,'       interpolated to grid GDT 3.',IGDTN,'=', &
                  (KGDTI(J),J=1,GETGDTLEN(IGDTN))
          ELSEIF(IRET.GT.0) THEN
             PRINT *,'       interpolation error code ',IRET
          ENDIF
          CALL INSTRUMENT(4,KALL4,TTOT4,TMIN4,TMAX4)
       ENDIF
       IF(IRET.EQ.-1) IRET=0
    ENDIF

    !  GET MAP FIELD
    IF(IRET.EQ.0.AND.JB.EQ.4) THEN
       KRB=0
       JGDS=-1
       JENS=-1
       CALL GETGBEM(LGB,LXB,MB,KRB,JPDSB,JGDS,JENS, &
            MBUF,CBUFB,NLENB,NNUMB,MNUMB, &
            KB,KRBX,KPDSB,KGDSB,KENSB,LR,FR,IRET)
       IF(LXX.GT.0) THEN
          IF(IRET.EQ.0) THEN
             PRINT *,'       map field retrieved'
             PRINT *,'       ...KPDS(1:24)=',(KPDSB(I),I=1,24)
          ELSEIF(IRET.EQ.99) THEN
             PRINT *,'       map field not found'
          ELSE
             PRINT *,'       map field retrieval error code ',IRET
          ENDIF
       ENDIF

       !  INTERPOLATE MAP FIELD
       IF(IRET.EQ.0) THEN
          IBB=MOD(KPDSB(4)/64,2)
          CALL INTGRIB2(0,IP,IPOPT,ISDUMMY,IADUMMY,SIZE(KGDSB),KGDSB, &
               KB,IBB,LR,FR,GR,ISDUMMY,KGDTI,MI, &
               IBBI,LBI,FBI,GBI,IRET)
          IF(LXX.GT.0) THEN
             IF(IRET.EQ.0) THEN
                PRINT *,'       interpolated to grid template 3.',IGDTN
             ELSEIF(IRET.GT.0) THEN
                PRINT *,'       interpolation error code ',IRET
             ENDIF
          ENDIF
          IF(IRET.EQ.-1) IRET=0
       ENDIF
    ENDIF

    !  INVOKE MAP MASK
    IF(IRET.EQ.0) THEN
       IF(JBK.EQ.0.AND.JB.EQ.1) THEN
          DO I=1,MI
             IF(L1I(I)) THEN
                IF((LAB.EQ.1.AND.F1I(I).LE.AB).OR. &
                     (LAB.EQ.-1.AND.F1I(I).GE.AB)) THEN
                   IB1I=1
                   L1I(I)=.FALSE.
                ENDIF
             ENDIF
          ENDDO
          IF(LXX.GT.0) THEN
             PRINT *,'       applied post-interpolation map mask'
          ENDIF
       ELSEIF(JB.EQ.4) THEN
          DO I=1,MI
             IF(LBI(I)) THEN
                IF((LAB.EQ.1.AND.FBI(I).LE.AB).OR. &
                     (LAB.EQ.-1.AND.FBI(I).GE.AB)) THEN
                   IB1I=1
                   L1I(I)=.FALSE.
                ENDIF
             ELSE
                IB1I=1
                L1I(I)=.FALSE.
             ENDIF
          ENDDO
          IF(LXX.GT.0) THEN
             PRINT *,'       applied fixed map mask'
          ENDIF
       ENDIF

       !  MASK VALUES
       IF(LAM.EQ.1.AND.IB1I.EQ.1) THEN
          IB1I=0
          DO I=1,MI
             IF(.NOT.L1I(I)) THEN
                L1I(I)=.TRUE.
                F1I(I)=AM
                IF(KRV.GT.0) G1I(I)=AM
             ENDIF
          ENDDO
          IF(LXX.GT.0) THEN
             PRINT *,'       substituted mask fill value ',AM
          ENDIF
       ENDIF
       IF(LXX.GT.0) CALL INSTRUMENT(3,KALL3,TTOT3,TMIN3,TMAX3)

       !  MERGE FIELD
       IF(LAM.EQ.5.AND.IB1I.EQ.1) THEN
          JDISC=GFLD1%DISCIPLINE
          JIDS=-9999
          JJPDTN=GFLD1%IPDTNUM
          JJPDT=-9999
          JJPDT(1:2)=GFLD1%IPDTMPL(1:2)
          JJPDT(10:15)=GFLD1%IPDTMPL(10:15)
          JGDTN=-1
          JGDT=-9999
          UNPACK=.TRUE.
          KRM=0
          CALL GETGB2(LGM,LXM,KRM,JDISC,JIDS,JJPDTN,JJPDT, &
               JGDTN,JGDT,UNPACK,KRMX,GFLDM,IRET)
          !          CALL GETGBEM(LGM,LXM,MM,KRM,JPDS,JGDS,JENS, &
          !                      MBUF,CBUFM,NLENM,NNUMM,MNUMM, &
          !                      KM,KRMX,KPDSM,KGDSM,KENSM,LR,FR,IRET)
          KM=GFLDM%NGRDPTS
          IF(IRET.EQ.0.AND.KRV.GT.0) THEN
             GFLDM%IPDTMPL(2)=GFLDM%IPDTMPL(2)+1
             CALL GETGB2(LGM,LXM,KRM,GFLDM%DISCIPLINE,GFLDM%IDSECT, &
                  GFLDM%IPDTNUM,GFLDM%IPDTMPL,GFLDM%IGDTNUM, &
                  GFLDM%IGDTMPL,UNPACK,KRMX,GFLDMV,IRET)
             !            CALL GETGBEM(LGM,LXM,MM,KRM,JPDS,JGDS,JENS, &
             !                        MBUF,CBUFM,NLENM,NNUMM,MNUMM, &
             !                        KM,KRMX,KPDSM,KGDSM,KENSM,LR,GR,IRET)
             GFLDM%IPDTMPL(2)=GFLDM%IPDTMPL(2)-1
          ENDIF
          IF(LXX.GT.0) THEN
             IF(IRET.EQ.0) THEN
                PRINT *,'       merge field retrieved'
                PRINT *,'       ...PDT 4.',GFLDM%IPDTNUM,'=', &
                     (GFLDM%IPDTMPL(I),I=1,GFLDM%IPDTLEN)
                IF(KRV.GT.0) &
                     PRINT *,'       ...PDT 4.',GFLDMV%IPDTNUM,'=', &
                     (GFLDMV%IPDTMPL(I),I=1,GFLDMV%IPDTLEN)
             ELSEIF(IRET.EQ.99) THEN
                PRINT *,'       merge field not found'
             ELSE
                PRINT *,'       merge field retrieval error code ',IRET
             ENDIF
          ENDIF
          IF(IRET.EQ.0) THEN
             ALLOCATE(LBI(MI),FBI(MI),GBI(MI))
             IF ( GFLDM%IBMAP.EQ.0 .OR. GFLDM%IBMAP.EQ.254 ) THEN
                IBM=1
             ELSE
                IBM=0
                ALLOCATE( GFLD1%BMAP(KM) )   ! dummy array
             ENDIF
             IF ( .NOT. ASSOCIATED(GFLDM%LIST_OPT))  &
                  ALLOCATE(GFLDM%LIST_OPT(1))
             IF ( .NOT. ASSOCIATED(GFLDMV%FLD)) ALLOCATE(GFLDMV%FLD(KM))
             CALL INTGRIB2(IV,IP,IPOPT,GFLDM%IGDTNUM,GFLDM%IGDTMPL, &
                  GFLDM%NUM_OPT,GFLDM%LIST_OPT, &
                  KM,IBM,GFLDM%BMAP,GFLDM%FLD,GFLDMV%FLD, &
                  IGDTN,KGDTI,MI, &
                  IBBI,LBI,FBI,GBI,IRET)
             IF(LXX.GT.0) THEN
                IF(IRET.EQ.0) THEN
                   PRINT *,'       interpolated to grid template 3.',IGDTN
                ELSEIF(IRET.GT.0) THEN
                   PRINT *,'       interpolation error code ',IRET
                ENDIF
             ENDIF
             IF(IRET.EQ.-1) IRET=0
          ENDIF
          IF(IRET.EQ.0) THEN
             DO I=1,MI
                IF(.NOT.L1I(I).AND.LBI(I)) THEN
                   L1I(I)=.TRUE.
                   F1I(I)=FBI(I)
                   IF(KRV.GT.0) G1I(I)=GBI(I)
                ENDIF
             ENDDO
             IF(LXX.GT.0) THEN
                PRINT *,'       merged output field with merge field'
             ENDIF
          ENDIF
          IRET=0
          IF (ASSOCIATED(LBI)) DEALLOCATE(LBI)
          IF (ASSOCIATED(FBI)) DEALLOCATE(FBI)
          IF (ASSOCIATED(GBI)) DEALLOCATE(GBI)
       ENDIF
       IF(LXX.GT.0) CALL INSTRUMENT(5,KALL5,TTOT5,TMIN5,TMAX5)
    ENDIF

    !  WRITE OUTPUT FIELD
    IF(IRET.EQ.0) THEN
       GFLD1%IBMAP=255
       IF ( IB1I .EQ. 1 ) GFLD1%IBMAP=0
       GFLD1%IDRTMPL(4)=0
       !K5=KPDS1(5)
       !IDS1=KPDS1(22)
       !IBS1=0
       !NBS1=0
       !IF(K5.GT.0.AND.K5.LT.256) THEN
       !  IF(IDS(K5).GE.-128.AND.IDS(K5).LT.128) IDS1=IDS(K5)
       !  IF(IBS(K5).GE.-128.AND.IBS(K5).LT.128) IBS1=IBS(K5)
       !  IF(NBS(K5).GE.0.AND.NBS(K5).LT.256) NBS1=NBS(K5)
       !ENDIF
       !KPDS1(22)=IDS1
       !  Assign new GDS/GDT info to GFLD1
       GFLD1%IGDTNUM=IGDTN
       DEALLOCATE(GFLD1%IGDTMPL)
       GFLD1%IGDTMPL => KGDTI
       GFLD1%IGDTLEN=GETGDTLEN(IGDTN)
       !  Assign new Bitmap and Data field to GFLD1
       GFLD1%NGRDPTS=MI
       IF ( ASSOCIATED(GFLD1%BMAP) ) DEALLOCATE(GFLD1%BMAP)
       GFLD1%BMAP => L1I
       IF ( ASSOCIATED(GFLD1%FLD) ) DEALLOCATE(GFLD1%FLD)
       GFLD1%FLD => F1I
       CALL PUTGB2(LG2,GFLD1,IRET)
       !        CALL PUTGBEN(LG2,MI,KPDS1,KGDSI,KENS1,IBS1,NBS1,L1I,F1I,IRET)
       IF(IRET.EQ.0) NO=NO+1
       IF(IRET.EQ.0.AND.KRV.GT.0) THEN
          IF ( ASSOCIATED(GFLD1%FLD) ) DEALLOCATE(GFLD1%FLD)
          !  Assign 2nd vector field to GFLD1
          GFLD1%FLD => G1I
          !  Assign new PDT for vector field to GFLD1
          TMPPTR => GFLD1%IPDTMPL
          GFLD1%IPDTMPL => GFLDV%IPDTMPL
          GFLD1%IDRTMPL(4)=0
          CALL PUTGB2(LG2,GFLD1,IRET)
          !          CALL PUTGBEN(LG2,MI,KPDS1,KGDSI,KENS1,IBS1,NBS1,L1I,G1I,IRET)
          IF(IRET.EQ.0) NO=NO+1
          GFLD1%IPDTMPL => TMPPTR
       ENDIF
       IF(LXX.GT.0) THEN
          IF(IRET.NE.0) THEN
             PRINT *,'       packing error code ',IRET
          ELSEIF(KRV.EQ.0) THEN
             PRINT *,'       wrote scalar field to record ',NO
             PRINT *,'       ...PDT 4.',GFLD1%IPDTNUM,'=', &
                  (GFLD1%IPDTMPL(I),I=1,GFLD1%IPDTLEN)
          ELSE
             PRINT *,'       wrote vector field to records ',NO-1,NO
             PRINT *,'       ...PDT 4.',GFLD1%IPDTNUM,'=', &
                  (GFLD1%IPDTMPL(I),I=1,GFLD1%IPDTLEN)
             PRINT *,'       ...PDT 4.',GFLDV%IPDTNUM,'=', &
                  (GFLDV%IPDTMPL(I),I=1,GFLDV%IPDTLEN)
          ENDIF
          CALL INSTRUMENT(6,KALL6,TTOT6,TMIN6,TMAX6)
       ENDIF
    ENDIF

    !  CLEAN UP - FREE MEMORY
    IF (IRET.EQ.0) NULLIFY(GFLD1%IGDTMPL)
    IF (ASSOCIATED(GFLD1%FLD,G1I)) then
       DEALLOCATE(G1I)
       nullify(GFLD1%FLD)
    elseif (ALLOCATED(G1I)) then
       deallocate(G1I)
    endif
    CALL GF_FREE(GFLDMV)
    CALL GF_FREE(GFLDV)
    CALL GF_FREE(GFLDM)
    CALL GF_FREE(GFLD1)
    !     IF (ASSOCIATED(L1I)) DEALLOCATE(L1I)
    !     IF (ASSOCIATED(F1I)) DEALLOCATE(F1I)
    !     IF (ASSOCIATED(G1I)) DEALLOCATE(G1I)

  END SUBROUTINE CPGB1

  !> Interpolate field.
  !>
  !> @param[in] iv integer vector flag
  !> @param[in] ip integer interpolation type
  !> @param[in] ipopt integer (20) interpolation options
  !> @param[in] ngdt1 integer (200) input grid definition template number
  !> @param[in] kgdt1 integer (200) input grid definition template values
  !> @param[in] idefn1 ???
  !> @param[in] idef1 ???
  !> @param[in] k1 integer input dimension
  !> @param[in] ib1 integer input bitmap flag
  !> @param[in] l1 logical*1 (k1) input bitmap if ib1=1
  !> @param[in] f1 real (k1) input field
  !> @param[in] g1 real (k1) input y-component if iv=1
  !> @param[in] ngdt2 integer (200) output grid definition template number
  !> @param[in] kgdt2 integer (200) output grid definition template values
  !> @param[in] k2 integer output dimension
  !> @param[in] ib2 integer output bitmap flag
  !> @param[in] l2 logical*1 (k2) output bitmap
  !> @param[in] f2 real (k2) output field
  !> @param[in] g2 real (k2) output y-component if iv=1
  !> @param[out] iret integer return code
  !>
  !> @author Iredell @date 96-07-19
  SUBROUTINE INTGRIB2(IV,IP,IPOPT,NGDT1,KGDT1,IDEFN1,IDEF1, &
       K1,IB1,L1,F1,G1, &
       NGDT2,KGDT2,K2,IB2,L2,F2,G2,IRET)
    USE GRIDTEMPLATES

    INTEGER IPOPT(20)
    INTEGER KGDT1(*),KGDT2(*),IGDS(5)
    INTEGER IDEF1(IDEFN1)
    ! make idef an array to be compliant with gdt2gds. IDEFN = 0, so idef will not be used.
    integer idef(1)
    INTEGER KGDS1(200),KGDS2(200)
    LOGICAL*1 L1(K1),L2(K2)
    REAL F1(K1),F2(K2)
    REAL G1(K1),G2(K2)
    INTEGER KGDS1F(200),KGDS2F(200)

    !  DETERMINE WHETHER INTERPOLATION IS NECESSARY
    IF(IP.EQ.4) THEN
       INT=1
    ELSE
       INT=0
       IF ( NGDT1 .EQ. NGDT2 ) THEN
          DO I=1,GETGDTLEN(NGDT1)
             INT=MAX(INT,ABS(KGDT1(I)-KGDT2(I)))
          ENDDO
       ELSE
          INT=1
       ENDIF
       INT=MIN(INT,1)
    ENDIF

    !  COPY FIELD
    IF(INT.EQ.0) THEN
       IB2=IB1
       DO I=1,K1
          L2(I)=L1(I)
          F2(I)=F1(I)
          IF(IV.NE.0) G2(I)=G1(I)
       ENDDO
       IRET=-1

       !  CONVERT GRIB2 GRID DEFINITION TEMPLATE TO GRIB1 GDS
       !  COMPUTE REGULARIZED GRIDS AND INTERPOLATE FIELD
    ELSE
       IDEFN=0
       IGDS=0
       IGDS(2)=K1
       IGDS(5)=NGDT1
       CALL GDT2GDS(IGDS,KGDT1,IDEFN1,IDEF1,KGDS1,IGI,IRET)
       IGDS(2)=K2
       IGDS(5)=NGDT2
       CALL GDT2GDS(IGDS,KGDT2,IDEFN,IDEF,KGDS2,IGI,IRET)
       K1F=LENGDSF(KGDS1,KGDS1F)
       IF(K1F.EQ.K1) K1F=1
       K2F=LENGDSF(KGDS2,KGDS2F)
       IF(K2F.EQ.K2) K2F=1
       MRL=MAX(K2,K2F)
       IF(IV.EQ.0) THEN
          MRO=1
       ELSE
          MRO=MRL
       ENDIF
       IF(K1F.GT.0.AND.K2F.GT.0) THEN
          CALL INTGRIB1(K1F,KGDS1F,K2F,KGDS2F,MRL,MRO, &
               IV,IP,IPOPT,KGDS1,K1,IB1,L1,F1,G1,KGDS2,K2, &
               IB2,L2,F2,G2,IRET)
       ELSE
          IRET=101
       ENDIF
    ENDIF

  END SUBROUTINE INTGRIB2

  !> Interpolate field.
  !>
  !> @param[in] k1f integer regularized input dimension
  !> @param[in] kgds1f integer (200) regularized input grid parameters
  !> @param[in] k2f integer regularized output dimension
  !> @param[in] kgds2f integer (200) regularized output grid parameters
  !> @param[in] mrl integer dimension of rlat and rlon
  !> @param[in] mro integer dimension of crot and srot
  !> @param[in] iv integer vector flag
  !> @param[in] ip integer interpolation type
  !> @param[in] ipopt integer (20) interpolation options
  !> @param[in] kgds1 integer (200) input grid parameters
  !> @param[in] k1 integer input dimension
  !> @param[in] ib1 integer input bitmap flag
  !> @param[in] l1 logical*1 (k1) input bitmap if ib1=1
  !> @param[in] f1 real (k1) input field
  !> @param[in] g1 real (k1) input y-component if iv=1
  !> @param[in] kgds2 integer (200) output grid parameters
  !> @param[in] k2 integer output dimension
  !> @param[in] ib2 integer output bitmap flag
  !> @param[in] l2 logical*1 (k2) output bitmap
  !> @param[in] f2 real (k2) output field
  !> @param[in] g2 real (k2) output y-component if iv=1
  !> @param[out] iret integer return code
  !>
  !> @author Iredell @date 96-07-19
  SUBROUTINE INTGRIB1(K1F,KGDS1F,K2F,KGDS2F,MRL,MRO, &
       IV,IP,IPOPT,KGDS1,K1,IB1,L1,F1,G1,KGDS2,K2, &
       IB2,L2,F2,G2,IRET)
#ifdef USEIPMOD
    USE IP_MOD
#endif
    INTEGER IPOPT(20)
    INTEGER KGDS1(200),KGDS2(200)
    LOGICAL*1 L1(K1),L2(K2)
    REAL F1(K1),F2(K2),G1(K1),G2(K2)
    INTEGER KGDS1F(200),KGDS2F(200)
    LOGICAL*1 L1F(K1F),L2F(K2F)
    REAL F1F(K1F),F2F(K2F),G1F(K1F),G2F(K2F)
    REAL RLAT(MRL),RLON(MRL),CROT(MRO),SROT(MRO)

    !  REGLR TO REGLR SCALAR
    IF(K1F.EQ.1.AND.K2F.EQ.1.AND.IV.EQ.0) THEN
       CALL IPOLATES(IP,IPOPT,KGDS1,KGDS2,K1,K2,1,IB1,L1,F1, &
            KI,RLAT,RLON,IB2,L2,F2,IRET)

       !  IRREG TO REGLR SCALAR
    ELSEIF(K1F.NE.1.AND.K2F.EQ.1.AND.IV.EQ.0) THEN
       CALL IPXWAFS2(1,K1,K1F,1, &
            KGDS1,IB1,L1,F1,KGDS1F,IB1F,L1F,F1F,IRET)
       IF(IRET.EQ.0) THEN
          CALL IPOLATES(IP,IPOPT,KGDS1F,KGDS2,K1F,K2,1,IB1F,L1F,F1F, &
               KI,RLAT,RLON,IB2,L2,F2,IRET)
       ENDIF

       !  REGLR TO IRREG SCALAR
    ELSEIF(K1F.EQ.1.AND.K2F.NE.1.AND.IV.EQ.0) THEN
       CALL IPOLATES(IP,IPOPT,KGDS1,KGDS2F,K1,K2F,1,IB1,L1,F1, &
            KI,RLAT,RLON,IB2F,L2F,F2F,IRET)
       IF(IRET.EQ.0) THEN
          CALL IPXWAFS2(-1,K2,K2F,1, &
               KGDS2,IB2,L2,F2,KGDS2F,IB2F,L2F,F2F,IRET)
       ENDIF

       !  IRREG TO IRREG SCALAR
    ELSEIF(K1F.NE.1.AND.K2F.NE.1.AND.IV.EQ.0) THEN
       CALL IPXWAFS2(1,K1,K1F,1, &
            KGDS1,IB1,L1,F1,KGDS1F,IB1F,L1F,F1F,IRET)
       IF(IRET.EQ.0) THEN
          CALL IPOLATES(IP,IPOPT,KGDS1F,KGDS2F,K1F,K2F,1,IB1F,L1F,F1F, &
               KI,RLAT,RLON,IB2F,L2F,F2F,IRET)
          IF(IRET.EQ.0) THEN
             CALL IPXWAFS2(-1,K2,K2F,1, &
                  KGDS2,IB2,L2,F2,KGDS2F,IB2F,L2F,F2F,IRET)
          ENDIF
       ENDIF

       !  REGLR TO REGLR VECTOR
    ELSEIF(K1F.EQ.1.AND.K2F.EQ.1.AND.IV.NE.0) THEN
       CALL IPOLATEV(IP,IPOPT,KGDS1,KGDS2,K1,K2,1,IB1,L1,F1,G1, &
            KI,RLAT,RLON,CROT,SROT,IB2,L2,F2,G2,IRET)
       IF(IRET.EQ.0.AND.KI.EQ.K2-1) THEN
          F2(K2)=0
          G2(K2)=0
       ENDIF

       !  IRREG TO REGLR VECTOR
    ELSEIF(K1F.NE.1.AND.K2F.EQ.1.AND.IV.NE.0) THEN
       CALL IPXWAFS2(1,K1,K1F,1, &
            KGDS1,IB1,L1,F1,KGDS1F,IB1F,L1F,F1F,IRET)
       CALL IPXWAFS2(1,K1,K1F,1, &
            KGDS1,IB1,L1,G1,KGDS1F,IB1F,L1F,G1F,IRET)
       IF(IRET.EQ.0) THEN
          CALL IPOLATEV(IP,IPOPT,KGDS1F,KGDS2,K1F,K2,1, &
               IB1F,L1F,F1F,G1F, &
               KI,RLAT,RLON,CROT,SROT,IB2,L2,F2,G2,IRET)
          IF(IRET.EQ.0.AND.KI.EQ.K2-1) THEN
             F2(K2)=0
             G2(K2)=0
          ENDIF
       ENDIF

       !  REGLR TO IRREG VECTOR
    ELSEIF(K1F.EQ.1.AND.K2F.NE.1.AND.IV.NE.0) THEN
       CALL IPOLATEV(IP,IPOPT,KGDS1,KGDS2F,K1,K2F,1,IB1,L1,F1,G1, &
            KI,RLAT,RLON,CROT,SROT,IB2F,L2F,F2F,G2F,IRET)
       IF(IRET.EQ.0) THEN
          CALL IPXWAFS2(-1,K2,K2F,1, &
               KGDS2,IB2,L2,F2,KGDS2F,IB2F,L2F,F2F,IRET)
          CALL IPXWAFS2(-1,K2,K2F,1, &
               KGDS2,IB2,L2,G2,KGDS2F,IB2F,L2F,G2F,IRET)
       ENDIF

       !  IRREG TO IRREG VECTOR
    ELSEIF(K1F.NE.1.AND.K2F.NE.1.AND.IV.NE.0) THEN
       CALL IPXWAFS2(1,K1,K1F,1, &
            KGDS1,IB1,L1,F1,KGDS1F,IB1F,L1F,F1F,IRET)
       CALL IPXWAFS2(1,K1,K1F,1, &
            KGDS1,IB1,L1,G1,KGDS1F,IB1F,L1F,G1F,IRET)
       IF(IRET.EQ.0) THEN
          CALL IPOLATEV(IP,IPOPT,KGDS1F,KGDS2F,K1F,K2F,1, &
               IB1F,L1F,F1F,G1F, &
               KI,RLAT,RLON,CROT,SROT,IB2F,L2F,F2F,G2F,IRET)
          IF(IRET.EQ.0) THEN
             CALL IPXWAFS2(-1,K2,K2F,1, &
                  KGDS2,IB2,L2,F2,KGDS2F,IB2F,L2F,F2F,IRET)
             CALL IPXWAFS2(-1,K2,K2F,1, &
                  KGDS2,IB2,L2,G2,KGDS2F,IB2F,L2F,G2F,IRET)
          ENDIF
       ENDIF
    ENDIF

  END SUBROUTINE INTGRIB1

  !> Return the length of a filled grid.
  !>
  !> Given a grid description section (in w3fi63 format), return the grid
  !> description section and size of its regularized counterpart. That is,
  !> if the input grid is regular, then itself is returned along with its
  !> grid size; however if the input grid is only quasi-regular (such as
  !> the wafs grids), then its filled regular version is returned along
  !> with its filled grid size.
  !>
  !> @param[in] kgds integer (200) gds parameters in w3fi63 format
  !> @param[out] kgdsf integer (200) regular gds parameters in w3fi63 format
  !>
  !> @return integer size of regularized grid
  !>
  !> @author Iredell @date 96-07-19
  FUNCTION LENGDSF(KGDS,KGDSF)
    INTEGER KGDS(200),KGDSF(200)

    IF(KGDS(1).EQ.201) THEN
       KGDSF=KGDS
       LENGDSF=KGDS(7)*KGDS(8)-KGDS(8)/2
    ELSEIF(KGDS(1).EQ.202) THEN
       KGDSF=KGDS
       LENGDSF=KGDS(7)*KGDS(8)
    ELSEIF(KGDS(19).EQ.0.AND.KGDS(20).NE.255) THEN
       CALL IPXWAFS(1,1,1,0,KGDS,DUM,KGDSF,DUMF,IRET)
       IF(IRET.EQ.0) THEN
          LENGDSF=KGDSF(2)*KGDSF(3)
       ELSE
          LENGDSF=0
       ENDIF
    ELSE
       KGDSF=KGDS
       LENGDSF=KGDS(2)*KGDS(3)
    ENDIF

  END FUNCTION LENGDSF

  !> Return the number of points in a grid.
  !>
  !> Given a grid definition template number and the grid definition
  !> template values, this function will return the number of grid points
  !> defined by the specified grid. If the grid template is not
  !> recognized, a negative value is returned.
  !>
  !> @param[in] igdtn integer grid definition template number
  !> @param[in] kgdt integer (200) grid definition template values
  !>
  !> @return integer number of grid points in grid
  !>
  !> @author Gilbert @date 2005-01-10
  FUNCTION NUMPTS(IGDTN,KGDT)
    INTEGER,INTENT(IN) :: IGDTN,KGDT(*)
    INTEGER :: ALLONES

    ALLONES = 0
    DO J=1,31
       ALLONES=IBSET(ALLONES,J)
    ENDDO

    SELECT CASE( IGDTN )
    CASE (0:3)   ! Lat/Lon
       IF ( KGDT(8).NE.ALLONES .AND. KGDT(9).NE.ALLONES ) THEN
          NUMPTS = KGDT(8) * KGDT(9)
       ELSE
          NUMPTS = -1
       ENDIF
    CASE (10)    ! Mercator
       IF ( KGDT(8).NE.ALLONES .AND. KGDT(9).NE.ALLONES ) THEN
          NUMPTS = KGDT(8) * KGDT(9)
       ELSE
          NUMPTS = -1
       ENDIF
    CASE (20)    ! Polar Stereographic
       IF ( KGDT(8).NE.ALLONES .AND. KGDT(9).NE.ALLONES ) THEN
          NUMPTS = KGDT(8) * KGDT(9)
       ELSE
          NUMPTS = -1
       ENDIF
    CASE (30)    ! Lambert Conformal
       IF ( KGDT(8).NE.ALLONES .AND. KGDT(9).NE.ALLONES ) THEN
          NUMPTS = KGDT(8) * KGDT(9)
       ELSE
          NUMPTS = -1
       ENDIF
    CASE (40:43)    ! Gaussian
       IF ( KGDT(8).NE.ALLONES .AND. KGDT(9).NE.ALLONES ) THEN
          NUMPTS = KGDT(8) * KGDT(9)
       ELSE
          NUMPTS = -1
       ENDIF
    CASE DEFAULT
       NUMPTS = -1
    END SELECT

  END FUNCTION NUMPTS
END PROGRAM COPYGB2
