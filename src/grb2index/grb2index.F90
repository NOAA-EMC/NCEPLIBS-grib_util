!> @file
!> Write a GRIB2 index file.
!> @author Iredell @date 1992-11-22

!> This program creates an index file from a GRIB2 file. The index file
!> serves as a table of contents for the GRIB file, enabling quick
!> access to the data. The GRIB file must be unblocked, but there can
!> be a gap before the first GRIB message of at most 32000 bytes and
!> gaps between messages of at most 4000 bytes. The two file names are
!> retrieved from the command line arguments. The first argument is the
!> name of the input GRIB file. The second argument is the name of the
!> output index file. For this program, only GRIB version 2 can be
!> read.
!>
!> The index file has two header records:
!> 1. an 81-byte header with 'GB2IX1' in columns 42-47
!> 2. an 81-byte header with number of bytes to skip before index
!> records, total length in bytes of the index records, number of
!> index records, and GRIB file basename written in format
!> ('IX1FORM:',3i10,2x,a40).
!>
!> Each record in the index table contains the following fields. All
!> integers are in big-endian format in the file.
!>
!> - byte 001 - 004 length of index record
!> - byte 005 - 008 bytes to skip in data file before grib message
!> - byte 009 - 012 bytes to skip in message before lus (local use) set = 0, if no local section.
!> - byte 013 - 016 bytes to skip in message before gds
!> - byte 017 - 020 bytes to skip in message before pds
!> - byte 021 - 024 bytes to skip in message before drs
!> - byte 025 - 028 bytes to skip in message before bms
!> - byte 029 - 032 bytes to skip in message before data section
!> - byte 033 - 040 bytes total in the message
!> - byte 041 - 041 grib version number (currently 2)
!> - byte 042 - 042 message discipline
!> - byte 043 - 044 field number within grib2 message
!> - byte 045 -  ii identification section (ids)
!> - byte ii+1-  jj grid definition section (gds)
!> - byte jj+1-  kk product definition section (pds)
!> - byte kk+1-  ll the data representation section (drs)
!> - byte ll+1-ll+6 first 6 bytes of the bit map section (bms)
!>`
!> ### Program History Log
!> Date | Programmer | Comments
!> -----|------------|---------
!> 92-11-22 | Iredell | Initial
!> 2002-01-03 | Gilbert | modified from program grbindex to work with GRIB2
!> 2005-02-25 | Gilbert | removed buffering option (see baseto).
!> 2012-08-01 | Vuong | changed hostname to hostnam
!>
!> @return
!> - 0 successful run
!> - 1 GRIB message not found
!> - 2 incorrect arguments
!> - 8 error accessing file
!>
!> @author Iredell @date 1992-11-22
PROGRAM GRB2INDEX
  PARAMETER(MSK1=32000,MSK2=4000)
  CHARACTER CGB*256,CGI*256
  CHARACTER(LEN=1),POINTER,DIMENSION(:) :: CBUF
  CHARACTER CARG*300
  INTEGER NARG,IARGC
  INTERFACE
     SUBROUTINE GETG2IR(LUGB,MSK1,MSK2,MNUM,CBUF,NLEN,NNUM, &
          NMESS,IRET)
       INTEGER,INTENT(IN) :: LUGB,MSK1,MSK2,MNUM
       CHARACTER(LEN=1),POINTER,DIMENSION(:) :: CBUF
       INTEGER,INTENT(OUT) :: NLEN,NNUM,NMESS,IRET
     END SUBROUTINE GETG2IR
  END INTERFACE

  !  GET ARGUMENTS
  NARG=IARGC()
  IF(NARG.NE.2) THEN
     CALL ERRMSG('grb2index:  Incorrect usage')
     CALL ERRMSG('Usage: grb2index gribfile indexfile')
     CALL ERREXIT(2)
  ENDIF
  CALL GETARG(1,CGB)
  NCGB=LEN_TRIM(CGB)
  CALL BAOPENR(11,CGB(1:NCGB),IOS)
  !CALL BASETO(1,1)
  IF(IOS.NE.0) THEN
     LCARG=LEN('grb2index:  Error accessing file '//CGB(1:NCGB))
     CARG(1:LCARG)='grb2index:  Error accessing file '//CGB(1:NCGB)
     CALL ERRMSG(CARG(1:LCARG))
     CALL ERREXIT(8)
  ENDIF
  CALL GETARG(2,CGI)
  NCGI=LEN_TRIM(CGI)
  CALL BAOPEN(31,CGI(1:NCGI),IOS)
  IF(IOS.NE.0) THEN
     LCARG=LEN('grb2index:  Error accessing file '//CGI(1:NCGI))
     CARG(1:LCARG)='grb2index:  Error accessing file '//CGI(1:NCGI)
     CALL ERRMSG(CARG(1:LCARG))
     CALL ERREXIT(8)
  ENDIF

  !  WRITE INDEX FILE
  MNUM=0
  CALL GETG2IR(11,MSK1,MSK2,MNUM,CBUF,NLEN,NNUM,NMESS,IRGI)
  IF(IRGI.GT.1.OR.NNUM.EQ.0.OR.NLEN.EQ.0) THEN
     CALL ERRMSG('grb2index:  No GRIB messages detected in file ' &
          //CGB(1:NCGB))
     CALL BACLOSE(11,IRET)
     CALL BACLOSE(31,IRET)
     CALL ERREXIT(1)
  ENDIF
  NUMTOT=NUMTOT+NNUM
  MNUM=MNUM+NMESS
  CALL WRGI1H(31,NLEN,NUMTOT,CGB(1:NCGB))
  IW=162
  CALL BAWRITE(31,IW,NLEN,KW,CBUF)
  IW=IW+NLEN

  !  EXTEND INDEX FILE IF INDEX BUFFER LENGTH TOO LARGE TO HOLD IN MEMORY
  IF(IRGI.EQ.1) THEN
     DO WHILE(IRGI.EQ.1.AND.NNUM.GT.0)
        IF (ASSOCIATED(CBUF)) THEN
           DEALLOCATE(CBUF)
           NULLIFY(CBUF)
        ENDIF
        CALL GETG2IR(11,MSK1,MSK2,MNUM,CBUF,NLEN,NNUM,NMESS,IRGI)
        IF(IRGI.LE.1.AND.NNUM.GT.0) THEN
           NUMTOT=NUMTOT+NNUM
           MNUM=MNUM+NMESS
           CALL BAWRITE(31,IW,NLEN,KW,CBUF)
           IW=IW+NLEN
        ENDIF
     ENDDO
     CALL WRGI1H(31,IW,NUMTOT,CGB(1:NCGB))
  ENDIF
  CALL BACLOSE(11,IRET)
  CALL BACLOSE(31,IRET)

END PROGRAM GRB2INDEX

!> Write index headers.
!>
!> ### Program History Log
!> Date | Programmer | Comments
!> -----|------------|---------
!> 95-10-31 | Iredell | modularize system calls
!> 2005-02-25 | Gilbert | et Header bytes  49-54 to blanks.
!> 2012-08-01 | Vuong | changed hostname to hostnam
!>
!> @param[in] lugi integer logical unit of output index file
!> @param[in] nlen integer total length of index records
!> @param[in] nnum integer number of index records
!> @param[in] cgb character name of GRIB file
!>
!> @author Iredell @date 93-11-22
SUBROUTINE WRGI1H(LUGI,NLEN,NNUM,CGB)
  CHARACTER CGB*(*)
#ifdef __GFORTRAN__
  CHARACTER CD8*8,CT10*10,HOSTNAME*15
  INTEGER ISTAT
#else
  CHARACTER CD8*8,CT10*10,HOSTNAM*15
#endif
  CHARACTER CHEAD(2)*81

  !  FILL FIRST 81-BYTE HEADER
  NCGB=LEN(CGB)
  NCGB1=NCBASE(CGB,NCGB)
  NCGB2=NCBASE(CGB,NCGB1-2)
  CALL DATE_AND_TIME(CD8,CT10)
  CHEAD(1)='!GFHDR!'
  CHEAD(1)(9:10)=' 1'
  CHEAD(1)(12:14)='  1'
  WRITE(CHEAD(1)(16:20),'(I5)') 162
  CHEAD(1)(22:31)=CD8(1:4)//'-'//CD8(5:6)//'-'//CD8(7:8)
  CHEAD(1)(33:40)=CT10(1:2)//':'//CT10(3:4)//':'//CT10(5:6)
  CHEAD(1)(42:47)='GB2IX1'
  !CHEAD(1)(49:54)=CGB(NCGB2:NCGB1-2)
  CHEAD(1)(49:54)='      '
#ifdef __GFORTRAN__
  ISTAT=HOSTNM(HOSTNAME)
  IF(ISTAT.eq.0) THEN
     CHEAD(1)(56:70)='0000'
  ELSE
     CHEAD(1)(56:70)='0001'
  ENDIF
#else
  CHEAD(1)(56:70)=HOSTNAM(HOSTNAME)
#endif
  CHEAD(1)(72:80)='grb2index'
  CHEAD(1)(81:81)=CHAR(10)

  !  FILL SECOND 81-BYTE HEADER
  CHEAD(2)='IX1FORM:'
  WRITE(CHEAD(2)(9:38),'(3I10)') 162,NLEN,NNUM
  CHEAD(2)(41:80)=CGB(NCGB1:NCGB)
  CHEAD(2)(81:81)=CHAR(10)

  !  WRITE HEADERS AT BEGINNING OF INDEX FILE
  CALL BAWRITE(LUGI,0,162,KW,CHEAD)

  RETURN
END SUBROUTINE WRGI1H

!> Locate basename of a file.
!>
!> This subprogram locates the character number after the last '/' in a
!> character string. For unix filenames, the character number returned
!> marks the beginning of the basename of the file.
!>
!> @param[in] c character string to search
!> @param[in] n integer length of string
!>
!> @return The index of the basename within the string.
!>
!> @author Iredell @date 93-11-22
FUNCTION NCBASE(C,N)
  CHARACTER C*(*)

  K=N
  DO WHILE(K.GE.1.AND.C(K:K).NE.'/')
     K=K-1
  ENDDO
  NCBASE=K+1

  RETURN
END FUNCTION NCBASE
