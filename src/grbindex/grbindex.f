!> @file
!> Create an index file from a grib file.
!> @author Iredell @date 1998-09-15

!> Create an index file from a grib file.
!>      
!> The index file serves as a table of contents for the grib file,
!> enabling quick access to the data. The grib file must be unblocked,
!> but there can be a gap before the first grib message of at most 32000
!> bytes and gaps between messages of at most 4000 bytes. The two file
!> names are retrieved from the command line arguments. The first
!> argument is the name of the input grib file. The second argument is
!> the name of the output index file.
!>
!> Currently, only version 1 of grib can be read.
!>
!> Version 1 of the index file has the following format:
!>      
!> 81-byte s.lord header with 'gb1ix1' in columns 42-47 followed by
!> 81-byte header with number of bytes to skip before index records,
!> number of bytes in each index record, number of index records, and
!> grib file basename written in format ('ix1form:',3i10,2x,a40).
!>      
!> Each following index record corresponds to a grib message
!> and has the internal format:
!> -  byte 001-004: bytes to skip in data file before grib message
!> -  byte 005-008: bytes to skip in message before pds
!> -  byte 009-012: bytes to skip in message before gds (0 if no gds)
!> -  byte 013-016: bytes to skip in message before bms (0 if no bms)
!> -  byte 017-020: bytes to skip in message before bds
!> -  byte 021-024: bytes total in the message
!> -  byte 025-025: grib version number
!> -  byte 026-053: product definition section (pds)
!> -  byte 054-095: grid definition section (gds) (or nulls)
!> -  byte 096-101: first part of the bit map section (bms) (or nulls)
!> -  byte 102-112: first part of the binary data section (bds)
!> -  byte 113-172: (optional) bytes 41-100 of the pds
!> -  byte 173-184: (optional) bytes 29-40 of the pds
!> -  byte 185-320: (optional) bytes 43-178 of the gds
!>
!> ### Program History Log
!> Date | Programmer | Comments
!> -----|------------|---------
!> 92-11-22 | Iredell | Initial
!> 94-06-08 | Ebisuzaki | eliminate ishell calls
!> 94-08-26 | Iredell | 40 byte pds extension
!> 95-10-31 | Iredell | considerably reduce i/o
!> 96-10-31 | Iredell | augmented optional definitions to byte 320
!> 1999-04-27 | Gilbert | Changed CALL EXIT(N) to CALL ERREXIT(N) to pass proper error code
!> 2012-07-31 | Vuong | changed hostname to hostnam
!>
!> ### Input File
!> - gribfile unblocked grib file
!>
!> ### Output File
!> -  indexfile    unblocked index file
!>
!> @return
!> - 0 - successful run
!> - 1 - grib message not found
!> - 2 - incorrect arguments
!> - 8 - error accessing file
!>
!> @author Iredell @date 1998-09-15      
      PROGRAM GRBINDEX
      PARAMETER(MSK1=32000,MSK2=4000)
      CHARACTER CGB*256,CGI*256
      PARAMETER(MBUF=256*1024)
      CHARACTER CBUF(MBUF)
      CHARACTER CARG*300
      INTEGER NARG,IARGC

!  GET ARGUMENTS
      NARG=IARGC()
      IF(NARG.NE.2) THEN
        CALL ERRMSG('grbindex:  Incorrect usage')
        CALL ERRMSG('Usage: grbindex gribfile indexfile')
        CALL ERREXIT(2)
      ENDIF
      CALL GETARG(1,CGB)
      NCGB=LEN_TRIM(CGB)
      CALL BAOPENR(11,CGB(1:NCGB),IOS)
      CALL BASETO(1,1)
      IF(IOS.NE.0) THEN
        LCARG=LEN('grbindex:  Error accessing file '//CGB(1:NCGB))
        CARG(1:LCARG)='grbindex:  Error accessing file '//CGB(1:NCGB)
        CALL ERRMSG(CARG(1:LCARG))
        CALL ERREXIT(8)
      ENDIF
      CALL GETARG(2,CGI)
      NCGI=LEN_TRIM(CGI)
      CALL BAOPEN(31,CGI(1:NCGI),IOS)
      IF(IOS.NE.0) THEN
        LCARG=LEN('grbindex:  Error accessing file '//CGI(1:NCGI))
        CARG(1:LCARG)='grbindex:  Error accessing file '//CGI(1:NCGI)
        CALL ERRMSG(CARG(1:LCARG))
        CALL ERREXIT(8)
      ENDIF

!  WRITE INDEX FILE
      MNUM=0
      CALL GETGIR(11,MSK1,MSK2,MNUM,MBUF,CBUF,NLEN,NNUM,IRGI)
      IF(IRGI.GT.1.OR.NNUM.EQ.0) THEN
        CALL ERRMSG('grbindex:  No GRIB messages detected in file '
     &              //CGB(1:NCGB))
        CALL BACLOSE(11,IRET)
        CALL BACLOSE(31,IRET)
        CALL ERREXIT(1)
      ENDIF
      MNUM=MNUM+NNUM
      CALL WRGI1H(31,NLEN,MNUM,CGB(1:NCGB))
      IW=162
      CALL BAWRITE(31,IW,NLEN*NNUM,KW,CBUF)
      IW=IW+NLEN*NNUM

!  EXTEND INDEX FILE IF INDEX BUFFER LENGTH GREATER THAN MBUF
      IF(IRGI.EQ.1) THEN
        DOWHILE(IRGI.EQ.1.AND.NNUM.GT.0)
          CALL GETGIR(11,MSK1,MSK2,MNUM,MBUF,CBUF,NLEN,NNUM,IRGI)
          IF(IRGI.LE.1.AND.NNUM.GT.0) THEN
            MNUM=MNUM+NNUM
            CALL BAWRITE(31,IW,NLEN*NNUM,KW,CBUF)
            IW=IW+NLEN*NNUM
          ENDIF
        ENDDO
        CALL WRGI1H(31,NLEN,MNUM,CGB(1:NCGB))
      ENDIF
      CALL BACLOSE(11,IRET)
      CALL BACLOSE(31,IRET)

      END

!> Write index headers.
!>
!> This subprogram writes two index headers. Currently, the length of
!> each index record is 152.
!>
!> ### Program History Log
!> Date | Programmer | Comments
!> -----|------------|---------
!> 93-11-22 | Iredell | Initial
!> 95-10-31 | Iredell | Modularize system calls
!> 2012-07-31 | Vuong | Changed hostname to hostnam
!>
!> @param[in] lugi integer logical unit of output index file
!> @param[in] nlen integer length of index records
!> @param[in] nnum integer number of index records
!> @param[in] cgb character name of grib file
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
      CHEAD(1)(42:47)='GB1IX1'
      CHEAD(1)(49:54)=CGB(NCGB2:NCGB1-2)
#ifdef __GFORTRAN__
      ISTAT=HOSTNM(HOSTNAME)
      IF(ISTAT.eq.0) THEN
        CHEAD(1)(56:70)='0000'
      ELSE
        CHEAD(1)(56:70)='0001'
      ENDIF
#else
      CHEAD(1)(56:70)=HOSTNAM(hostname)
#endif
!     print*,' CHEAD(1)(56:70) = ', CHEAD(1)(56:70)
      CHEAD(1)(72:80)='grbindex '
      CHEAD(1)(81:81)=CHAR(10)

!  FILL SECOND 81-BYTE HEADER
      CHEAD(2)='IX1FORM:'
      WRITE(CHEAD(2)(9:38),'(3I10)') 162,NLEN,NNUM
      CHEAD(2)(41:80)=CGB(NCGB1:NCGB)
      CHEAD(2)(81:81)=CHAR(10)

!  WRITE HEADERS AT BEGINNING OF INDEX FILE
      CALL BAWRITE(LUGI,0,162,KW,CHEAD)

      RETURN
      END

!> Locate basename of a file.
!>
!> This subprogram locates the character number after the last in a
!> character string. For unix filenames, the character number returned
!> marks the beginning of the basename of the file.
!>
!> @param[in] C CHARACTER STRING TO SEARCH
!> @param[in] N INTEGER LENGTH OF STRING
!>
!> @author Iredell @date 93-11-22
      FUNCTION NCBASE(C,N)
      CHARACTER C*(*)

      K=N
      DOWHILE(K.GE.1.AND.C(K:K).NE.'/')
        K=K-1
      ENDDO
      NCBASE=K+1

      RETURN
      END
