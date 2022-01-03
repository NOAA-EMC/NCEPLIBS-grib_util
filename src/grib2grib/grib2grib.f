!> @file
!> @brief Finds and extracts grib records from a grib file made
!> by gribawp1 for WFO (AWIPS).
!> @author Southall @date 96-05-21

!> Finds and extracts grib records from a grib file made
!> by gribawp1 for family of services.
!>
!> Control cards can be used to read and extract all or selected grib
!> records from a grib file. To extract only a selected number of grib
!> records from a grib file, a control card with a pds can be used. If
!> the pds in the control card is found in the input grib file, the
!> entire grib message 'grib' to '7777' is written to the designated
!> output file. Control cards can also be used to extract grib records
!> according to their grid type.
!>
!> @note This program was derived from the 'unpkgrb1.f' code but does
!> not unpack any data.
!>
!> ### Program History Log
!> Date | Programmer | Comments
!> -----|------------|---------
!> 96-05-21 | Southall | Initial
!> 98-05-07 | Johnson | Changes
!> 98-07-01 | Vuong | converted to fortran 90 and y2k compliant removed calls to w3log
!> 99-05-19 | Vuong | converted to ibm rs/6000 sp
!> 01-05-18 | Vuong | increasing the size of work array to handle the larger grib file.
!> 2012-08-09 | Vuong | modified to use module ifport
!>
!> ### Input Files
!> - 5 input control cards (3 types)
!>   - format (i1,i3,a44)
!>   - format (a44)
!>   - format (6(2x,4z2),2x,i1)
!> - 11 internal unit number of awip grib input file
!>
!> ### Output Files
!> - 6 print output (standard output - fortran)
!> - 51 1 write for each grib record entire grib message, 'grib' to '7777'
!>
!> @return
!> - 0    successful run
!> - 10   error on open of control card file
!> - 20   error on open of input grib file
!> - 30   error on open of output packed grib file
!> - 40   error, input grib file is too big, change program
!> - 60   error reading grib file
!> - 99   i/o error using stat function
!> - 100  programmer forgot control cards
!> - 110  no card with ffffffff showing end of pds cards
!>
!> ### Warning
!> On a CRAY you may have to use assign cards.  If your job prints the
!> file name and file byte size O.K. but quits with an OPEN INPUT FILE
!> ERROR, use these assign cards (at the prompt).
!> 
!> <pre>
!>            $ assign -R
!>            $ assign -a input-file-name -s sbin input-file-name
!> </pre>
!>            
!> ### Examples:  Control Cards in 'grib2grib.dat' File
!>
!> #### Example 1.
!> To extract selected records (w/out unpacking them) from file
!> xtrn.eta24 and xtrn.mrf144 using cards read in with the PDS of the
!> record. After the last card with a PDS, the next card must have
!> FFFFFFFF starting in column 3.  Get the 500 mb HGT and 500 mb TMP
!> (1st 2 PDS's) and the 850 mb RH and the MSL (2nd 2 PDS's) and write
!> the packed data into eta.packed.dat and mso.packed.dat respectively.
!> The PDS's of these fields and other can be obtained by getting an
!> inventory of the input grib files xtrn.eta24 and xtrn.mso30.  Column
!> 63 is set to 2 so that only 11 of the 1st 12 bytes are used to find
!> records. For more info. concerning the number in Col. 63 refer to the
!> subroutine "iw3pds.f"
!>
!> <pre>      
!>  col. 1
!>       0000xtrn.eta24
!>       eta.packed.dat
!>         00001C02  0759D380  076401F4  00000000  00000000  00000000  2
!>         00001C02  0759D380  0B6401F4  00000000  00000000  00000000  2
!>         FFFFFFFF  00000000  00000000  00000000  00000000  00000000  0
!>       0000xtrn.mso30
!>       mso.packed.dat
!>         00001C02  0755D4C0  34640352  00000000  00000000  00000000  2
!>         00001C02  074EC980  02660000  00000000  00000000  00000000  2
!>         FFFFFFFF  00000000  00000000  00000000  00000000  00000000  0
!> </pre>      
!>
!> #### Example 2.
!> To extract only the grib messages from xtrn.mrf144 and write them
!> (without unpacking) into mrf.packed.dat.
!>
!> <pre>      
!>  col. 1
!>       1000xtrn.mrf144
!>       mrf.packed.dat
!> </pre>      
!>
!> #### Example 3.
!> To extract all grib records in the file xtrn.mrf144 by grid type 201
!> and 203 placing them into mrf201.dat and mrf203.dat respectively.
!>
!> <pre>      
!>  col. 1
!>       2201xtrn.mrf144
!>       mrf201.dat
!>       2203xtrn.mrf144
!>       mrf203.dat
!> </pre>      
!>
!> @author Southall @date 96-05-21
      PROGRAM GRIB2GRIB
#ifdef __INTEL__
      USE IFPORT
#endif
      PARAMETER          (MXSIZE=300000)
      PARAMETER          (MSIZE=MXSIZE*3)
      PARAMETER          (IBUFSIZE=35000000)
!
!      IBUF is set bigger than the largest GRIB file you may read.
!
      CHARACTER * 1      IBUF(IBUFSIZE)
      CHARACTER * 28     V1(900)
      CHARACTER * 2      HEXPDS(24)
      CHARACTER * 44     IFILE5
      CHARACTER * 44     NFILE
      CHARACTER * 1      MSGA(MSIZE)
      CHARACTER * 44     OFILE
      CHARACTER * 1      IPDS(24)
!
      INTEGER            JPDS(4)
      INTEGER            RCBYTE(900)
      INTEGER            ISTART(900)
      INTEGER            IUNPK(900)
#ifdef __GFORTRAN__
      INTEGER*8          JSTAT(13)
#else
      INTEGER*4          JSTAT(12)
#endif
      INTEGER            TYPEGRID
!
      LOGICAL            IW3PDS
!
!
      SAVE
!
      DATA  IFILE5/'grib2grib.dat'/
      DATA  JJJ   /0/

      CALL W3TAGB('GRIB2GRIB',2001,0138,0064,'NP11') 
      KKK = 0
!
!          OPEN CONTROL CARD FILE
!
      OPEN (UNIT=5,FILE=IFILE5,STATUS='OLD',
     & FORM='FORMATTED',IOSTAT=MERR)
      IF (MERR.NE.0) THEN
         PRINT *,' ERROR OPENING CONTROL CARD FILE = ',IFILE5
         CALL ERREXIT (10)
      ENDIF
!
!      PARAMETERS ON CONTROL CARD TYPE 1
!
!      IALL  = 0    WRITE ONLY SELECTED RECORDS USING CARDS WITH PDS
!      IALL  = 1    WRITE ALL RECORDS IN FILE
!      IALL  = 2    WRITE ONLY ONE GRID TYPE
!
   10 CONTINUE
      READ (5,'(I1,I3,A44)',END=100) IALL, TYPEGRID, NFILE
      PRINT 15, IALL, TYPEGRID
   15 FORMAT (' IALL TYPEGRID = ',I1,I4)
      WRITE(*,'(A,A10)')' INPUT AWIPS FILE = ',NFILE
      KKK = KKK + 1
!
!        READ OUTPUT FILE CARD WITH FILE NAME
!
      JJJ = JJJ + 1
      READ (5,'(A44)',END=100) OFILE
      WRITE(6,'(A,A10)')' OUTPUT FILE FOR PACKED GRIB MESSAGES = ',OFILE
!
      IF (IALL.EQ.2.AND.TYPEGRID.EQ.0) THEN
         PRINT *,'ERROR, IALL = 2, AND TYPEGRID = 0'
         GO TO 10
      END IF
!
!        CALL FUNCTION STAT TO FIND NUMBER OF BYTES IN FILE
!        WORD 8 OF ARRAY JSTAT HAS THE NUMBER OF BYTES IN FILE
!        IF YOUR COMPUTER DOES NOT HAVE THE C FUNCTION STAT
!        COMMENT OUT THE NEXT 7 LINES
!
!        STRING VALUES IN THE C LANGUAGE ARE TERMINATED WITH
!        NULL CHARACTER CHAR(0)
!
      NFILE = TRIM(NFILE)//CHAR(0)

      IF (STAT(NFILE,JSTAT).NE.0) THEN
         PRINT *,'ERROR IN FUNCTION STAT GETTING FILE STATS'
         CALL ERREXIT (99)
      ELSE
         KBYTES = JSTAT(8)
         WRITE(6,'(A,I0)')' NUMBER OF BYTES IN GRIB FILE   = ',KBYTES
      END IF
!
!        TEST TO SEE IF INPUT GRIB FILE IS TOO BIG
!
      IF (IBUFSIZE.LT.KBYTES) THEN
         PRINT *,'GRIB INPUT FILE IS TO BIG FOR GRIB2GRIB PROGRAM'
         PRINT *,'CHANGE PROGRAM SO PARAMETER IBUFSIZE IS LARGER'
         PRINT *,'THAN THE NUMBER OF BYTES IN THE FILE'
         CALL ERREXIT (40)
      END IF
!
!       OPEN INPUT GRIB FILE - ERRORS?  read WARNING above
!
      OPEN (UNIT=11,FILE=NFILE,STATUS='OLD',ACCESS='DIRECT',
     & FORM='UNFORMATTED',IOSTAT=MERR,RECL=KBYTES)
      IF (MERR.NE.0) THEN
         PRINT *,'OPEN INPUT FILE ERROR ON FILE = ', NFILE
         PRINT *,'ERROR = ',MERR
         CALL ERREXIT (20)
      END IF
!
!
!       OPEN OUTPUT FILE FOR PACKED GRIB DATA IN FLOATING POINT
!
!     OPEN (UNIT=51,FILE=OFILE,STATUS='unknown',
!    & FORM='UNFORMATTED',ACCESS='SEQUENTIAL',IOSTAT=MERR)

      CALL BAOPENW(51,OFILE,IRET)
      IF (IRET.NE.0) THEN
         PRINT *,'OPEN OUTPUT FILE ERROR ON FILE = ', OFILE
         PRINT *,'ERROR = ',IRET
         CALL ERREXIT(30)
      END IF
!
!        Read the entire GRIB file into array IBUF
!        On SUN and SGI the stat function works, when RECL=1
!        REC is a pointer at what byte in the file to start
!        reading at. The SUN and SGI compilers compile this as
!        one big read, other compilers, such as the INTERGRAPH,
!        give a compile error, or run time error.
!        If so, comment out this line, uncomment the code that
!        that applies to your computer. This works, but is very
!        slow because you are doing one read for each byte in
!        file.  Replace this if your computer has a better way of
!        doing it. This should also work if RECL=KBYTES.
!
      READ (11,REC=1,ERR=60) (IBUF(I),I=1,KBYTES)
!
!      Use this if you have no stat function or system function
!      This is very slow.
!
!      I = 1
! 1    CONTINUE
!        READ (11,REC=I,IOSTAT=IERR) IBUF(I)
!        IF (IERR.EQ.0) THEN
!          I = I + 1
!          GO TO 1
!        ELSE
!          KBYTES = I - 1
!          PRINT *,'NUMBER OF BYTES IN GRIB FILE   = ',KBYTES
!        END IF
!
!      Use this if you have stat or system function and RECL=1 but
!      the read does not work. This is very slow.
!
!      DO I = 1,KBYTES
!        READ (11,REC=I,ERR=60) IBUF(I)
!      END DO
!
!  -------------------  FIND  all 'GRIB' in file, save address
!  -------------------  PDS, length of grib message.
!
      JSTART = 1
      NN = 0
      DO 101 I = JSTART, KBYTES
        IF (MOVA2I(IBUF(I  )).NE.71) GO TO 101
        IF (MOVA2I(IBUF(I+1)).NE.82) GO TO 101
        IF (MOVA2I(IBUF(I+2)).NE.73) GO TO 101
        IF (MOVA2I(IBUF(I+3)).NE.66) GO TO 101
        IF (MOVA2I(IBUF(I+7)).NE.1)  GO TO 101
        NN = NN + 1
           DO J = I,I+27
             V1(NN)(J-I+1:J-I+1) = IBUF(J+8)
           END DO

        RCBYTE(NN) = MOVA2I(IBUF(I+4)) * 65536 + MOVA2I(IBUF(I+5)) *
     &  256 + MOVA2I(IBUF(I+6))
        JSTART     = I + RCBYTE(NN)
        ISTART(nn) = I
!       PRINT *,'NN,I,JSTART,RCBYTE(NN) = ',NN,I,JSTART,RCBYTE(NN)
  101 CONTINUE
!
      WRITE(6,'(A34,I0)') 'NUMBER OF GRIB RECORDS IN FILE = ',NN
!
!
      NUMREC = NN
!
!
!      WRITE ALL GRIB RECORDS OR JUST THOSE OF A CERTAIN GRID TYPE
!
      IF (IALL.EQ.1.OR.IALL.EQ.2) THEN
         NREC = 0
         IF (IALL.EQ.1) PRINT *,'ALL GRIB RECORDS IN THE FILE'
         IF (IALL.EQ.2) THEN
            PRINT *,'IALL = 2, OUTPUT ONLY RECORDS OF GRID TYPE ',
     &      TYPEGRID
            PRINT *,' '
         END IF
!
!     PROCESS ALL GRIB RECORDS, IF IALL=2, SAVE ONLY CERTAIN GRID TYPES
!
         DO 30 K =1,NUMREC
           MBYTES = RCBYTE(K)
           KSTART = ISTART(K)
           IF (IALL.EQ.2) THEN
              IGRID = MOVA2I(IBUF(KSTART+14))
!             PRINT *,'IGRID = ',IGRID
              IF (TYPEGRID.NE.IGRID) GO TO 30
           END IF
!
!          MOVE GRIB RECORD SO IT IS ON WORD BOUNDARY
!
           CALL XMOVEX(MSGA,IBUF(KSTART),MBYTES)
           NGRID = IGRID
!
!
           IF (IALL.EQ.2) THEN
              PRINT 29, K, NGRID,OFILE
   29         FORMAT (3X,'GRIB RECORD #',I4,' IS OF GRID TYPE: ',
     &        I4,', WRITING ENTIRE GRIB MESSAGE ','TO ',A44)
           ENDIF
!          WRITE (51) (IBUF(M),M=KSTART,KSTART+MBYTES)
           CALL WRYTE(51, MBYTES+1, IBUF(KSTART))
           NREC = NREC + 1
!
!
   30    CONTINUE
      ENDIF
!
!      READ ONLY SELECTED RECORDS BY CONTROL CARD, CONTROL CARD
!      HAS PDS, INDEX RECORD IS SEARCHED FOR MATCH OF PDS, WITH
!      OR WITHOUT USING THE DATE IN THE PDS.  KEY = 0 IF YOU DO
!      NOT USE DATE IN PRODUCT DEFINITION SECTION (PDS).
!
      IF (IALL.EQ.0) THEN
         DO I = 1,NUMREC
           IUNPK(I) = 0
         END DO
         N      = 0
         ICARDS = 0
!
   32    CONTINUE
         READ (5,'(6(2X,4A2),2X,I1)',END=110) HEXPDS, KEY
         DO J = 1,24
           CALL HEXCHAR(HEXPDS(J),IPDS(J),IER)
           IF (IER.EQ.1) PRINT *,'HEXCHAR ERROR '
         END DO
!
!     TEST FOR  FFFFFFFF, END OF (PDS) CARDS.
!
         IF (MOVA2I(IPDS(1)).EQ.255) GO TO 36
         N = N + 1
         PRINT 33, HEXPDS, KEY
   33    FORMAT (' PDS = ',6(2X,4A2),2X,I1)
         DO 35 K = 1,NUMREC
           IF (IW3PDS(V1(K),IPDS,KEY)) THEN
              ICARDS        = ICARDS + 1
              IUNPK(ICARDS) = K
!             print *,'IUNPK(',icards,') = ',k
              GO TO 32
           ENDIF
   35    CONTINUE
         PRINT 37, HEXPDS, KEY
   37    FORMAT (' CAN NOT FIND PDS = ',6(2X,4A2),2X,I1)
         IF (N.LE.NUMREC) GO TO 32
         IF (N.GT.NUMREC) THEN
            PRINT *,' POSSIBLE ERROR, YOU HAVE MORE PDS CARDS'
            PRINT *,' THEN RECORDS IN FILE. YOU MAY WANT A '
            PRINT *,' DUPLICATE RECORD, I WILL CONTINUE.'
            GO TO 32
         END IF
   36    CONTINUE
         PRINT *,' '
      ENDIF
!
!       WRITE ONLY SELECTED PACKED GRIB RECORDS TO OUTPUT FILE
!
      IF (IALL.EQ.0) THEN
         IF (ICARDS.EQ.0) GO TO 50
         NREC = 0
         DO 40 K = 1,ICARDS
           MBYTES = RCBYTE(IUNPK(K))
           KSTART = ISTART(IUNPK(K))
!
!          MOVE GRIB RECORD SO IT IS ON WORD BOUNDARY
!
           CALL XMOVEX(MSGA,IBUF(KSTART),MBYTES)
!
           PRINT 39, K, IUNPK(K) ,OFILE
   39      FORMAT ('PDS FOR CONTROL CARD ',I2,' MATCHES THAT OF GRIB ',
     &     'RECORD # ',I3,', WRITE ENTIRE GRIB MESSAGE TO: ',A44)
!          WRITE (51) (IBUF(M),M=KSTART,KSTART+MBYTES)
           CALL WRYTE(51, MBYTES+1, IBUF(KSTART))
           NREC = NREC + 1
!
   40    CONTINUE
      ENDIF
!
   50 CONTINUE
      PRINT *,' '
      IF (NREC.NE.0) THEN
         WRITE(6,'(A7,I0,A22,A)')' WROTE ',NREC,
     &                ' GRIB RECORDS IN FILE ',OFILE(1:30)
      ENDIF
      PRINT *,' '
!
      CLOSE (UNIT=11)
!     END FILE 51
      CALL BACLOSE(51,IRET)
!     CLOSE (UNIT=51)
      GO TO 10
!
   60 CONTINUE
      PRINT *,'READ ERROR READING GRIB FILE, ERROR = ',JERR
      CALL ERREXIT (60)
!
  100 CONTINUE
      CALL W3TAGE('GRIB2GRIB') 
      IF (KKK.EQ.0) THEN
         PRINT *,'EOF READING 1ST CONTROL CARD, FILE = ',IFILE5
         CALL ERREXIT (100)
      ENDIF
      STOP
!
  110 CONTINUE
      PRINT *,' EOF READING CONTROL CARDS WITH PDS '
      CALL ERREXIT (110)
      END
