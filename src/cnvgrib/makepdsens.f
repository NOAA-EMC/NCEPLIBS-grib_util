!> @file
!>                .     .   .                                      .
!> @author Gilbert @date 2003-06-12
!
!>  This routine creates the GRIB1 NCEP Ensemble PDS
!>  extension information from appropriate information from a GRIB2 
!>   Product Definition Template.
!>
!> PROGRAM HISTORY LOG:
!> 2003-06-12  Gilbert
!> 2007-05-14  Boi Vuong  -Corrected scale factor probabilities 
!> 2010-07-26  Boi Vuong  -Added two type of ensemblers (4 and 192)
!>
!> USAGE:    CALL makepdsens(ipdsnum,ipdstmpl,kpds,kens,kprob,
!>                        xprob,kclust,kmembr,iret)
!>   INPUT ARGUMENT LIST:
!>     ipdsnum    - GRIB2 Product Definition Template Number
!>     ipdstmpl() - GRIB2 Product Definition Template entries for PDT 4.ipdsnum
!>     kpds()     - GRIB1 PDS info as specified in W3FI63.
!>          (1)   - ID OF CENTER
!>          (2)   - GENERATING PROCESS ID NUMBER
!>          (3)   - GRID DEFINITION
!>          (4)   - GDS/BMS FLAG (RIGHT ADJ COPY OF OCTET 8)
!>          (5)   - INDICATOR OF PARAMETER
!>          (6)   - TYPE OF LEVEL
!>          (7)   - HEIGHT/PRESSURE , ETC OF LEVEL
!>          (8)   - YEAR INCLUDING (CENTURY-1)
!>          (9)   - MONTH OF YEAR
!>          (10)  - DAY OF MONTH
!>          (11)  - HOUR OF DAY
!>          (12)  - MINUTE OF HOUR
!>          (13)  - INDICATOR OF FORECAST TIME UNIT
!>          (14)  - TIME RANGE 1
!>          (15)  - TIME RANGE 2
!>          (16)  - TIME RANGE FLAG
!>          (17)  - NUMBER INCLUDED IN AVERAGE
!>          (18)  - VERSION NR OF GRIB SPECIFICATION
!>          (19)  - VERSION NR OF PARAMETER TABLE
!>          (20)  - NR MISSING FROM AVERAGE/ACCUMULATION
!>          (21)  - CENTURY OF REFERENCE TIME OF DATA
!>          (22)  - UNITS DECIMAL SCALE FACTOR
!>          (23)  - SUBCENTER NUMBER
!>
!>   OUTPUT ARGUMENT LIST:
!>     kpds()     - GRIB1 PDS info as specified in W3FI63.
!>          (1)   - ID OF CENTER
!>          (2)   - GENERATING PROCESS ID NUMBER
!>          (3)   - GRID DEFINITION
!>          (4)   - GDS/BMS FLAG (RIGHT ADJ COPY OF OCTET 8)
!>          (5)   - INDICATOR OF PARAMETER
!>          (6)   - TYPE OF LEVEL
!>          (7)   - HEIGHT/PRESSURE , ETC OF LEVEL
!>          (8)   - YEAR INCLUDING (CENTURY-1)
!>          (9)   - MONTH OF YEAR
!>          (10)  - DAY OF MONTH
!>          (11)  - HOUR OF DAY
!>          (12)  - MINUTE OF HOUR
!>          (13)  - INDICATOR OF FORECAST TIME UNIT
!>          (14)  - TIME RANGE 1
!>          (15)  - TIME RANGE 2
!>          (16)  - TIME RANGE FLAG
!>          (17)  - NUMBER INCLUDED IN AVERAGE
!>          (18)  - VERSION NR OF GRIB SPECIFICATION
!>          (19)  - VERSION NR OF PARAMETER TABLE
!>          (20)  - NR MISSING FROM AVERAGE/ACCUMULATION
!>          (21)  - CENTURY OF REFERENCE TIME OF DATA
!>          (22)  - UNITS DECIMAL SCALE FACTOR
!>          (23)  - SUBCENTER NUMBER
!>     kens()     - Ensemble identification for PDS octets 41-45
!>     kprob()    - Ensemble probability info for PDS octets 46 & 47
!>     xprob()    - Ensemble probability info for PDS octets 48-55
!>     kclust()   - Ensemble cluster info for PDS octets 61-76
!>     kmembr()   - Ensemble membership info for PDS octest 77-86
!>     iret       - Error return value:
!>                  0  = Successful
!>                  2  = Unrecognized GRIB2 PDT 4.ipdsnum
!>
!> REMARKS:  Use pds2pdtens for ensemble related PDS
!>
!> ATTRIBUTES:
!>   LANGUAGE: Fortran 90
!>   MACHINE:  IBM SP
!>
!>
      subroutine makepdsens(ipdsnum,ipdstmpl,kpds,kens,kprob,
     &                     xprob,kclust,kmembr,iret)

        
        use params

        integer,intent(in) :: ipdstmpl(*)
        integer,intent(in) :: ipdsnum
        integer,intent(inout) :: kpds(*)
        integer,intent(out) :: kens(5),kprob(2)
        integer,intent(out) :: kclust(16),kmembr(80)
        real,intent(out) :: xprob(2)
        integer,intent(out) :: iret

        iret=0
        kpds(23)=2          !  subcenter = ensemble

        kens(1:5)=0
        kprob(1:2)=0
        xprob(1:2)=0.
        kclust(1:16)=0
        kmembr(1:80)=0
        !
        !  Individual Ensemble Fcst
        !
        if ( ipdsnum.eq.1.OR.ipdsnum.eq.11 ) then
           kens(1)=1
           selectcase ( ipdstmpl(16) )
             case(0)
                kens(2)=1
                kens(3)=1
             case(1)
                kens(2)=1
                kens(3)=2
             case(2)
                kens(2)=2
                kens(3)=ipdstmpl(17)
             case(3)
                kens(2)=3
                kens(3)=ipdstmpl(17)
             case(4)
                kens(2)=3
                kens(3)=ipdstmpl(17)
             case(192)
                kens(2)=3
                kens(3)=ipdstmpl(17)
           end select
           kens(4)=1
           kens(5)=255
        !
        !  Probability Fcst
        !
        elseif ( ipdsnum.eq.5.OR.ipdsnum.eq.9 ) then
           kens(1)=1
           kens(2)=5
           kens(3)=0
           kens(4)=0
           kens(5)=255
           kprob(1)=kpds(5)
           kpds(5)=191
           kprob(2)=ipdstmpl(18)+1
           if ( kprob(2).eq.1 ) then
              rscale=10.**ipdstmpl(19)
              xprob(1)=real(ipdstmpl(20))/rscale
              xprob(2)=0.0
           elseif ( kprob(2).eq.2 ) then
              xprob(1)=0.0
              rscale=10.**ipdstmpl(21)
              xprob(2)=real(ipdstmpl(22))/rscale
           elseif ( kprob(2).eq.3 ) then
              rscale=10.**ipdstmpl(19)
              xprob(1)=real(ipdstmpl(20))/rscale
              rscale=10.**ipdstmpl(21)
              xprob(2)=real(ipdstmpl(22))/rscale
           endif
           kclust(1)=ipdstmpl(17)
        !
        !  Derived Ensemble Fcst
        !
        elseif ( ipdsnum.eq.2.OR.ipdsnum.eq.12 ) then
           kens(1)=1
           kens(2)=5
           kens(3)=0
           selectcase ( ipdstmpl(16) )
             case(0)
                kens(4)=1
             case(1)
                kens(4)=2
             case(2)
                kens(4)=11
             case(3)
                kens(4)=12
           end select
           !kens(5)=89
           kens(5)=0
           kclust(1)=ipdstmpl(17)
        else
           print *,'makepdsens: Don:t know GRIB2 PDT 4.',ipdsnum
           iret=2
        endif

        return
        end

