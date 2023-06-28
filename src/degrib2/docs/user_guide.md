# degrib2

# Introduction

Inventory a GRIB2 file.

The degrib2 utility prints a summary of a GRIB2 file.

The degrib2 output for NCEPLIBS-g2c test data file seaice.t00z.grb.grib2 is:

'''
>degrib2 seaice.t00z.grb.grib2 

 GRIB MESSAGE  1  starts at 1

  SECTION 0:  10 2 51345
  SECTION 1:  7 0 2 1 1 2022 11 17 0 0 0 0 1
  Contains  0  Local Sections  and  1  data fields.

  FIELD  1
  SECTION 0:  10 2
  SECTION 1:  7 0 2 1 1 2022 11 17 0 0 0 0 1
  SECTION 3:  0 259200 0 0 0
  GRID TEMPLATE 3. 0 :  6 0 0 0 0 0 0 720 360 0 0 89750000 250000 48 -89750000 359750000 500000 500000 0
  NO Optional List Defining Number of Data Points.
  PRODUCT TEMPLATE 4. 0: ( PARAMETER = ICEC     10 2 0 )  2 0 2 0 120 0 0 1 0 101 0 0 255 0 0
  FIELD: ICEC     Mean Sea Level valid  0 hour after 2022111700:00:00
  NO Optional Vertical Coordinate List.
  Num. of Data Points =  259200     NO BIT-MAP 
  DRS TEMPLATE 5. 40 :  0 0 2 8 0 0 255
  Data Values:
  Num. of Data Points =  259200   Num. of Data Undefined = 0
( PARM= ICEC ) :  MIN=               0.00000000 AVE=               0.64117813 MAX=               1.56999993
  
  Total Number of Fields Found =  1
'''  
