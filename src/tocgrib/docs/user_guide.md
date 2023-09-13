# tocgrib

# Introduction

This program reads selected GRIB2 fields from a file, adds a TOC
Flag Field separator block and WMO Header in front of each GRIB2
field, and writes them out to a new file. The output file is in the
format required for TOC's FTP Input Service, which can be used to
disseminate the GRIB bulletins. This service is described at
http://weather.gov/tg/ftpingest.html.

### Input Files
 - 5 list of grib fields and associated wmo headers.
 - 11 input grib file.
 - 31 corresponding input grib index file.
 - parm pass in 4 characters 'kwbx' with parm field

### Output Files (Including Scratch Files)
 - 6 standard fortran print file
 - 51 output grib bulletin file in toc format
