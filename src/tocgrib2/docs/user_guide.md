# tocgri2

# Introduction

This program reads selected GRIB2 fields from a file, adds a TOC Flag
Field separator block and WMO Header in front of each GRIB2 field, and
writes them out to a new file. The output file is in the format
required for TOC's FTP Input Service, which can be used to disseminate
the GRIB2 bulletins. This service is described at
http://weather.gov/tg/ftpingest.html.

The "EXTRACT" variable in the namelist allows users to choose whether
they want the entire GRIB2 message containing the requested field
(extract=.false.), OR a GRIB2 message containing only the requested
field (extract=.true.). Both options return the same message if the
requested field is the only field in the GRIB2 message.

### Input Files
- 5 namelist of grib fields and associated wmo headers.
- 11 input grib2 file.
- 31 corresponding input grib2 index file.

### Output Files (Including Scratch Files)
- 6 standard fortran print file
- 51 output grib bulletin file in toc format

## Return Values

-  0 - Successful run
- 10 - Error opening input GRIB2 data file
- 20 - Error opening output GRIB transmission file
- 19 - Error reading control card file - all bulletins missing
- 30 - Some bulletins are missing
