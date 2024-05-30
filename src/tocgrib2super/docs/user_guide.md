# tocgrib2super

# Introduction

This program reads selected GRIB2 fields from a file, adds a flag Field
separator block and the size in bytes of the grib file, and the WMO
super header and the time stamp, adds a TOC Flag Field separator
block and WMO Header in front of each GRIB2 field, and writes them
out to a new file. The output file is in the format required for
TOC's FTP Input Service, which can be used to disseminate the GRIB2
bulletins. This service is described at
http://weather.gov/tg/ftpingest.html.

The "EXTRACT" variable in the namelist allows users to choose
whether they want the entire GRIB2 message containing the requested
field (extract=.false.), OR a GRIB2 message containing only the
requested field (extract=.true.). Both options return the same
message if the requested field is the only field in the GRIB2
message.

This program is used by operational applications including:
 - [Air Quality Model (AQM)](https://www.nco.ncep.noaa.gov/pmb/products/aqm/)
 - [Gridded LAMP (GLMP)](https://vlab.noaa.gov/web/mdl/gridded-lamp)
 - [Rapid Refresh (RAP)](https://rapidrefresh.noaa.gov/)

### Input Files

- 5 Namelist of grib fields and associated wmo headers.
- 11 Input grib2 file.
- 12 Get the file size of the grib file
- 31 Corresponding input grib2 index file.

### Output Files  (Including Scratch Files)

- 6 standard fortran print file
- 51 output grib bulletin file in toc format

## Return Values

-  0 - Successful run
- 10 - Error opening input GRIB2 data file
- 20 - Error opening output GRIB transmission file
- 19 - error reading control card file - all bulletins missing
- 30 - some bulletins are missing
