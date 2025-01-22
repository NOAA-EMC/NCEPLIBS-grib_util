# tocgrib2

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

This program is used by a number of operational applications, 
including:
 - ECMWF
 - [Extra-Tropical Storm Surge (ETSS)](https://slosh.nws.noaa.gov/etsurge/)
 - [Global Ensemble Forecast System (GEFS)](https://www.ncei.noaa.gov/products/weather-climate-models/global-ensemble-forecast)
 - [Global Forecast System (GFS)](https://www.ncei.noaa.gov/products/weather-climate-models/global-forecast)
 - [Gridded LAMP (GLMP)](https://vlab.noaa.gov/web/mdl/gridded-lamp)
 - [Great Lakes Wave model (GLWU)](https://polar.ncep.noaa.gov/waves/index.php)
 - [High-Resolution Window Forecast System (HiResW)](https://www.nco.ncep.noaa.gov/pmb/products/hiresw/)
 - [High Resolution Ensemble Forecast (HREF)](https://mag.ncep.noaa.gov/model-guidance-model-parameter.php?group=Model%20Guidance&model=HREF&area=CONUS&ps=area)
 - [High-Resolution Rapid Refresh (HRRR)](https://rapidrefresh.noaa.gov/hrrr/)
 - [Localized Aviation MOS Program (LMP)](https://vlab.noaa.gov/web/mdl/lamp)
 - [Nearshore Wave Prediction System (NWPS)](https://polar.ncep.noaa.gov/nwps/)
 - [Rapid Refresh (RAP)](https://rapidrefresh.noaa.gov/)
 - [Real-Time & Unrestricted Mesoscale Analysis (RTMA/URMA)](https://www.nco.ncep.noaa.gov/pmb/products/rtma/)
 - [Real-Time Ocean Forecast System (RTOFS)](https://polar.ncep.noaa.gov/global/)
 - seaice_analysis
 - [Short-Range Ensemble Forecast (SREF)](https://www.nco.ncep.noaa.gov/pmb/products/sref/)
 - [Wind Speed Probabilities (WSP)](https://www.nhc.noaa.gov/gis/)

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
