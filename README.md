![Status](https://github.com/NOAA-EMC/NCEPLIBS-grib_util/workflows/developer/badge.svg)

# NCEPLIBS-grib_util

This is a collection of NCEP GRIB related utilities, for GRIB1 and
GRIB2.

GRIdded Binary or General Regularly-distributed Information in Binary
form (GRIB) is a data format for meteorological and forecast data,
standardized by the World Meteorological Organization (WMO). GRIB
edition 1 was approved by the WMO Working Group on Data Management
(WGDM) in 1994. GRIB edition 2 (GRIB2) was approved by the WGDM in
2003.

This is part of the [NCEPLIBS](https://github.com/NOAA-EMC/NCEPLIBS)
project.

For more information see the [NCEPLIBS-grib_util
documentation](https://noaa-emc.github.io/NCEPLIBS-grib_util/) and the
[NCEP WMO GRIB2
Documentation](https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/).

To submit bug reports, feature requests, or other code-related issues
including installation and usage questions, please create a [GitHub
issue](https://github.com/NOAA-EMC/NCEPLIBS-grib_util/issues). For general
NCEPLIBS inquiries, contact [Edward
Hartnett](mailto:edward.hartnett@noaa.gov) (secondary point of contact
[Alex Richert](mailto:alexander.richert@noaa.gov)).

## The grib_util Utilities

Utility | Purpose
--------|--------
cnvgrib | Convert between GRIB1 and GRIB2.
copygb | Copy all or part of a GRIB1 file.p
copygb2 | Copy all or part of a GRIB2 file.
degrib2 | Inventory a GRIB2 file.
grb2index | Create an index from a GRIB1 file.
grbindex | Create an index from a GRIB2 file.
tocgrib | Copy some GRIB2 fields to a new GRIB1 file.
tocgrib2 | Copy some GRIB2 fields to a new GRIB2 file.
tocgrib2super | Copy some GRIB2 fields to a new GRIB2 file with super WMO header.
wgrib | Manipulate GRIB1 files.

## Prerequisite External Projects

Project | Notes
-----------|------
[Jasper](http://www.ece.uvic.ca/~mdadams/jasper/) | [JPEG-2000](http://www.jpeg.org/JPEG2000.html) library
[libpng](http://www.libpng.org/pub/png/libpng.html) | PNG compression library
[zlib](http://www.zlib.net/) | zlib compression library

## Prerequisite NCEPLIBS Projects

Repository | Notes
-----------|------
[NCEPLIBS-bacio](https://github.com/NOAA-EMC/NCEPLIBS-bacio) | binary I/O for NCEP models
[NCEPLIBS-ip](https://github.com/NOAA-EMC/NCEPLIBS-ip) | interpolating between NCEP grids
[NCEPLIBS-sp](https://github.com/NOAA-EMC/NCEPLIBS-sp) | spectral transform functions - only needed for ip v4 and lower
[NCEPLIBS-g2](https://github.com/NOAA-EMC/NCEPLIBS-g2) | Fortran implementation of the GRIB 2 functions
[NCEPLIBS-w3emc](https://github.com/NOAA-EMC/NCEPLIBS-w3emc) | GRIB1 library

## Other Related NCEPLIBS Projects

Repository | Notes
-----------|------
[NCEPLIBS-g2c](https://github.com/NOAA-EMC/NCEPLIBS-g2c) | C implementation of the GRIB 2 functions
[NCEPLIBS-g2tmpl](https://github.com/NOAA-EMC/NCEPLIBS-g2tmpl) | Utilities for GRIB2 templates

## Authors

Utility | Author(s) | User(s)
--------|-----------|--------
cnvgrib | Stephen Gilbert, Gordon, Mark Iredell, Boi Vuong | 
copygb | Stephen Gilbert, Mark Iredell, Trojan, Boi Vuong | UFS_UTILS
copygb2 | Stephen Gilbert, Mark Iredell, Boi Vuong | 
degrib2 | Stephen Gilbert, Boi Vuong | many GRIB2 users
grb2index | Mark Iredell, Stephen Gilbert, Boi Vuong | 
grbindex | W. Ebisuzaki, Farley, Stephen Gilbert, Mark Iredell, Boi Vuong | FAA and AWIPS (CONUS grid id 211)
tocgrib | Farley, Stephen Gilbert, R. E. Jones, Boi Vuong | RAP for FAA
tocgrib2 | Farley, Stephen Gilbert, R. E. Jones, Boi Vuong  | (GFS, NAM, SMOKE, RAP, HRRR, NWPS, etc.) in production for AWIPS and NDFD
tocgrib2super | Farley, Stephen Gilbert, R. E. Jones, Boi Vuong  | (GFS, NAM, SMOKE, RAP, HRRR, NWPS, etc.) in production for AWIPS and NDFD
wgrib | W. Ebisuzaki | FAA and AWIPS (CONUS grid id 211)

Code Manager : [Hang Lei](mailto:hang.lei@noaa.gov), [Ed
Hartnett](mailto:edward.hartnett@noaa.gov)

## Installing

```
mkdir build
cd build
cmake -DCMAKE_INSTALL_PREFIX=/path/to/install -DCMAKE_PREFIX_PATH=/path/to/dependencies ..
make -j4
make install
```

## Disclaimer

The United States Department of Commerce (DOC) GitHub project code is
provided on an "as is" basis and the user assumes responsibility for
its use. DOC has relinquished control of the information and no longer
has responsibility to protect the integrity, confidentiality, or
availability of the information. Any claims against the Department of
Commerce stemming from the use of its GitHub project will be governed
by all applicable Federal law. Any reference to specific commercial
products, processes, or services by service mark, trademark,
manufacturer, or otherwise, does not constitute or imply their
endorsement, recommendation or favoring by the Department of
Commerce. The Department of Commerce seal and logo, or the seal and
logo of a DOC bureau, shall not be used in any manner to imply
endorsement of any commercial product or activity by DOC or the United
States Government.

