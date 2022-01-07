# NCEPLIBS-grib_util

This is a collection of NCEP GRIB related utilities. This is related
to the [NCEPLIBS](https://github.com/NOAA-EMC/NCEPLIBS) project.

For complete documentation see
https://noaa-emc.github.io/NCEPLIBS-grib_util/. For the NCEP WMO GRIB2
Documentation see
https://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc/.

## Authors

Utility | Author(s) | User(s)
--------|-----------|--------
cnvgrib | Stephen Gilbert, Gordon, Boi Vuong | ???
copygb | Mark Iredell, Stephen Gilbert, Trojan, Boi Vuong | UFS_UTILS
copygb2 | Mark Iredell, Stephen Gilbert, Boi Vuong | ???
degrib2 | Boi Vuong | ???
grb2index | Mark Iredell, Stephen Gilbert, Boi Vuong | ???
grbindex | Mark Iredell, Stephen Gilbert, Boi Vuong, W. Ebisuzaki | FAA and AWIPS (CONUS grid id 211)
tocgrib | Stephen Gilbert, Boi Vuong, Farley, R. E. Jones | RAP for FAA
tocgrib2 | Stephen Gilbert, Boi Vuong | (GFS, NAM, SMOKE, RAP, HRRR, NWPS, etc.) in production for AWIPS  and NDFD
tocgrib2super | Stephen Gilbert, Boi Vuong | (GFS, NAM, SMOKE, RAP, HRRR, NWPS, etc.) in production for AWIPS  and NDFD
wgrib | W. Ebisuzaki | FAA and AWIPS (CONUS grid id 211)

Code Manager : Hang Lei, Edward Hartnett

## Prerequisites

This package requires the following third party libraries:
- [Jasper](http://www.ece.uvic.ca/~mdadams/jasper/)
- [libpng](http://www.libpng.org/pub/png/libpng.html)
- [libz](http://www.gzip.org/zlib/)

This package requires the folling NCEPLIBS libraries:
- [NCEPLIBS-sp](https://github.com/NOAA-EMC/NCEPLIBS-sp)
- [NCEPLIBS-ip](https://github.com/NOAA-EMC/NCEPLIBS-ip)
- [NCEPLIBS-bacio](https://github.com/NOAA-EMC/NCEPLIBS-bacio)
- [NCEPLIBS-g2](https://github.com/NOAA-EMC/NCEPLIBS-g2)
- [NCEPLIBS-w3nco](https://github.com/NOAA-EMC/NCEPLIBS-w3nco) (before version 1.3.0)
- [NCEPLIBS-w3emc](https://github.com/NOAA-EMC/NCEPLIBS-w3emc) (starting version 1.3.0)

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

