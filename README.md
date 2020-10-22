# NCEP Grib related utilities

A collection of NCEP grib related utilities. This is related to the
[NCEPLIBS](https://github.com/NOAA-EMC/NCEPLIBS) project.

## Authors

NCEP/EMC developers.

Code Manager : Boi Vuong

### Prerequisites

This package requires the following third party libraries:
- [Jasper](http://www.ece.uvic.ca/~mdadams/jasper/)
- [libpng](http://www.libpng.org/pub/png/libpng.html)
- [libz](http://www.gzip.org/zlib/)

This package requires the folling NCEPLIBS libraries:
- [NCEPLIBS-sp](https://github.com/NOAA-EMC/NCEPLIBS-sp)
- [NCEPLIBS-ip](https://github.com/NOAA-EMC/NCEPLIBS-ip)
- [NCEPLIBS-bacio](https://github.com/NOAA-EMC/NCEPLIBS-bacio)
- [NCEPLIBS-g2](https://github.com/NOAA-EMC/NCEPLIBS-g2)
- [NCEPLIBS-w3nco](https://github.com/NOAA-EMC/NCEPLIBS-w3nco)

## Installing

```
mkdir build
cd build
cmake -DCMAKE_INSTALL_PREFIX=/path/to/install /path/to/NCEPLIBS-grib_util
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

