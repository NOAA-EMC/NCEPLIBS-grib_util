@mainpage

# Introduction

The NCEPLIBS-grib_util project contains a collection of NCEP grib
related utilities. This is part of the
[NCEPLIBS](https://github.com/NOAA-EMC/NCEPLIBS) project.

The utilities:
- <a href="cnvgrib/index.html">cnvgrib</a> - Convert between GRIB1 and GRIB2.
- <a href="copygb/index.html">copygb</a> - Copy all or part of a GRIB1 file.
- <a href="copygb2/index.html">copygb2</a> - Copy all or part of a GRIB2 file.
- <a href="degrib2/index.html">degrib2</a> - Inventory a GRIB2 file.
- <a href="grbindex/index.html">grbindex</a> - Create an index from a GRIB1 file.
- <a href="grb2index/index.html">grb2index</a> - Create an index from a GRIB2 file.
- <a href="tocgrib/index.html">tocgrib</a> - Copy some GRIB2 fields to a new GRIB1 file.
- <a href="tocgrib2/index.html">tocgrib2</a> - Copy some GRIB2 fields to a new GRIB2 file.
- <a href="tocgrib2super/index.html">tocgrib2super</a> - Copy some GRIB2 fields to a new GRIB2 file with super WMO header.
- <a href="wgrib/index.html">wgrib</a> - Manipulate GRIB1 files.

## Installation

```
git clone https://github.com/NOAA-EMC/NCEPLIBS-grib_util # or download a release from https://github.com/NOAA-EMC/NCEPLIBS-grib_util/releases
# Use $CMAKE_PREFIX_PATH shell variable or -DCMAKE_PREFIX_PATH to point to dependencies
cmake -S NCEPLIBS-grib_util -B NCEPLIBS-grib_util/build # <add'l CMake options>
cmake --build NCEPLIBS-grib_util/build
ctest --test-dir NCEPLIBS-grib_util/build # <add'l CTest options>
# Install to CMAKE_INSTALL_PREFIX (/usr/local by default):
cmake --install NCEPLIBS-grib_util/build
```

The following CMake build options can be used to configure the build by setting them with `-D<OPTION>=<VALUE>`.

| Option | Description | Default |
|--------|-------------|---------|
| CMAKE_INSTALL_PREFIX | Installation path | /usr/local |
| CMAKE_POSITION_INDEPENDENT_CODE | Enable position-independent code (PIC) for static build | OFF |
| OPENMP | Use OpenMP threading | OFF |
| ENABLE_DOCS | Enable generation of doxygen-based documentation. | OFF |
| FTP_TEST_FILES | Fetch and test with files on FTP site. | OFF |
| FTP_LARGE_TEST_FILES | Fetch and test with very large files on FTP site. | OFF |
| FTP_EXTRA_TEST_FILES | Test with even more large files available via FTP. | OFF |
| G2C_COMPARE | Enable copygb2 tests using g2c_compare | OFF |

## Documentation for Previous Versions of NCEPLIBS-grib_util

* [NCEPLIBS-grib_util Version 1.4.0](ver-1.4.0/index.html)
* [NCEPLIBS-grib_util Version 1.3.0](ver-1.3.0/index.html)
* [NCEPLIBS-grib_util Version 1.2.3](ver-1.2.3/index.html)
