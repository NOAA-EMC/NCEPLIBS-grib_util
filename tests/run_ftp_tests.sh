#!/bin/sh
# This is a test script for the NCEPLIBS-grib_util project.
#
# This script tests with GRIB2 data files downloaded from the NOAA EMC FTP site.
#
# Ed Hartnett, 8/8/22

set -e
echo ""
echo "*** Running FTP file tests"

# Convert test file to GRIB1.
../src/cnvgrib/cnvgrib -g21 WW3_Regional_US_West_Coast_20220718_0000.grib2 test_WW3_West.grib1

# Generate an inventory of the GRIB1 file.
../src/wgrib/wgrib test_WW3_West.grib1 &> test_WW3_West.grib1.inventory.txt

# Check against expected output.
#cmp test_gdaswave.t00z.wcoast.0p16.f000.grib1.inventory.txt ref_gdaswave_grib1_inventory.txt

# Convert GRIB1 output back to GRIB2.
#../src/cnvgrib/cnvgrib -g12 test_gdaswave.t00z.wcoast.0p16.f000.grib1 test_gdaswave.t00z.wcoast.0p16.f000.grib2

# Create an index of a GRIB2 file.
#../src/grb2index/grb2index test_gdaswave.t00z.wcoast.0p16.f000.grib2 test_gdaswave.t00z.wcoast.0p16.f000.grib2.idx

# Check against expected output. First 120 bytes contain differences,
# so ignore them.
#cmp -i 120 test_gdaswave.t00z.wcoast.0p16.f000.grib2.idx ref_gdaswave.t00z.wcoast.0p16.f000.grib2.idx

# Convert test file to another GRIB2 file.
#../src/cnvgrib/cnvgrib -g22 ref_gdaswave.t00z.wcoast.0p16.f000.grib2 test_gdaswave.t00z.wcoast.0p16.f000_2.grib2

# Create an index of the new GRIB2 file.
#../src/grb2index/grb2index test_gdaswave.t00z.wcoast.0p16.f000_2.grib2 test_gdaswave.t00z.wcoast.0p16.f000_2.grib2.idx

# Check against expected output. First 120 bytes contain differences,
# so ignore them.
#cmp -i 120 test_gdaswave.t00z.wcoast.0p16.f000_2.grib2.idx ref_gdaswave.t00z.wcoast.0p16.f000_2.grib2.idx

echo "*** SUCCESS!"
exit 0
